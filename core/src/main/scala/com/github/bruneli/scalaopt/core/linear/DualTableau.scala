/*
 * Copyright 2016 Renaud Bruneliere
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.bruneli.scalaopt.core.linear

import com.github.bruneli.scalaopt.core._
import com.github.bruneli.scalaopt.core.ObjectiveType._
import SeqDataSetConverter._
import SimplexPhase._
import ObjectiveType._
import com.github.bruneli.scalaopt.core.constraint._
import com.github.bruneli.scalaopt.core.function.LinearObjectiveFunction
import com.github.bruneli.scalaopt.core.linalg.{DenseVector, SimpleDenseVector}
import com.github.bruneli.scalaopt.core.variable.{Constant, ContinuousVariable}

import scala.util.{Failure, Success}

/**
 * Define a tableau used by the Dual Simplex Method
 *
 * Given the primal problem
 * min(cx) st Ax <= b
 * the dual linear program is defined as
 * max(by) st t_A y <= c
 * with t_A the transpose of matrix A
 *
 * @author bruneli
 */
case class DualTableau(
  variables: ContinuousVariablesType,
  objective: ObjectiveType,
  columns: DataSet[TableauColumn],
  rhs: TableauColumn,
  constraintTypes: Vector[ConstraintOperator],
  negativeColumn: Option[TableauColumn] = None) extends SimplexTableau {

  /**
   * Extract the solution vector of this tableau
   */
  override def solution: ContinuousVariablesType = {
    variables.withValues(dual.coordinates)
  }

  /**
   * Add a set of constraints to the tableau
   *
   * @param constraints comma separated list of constraints
   * @return extended tableau
   */
  override def subjectTo(constraints: Constraint[ContinuousVariable]*): DualTableau = {
    if (constraints.isEmpty) {
      this
    } else {
      val n = rhs.constrains.size
      val headConstraint = this.toLinearConstraint(constraints.head, n)
      constraints.tail.foldLeft(this.addLinearConstraint(headConstraint)) {
        case (previousTableau, constraint) =>
          val linearConstraint = this.toLinearConstraint(constraint, n)
          previousTableau.addLinearConstraint(linearConstraint)
      }
    }
  }

  /**
   * Add a constraint to the problem to solve
   *
   * @param constraint constraint
   * @return problem augmented of one additional constraint
   */
  override def addConstraint(constraint: Constraint[ContinuousVariable]): LP = {
    this.addLinearConstraint(constraint.toLinearConstraint()(ContinuousVariableFromDouble).get)
  }

  override def toTableau: SimplexTableau = this

  /**
   * Add a linear constraint to the existing tableau
   *
   * In case the new constraint include variables that are not yet included in the cost function,
   * the tableau is extended.
   * In case of a new inequality, a new column is created to host a slack variable.
   *
   * @param constraint a linear constraint
   * @return updated tableau
   */
  def addLinearConstraint(constraint: LinearConstraint[ContinuousVariable]): DualTableau = {
    if (constraint.b >= 0.0) {
      // Remove slack variables to re-include them at the tail of the queue
      val nonSlackColumns = columns.filter(!_.isSlack)
      // Initial tableau size
      val m0 = this.rhs.constrains.size
      val n0 = nonSlackColumns.size.toInt
      // Updated tableau size
      val m = Math.max(m0, constraint.a.size)
      val n = n0 + 1
      // Transform the constraint into an equality constraint and then a column
      val signedConstraint = constraint.toLinearConstraint(Some(m))(ContinuousVariableFromDouble) match {
        case Success(resizedConstraint) =>
          resizedConstraint.a.mapWithIndex {
            (value, i) =>
              if (constraintTypes(i) == LowerOrEqualOperator && objective == MINIMIZE ||
                  constraintTypes(i) == GreaterOrEqualOperator && objective == MAXIMIZE) {
                value
              } else {
                -value
              }
          }
        case Failure(e) => throw e
      }
      val sign = if (objective == MAXIMIZE) -1.0 else 1.0
      val newColumn = TableauColumn(0.0, sign * constraint.b, signedConstraint, n0)
      // Add as many slack variables as there are rows
      val slackVariables = constraintTypes.zipWithIndex.map {
        case (constraintType, i) => constraintType match {
          case LowerOrEqualOperator =>
            TableauColumn(0.0, 0.0, SimpleDenseVector(), n + i, i, true, true, false)
          case _ =>
            TableauColumn(0.0, 0.0, DenseVector.e[Constant](m, i) * -1.0, n + i, i, true, false, false)
        }
      }
      // Add the new column to existing ones
      val modifiedColumns = nonSlackColumns ++ Seq(newColumn) ++ slackVariables
      // Recompute the negative column
      val negativeColumn = this.negativeColumn.map(column => getNegativeColumn(modifiedColumns))
      // Build the new tableau with modified information
      DualTableau(variables, objective, modifiedColumns, rhs, constraintTypes, negativeColumn)
    } else {
      // If constraint has a negative right-hand side, invert it
      addLinearConstraint(constraint.withPositiveRhs)
    }
  }

  /**
   * Remove all columns flagged as artificial variables
   */
  override def withoutArtificialVariables: SimplexTableau = {
    this.copy(columns = columns.filter(!_.isArtificial))
  }

  /**
   * Add new column corresponding to artificial variables for every equality or >= constraint.
   *
   * In this implementation, artificial variables have a phase-1 cost of 1, but following some matrix row
   * additions, the costs of artificial columns are set to 0 while other columns costs are scaled.
   *
   * @return extended tableau with artificial variables
   */
  override def withArtificialVariables: SimplexTableau = {
    val (newColumns, newRhs) = addArtificialVariables(this.withoutArtificialVariables)
    this.copy(columns = newColumns, rhs = newRhs)
  }

  override def withNegativeVariables: DualTableau = {
    this
  }

  def withPositiveRhs: SimplexTableau = {
    this.copy(
      columns = this.columns.map(checkSign(this.rhs)),
      rhs = checkSign(this.rhs)(this.rhs))
  }

  protected def checkSign(rhs: TableauColumn)(column: TableauColumn): TableauColumn = {
    if (column.isBasic) {
      if (rhs.getConstraint(column.row) < 0.0) {
        column.copy(
          constrains = DenseVector.e[Constant](rhs.constrains.length, column.row) * -1.0,
          isBasic = false)
      } else {
        column
      }
    } else {
      val updatedConstraints = column.constrains.zipAndMap(
        rhs.constrains,
        (constraint, rhsValue) => if (rhsValue < 0.0) -constraint else constraint)
      column.copy(constrains = updatedConstraints)
    }
  }

  /**
   * Pivot the tableau columns given a pivot column and row
   *
   * Pivot separately the variables A and the right hand side b.
   *
   * @param simplexPhase simplex phase
   * @param pivotColumn  pivot column with its row index specifying the pivot row
   * @return pivoted tableau
   */
  override def pivot(simplexPhase: SimplexPhase)(pivotColumn: TableauColumn): SimplexTableau = {
    this.copy(
      columns = columns.map(_.pivot(simplexPhase)(pivotColumn)),
      rhs = rhs.pivot(simplexPhase)(pivotColumn),
      negativeColumn = negativeColumn.map(_.pivot(simplexPhase)(pivotColumn)))
  }

}

object DualTableau extends CPBuilder[ContinuousVariable, LinearObjectiveFunction[ContinuousVariable], DualTableau] {

  /**
   * Minimize an objective function acting on a set of optimization variables
   */
  override def min(
    objectiveFunction: LinearObjectiveFunction[ContinuousVariable],
    variables: ContinuousVariablesType): DualTableau = {
    objective(MINIMIZE, objectiveFunction.cost, variables)
  }

  /**
   * Maximize an objective function acting on a set of optimization variables
   */
  override def max(
    objectiveFunction: LinearObjectiveFunction[ContinuousVariable],
    variables: ContinuousVariablesType): DualTableau = {
    objective(MAXIMIZE, objectiveFunction.cost, variables)
  }

  private def objective(
    objectiveType: ObjectiveType,
    cost: DenseVector[Constant],
    variables: ContinuousVariablesType): DualTableau = {
    require(cost.length == variables.length,
      s"Optimization variables size ${variables.length} != linear coefficients ${cost.length} size")
    val constraintTypes = cost.toVector.map {
      value => objectiveType match {
        case MINIMIZE => if (value.x >= 0.0) LowerOrEqualOperator else GreaterOrEqualOperator
        case MAXIMIZE => if (value.x >= 0.0) GreaterOrEqualOperator else LowerOrEqualOperator
        case _ => throw new IllegalArgumentException(s"objective $objectiveType is not supported")
      }
    }
    val rhs = TableauColumn(0.0, 0.0, cost.mapValues(Math.abs), -2)
    DualTableau(variables, objectiveType, Vector(), rhs, constraintTypes)
  }

}