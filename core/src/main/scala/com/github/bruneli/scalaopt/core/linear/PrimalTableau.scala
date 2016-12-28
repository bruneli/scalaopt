/*
 * Copyright 2014 Renaud Bruneliere
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
import com.github.bruneli.scalaopt.core.constraint._
import com.github.bruneli.scalaopt.core.function.LinearObjectiveFunction
import com.github.bruneli.scalaopt.core.linalg.DenseVector
import com.github.bruneli.scalaopt.core.variable.{Constant, ContinuousVariable}

/**
 * Define a tableau used with the Standard Primal Simplex algorithm
 *
 * The linear programming problem:
 *
 * min cx
 * subject to
 * ax <= b,
 * x >= 0
 *
 * is rewritten in a matrix form:
 *
 * x1   x2   x2-   s1   s2   a1   a2   z
 * --------------------------------------
 * 0    0    0     0    0    -1   -1   w
 * -c1  -c2  c2    0    0    0    0    z
 * a11  a12  -a12  1    0    1    0    b1
 * a21  a22  -a22  0    1    0    1    b2
 *
 * with
 * - row1 = phase1 objective function
 * - row2 = phase2 objective function cx
 * - rows 3 & 4 = (inequality) constrains ax <= b
 * - x1 & x2 = decision variables
 * - x2- = additional variable in case x2 is not restricted to be >= 0 when defined
 * - s1 & s2 = additional slack variables to transform inequality constrains into equality constrains
 * - a1 & a2 = additional artificial variables to solve the phase1 problem
 *
 * @param columns         tableau columns except the right hand side and negative sum columns
 * @param rhs             tableau right hand side with objective function values and constrains parameters
 * @param constraintTypes equality type (>=, ==, <=) of every constraint
 * @param negativeColumn  optional column that represents the opposite of the sum of all columns
 * @author bruneli
 */
case class PrimalTableau(
  variables: ContinuousVariablesType,
  objective: ObjectiveType,
  columns: DataSet[TableauColumn],
  rhs: TableauColumn,
  constraintTypes: Vector[ConstraintOperator],
  negativeColumn: Option[TableauColumn] = None)
  extends SimplexTableau {

  /**
   * Extract the solution vector of this tableau
   */
  override def solution: DenseVector[ContinuousVariable] = {
    variables.withValues(primal.raw)
  }

  /**
   * Add a constraint to the problem to solve
   *
   * @param constraint constraint
   * @return problem augmented of one additional constraint
   */
  override def addConstraint(constraint: Constraint[ContinuousVariable]): PrimalTableau = {
    this.addLinearConstraint(this.toLinearConstraint(constraint, this.columns.size.toInt))
  }

  /**
   * Add a set of constraints to the tableau
   *
   * @param constraints comma separated list of constraints
   * @return extended tableau
   */
  override def subjectTo(constraints: Constraint[ContinuousVariable]*): PrimalTableau = {
    if (constraints.isEmpty) {
      this
    } else {
      val headConstraint = this.toLinearConstraint(constraints.head, this.columns.size.toInt)
      constraints.tail.foldLeft(this.addLinearConstraint(headConstraint)) {
        case (previousTableau, constraint) =>
          val n = previousTableau.columns.size.toInt
          val linearConstraint = this.toLinearConstraint(constraint, n)
          previousTableau.addLinearConstraint(linearConstraint)
      }
    }
  }

  override def toTableau: SimplexTableau = this

  override def pivot(simplexPhase: SimplexPhase)(pivotColumn: TableauColumn): SimplexTableau = {
    this.copy(
      columns = columns.map(_.pivot(simplexPhase)(pivotColumn)),
      rhs = rhs.pivot(simplexPhase)(pivotColumn),
      negativeColumn = negativeColumn.map(_.pivot(simplexPhase)(pivotColumn)))
  }

  /**
   * Remove all columns flagged as artificial variables
   */
  override def withoutArtificialVariables: PrimalTableau = {
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
  override def withArtificialVariables: PrimalTableau = {
    val (newColumns, newRhs) = addArtificialVariables(this.withoutArtificialVariables)
    this.copy(columns = newColumns, rhs = newRhs)
  }

  /**
   * Restrict decision variables to take only positive values
   */
  def withPositiveVariables: PrimalTableau = {
    this.copy(negativeColumn = None)
  }

  /**
   * Add an extra column with all sign reverted for all non-slack and non-artificial variables
   */
  def withNegativeVariables: PrimalTableau = {
    this.copy(
      negativeColumn = Some(getNegativeColumn(columns)))
  }

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
  def addLinearConstraint(constraint: LinearConstraint[ContinuousVariable]): PrimalTableau = {
    if (constraint.b >= 0.0) {
      // Initial tableau size
      val n0 = columns.size.toInt
      val m0 = this.rhs.constrains.size
      val isEquality = constraint.operator.isInstanceOf[EqualityOperator]
      // Updated tableau size
      val n = if (isEquality) {
        Math.max(n0, constraint.a.length)
      } else {
        Math.max(n0, constraint.a.length) + 1
      }
      // Transform the constraint into an equality constraint
      val resizedConstraint = if (isEquality) {
        constraint.toEquality(n)
      } else {
        constraint.toEquality(n, Some(n - 1))
      }
      // Add new columns like slack variables if necessary
      val resizedColumns = if (n > n0) {
        val newColumns = (n0 until n).map(addSlackVariable(n, m0, constraint.operator))
        columns ++ newColumns
      } else {
        columns
      }
      // Add the new constraint to the columns
      val modifiedColumns = resizedColumns.zip(resizedConstraint.a).map {
        case (column, newElement) => column.copy(constrains = column.constrains :+ Constant(newElement.x))
      }
      val rhs = this.rhs.copy(constrains = this.rhs.constrains :+ Constant(resizedConstraint.b))
      val constraintTypes = this.constraintTypes :+ constraint.operator
      // Recompute the negative column
      val negativeColumn = this.negativeColumn.map(column => getNegativeColumn(modifiedColumns))
      // Build the new tableau with modified information
      PrimalTableau(variables, objective, modifiedColumns, rhs, constraintTypes, negativeColumn)
    } else {
      // If constraint has a negative right-hand side, invert it
      addLinearConstraint(constraint.withPositiveRhs)
    }
  }

}

object PrimalTableau extends CPBuilder[ContinuousVariable, LinearObjectiveFunction[ContinuousVariable], PrimalTableau] {

  /**
   * Minimize an objective function acting on a set of optimization variables
   */
  override def min(
    objectiveFunction: LinearObjectiveFunction[ContinuousVariable],
    variables: ContinuousVariablesType): PrimalTableau = {
    objective(MINIMIZE, objectiveFunction.cost.toVector, -1.0, variables) // negative cost when minimizing
  }

  /**
   * Maximize an objective function acting on a set of optimization variables
   */
  override def max(
    objectiveFunction: LinearObjectiveFunction[ContinuousVariable],
    variables: ContinuousVariablesType): PrimalTableau = {
    objective(MAXIMIZE, objectiveFunction.cost.toVector, 1.0, variables)
  }

  private def objective(
    objectiveType: ObjectiveType,
    costVector: Vector[Constant],
    sign: Double,
    variables: ContinuousVariablesType): PrimalTableau = {
    require(costVector.length == variables.length,
      s"Optimization variables size ${variables.length} != linear coefficients ${costVector.length} size")
    // Prepare the columns, one per variable
    val columns = costVector.zipWithIndex.map {
      case (cost, index) => TableauColumn.costOnlyColumn(index, cost.x * sign)
    }
    val rhs = TableauColumn.costOnlyColumn(costVector.length, 0.0)
    // Add contraints to the tableau if some of the variable have lower or upper bounds
    val initialTableau = PrimalTableau(variables, objectiveType, columns, rhs, Vector())
    variables.zipWithIndex.foldLeft(initialTableau)(addVariableConstraints)
  }

  private def addVariableConstraints(
    tableau: PrimalTableau,
    variableAndIndex: (ContinuousVariable, Int)): PrimalTableau = {
    val (variable, index) = variableAndIndex
    val n = tableau.columns.size.toInt
    def addLowerBound(lowerBound: Double): LinearConstraint[ContinuousVariable] = {
      LinearConstraintBuilder(DenseVector.e[Constant](n, index)).ge(lowerBound)
    }
    def addUpperBound(upperBound: Double): LinearConstraint[ContinuousVariable] = {
      LinearConstraintBuilder(DenseVector.e[Constant](n, index)).le(upperBound)
    }
    (variable.lower.toList.filterNot(_ == 0.0).map(addLowerBound) ++
      variable.upper.toList.filterNot(_ == 0.0).map(addUpperBound))
      .foldLeft(tableau)(_.addLinearConstraint(_))
  }

}