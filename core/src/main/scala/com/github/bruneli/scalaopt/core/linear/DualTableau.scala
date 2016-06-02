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
import SeqDataSetConverter._
import ConstraintOperator._
import SimplexPhase._

import scala.util.{Failure, Success, Try}

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
  columns: DataSet[TableauColumn],
  rhs: TableauColumn,
  constraintTypes: Vector[ConstraintOperator],
  negativeColumn: Option[TableauColumn] = None) extends SimplexTableau {

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
  def addLinearConstraint(constraint: LinearConstraint): DualTableau = {
    if (constraint.b >= 0.0) {
      // Remove slack variables to re-include them at the tail of the queue
      val nonSlackColumns = columns.filter(!_.isSlack)
      // Initial tableau size
      val m0 = this.rhs.constrains.size
      val n0 = nonSlackColumns.size.toInt
      // Updated tableau size
      val m = Math.max(m0, constraint.a.size.toInt)
      // Transform the constraint into an equality constraint and then a column
      val resizedConstraint = constraint.resize(m).toColumn(n0.toLong)
      // Add as many slack variables as there are rows
      val slackVariables = (0 until m).map {
        i => TableauColumn(0.0, 0.0, Vector(), n0 + i, i, true, true, false)
      }
      val constraintTypes = (0 until m).toVector.map(i => ConstraintOperator.LE)
      // Add the new column to existing ones
      val modifiedColumns = nonSlackColumns ++ Seq(resizedConstraint) ++ slackVariables
      // Recompute the negative column
      val negativeColumn = this.negativeColumn.map(column => getNegativeColumn(modifiedColumns))
      // Build the new tableau with modified information
      DualTableau(modifiedColumns, rhs, constraintTypes, negativeColumn)
    } else {
      // If constraint has a negative right-hand side, invert it
      addLinearConstraint(constraint.withPositiveRhs)
    }
  }

  /**
   * Add a set of constraints to the tableau
   *
   * @param constraints comma separatated list of constraints
   * @return extended tableau
   */
  override def subjectTo(constraints: Constraint*): DualTableau = {
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

  /**
   * Extract the solution vector of this tableau
   */
  override def solution: Variables = {
    dual
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

  def withPositiveRhs: SimplexTableau = {
    this.copy(
      columns = this.columns.map(checkSign(this.rhs)),
      rhs = checkSign(this.rhs)(this.rhs))
  }

  protected def checkSign(rhs: TableauColumn)(column: TableauColumn): TableauColumn = {
    if (column.isBasic) {
      if (rhs.constrains(column.row) < 0.0) {
        column.copy(
          constrains = (e(rhs.constrains.length, column.row) * -1.0).toVector,
          isBasic = false)
      } else {
        column
      }
    } else {
      val updatedConstraints = (column.constrains, rhs.constrains).zipped.map {
        case (constraint, rhsValue) => if (rhsValue < 0.0) -constraint else constraint
      }
      column.copy(constrains = updatedConstraints)
    }
  }

  /**
   * Pivot the tableau columns given a pivot column and row
   *
   * Pivot separately the variables A and the right hand side b.
   *
   * @param pivotColumn pivot column with its row index specifying the pivot row
   * @return pivoted tableau
   */
  override def pivot(pivotColumn: TableauColumn): SimplexTableau = {
    this.copy(
      columns = columns.map(_.pivot(pivotColumn)),
      rhs = rhs.pivot(pivotColumn),
      negativeColumn = negativeColumn.map(_.pivot(pivotColumn)))
  }

}

object DualTableau {

  /**
   * Given a linear cost function that should be minimized, build an initial tableau
   *
   * @param f linear objective function
   * @return tableau containing only the linear objective function
   */
  def min(f: Variables => Double): DualTableau = objective(f, true)

  /**
   * Given a cost vector, build a initial simplex tableau without any constraint
   *
   * @param c cost vector
   * @return tableau with only the linear cost function
   */
  def min(c: Variables): DualTableau = {
    val rhs = TableauColumn(0.0, 0.0, c.toVector, 1)
    DualTableau(Vector(), rhs, Vector())
  }

  /**
   * Given a linear cost function that should be maximized, build an initial tableau
   *
   * @param f linear objective function
   * @return tableau containing only the linear objective function
   */
  def max(f: Variables => Double): DualTableau = objective(f, false)

  /**
   * Given a cost vector, build a initial simplex tableau without any constraint
   *
   * @param c cost vector
   * @return tableau with only the linear cost function
   */
  def max(c: Variables): DualTableau = {
    val rhs = TableauColumn(0.0, 0.0, c.toVector, 1)
    DualTableau(Vector(), rhs, Vector())
  }

  /**
   * Given a linear cost function, build an initial primal tableau
   *
   * @param f            linear objective function
   * @param isMinimizing true if minimizing f, false if maximizing f
   * @return tableau containing only the linear objective function
   */
  def objective(
    f: Variables => Double,
    isMinimizing: Boolean): DualTableau = {
    def iterate(x: Vector[Double]): Int = Try(f(x)) match {
      case Failure(e) => iterate(x :+ 0.0)
      case Success(value) => x.size
    }
    val n = iterate(Vector(0.0))
    // Negate cost values when searching for minimal cost
    val costSign = 1.0 //if (isMinimizing) -1.0 else 1.0
    val constrains = (0 until n).map(i => costSign * f(e(n, i))).toVector
    val rhs = TableauColumn(0.0, 0.0, constrains, 1)
    DualTableau(Vector(), rhs, Vector())
  }

}