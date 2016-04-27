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
import ConstraintOperator._
import SeqDataSetConverter._

import scala.util.{Success, Failure, Try}

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
  columns: DataSet[TableauColumn],
  rhs: TableauColumn,
  constraintTypes: Vector[ConstraintOperator],
  negativeColumn: Option[TableauColumn] = None)
  extends SimplexTableau {

  /**
   * Extract the primal solution vector of this tableau
   */
  def solution: Variables = {
    val offset = negativeColumn.map {
      column: TableauColumn => if (column.isBasic) rhs.constrains(column.row) else 0.0
    }.getOrElse(0.0)
    columns
      .filter(isInitialColumn)
      .collect()
      .map(_.solution(rhs) - offset)
  }

  private def isInitialColumn(column: TableauColumn) = {
    !(column.isSlack || column.isArtificial)
  }

  /**
   * Add a set of constraints to the tableau
   *
   * @param constraints comma separatated list of constraints
   * @return extended tableau
   */
  override def subjectTo(constraints: Constraint*): PrimalTableau = {
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
   * Add a set of linear constraints to the tableau
   *
   * @param linearConstraints set of linear constraints
   * @return extended tableau
   */
  def subjectTo(linearConstraints: Set[LinearConstraint]): PrimalTableau = {
    if (linearConstraints.isEmpty) {
      this
    } else {
      linearConstraints.tail.foldLeft(this.addLinearConstraint(linearConstraints.head)) {
        case (previousTableau, linearConstraint) =>
          previousTableau.addLinearConstraint(linearConstraint)
      }
    }
  }

  override def pivot(pivotColumn: TableauColumn): SimplexTableau = {
    this.copy(
      columns = columns.map(_.pivot(pivotColumn)),
      rhs = rhs.pivot(pivotColumn),
      negativeColumn = negativeColumn.map(_.pivot(pivotColumn)))
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
  def addLinearConstraint(constraint: LinearConstraint): PrimalTableau = {
    if (constraint.b >= 0.0) {
      // Initial tableau size
      val n0 = columns.size.toInt
      val m0 = this.rhs.constrains.size
      // Updated tableau size
      val n = if (constraint.op == EQ) {
        Math.max(n0, constraint.a.size.toInt)
      } else {
        Math.max(n0, constraint.a.size.toInt) + 1
      }
      // Transform the constraint into an equality constraint
      val resizedConstraint = if (constraint.op == EQ) {
        constraint.toEquality(n)
      } else {
        constraint.toEquality(n, Some(n - 1))
      }
      // Add new columns like slack variables if necessary
      val resizedColumns = if (n > n0) {
        val newColumns = (n0 until n).map(addSlackVariable(n, m0, constraint.op))
        columns ++ newColumns
      } else {
        columns
      }
      // Add the new constraint to the columns
      val modifiedColumns = resizedColumns.zip(resizedConstraint.a).map {
        case (column, newElement) => column.copy(constrains = column.constrains :+ newElement)
      }
      val rhs = this.rhs.copy(constrains = this.rhs.constrains :+ resizedConstraint.b)
      val constraintTypes = this.constraintTypes :+ constraint.op
      // Recompute the negative column
      val negativeColumn = this.negativeColumn.map(column => getNegativeColumn(modifiedColumns))
      // Build the new tableau with modified information
      PrimalTableau(modifiedColumns, rhs, constraintTypes, negativeColumn)
    } else {
      // If constraint has a negative right-hand side, invert it
      addLinearConstraint(constraint.withPositiveRhs)
    }
  }

  private def addSlackVariable(n: Int, m0: Int, op: ConstraintOperator)(i: Int) = {
    val isSlack = i == (n - 1) && op != EQ
    val isBasic = isSlack && op == LE
    val row = if (isBasic) m0 else -1
    TableauColumn(
      0.0,
      0.0,
      zeros(m0).toVector,
      i,
      isSlack = isSlack,
      isBasic = isBasic,
      row = row)
  }

  private def toLinearConstraint(constraint: Constraint, n0: Int) = {
    def iterate(n: Int): LinearConstraint = LinearConstraint(constraint, n) match {
      case Success(linearConstraint) => linearConstraint
      case Failure(e) => iterate(n + 1)
    }
    iterate(n0)
  }

}

object PrimalTableau {

  /**
   * Given a linear cost function that should be minimized, build an initial tableau
   *
   * @param f linear objective function
   * @return tableau containing only the linear objective function
   */
  def min(f: Variables => Double): PrimalTableau = objective(f, true)

  /**
   * Given a cost vector, build a initial simplex tableau without any constraint
   *
   * @param c cost vector
   * @return tableau with only the linear cost function
   */
  def min(c: DataSet[Double]): PrimalTableau = {
    val columns = c.zipWithIndex.map {
      case (cost, index) => TableauColumn.costOnlyColumn(index, -cost) // negative cost when minimizing
    }
    PrimalTableau(columns, TableauColumn.costOnlyColumn(c.size, 0.0), Vector())
  }

  /**
   * Given a linear cost function that should be maximized, build an initial tableau
   *
   * @param f linear objective function
   * @return tableau containing only the linear objective function
   */
  def max(f: Variables => Double): PrimalTableau = objective(f, false)

  /**
   * Given a cost vector, build a initial simplex tableau without any constraint
   *
   * @param c cost vector
   * @return tableau with only the linear cost function
   */
  def max(c: DataSet[Double]): PrimalTableau = {
    val columns = c.zipWithIndex.map {
      case (cost, index) => TableauColumn.costOnlyColumn(index, cost)
    }
    PrimalTableau(columns, TableauColumn.costOnlyColumn(c.size, 0.0), Vector())
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
    isMinimizing: Boolean): PrimalTableau = {
    def iterate(x: Vector[Double]): Int = Try(f(x)) match {
      case Failure(e) => iterate(x :+ 0.0)
      case Success(value) => x.size
    }
    val n = iterate(Vector(0.0))
    // Negate cost values when searching for minimal cost
    val costSign = if (isMinimizing) -1.0 else 1.0
    val columns =
      (0 until n).map(i => TableauColumn(0.0, costSign * f(e(n, i)), Vector(), i))
    val rhs = TableauColumn(0.0, 0.0, Vector(), n + 1)
    PrimalTableau(columns, rhs, Vector())
  }

}