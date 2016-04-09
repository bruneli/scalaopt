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
import SeqDataSetConverter._

import scala.util.{Success, Failure, Try}

/**
 * Define a tableau used with the Standard Simplex algorithm
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
case class SimplexTableau(
  columns: DataSet[TableauColumn],
  rhs: TableauColumn,
  constraintTypes: Vector[ConstraintOperator.Value],
  negativeColumn: Option[TableauColumn] = None) extends ConstrainedObjectiveFunction {

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
  def addLinearConstraint(constraint: LinearConstraint): SimplexTableau = {
    if (constraint.b >= 0.0) {
      // Initial tableau size
      val n0 = columns.size.toInt
      val m0 = this.rhs.constrains.size
      // Updated tableau size
      val n = if (constraint.op == ConstraintOperator.Eq) {
        Math.max(n0, constraint.a.size.toInt)
      } else {
        Math.max(n0, constraint.a.size.toInt) + 1
      }
      // Transform the constraint into an equality constraint
      val resizedConstraint = if (constraint.op == ConstraintOperator.Eq) {
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
      SimplexTableau(modifiedColumns, rhs, constraintTypes, negativeColumn)
    } else {
      // If constraint has a negative right-hand side, invert it
      addLinearConstraint(constraint.withPositiveRhs)
    }
  }

  private def addSlackVariable(n: Int, m0: Int, op: ConstraintOperator.Value)(i: Int) = {
    val isSlack = i == (n - 1) && op != ConstraintOperator.Eq
    val isBasic = isSlack && op == ConstraintOperator.Le
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

  /**
   * Evaluate the objective function for a given vector of variables
   *
   * @param x vector of variables
   * @return real-valued objective function at x
   */
  override def apply(x: Variables): Double = columns.aggregate(0.0)(addColumnCost, _ + _)

  /**
   * Get a constraint
   *
   * @param i index of the constraint
   * @return constraint
   */
  override def constraint(i: Int): Constraint = {
    val a = columns.map(_.constrains(i))
    LinearConstraint(a, ConstraintOperator.Eq, rhs.constrains(i)).toConstraint
  }

  /**
   * Check if the phase1 and phase2 objective functions are optimal
   *
   * The basic feasible solution is optimal when all costs are negative or null.
   *
   * @param epsilon error precision
   * @return true when all cost are <= 0
   */
  def isOptimal(
    epsilon: Double,
    simplexPhase: SimplexPhase.Value): Boolean = simplexPhase match {
    case SimplexPhase.Phase1 =>
      columns.filter(_.phase1Cost > epsilon).size == 0 &&
        negativeColumn.forall(_.phase1Cost < epsilon)
    case SimplexPhase.Phase2 =>
      columns.filter(_.phase2Cost > epsilon).size == 0 &&
        negativeColumn.forall(_.phase2Cost < epsilon)
  }

  /**
   * Return the number of constraints
   */
  override def numberOfConstraints: Int = rhs.constrains.size

  /**
   * Pivot the tableau columns given a pivot column and row
   *
   * Pivot separately the variables A and the right hand side b.
   *
   * @param pivotColumn pivot column with its row index specifying the pivot row
   * @return pivoted tableau
   */
  def pivot(pivotColumn: TableauColumn): SimplexTableau = {
    this.copy(
      columns = columns.map(_.pivot(pivotColumn)),
      rhs = rhs.pivot(pivotColumn),
      negativeColumn = negativeColumn.map(_.pivot(pivotColumn)))
  }

  /**
   * Extract the solution of this tableau
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
    !(column.isSlack || column.isArtificial || column.isNegative)
  }

  private def addColumnToSolution(
    rhs: TableauColumn)(
    previous: Vector[Double],
    column: TableauColumn) = {
    val solution = column.solution(rhs)
    if (column.isNegative) {
      previous.updated(previous.size - 1, previous(previous.size - 1) - solution)
    } else {
      previous :+ solution
    }
  }

  /**
   * Add a set of constraints to the tableau
   *
   * @param constraints comma separatated list of constraints
   * @return extended tableau
   */
  override def subjectTo(constraints: Constraint*): SimplexTableau = {
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
   * Add new column corresponding to artificial variables for every equality or >= constraint.
   *
   * In this implementation, artificial variables have a phase-1 cost of 1, but following some matrix row
   * additions, the costs of artificial columns are set to 0 while other columns costs are scaled.
   *
   * @return extended tableau with artificial variables
   */
  def withArtificialVariables: SimplexTableau = {
    val tableau0 = this.withoutArtificialVariables
    val m = this.rhs.constrains.size
    val artificialConstraints =
      tableau0.constraintTypes.zipWithIndex.filter(_._1 != ConstraintOperator.Le)
    val artificialConstraintIdx = artificialConstraints.map(_._2)
    val n0 = tableau0.columns.size.toInt
    val n = n0 + artificialConstraints.size
    val newColumns = (n0 until n)
      .map { column =>
        val row = artificialConstraints(column - n0)._2
        TableauColumn.artificialVariable(column, row, m)
      }
    this.copy(
      columns = tableau0.columns.map(scaleCost(artificialConstraintIdx)) ++ newColumns,
      rhs = scaleCost(artificialConstraintIdx)(tableau0.rhs))
  }

  private def scaleCost(indices: Vector[Int])(column: TableauColumn) = {
    val scaledPhase1Cost = column.constrains.zipWithIndex.foldLeft(0.0) {
      case (previous, (cost, idx)) =>
        if (indices.contains(idx)) previous + cost else previous
    }
    column.copy(phase1Cost = scaledPhase1Cost)
  }

  /**
   * Remove all columns flagged as artificial variables
   */
  def withoutArtificialVariables: SimplexTableau = {
    this.copy(columns = columns.filter(!_.isArtificial))
  }

  /**
   * Restrict decision variables to take only positive values
   */
  def withPositiveVariables: SimplexTableau = {
    this.copy(columns = columns.filter(!_.isNegative), negativeColumn = None)
  }

  /**
   * Add an extra column with all sign reverted for all non-slack and non-artificial variables
   */
  def withNegativeVariables: SimplexTableau = {
    this.copy(
      columns = columns.filter(!_.isNegative), //.flatMap(splitIntoPosAndNegVars),
      negativeColumn = Some(getNegativeColumn(columns)))
  }

  /**
   * Solve the current tableau with specified method
   *
   * @param method method used to solve the linear programming problem
   * @param pars   parameters associated to the method
   * @tparam B type of class holding method configuration parameters
   * @return values of the decision variables at their optimum or failure
   */
  def solveWith[B <: ConfigPars](
    method: Optimizer[SimplexTableau, B])(
    implicit pars: B): Try[Variables] = {
    method.minimize(this, Vector.empty[Double])(pars)
  }

  private def addColumnCost(previousCost: Double, column: TableauColumn): Double = {
    previousCost + column.phase2Cost
  }

  private def toLinearConstraint(constraint: Constraint, n0: Int) = {
    def iterate(n: Int): LinearConstraint = LinearConstraint(constraint, n) match {
      case Success(linearConstraint) => linearConstraint
      case Failure(e) => iterate(n + 1)
    }
    iterate(n0)
  }

  /**
   * Split a decision variables into its positive and negative components.
   *
   * To do that, create 2 columns, one with positive components, one with negative components
   */
  private def splitIntoPosAndNegVars(column: TableauColumn): Vector[TableauColumn] = {
    if (column.isArtificial || column.isSlack) {
      Vector(column)
    } else {
      Vector(column, column.negate)
    }
  }

  private def getNegativeColumn(columns: DataSet[TableauColumn]): TableauColumn = {
    val initialColumn = TableauColumn(0.0, 0.0, zeros(constraintTypes.size).toVector, 0)
    val isPositiveColumn = (column: TableauColumn) => !column.isSlack && !column.isArtificial && !column.isNegative
    val seqOp = (previous: TableauColumn, current: TableauColumn) => {
      previous + current.negate
    }
    columns
      .filter(isPositiveColumn)
      .aggregate(initialColumn)(seqOp, _ + _)
      .copy(column = -1, isNegative = true, isSlack = false, isArtificial = false, isBasic = false)
  }

}

object SimplexTableau {

  /**
   * Given a linear cost function that should be minimized, build an initial tableau
   *
   * @param f linear objective function
   * @return tableau containing only the linear objective function
   */
  def min(f: Variables => Double): SimplexTableau = objective(f, true)

  /**
   * Given a linear cost function that should be maximized, build an initial tableau
   *
   * @param f linear objective function
   * @return tableau containing only the linear objective function
   */
  def max(f: Variables => Double): SimplexTableau = objective(f, false)

  /**
   * Given a linear cost function, build an initial tableau
   *
   * @param f            linear objective function
   * @param isMinimizing true if minimizing f, false if maximizing f
   * @return tableau containing only the linear objective function
   */
  def objective(
    f: Variables => Double,
    isMinimizing: Boolean): SimplexTableau = {
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
    SimplexTableau(columns, rhs, Vector())
  }

}

/**
 * Tableau column used to describe a decision variable
 *
 * @param phase1Cost   impact of the variable on the phase1 cost function
 * @param phase2Cost   impact of the variable on the phase2 cost function
 * @param constrains   impact of the non basic variable on the m constrains
 * @param column       column index
 * @param row          basic variable non null index or pivoting index
 * @param isSlack      true if it is a slack variable added for an inequality constraint
 * @param isNegative   true if it is the negative part of a decision variable
 * @param isBasic      true if this column is a basic variable
 * @param isArtificial true if this column is an artificial variable used to solve the phase1 problem
 */
case class TableauColumn(
  phase1Cost: Double,
  phase2Cost: Double,
  constrains: Vector[Double],
  column: Long,
  row: Int = -1,
  isSlack: Boolean = false,
  isNegative: Boolean = false,
  isBasic: Boolean = false,
  isArtificial: Boolean = false) {

  def +(that: TableauColumn): TableauColumn = {
    TableauColumn(
      this.phase1Cost + that.phase1Cost,
      this.phase2Cost + that.phase2Cost,
      (this.constrains + that.constrains).toVector,
      0)
  }

  /**
   * Multiply the content of a column by a constant factor
   */
  def *(multiplier: Double): TableauColumn = {
    this.copy(phase1Cost = phase1Cost * multiplier,
      phase2Cost = phase2Cost * multiplier,
      constrains = constrains.map(_ * multiplier))
  }

  /**
   * Build a new column with all sign reverted
   */
  def negate: TableauColumn = {
    this.copy(
      phase1Cost = -this.phase1Cost,
      phase2Cost = -this.phase2Cost,
      constrains = this.constrains.map(_ * -1.0),
      isNegative = !this.isNegative)
  }

  /**
   * Pivot a column with another column
   *
   * Pivoting column X with pivot column Y means that every value of X is changed to:
   * X_i := X_i - Y_i * X_j / Y_j if i !=j
   * X_j := X_j / Y_j
   * with j the pivoting row
   *
   * @param that pivot column
   * @return pivoted column
   */
  def pivot(that: TableauColumn): TableauColumn = {
    if (this.column == that.column) {
      // The entering column becomes a basic variable
      this.toBasicVariable(that.row)
    } else if (this.isBasic && this.row != that.row) {
      // Basic variables are unchanged if their row index is different from the pivoting row
      this
    } else {
      // Column is rescaled according to the pivot column
      val constrains0 = if (this.isBasic) {
        e(that.constrains.size, this.row).toVector
      } else {
        this.constrains
      }
      val pivotValue = constrains0(that.row) / that.constrains(that.row)
      val phase1Cost = this.phase1Cost - that.phase1Cost * pivotValue
      val phase2Cost = this.phase2Cost - that.phase2Cost * pivotValue
      val constrains = constrains0.zipWithIndex.map {
        case (value, i) =>
          if (i == that.row) pivotValue else value - that.constrains(i) * pivotValue
      }
      this.copy(
        phase1Cost = phase1Cost,
        phase2Cost = phase2Cost,
        constrains = constrains,
        row = -1,
        isBasic = false)
    }
  }

  /**
   * Get the current solution
   */
  def solution(rhs: TableauColumn): Double = {
    if (isBasic) rhs.constrains(row) else 0.0
  }

  /**
   * Fill the column with 0 except for the row-th element equal to 1
   */
  def toBasicVariable(row: Int) = {
    this.copy(
      phase1Cost = 0.0,
      phase2Cost = 0.0,
      constrains = Vector.empty[Double],
      row = row,
      isBasic = true)
  }

}

object TableauColumn {

  def artificialVariable(column: Long, i: Int, n: Int): TableauColumn = {
    TableauColumn(0.0, 0.0, e(n, i).toVector, column, -1, isArtificial = true, isBasic = true)
  }

}