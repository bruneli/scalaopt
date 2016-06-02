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

import com.github.bruneli.scalaopt.core.ConstraintOperator._
import com.github.bruneli.scalaopt.core.SimplexPhase._
import com.github.bruneli.scalaopt.core._
import SeqDataSetConverter._

import scala.util.{Failure, Success, Try}

/**
 * Define a generic simplex tableau implemented either for the primal or the dual simplex method
 *
 * @author bruneli
 */
trait SimplexTableau extends ConstrainedObjectiveFunction {

  val columns: DataSet[TableauColumn]
  val rhs: TableauColumn
  val constraintTypes: Vector[ConstraintOperator]
  val negativeColumn: Option[TableauColumn]

  /**
   * Extract the primal solution vector of this tableau
   */
  def primal: Variables = {
    val offset = negativeColumn.map {
      column: TableauColumn => if (column.isBasic) rhs.constrains(column.row) else 0.0
    }.getOrElse(0.0)
    columns
      .filter(isInitialColumn)
      .collect()
      .map(_.solution(rhs) - offset)
  }

  /**
   * Extract the dual solution vector of this tableau
   */
  def dual: Variables = {
    columns
      .filter(_.isSlack)
      .map(_.phase2Cost * -1.0)
      .collect()
  }

  /**
   * Evaluate the objective function for a given vector of variables
   *
   * @param x vector of variables
   * @return real-valued objective function at x
   */
  override def apply(x: Variables): Double = columns.aggregate(0.0)(addColumnCost, _ + _)

  /**
   * Return the number of constraints
   */
  override def numberOfConstraints: Int = rhs.constrains.size

  /**
   * Get a constraint
   *
   * @param i index of the constraint
   * @return constraint
   */
  override def constraint(i: Int): Constraint = {
    val a = columns.map(_.constrains(i))
    LinearConstraint(a, EQ, rhs.constrains(i)).toConstraint
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
    simplexPhase: SimplexPhase): Boolean = simplexPhase match {
    case PHASE1 =>
      columns.filter(_.phase1Cost > epsilon).size == 0 &&
        negativeColumn.forall(_.phase1Cost < epsilon)
    case PHASE2 =>
      columns.filter(_.phase2Cost > epsilon).size == 0 &&
        negativeColumn.forall(_.phase2Cost < epsilon)
  }

  /**
   * Extract the primal solution vector of this tableau
   */
  def solution: Variables

  protected def isInitialColumn(column: TableauColumn) = {
    !(column.isSlack || column.isArtificial)
  }

  /**
   * Extract the total cost for a given simplex phase
   */
  def cost(simplexPhase: SimplexPhase): Double = simplexPhase match {
    case PHASE1 => rhs.phase1Cost
    case PHASE2 => rhs.phase2Cost
    case _ => ???
  }

  /**
   * Add new column corresponding to artificial variables for every equality or >= constraint.
   *
   * In this implementation, artificial variables have a phase-1 cost of 1, but following some matrix row
   * additions, the costs of artificial columns are set to 0 while other columns costs are scaled.
   *
   * @return extended tableau with artificial variables
   */
  def withArtificialVariables: SimplexTableau

  /**
   * Remove all columns flagged as artificial variables
   */
  def withoutArtificialVariables: SimplexTableau

  /**
   * Pivot the tableau columns given a pivot column and row
   *
   * Pivot separately the variables A and the right hand side b.
   *
   * @param pivotColumn pivot column with its row index specifying the pivot row
   * @return pivoted tableau
   */
  def pivot(pivotColumn: TableauColumn): SimplexTableau

  /**
   * Find a pivot column and row and perform a pivot of the tableau around it
   *
   * @param simplexPhase simplex phase
   * @return pivoted tableau or failure
   */
  def performPivot(simplexPhase: SimplexPhase): Try[SimplexTableau] = {
    Try(pivotColumn(simplexPhase)).map(pivot)
  }

  /**
   * Find the column with the smallest cost and set its row index to the row with the smallest min ratio
   *
   * @param phase   look at simplex phase 1 or phase 2 cost
   * @return column with the smallest cost
   */
  protected def pivotColumn(phase: SimplexPhase): TableauColumn = {
    val pivotCandidate = phase match {
      case PHASE1 =>
        val posCandidate = columns.maxBy(_.phase1Cost)
        if (negativeColumn.isDefined &&
          negativeColumn.get.phase1Cost > posCandidate.phase1Cost) {
          negativeColumn.get
        } else {
          posCandidate
        }
      case PHASE2 =>
        val posCandidate = columns.filter(!_.isArtificial).maxBy(_.phase2Cost)
        if (negativeColumn.isDefined &&
          negativeColumn.get.phase2Cost > posCandidate.phase2Cost) {
          negativeColumn.get
        } else {
          posCandidate
        }
    }
    minRatioRow(pivotCandidate, rhs)
  }

  /**
   * Find the row index with the minimum ratio b_i / A_(i,j)
   *
   * @param pivotColumn pivot column
   * @param rhs         equality constrains right hand side
   * @return pivot column with row index corresponding to minimum ratio
   * @throws UnboundedProgramException when all b_i are negative
   */
  protected def minRatioRow(
    pivotColumn: TableauColumn,
    rhs: TableauColumn): TableauColumn = {
    if (pivotColumn.isBasic) {
      pivotColumn
    } else {
      val (minRatioValue, minRatioIdx) = pivotColumn
        .constrains
        .zip(rhs.constrains)
        .map(computeRatio)
        .zipWithIndex
        .minBy(_._1)
      if (minRatioValue >= Double.PositiveInfinity) {
        throw new UnboundedProgramException("Linear program is unbounded")
      } else {
        pivotColumn.copy(row = minRatioIdx)
      }
    }
  }

  /**
   * Compute the ratio b_i / A_(i,j) except if b_i is negative as it leads to unbounded solutions
   *
   * @param values (b_i, A_(i,j))
   * @return b_i / A_(i,j)
   */
  private def computeRatio(values: (Double, Double)): Double = {
    val (row, rhs) = values
    if (row <= 0.0) Double.PositiveInfinity else rhs / row
  }

  protected def addArtificialVariables(tableau0: SimplexTableau) = {
    val m = tableau0.rhs.constrains.size
    val artificialConstraints =
      tableau0.constraintTypes.zipWithIndex.filter(_._1 != LE)
    val artificialConstraintIdx = artificialConstraints.map(_._2)
    val n0 = tableau0.columns.size.toInt
    val n = n0 + artificialConstraints.size
    val newColumns = (n0 until n)
      .map { column =>
        val row = artificialConstraints(column - n0)._2
        TableauColumn.artificialVariable(column, row, m)
      }
    val updatedColumns = tableau0.columns.map(scaleCost(artificialConstraintIdx)) ++ newColumns
    val updatedRhs = scaleCost(artificialConstraintIdx)(tableau0.rhs)
    (updatedColumns, updatedRhs)
  }

  protected def scaleCost(indices: Vector[Int])(column: TableauColumn): TableauColumn = {
    val scaledPhase1Cost = column.constrains.zipWithIndex.foldLeft(0.0) {
      case (previous, (cost, idx)) =>
        if (indices.contains(idx)) previous + cost else previous
    }
    column.copy(phase1Cost = scaledPhase1Cost)
  }

  protected def getNegativeColumn(columns: DataSet[TableauColumn]): TableauColumn = {
    val initialColumn = TableauColumn(0.0, 0.0, zeros(constraintTypes.size).toVector, 0)
    val isPositiveColumn = (column: TableauColumn) => !column.isSlack && !column.isArtificial
    val seqOp = (previous: TableauColumn, current: TableauColumn) => {
      previous + current.negate
    }
    columns
      .filter(isPositiveColumn)
      .aggregate(initialColumn)(seqOp, _ + _)
      .copy(column = -1, isSlack = false, isArtificial = false, isBasic = false)
  }

  private def addColumnCost(previousCost: Double, column: TableauColumn): Double = {
    previousCost + column.phase2Cost
  }

  protected def toLinearConstraint(constraint: Constraint, n0: Int): LinearConstraint = {
    def iterate(n: Int): LinearConstraint = LinearConstraint(constraint, n) match {
      case Success(linearConstraint) => linearConstraint
      case Failure(e) => iterate(n + 1)
    }
    iterate(n0)
  }

  protected def addSlackVariable(
    n: Int,
    m0: Int,
    op: ConstraintOperator)(
    i: Int): TableauColumn = {
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
      constrains = this.constrains.map(_ * -1.0))
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

  def costOnlyColumn(column: Long, phase2Cost: Double): TableauColumn = {
    TableauColumn(0.0, phase2Cost, Vector(), column)
  }

  def artificialVariable(column: Long, i: Int, n: Int): TableauColumn = {
    TableauColumn(0.0, 0.0, e(n, i).toVector, column, -1, isArtificial = true, isBasic = true)
  }

}