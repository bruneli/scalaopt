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

import com.github.bruneli.scalaopt.core.SimplexPhase._
import com.github.bruneli.scalaopt.core._
import SeqDataSetConverter._
import com.github.bruneli.scalaopt.core.constraint._
import com.github.bruneli.scalaopt.core.function.LinearContinuousObjectiveFunction
import com.github.bruneli.scalaopt.core.linalg.{DenseVector, SimpleDenseVector}
import com.github.bruneli.scalaopt.core.variable._

import scala.util.Try

/**
 * Define a generic simplex tableau implemented either for the primal or the dual simplex method
 *
 * @author bruneli
 */
trait SimplexTableau extends LP with SimplexTableauHelpers {

  val columns: DataSet[TableauColumn]
  val rhs: TableauColumn
  val constraintTypes: Vector[ConstraintOperator]
  val negativeColumn: Option[TableauColumn]

  def optimum: Optimum[ContinuousVariable]

  /**
   * Extract the primal solution vector of this tableau
   */
  def primal: DenseVector[ContinuousVariable] = {
    val offset = negativeColumn.map {
      column: TableauColumn => if (column.isBasic) rhs.getConstraint(column.row) else 0.0
    }.getOrElse(0.0)
    val primalSolutions = columns
      .filter(isInitialColumn)
      .collect()
      .map(_.solution(rhs) - offset)
      .toArray
    new UnconstrainedVariables(primalSolutions)
  }

  /**
   * Extract the dual solution vector of this tableau
   */
  def dual: DenseVector[ContinuousVariable] = {
    val dualSolutions = columns
      .filter(_.isSlack)
      .map(_.phase2Cost * -1.0)
      .collect()
      .toArray
    new UnconstrainedVariables(dualSolutions)
  }

  /**
   * Get the objective function
   *
   * @return linear objective function
   */
  override def objectiveFunction: LinearContinuousObjectiveFunction[ContinuousVariable] = {
    val phase2Costs = columns.map(_.phase2Cost).collect().toArray
    LinearContinuousObjectiveFunction[ContinuousVariable](new Constants(phase2Costs))
  }

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
  override def constraint(i: Int): LinearConstraint[ContinuousVariable] = {
    val a = columns.map(_.getConstraint(i)).collect().toArray
    val left = LinearLeftOperand[ContinuousVariable](new Constants(a))
    LinearConstraint(left, EqualityOperator(), rhs.getConstraint(i))
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
   * Add negative variables if relevant
   */
  def withNegativeVariables: SimplexTableau

  /**
   * Pivot the tableau columns given a pivot column and row
   *
   * Pivot separately the variables A and the right hand side b.
   *
   * @param simplexPhase simplex phase
   * @param pivotColumn pivot column with its row index specifying the pivot row
   * @return pivoted tableau
   */
  def pivot(simplexPhase: SimplexPhase)(pivotColumn: TableauColumn): SimplexTableau

  /**
   * Find a pivot column and row and perform a pivot of the tableau around it
   *
   * @param simplexPhase simplex phase
   * @return pivoted tableau or failure
   */
  def performPivot(simplexPhase: SimplexPhase): Try[SimplexTableau] = {
    Try(pivotColumn(simplexPhase)).map(pivot(simplexPhase))
  }

  /**
   * Check if there are variables that can take negative values. If so, split them into x = x+ - x-
   */
  def checkNegativeVariables: SimplexTableau = {
    val isOnlyPositive = variables.force.forall(variable => variable.lower.isDefined && variable.lower.get >= 0.0)
    if (isOnlyPositive) {
      this
    } else {
      this.withNegativeVariables
    }
  }

  /**
   * Find the column with the smallest cost and set its row index to the row with the smallest min ratio
   *
   * @param phase look at simplex phase 1 or phase 2 cost
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
      val ratioVector = pivotColumn
        .constrains
        .zipAndMap(rhs.constrains, computeRatio)
      val (minRatioValue, minRatioIdx, _) = ratioVector
        .foldLeft((Double.PositiveInfinity, 0, 0)) {
          case (previous, ratio) =>
            val (minRatio, minRatioIdx, idx) = previous
            if (ratio.x < minRatio) {
              (ratio.x, idx, idx + 1)
            } else {
              (minRatio, minRatioIdx, idx + 1)
            }
        }
      if (minRatioValue >= Double.PositiveInfinity) {
        throw UnboundedProgramException("Linear program is unbounded")
      } else {
        pivotColumn.copy(row = minRatioIdx)
      }
    }
  }

  /**
   * Compute the ratio b_i / A_(i,j) except if b_i is negative as it leads to unbounded solutions
   *
   * @param row A_(i,j)
   * @param rhs b_i
   * @return b_i / A_(i,j)
   */
  private def computeRatio(row: Double, rhs: Double): Double = {
    if (row <= 0.0) Double.PositiveInfinity else rhs / row
  }

  protected def addArtificialVariables(tableau0: SimplexTableau) = {
    val m = tableau0.rhs.constrains.size
    val artificialConstraints =
      tableau0.constraintTypes.zipWithIndex.filter(_._1 != LowerOrEqualOperator)
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
        if (indices.contains(idx)) previous + cost.x else previous
    }
    column.copy(phase1Cost = scaledPhase1Cost)
  }

  private def addColumnCost(previousCost: Double, column: TableauColumn): Double = {
    previousCost + column.phase2Cost
  }

  protected def toLinearConstraint(
    constraint: Constraint[ContinuousVariable],
    n0: Int): LinearConstraint[ContinuousVariable] = {
    constraint.toLinearConstraint(Some(n0))(ContinuousVariableFromDouble)
      .getOrElse(throw new IllegalArgumentException(s"could not express constraint $constraint as a linear constraint"))
  }

  override def toString = {
    val phase1Cost = f"z1|${columns.collect().map(_.phase1Cost.toString).map(formatStr).mkString("|")}|${rhs.phase1Cost}%3.1f\n"
    val phase2Cost = f"z2|${columns.collect().map(_.phase2Cost.toString).map(formatStr).mkString("|")}|${rhs.phase2Cost}%3.1f\n"
    rhs.constrains.indices.foldLeft(phase1Cost + phase2Cost) {
      case (previous, index) =>
        previous + f"c$index|${columns.collect().map(_.getConstraint(index).toString).map(formatStr).mkString("|")}|${rhs.getConstraint(index)}%3.1f\n"
    }
  }

  private def formatStr(str: String): String = f"$str%6s"

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
  constrains: DenseVector[Constant],
  column: Long,
  row: Int = -1,
  isSlack: Boolean = false,
  isBasic: Boolean = false,
  isArtificial: Boolean = false) {

  def getConstraints(n: Int = 0): DenseVector[Constant] = {
    if (constrains.isEmpty) {
      DenseVector.e[Constant](Math.max(row, n), row)
    } else {
      constrains
    }
  }

  def getConstraint(index: Int): Double = {
    if (constrains.isEmpty) {
      if (index == row) 1.0 else 0.0
    } else {
      constrains(index).x
    }
  }

  def +(that: TableauColumn): TableauColumn = {
    TableauColumn(
      this.phase1Cost + that.phase1Cost,
      this.phase2Cost + that.phase2Cost,
      this.getConstraints() + that.getConstraints(),
      0)
  }

  /**
   * Multiply the content of a column by a constant factor
   */
  def *(multiplier: Double): TableauColumn = {
    this.copy(
      phase1Cost = phase1Cost * multiplier,
      phase2Cost = phase2Cost * multiplier,
      constrains = this.getConstraints() * multiplier)
  }

  /**
   * Build a new column with all sign reverted
   */
  def negate: TableauColumn = {
    this.copy(
      phase1Cost = -this.phase1Cost,
      phase2Cost = -this.phase2Cost,
      constrains = -this.getConstraints())
  }

  /**
   * Pivot a column with another column
   *
   * Pivoting column X with pivot column Y means that every value of X is changed to:
   * X_i := X_i - Y_i * X_j / Y_j if i !=j
   * X_j := X_j / Y_j
   * with j the pivoting row
   *
   * @param simplexPhase simplex phase
   * @param that pivot column
   * @return pivoted column
   */
  def pivot(simplexPhase: SimplexPhase)(that: TableauColumn): TableauColumn = {
    if (this.column == that.column) {
      // The entering column becomes a basic variable
      this.toBasicVariable(simplexPhase, that.row)
    } else if (this.isBasic && this.row != that.row) {
      // Basic variables are unchanged if their row index is different from the pivoting row
      this
    } else {
      // Column is rescaled according to the pivot column
      val constrains0 = getConstraints(that.constrains.size)
      val pivotValue = constrains0(that.row).x / that.getConstraint(that.row)
      val phase1Cost = this.phase1Cost - that.phase1Cost * pivotValue
      val phase2Cost = this.phase2Cost - that.phase2Cost * pivotValue
      //val phase1Cost = if (simplexPhase == PHASE1) this.phase1Cost - that.phase1Cost * pivotValue else this.phase1Cost
      //val phase2Cost = if (simplexPhase == PHASE2) this.phase2Cost - that.phase2Cost * pivotValue else this.phase2Cost
      val constrains = constrains0.mapWithIndex {
        (value, i) =>
          if (i == that.row) pivotValue else value - that.getConstraint(i) * pivotValue
      }
      val newColumn = this.copy(
        phase1Cost = phase1Cost,
        phase2Cost = phase2Cost,
        constrains = constrains,
        row = -1,
        isBasic = false)
      newColumn
    }
  }

  /**
   * Get the current solution
   */
  def solution(rhs: TableauColumn): Double = {
    if (isBasic) rhs.constrains(row).x else 0.0
  }

  /**
   * Fill the column with 0 except for the row-th element equal to 1
   */
  def toBasicVariable(simplexPhase: SimplexPhase, row: Int) = {
    this.copy(
      phase1Cost = if (simplexPhase == PHASE1) 0.0 else this.phase1Cost,
      phase2Cost = if (simplexPhase == PHASE2) 0.0 else this.phase2Cost,
      constrains = Constants(),
      row = row,
      isBasic = true)
  }

}

object TableauColumn {

  def costOnlyColumn(column: Long, phase2Cost: Double): TableauColumn = {
    TableauColumn(0.0, phase2Cost, Constants(), column)
  }

  def artificialVariable(column: Long, i: Int, n: Int): TableauColumn = {
    TableauColumn(
      0.0,
      0.0,
      DenseVector.e[Constant](n, i),
      column,
      -1,
      isArtificial = true,
      isBasic = true)
  }

}
