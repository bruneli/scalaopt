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
import com.github.bruneli.scalaopt.core.function.{LinearContinuousObjectiveFunction, ObjectiveFunction}
import com.github.bruneli.scalaopt.core.linalg.{DenseVector, DenseVectorLike}
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

  override def optimum: Optimum[ContinuousVariable] = {
    Optimum(variables.withValues(primal.coordinates), rhs.phase2Cost)
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
      negativeColumn = Some(getNegativeColumn(columns, constraintTypes)))
  }

}

object PrimalTableau extends CPFactory[ContinuousVariable, PrimalTableauBuilder] {

  def given(variables: ContinuousVariablesType): PrimalTableauBuilder = {
    PrimalTableauBuilder(variables, None, Vector(), None, Vector())
  }

  implicit def canBuildFrom: CanBuildCPFrom[ContinuousVariable, PrimalTableau, PrimalTableau] = {
    new CanBuildCPFrom[ContinuousVariable, PrimalTableau, PrimalTableau] {

      def apply(tableau: PrimalTableau): PrimalTableauBuilder = {
        PrimalTableauBuilder(tableau.variables, Some(tableau.objective), tableau.columns, Some(tableau.rhs),
          tableau.constraintTypes, tableau.negativeColumn)
      }

    }
  }

}

case class PrimalTableauBuilder(variables: ContinuousVariablesType,
                                objective: Option[ObjectiveType],
                                columns: DataSet[TableauColumn],
                                rhs: Option[TableauColumn],
                                constraintTypes: Vector[ConstraintOperator],
                                negativeColumn: Option[TableauColumn] = None) extends LPBuilder[PrimalTableau] with SimplexTableauHelpers {

  type Self = PrimalTableauBuilder

  /**
    * Minimize an objective function acting on a set of optimization variables
    */
  def minimize(function: ObjectiveFunction[ContinuousVariable]): PrimalTableauBuilder = function match {
    case linear: LinearContinuousObjectiveFunction[_] =>
      objective(MINIMIZE, linear.cost.toVector, -1.0) // negative cost when minimizing
    case _ =>
      throw new IllegalArgumentException("objective function should be linear")
  }

  /**
    * Maximize an objective function acting on a set of optimization variables
    */
  def maximize(function: ObjectiveFunction[ContinuousVariable]): PrimalTableauBuilder = function match {
    case linear: LinearContinuousObjectiveFunction[_] =>
      objective(MAXIMIZE, linear.cost.toVector, 1.0)
    case _ =>
      throw new IllegalArgumentException("objective function should be linear")
  }

  override def addLinearConstraint(constraint: LinearConstraint[ContinuousVariable]): PrimalTableauBuilder = objective match {
    case Some(objType) =>
      if (constraint.b >= 0.0) {
        // Initial tableau size
        val n0 = columns.size.toInt
        val m0 = this.rhs.get.constrains.size
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
        val rhs = this.rhs.get.copy(constrains = this.rhs.get.constrains :+ Constant(resizedConstraint.b))
        val constraintTypes = this.constraintTypes :+ constraint.operator
        // Recompute the negative column
        val negativeColumn = this.negativeColumn.map(column => getNegativeColumn(modifiedColumns, constraintTypes))
        // Build the new tableau with modified information
        PrimalTableauBuilder(variables, objective, modifiedColumns, Some(rhs), constraintTypes, negativeColumn)
      } else {
        // If constraint has a negative right-hand side, invert it
        addLinearConstraint(constraint.withPositiveRhs)
      }
    case None => throw new IllegalArgumentException("cannot add a constraint without specifying first an objective function")
  }

  override def create: PrimalTableau = objective match {
    case Some(objType) => PrimalTableau(variables, objType, columns, rhs.get, constraintTypes, negativeColumn)
    case None => throw new IllegalArgumentException("Cannot create a tableau without specifying an objective function")
  }

  override protected def addConstraintTo(previousBuilder: PrimalTableauBuilder,
                                         constraint: Constraint[ContinuousVariable]): PrimalTableauBuilder = {
    previousBuilder.addConstraint(constraint)
  }

  private def objective(objectiveType: ObjectiveType,
                        costVector: Vector[Constant],
                        sign: Double): PrimalTableauBuilder = {
    require(costVector.length == variables.length,
      s"Optimization variables size ${variables.length} != linear coefficients ${costVector.length} size")
    // Prepare the columns, one per variable
    val columns = costVector.zipWithIndex.map {
      case (cost, index) => TableauColumn.costOnlyColumn(index, cost.x * sign)
    }
    val rhs = TableauColumn.costOnlyColumn(costVector.length, 0.0)
    // Add contraints to the tableau if some of the variable have lower or upper bounds
    val initialTableau = PrimalTableauBuilder(variables, Some(objectiveType), columns, Some(rhs), Vector())
    variables.force.zipWithIndex.foldLeft(initialTableau)(addVariableConstraints)
  }

  private def addVariableConstraints(tableau: PrimalTableauBuilder,
                                     variableAndIndex: (ContinuousVariable, Int)): PrimalTableauBuilder = {
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