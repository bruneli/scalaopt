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

import com.github.bruneli.scalaopt.core.{Variables, ConstrainedFunction, DataSet}

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
 * @param columns tableau columns except the right hand side
 * @param rhs tableau right hand side with objective function values and constrains parameters
 *
 * @author bruneli
 */
case class SimplexTableau(columns: DataSet[TableauColumn], rhs: TableauColumn) extends ConstrainedFunction {

  override val equalities: Vector[(Variables) => Double] = ???

  override val inequalities: Vector[(Variables) => Double] = ???

  /**
   * Evaluate the objective function for a given vector of variables
   *
   * @param x vector of variables
   * @return real-valued objective function at x
   */
  override def apply(x: Variables): Double = columns.aggregate(0.0)(addColumnCost, _ + _)

  /**
   * Check if the phase1 and phase2 objective functions are optimal
   *
   * @param epsilon error precision
   * @return true when all cost are positive or null
   */
  def isOptimal(epsilon: Double): Boolean = {
    columns.filter(_.phase1Cost < -epsilon).size == 0 && columns.filter(_.phase2Cost < -epsilon).size == 0
  }

  /**
   * Pivot the tableau columns given a pivot column and row
   *
   * Pivot separately the variables A and the right hand side b.
   *
   * @param pivotColumn pivot column with row index specifying the pivot row
   * @return pivoted tableau
   */
  def pivot(pivotColumn: TableauColumn): SimplexTableau = {
    this.copy(columns = columns.map(_.pivot(pivotColumn)), rhs = rhs.pivot(pivotColumn))
  }

  def solution: Variables = {
    columns.map(column => column.solution(rhs)).collect()
  }

  private def addColumnCost(previousCost: Double, column: TableauColumn) = {
    previousCost + column.phase2Cost
  }

}

/**
 * Tableau column describing a decision variable
 *
 * @param phase1Cost impact of the variable on the phase1 cost function
 * @param phase2Cost impact of the variable on the phase2 cost function
 * @param constrains impact of the non basic variable on the m constrains
 * @param column     column index
 * @param row        basic variable non null index or pivoting index
 * @param columnType column type                 
 */
case class TableauColumn(
  phase1Cost: Double, 
  phase2Cost: Double, 
  constrains: Vector[Double],
  column: Long,
  row: Int,
  columnType: ColumnType.Value) {

  /**
   * Multiply the content of a column by a constant factor
   */
  def *(multiplier: Double): TableauColumn = {
    this.copy(phase1Cost = phase1Cost * multiplier, 
              phase2Cost = phase2Cost * multiplier, 
              constrains = constrains.map(_ * multiplier))
  }
  
  /**
   * Pivot a column using another column
   *
   * @param that pivot column
   * @return pivoted column
   */
  def pivot(that: TableauColumn): TableauColumn = {
    if (this.column == that.column) {
      // The entering column becomes a basic variable
      TableauColumn.basicVariable(column, row)
    } else if (this.columnType == ColumnType.BasicVariable && this.row != that.row) {
      // Basic variables are unchanged if their row index is different from the pivoting row
      this
    } else {
      // Column is rescaled according to the pivot column
      val pivotValue = this.constrains(row) / that.constrains(row)
      val phase1Cost = this.phase1Cost - that.phase1Cost * pivotValue
      val phase2Cost = this.phase2Cost - that.phase2Cost * pivotValue
      val constrains = this.constrains.zipWithIndex.map {
         case (value, i) => if (i == that.row) pivotValue else value - that.constrains(i) * pivotValue
      }
      this.copy(phase1Cost = phase1Cost, phase2Cost = phase2Cost, constrains = constrains, row = -1)
    }
  }

  /**
   * Get the current solution
   */
  def solution(rhs: TableauColumn): Double = columnType match {
    case ColumnType.BasicVariable => rhs.constrains(row)
    case _ => 0.0
  }
  
}

object TableauColumn {

  def basicVariable(column: Long, row: Int): TableauColumn = {
    TableauColumn(0.0, 0.0, Vector.empty[Double], column, row, ColumnType.BasicVariable)
  }

}


/**
 * Define the different type of decision variables
 */
object ColumnType extends Enumeration {
  
  val NonBasicVariable = Value(0)
  val BasicVariable = Value(1)
  val ArtificialVariable = Value(2)
  
}