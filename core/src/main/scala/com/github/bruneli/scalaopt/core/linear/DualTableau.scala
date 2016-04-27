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
   * Extract the primal solution vector of this tableau
   */
  override def solution: Variables = ???

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

  /**
   * Pivot the tableau columns given a pivot column and row
   *
   * Pivot separately the variables A and the right hand side b.
   *
   * @param pivotColumn pivot column with its row index specifying the pivot row
   * @return pivoted tableau
   */
  override def pivot(pivotColumn: TableauColumn): SimplexTableau = ???

}

object DualTableau {

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
    val costSign = if (isMinimizing) -1.0 else 1.0
    val constrains = (0 until n).map(i => costSign * f(e(n, i))).toVector
    val rhs = TableauColumn(0.0, 0.0, constrains, 1)
    DualTableau(Vector(), rhs, Vector())
  }



}