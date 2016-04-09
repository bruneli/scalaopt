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

import scala.annotation.tailrec
import scala.util.{Try, Success, Failure}

/**
 * Standard primal simplex method
 *
 * Example, find the minimum of a linear objective function subject to linear constraints
 * {{{
 * scala> import com.github.bruneli.scalaopt.core._
 * scala> import linear.SimplexTableau
 * scala> import linear.StandardSimplex._
 * scala> val c1: Constraint = ((x: Variables) => 0.5 * x(0) + 1.0 * x(1)) >= 1.0
 * scala> SimplexTableau.min((x: Variables) => x(0) + x(1)).subjectTo(c1).withMethod(StandardSimplex)
 * }}}
 *
 * @author bruneli
 */
object StandardSimplex extends Optimizer[SimplexTableau, StandardSimplexConfig] {

  implicit val defaultConfig: StandardSimplexConfig = StandardSimplexConfig()

  /**
   * Minimize an objective function acting on a vector of real values.
   *
   * @param tableau real-valued objective function that is in this case a linear program
   * @param x0      initial Variables
   * @param pars    algorithm configuration parameters
   * @return Variables at a local minimum or failure
   */
  override def minimize(
    tableau: SimplexTableau, x0: Variables)(
    implicit pars: StandardSimplexConfig): Try[Variables] = {
    solvePhase1(pars)(tableau).flatMap(solvePhase2(pars)).map(_.solution)
  }

  /**
   * Find the column with the smallest cost and set its row index to the row with the smallest min ratio
   *
   * @param phase   look at simplex phase 1 or phase 2 cost
   * @param tableau simplex tableau
   * @return column with the smallest cost
   */
  def pivotColumn(phase: SimplexPhase.Value)(tableau: SimplexTableau) = {
    val pivotCandidate = phase match {
      case SimplexPhase.Phase1 =>
        val posCandidate = tableau.columns.maxBy(_.phase1Cost)
        if (tableau.negativeColumn.isDefined &&
          tableau.negativeColumn.get.phase1Cost > posCandidate.phase1Cost) {
          tableau.negativeColumn.get
        } else {
          posCandidate
        }
      case SimplexPhase.Phase2 =>
        val posCandidate = tableau.columns.filter(!_.isArtificial).maxBy(_.phase2Cost)
        if (tableau.negativeColumn.isDefined &&
          tableau.negativeColumn.get.phase2Cost > posCandidate.phase2Cost) {
          tableau.negativeColumn.get
        } else {
          posCandidate
        }
    }
    minRatioRow(pivotCandidate, tableau.rhs)
  }

  /**
   * Find the row index with the minimum ratio b_i / A_(i,j)
   *
   * @param pivotColumn pivot column
   * @param rhs         equality constrains right hand side
   * @return pivot column with row index corresponding to minimum ratio
   * @throws UnboundedProgramException when all b_i are negative
   */
  def minRatioRow(pivotColumn: TableauColumn, rhs: TableauColumn) = {
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
   * Perform phase 1 of the simplex algorithm, that is find a basic feasible solution
   *
   * A basic feasible solution is found by adding artificial variables with a cost of 1 and then
   * try to minimize that cost function.
   *
   * @param pars    config parameters
   * @param tableau simplex tableau
   * @return simplex tableau satisfying a basic feasible solution or failure if no solution exists
   */
  def solvePhase1(pars: StandardSimplexConfig)(tableau: SimplexTableau): Try[SimplexTableau] = {
    iterate(0, tableau.withArtificialVariables, SimplexPhase.Phase1, pars)
  }

  /**
   * Perform phase 2 of the simplex algorithm, that is find the optimal basic feasible solution
   *
   * @param pars    config parameters
   * @param tableau simplex tableau being a basic feasible solution
   * @return simplex tableau satisfying the best feasible solution or failure if problem is unbounded
   */
  def solvePhase2(pars: StandardSimplexConfig)(tableau: SimplexTableau): Try[SimplexTableau] = {
    iterate(0, tableau.withoutArtificialVariables, SimplexPhase.Phase2, pars)
  }

  /**
   * Compute the ratio b_i / A_(i,j) except if b_i is negative as it leads to unbounded solutions
   *
   * @param values (b_i, A_(i,j))
   * @return b_i / A_(i,j)
   */
  private def computeRatio(values: (Double, Double)) = {
    val (row, rhs) = values
    if (row <= 0.0) Double.PositiveInfinity else rhs / row
  }

  @tailrec
  private def iterate(
    i: Int,
    tableau: SimplexTableau,
    simplexPhase: SimplexPhase.Value,
    pars: StandardSimplexConfig): Try[SimplexTableau] = {
    if (tableau.isOptimal(pars.eps, simplexPhase)) {
      // If tableau is optimal (all costs are negative), stop iteration loop
      if (simplexPhase == SimplexPhase.Phase1 && Math.abs(tableau.rhs.phase1Cost) > pars.eps) {
        Failure(throw new NoSolutionException("Simplex phase1 failed to find a solution at zero cost"))
      } else {
        Success(tableau)
      }
    } else if (i >= pars.maxIter) {
      // If maximum number of iterations is reached, stop iteration loop
      Failure(throw new MaxIterException(s"Simplex $simplexPhase: number of iterations exceeds ${pars.maxIter}"))
    } else {
      // If tableau is not optimal, try to find a pivot column (might fail if problem is unbounded)
      Try(pivotColumn(simplexPhase)(tableau)) match {
        case Failure(e) => Failure(e)
        case Success(column) =>
          // Perform a pivoting operation and start a new iteration
          iterate(i + 1, tableau.pivot(column), simplexPhase, pars)
      }
    }
  }

}

case class StandardSimplexConfig(override val tol: Double = 1.49012e-8,
                                 override val maxIter: Int = 10,
                                 override val eps: Double = 1.0e-8) extends ConfigPars(tol, maxIter, eps)

/**
 * Define the two different simplex phases
 */
object SimplexPhase extends Enumeration {

  /** Phase1: find a basic feasible solution */
  val Phase1 = Value(0)

  /** Phase2: find the optimal basic feasible solution */
  val Phase2 = Value(1)

}