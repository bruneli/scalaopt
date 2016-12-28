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

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/**
 * Standard simplex method
 *
 * It can be called either with a primal or a dual tableau
 *
 * Example, find the minimum of a linear objective function subject to linear constraints
 * {{{
 * scala> import com.github.bruneli.scalaopt.core._
 * scala> import linear.PrimalTableau
 * scala> import linear.StandardSimplex._
 * scala> val c1: Constraint = ((x: Variables) => 0.5 * x(0) + 1.0 * x(1)) >= 1.0
 * scala> PrimalTableau.min((x: Variables) => x(0) + x(1)).subjectTo(c1).withMethod(PrimalSimplex)
 * }}}
 *
 * @author bruneli
 */
case class StandardSimplex() extends LPSolver[StandardSimplexConfig] {

  /**
   * Solve a linear program expressed in a tableau form.
   *
   * @param lp   linear program
   * @param pars algorithm configuration parameters
   * @return Tableau at a local minimum or failure
   */
  override def solve(lp: LP)(
    implicit pars: StandardSimplexConfig): Try[LP] = {
    val tableau = lp.toTableau.checkNegativeVariables
    solvePhase1(pars)(tableau).flatMap(solvePhase2(pars))
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
    iterate(0, tableau.withArtificialVariables, PHASE1, pars)
  }

  /**
   * Perform phase 2 of the simplex algorithm, that is find the optimal basic feasible solution
   *
   * @param pars    config parameters
   * @param tableau simplex tableau being a basic feasible solution
   * @return simplex tableau satisfying the best feasible solution or failure if problem is unbounded
   */
  def solvePhase2(pars: StandardSimplexConfig)(tableau: SimplexTableau): Try[SimplexTableau] = {
    iterate(0, tableau.withoutArtificialVariables, PHASE2, pars)
  }

  @tailrec
  private def iterate(
    i: Int,
    tableau: SimplexTableau,
    simplexPhase: SimplexPhase,
    pars: StandardSimplexConfig): Try[SimplexTableau] = {
    if (tableau.isOptimal(pars.eps, simplexPhase)) {
      // If tableau is optimal (all costs are negative), stop iteration loop
      if (simplexPhase == PHASE1 && Math.abs(tableau.cost(PHASE1)) > pars.eps) {
        Failure(throw NoSolutionException("Simplex phase1 failed to find a solution at zero cost"))
      } else {
        Success(tableau)
      }
    } else if (i >= pars.maxIter) {
      // If maximum number of iterations is reached, stop iteration loop
      Failure(throw MaxIterException(s"Simplex $simplexPhase: number of iterations exceeds ${pars.maxIter}"))
    } else {
      // If tableau is not optimal, try to find a pivot column (might fail if problem is unbounded)
      // and perform a pivoting operation
      tableau.performPivot(simplexPhase) match {
        case Failure(e) => Failure(e)
        case Success(pivotedTableau) =>
          // If pivoting succeeded, starts a new iteration
          iterate(i + 1, pivotedTableau, simplexPhase, pars)
      }
    }
  }

}

object StandardSimplex {

  implicit val defaultConfig: StandardSimplexConfig = StandardSimplexConfig()

}

case class StandardSimplexConfig(override val tol: Double = 1.49012e-8,
  override val maxIter: Int = 1000,
  override val eps: Double = 1.0e-8) extends ConfigPars(tol, maxIter, eps)

