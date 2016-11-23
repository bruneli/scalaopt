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

package com.github.bruneli.scalaopt.core.derivativefree

import com.github.bruneli.scalaopt.core._
import com.github.bruneli.scalaopt.core.linesearch.GoldSearch
import GoldSearch.GoldSearchConfig
import com.github.bruneli.scalaopt.core.function.ContinuousObjectiveFunction
import com.github.bruneli.scalaopt.core.linalg.DenseVector
import com.github.bruneli.scalaopt.core.variable.{UnconstrainedVariable, VariableFromDouble}

import scala.util.{Failure, Success, Try}

/**
 * Implements the Powell optimization algorithm.
 * 
 * This algorithm is described in "J. Kiusalaas, Numerical Methods in Engineering
 * with Python, Cambridge University Press". Iterations are performed till
 * the distance between two iterations is less than some tolerance error or the
 * maximum number of iterations is reached. For each iteration, n+1 
 * minimisations are performed along n+1 directions v(i) each time starting from
 * the previously found minimum x(i-1). The last direction is obtained from the
 * n-th minimum and the starting point: v(n+1) = x(n) - x(0). At the end of
 * each iteration, the direction providing the largest decrease of F is thrown
 * out. Minimum line searches are done via the Golden section search algorithm 
 * and therefore do not require the computation of derivatives.
 * Example, find the minimum of a quadratic convex function
 * {{{
 * scala> import com.github.bruneli.scalaopt.core._
 * scala> import derivativefree.Powell._
 * scala> minimize((x: Variables) => x dot x, Vector(2.0, 4.0))
 * }}}
 * 
 * @author bruneli
 */
object Powell extends DerivativeFreeMethod[PowellConfig] with VariableFromDouble {

  implicit val defaultConfig: PowellConfig = new PowellConfig
  
  /**
   * Minimize a real-valued continuous objective function acting on a vector of unconstrained variables.
   * 
   * @param f    real-valued continuous objective function
   * @param x0   initial coordinates
   * @param pars algorithm configuration parameters
   * @return coordinates at a local minimum
   */
  override def minimize(
    f:  ContinuousObjectiveFunction[UnconstrainedVariable],
    x0: UnconstrainedVariablesType)(
    implicit pars: PowellConfig): Try[UnconstrainedVariablesType] = {

    val n = x0.length // Store number of dimensions
    
    def stoppingRule(
      iter: Int,
      xnpp: UnconstrainedVariablesType,
      x0: UnconstrainedVariablesType): Boolean = {
      val d = xnpp - x0
      (d dot d) < pars.tol * pars.tol
    }
    
    // Cycle till the stopping rule is reached
    def iterate(
      iter: Int, 
      xOld: UnconstrainedVariablesType,
      vOld: Vector[UnconstrainedVariablesType]): Try[UnconstrainedVariablesType] = {
      
      def lineSearch(
        xi: UnconstrainedVariablesType,
        vi: UnconstrainedVariablesType): Try[(UnconstrainedVariablesType, Double)] = {
        def fLine(s: Double): Double = f(xi + (vi * s))
        GoldSearch.bracket(fLine, 0.0)(pars.goldSearch) match {
          case Success((a, b)) => {
        	val (s, fMin) = GoldSearch.minimize(fLine, a, b)(pars.goldSearch)
            Success((xi + (vi * s), fMin - fLine(0.0)))
          }
          case Failure(e) => Failure(e)
        }
      }
      
      def lineSearchCycle(
        i: Int, 
        iMaxOld: Int, 
        dfOld: Double,
        xi: UnconstrainedVariablesType
        ): Try[(UnconstrainedVariablesType, Vector[UnconstrainedVariablesType])] = {
        // At the last iteration, try new direction v(n) = x(n-1) - x(0)
        val vi = if (i < n) vOld(i) else xi - xOld
        lineSearch(xi, vi) match {
          case Success((xipp, df)) => {
            if (i == n) {
              // Remove direction with largest function reduction
              val vNew = (vOld take iMaxOld) ++ (vOld drop (iMaxOld + 1)) :+ vi
              Success((xi, vNew))
            } else {
              val (iMaxNew, dfNew) =
                if (df >= dfOld) (i, df)
                else (iMaxOld, dfOld)
              lineSearchCycle(i + 1, iMaxNew, dfNew, xipp)
            }
          }
          case Failure(e) => Failure(e)
        }
      }
      
      if (iter >= pars.maxIter)
        Failure(throw MaxIterException(
        		"Maximum number of iterations reached."))

      // At each iteration perform a search in every direction
      lineSearchCycle(0, 0, 0.0, xOld) match {
        case Success((xNew, vNew)) => {
          if (stoppingRule(iter, xNew, xOld)) Success(xNew)
          else iterate(iter + 1, xNew, vNew)
        }
        case Failure(e) => Failure(e)
      }
    }
    
    def unitVectors: Vector[UnconstrainedVariablesType] = {
      ((0 until n) map (i => DenseVector.e[UnconstrainedVariable](n, i))).toVector
    }
    
    iterate(0, x0, unitVectors)
  }
}

/**
 * Configuration parameters for the Powell algorithm.
 *
 * @param tol tolerance error for convergence
 * @param maxIter maximum number of iterations per dimension
 * @param h step size to bracket a minimum along a line
 * @param tolLine tolerance error for a line search
 * @param maxIterLine maximum number of iterations to bracket a minimum
 */
case class PowellConfig(
  override val tol: Double = 1.0e-6,
  override val maxIter: Int = 30,
  h: Double = 0.1,
  tolLine: Double = 1.0e-9,
  maxIterLine: Int = 100) extends ConfigPars(tol, maxIter) {
  val goldSearch = new GoldSearchConfig(h, tolLine, maxIterLine)
}