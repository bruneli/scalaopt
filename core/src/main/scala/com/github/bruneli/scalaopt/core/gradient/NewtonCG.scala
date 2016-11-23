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

package com.github.bruneli.scalaopt.core.gradient

import com.github.bruneli.scalaopt.core.linesearch.StrongWolfe
import com.github.bruneli.scalaopt.core._
import StrongWolfe._
import com.github.bruneli.scalaopt.core.function.DifferentiableObjectiveFunction
import com.github.bruneli.scalaopt.core.variable.{LineSearchPoint, UnconstrainedVariable}
import com.github.bruneli.scalaopt.core.linalg.DenseVector._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/**
 * Line search Newton Conjugate Gradient method.
 *
 * Inexact Newton method that apply the Conjugate Gradient method to the Newton
 * equation to define a search direction.
 * {{{
 * scala> import com.github.bruneli.scalaopt.core._
 * scala> import gradient.NewtonCG._
 * scala> minimize((x: Variables) => x dot x, Vector(2.0, 4.0)) // Approximate derivatives
 * scala> minimize(((x: Variables) => x dot x, (x: Variables) => x * 2.0), Vector(2.0, 4.0)) // Exact derivatives
 * }}}
 *
 * @author bruneli
 */
object NewtonCG extends GradientMethod[CGConfig] {

  implicit val defaultConfig: CGConfig = new CGConfig

  /**
   * Minimize an objective function acting on a vector of real values.
   *
   * @param f    real-valued objective function
   * @param x0   initial Variables
   * @param pars algorithm configuration parameters
   * @return Variables at a local minimum or failure
   */
  override def minimize(
    f: DifferentiableObjectiveFunction[UnconstrainedVariable],
    x0: UnconstrainedVariablesType)(
    implicit pars: CGConfig): Try[UnconstrainedVariablesType] = {

    def iterate(k: Int, ptk: LineSearchPoint): Try[UnconstrainedVariablesType] = {
      if (k >= pars.maxIter)
        Failure(throw MaxIterException(
          "Maximum number of iterations reached."))

      val dfxNorm = ptk.grad.norm
      val epsk = Math.min(0.5, Math.sqrt(dfxNorm)) * dfxNorm
      val z0 = zeros[UnconstrainedVariable](ptk.x.size)
      val r0 = ptk.grad
      val d0 = -r0
      val ptkPrim = searchDirection(0, ptk.copy(d = d0), z0, r0, epsk)
      // Try to find an approximate step length satisfying the strong
      // Wolfe conditions
      stepLength(ptkPrim)(pars.strongWolfe) match {
        case Failure(e) => Failure(e)
        case Success(ptkpp) =>
          if (ptkpp.grad.norm < pars.tol) Success(ptkpp.x)
          else iterate(k + 1, ptkpp)
      }
    }

    iterate(0, LineSearchPoint(x0, f, zeros[UnconstrainedVariable](x0.size)))
  }

  @tailrec
  private def searchDirection(
    j: Int,
    ptk: LineSearchPoint,
    zj: UnconstrainedVariablesType,
    rj: UnconstrainedVariablesType,
    epsk: Double): LineSearchPoint = {
    if ((ptk.d dot ptk.d2fxd) <= 0.0) {
      if (j == 0) ptk.copy(d = -ptk.grad) else ptk.copy(d = zj)
    } else {
      val alphaj = (rj dot rj) / (ptk.d dot ptk.d2fxd)
      val zjpp = zj + ptk.d * alphaj
      val rjpp = rj + ptk.d2fxd * alphaj
      if (rjpp.norm < epsk) {
        ptk.copy(d = zjpp)
      } else {
        val betajpp = (rjpp dot rjpp) / (rj dot rj)
        val djpp = -rjpp + ptk.d * betajpp
        searchDirection(j + 1, ptk.copy(d = djpp), zjpp, rjpp, epsk)
      }
    }
  }
}
