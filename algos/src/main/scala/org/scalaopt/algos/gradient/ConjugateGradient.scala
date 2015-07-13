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

package org.scalaopt.algos.gradient

import org.scalaopt.algos._
import org.scalaopt.algos.linesearch.StrongWolfe.{StrongWolfeConfig, stepLength}
import scala.util.{Try, Success, Failure}

/**
 * Implements the non-linear conjugate gradient method.
 * 
 * This algorithm is described in Chapter 5 of:
 * "J. Nocedal, S.J. Wright, Numerical Optimization, Springer."
 * {{{
 * scala> import org.scalaopt.algos._
 * scala> import org.scalaopt.algos.gradient.ConjugateGradient._
 * scala> minimize(x => x dot x, Vector(2.0, 4.0)) // Approximate derivatives
 * scala> minimizeWithGradient(x => x dot x, x => x * 2.0, Vector(2.0, 4.0)) // Exact derivatives
 * }}}
 * 
 * @author bruneli
 */
object ConjugateGradient extends Optimizer[ObjectiveFunction, CGConfig] {

  implicit val defaultConfig: CGConfig = new CGConfig

  /**
   * Minimize an objective function acting on a vector of real values.
   * 
   * @param f    real-valued objective function
   * @param x0   initial Variables
   * @param pars algorithm configuration parameters
   * @return Variables at a local minimum
   */
  override def minimize(
    f:  ObjectiveFunction,
    x0: Variables)(
    implicit pars: CGConfig): Try[Variables] = {

    // Number of dimensions
    val n = x0.length

    // Iterate until the gradient is lower than tol
    def iterate(
      k:  Int,
      ptk: LineSearchPoint): Try[Variables] = {
      if (k >= pars.maxIter)
        Failure(throw new MaxIterException(
        		"Maximum number of iterations reached."))
      
      // Try to find an approximate step length satisfying the strong
      // Wolfe conditions
      stepLength(ptk)(pars.strongWolfe) match {
        case Success(ptkpp) => {
          // Gradient evaluated in xk
          val dfk = ptk.grad

          // Variables, gradient and search direction at step k+1
          val xkpp = ptkpp.x
          val dfkpp = ptkpp.grad
          val pkpp = -dfkpp + ptk.d * beta(dfk, dfkpp, pars.method)

          // Check stopping rule, if not satisfied iterate
          if (dfkpp.norm < pars.tol) Success(xkpp)
          else iterate(k + 1, ptkpp.copy(d = pkpp))
        }
        case Failure(e) => Failure(e)
      }
    }
    
    iterate(0, LineSearchPoint(x0, f, -f.gradient(x0)))
  }

  def beta(
    dfk: Variables,
    dfkpp: Variables,
    method: String): Double = {
    val norm2dfk = dfk dot dfk
    method match {
      case "FR" => (dfkpp dot dfkpp) / norm2dfk
      case "PR" => (dfkpp dot (dfkpp - dfk)) / norm2dfk
      case _ => Math.max(0.0, (dfkpp dot (dfkpp - dfk)) / norm2dfk)
    }
  }

}

/**
 * Configuration parameters for the conjugate gradient algorithm.
 *
 * @param tol tolerance error for convergence
 * @param maxIter maximum number of iterations
 * @param eps finite differences step to evaluate derivatives
 * @param maxIterLine maximum number of iterations per line search
 * @param maxIterZoom maximum number of iterations per line zoom
 * @param c1 sufficient decrease condition parameter
 * @param c2 curvature condition parameter
 * @param c3 parameter to extend the search interval
 * @param method method name FR, PR, PR+ (default)
 */
case class CGConfig(
  override val tol: Double = 1.0e-5,
  override val maxIter: Int = 200,
  override val eps: Double = 1.0e-8,
  maxIterLine: Int = 10,
  maxIterZoom: Int = 10,
  c1: Double = 1.0e-4,
  c2: Double = 0.4,
  c3: Double = 2.0,
  method: String = "PR+") extends ConfigPars(tol, maxIter, eps) {
  val strongWolfe: StrongWolfeConfig =
    new StrongWolfeConfig(maxIterLine, maxIterZoom, c1, c2, c3)
}