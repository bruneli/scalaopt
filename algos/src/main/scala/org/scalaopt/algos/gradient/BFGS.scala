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

import org.apache.commons.math3.linear.{RealMatrix, MatrixUtils}
import org.scalaopt.algos._
import org.scalaopt.algos.linesearch.StrongWolfe.{StrongWolfeConfig, stepLength}
import scala.util.{Try, Success, Failure}

/**
 * Implements the quasi-Newton BFGS method.
 * 
 * This algorithm is described in Chapter 6 of:
 * "J. Nocedal, S.J. Wright, Numerical Optimization, Springer."
 * For this quasi-Newton method, the inverse Hessian is approximated
 * by successive rank-one updates from Gradient evaluations.
 * Example, find the minimum of a quadratic convex function
 * {{{
 * scala> import org.scalaopt.algos._
 * scala> import org.scalaopt.algos.gradient.BFGS._
 * scala> minimize(x => x dot x, Vector(2.0, 4.0)) // Approximate derivatives
 * scala> minimizeWithGradient(x => x dot x, x => x * 2.0, Vector(2.0, 4.0)) // Exact derivatives
 * }}}
 * 
 * @author bruneli
 */
object BFGS extends Optimizer[BFGSConfig] {

  implicit val defaultConfig: BFGSConfig = new BFGSConfig

  /**
   * Minimize an objective function acting on a vector of real values.
   * 
   * @param f    real-valued objective function
   * @param x0   initial Variables
   * @param pars algorithm configuration parameters
   * @return Variables at a local minimum
   */
  def minimize[T <: ObjectiveFunction](
    f:  T,
    x0: Variables)(
    implicit pars: BFGSConfig): Try[Variables] = {

    // Number of dimensions
    val n = x0.length
    
    // Identity matrix (used multiple times)
    val identity = MatrixUtils.createRealIdentityMatrix(n)

    // Update the inverse Hessian matrix by applying the BFGS formula
    def updateHessian(
      iHk : RealMatrix,
      ptk: LineSearchPoint,
      ptkpp: LineSearchPoint): RealMatrix = {
      val sk = ptkpp.x - ptk.x
      val yk = ptkpp.grad - ptk.grad
      val yDots = yk dot sk
      val rhok = if (yDots != 0.0) 1.0 / yDots else 1000.0
      val m1 = identity - (sk outer yk * rhok)
      val m2 = identity - (yk outer sk * rhok)
      m1 * iHk * m2 + (sk outer sk) * rhok
    }
      
    // Iterate until the gradient is lower than tol
    def iterate(
      k:   Int,
      ptk: LineSearchPoint,
      iHk: RealMatrix): Try[Variables] = {
      if (k >= pars.maxIter)
        Failure(throw new MaxIterException(
        		"Maximum number of iterations reached."))

      // Try to get an approximate step length satisfying the strong
      // Wolfe conditions
      stepLength(ptk)(pars.strongWolfe) match {
        case Success(ptkpp) => {
          if (ptkpp.grad.norm < pars.tol) {
            Success(ptkpp.x)
          } else {
            // Update the Hessian and the search direction
            val iHkpp = updateHessian(iHk, ptk, ptkpp)
            val d = iHkpp * ptkpp.grad * -1.0
            iterate(k + 1, ptkpp.copy(d = d), iHkpp)
          }
        }
        case Failure(e) => Failure(e)
      }
    }
    
    // Initialize the inverse Hessian with an identity matrix
    iterate(0, LineSearchPoint(x0, f, -f.gradient(x0)), identity)
  }
}

/**
 * Configuration parameters for the BFGS algorithm.
 *
 * @param tol tolerance error for convergence
 * @param maxIter maximum number of iterations
 * @param eps finite differences step to evaluate derivatives
 * @param maxIterLine maximum number of iterations per line search
 * @param maxIterZoom maximum number of iterations per line zoom
 * @param c1 sufficient decrease condition parameter
 * @param c2 curvature condition parameter
 * @param c3 parameter to extend the search interval
 */
class BFGSConfig(
  override val tol: Double = 1.0e-5,
  override val maxIter: Int = 200,
  override val eps: Double = 1.0e-8,
  maxIterLine: Int = 10,
  maxIterZoom: Int = 10,
  c1: Double = 1.0e-4,
  c2: Double = 0.9,
  c3: Double = 2.0) extends ConfigPars(tol, maxIter, eps) {
  val strongWolfe: StrongWolfeConfig =
    new StrongWolfeConfig(maxIterLine, maxIterZoom, c1, c2, c3)
}