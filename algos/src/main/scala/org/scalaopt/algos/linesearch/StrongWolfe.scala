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

package org.scalaopt.algos.linesearch

import org.scalaopt.algos._
import scala.util.{Try, Success, Failure}

/**
 * Line search algorithm satisfying the strong Wolfe conditions.
 * 
 * The algorithm consists into the search of an acceptable step length 
 * alpha along a line. For the strong Wolfe conditions, it requires
 *  - a sufficient decrease condition: f(alpha) <= f(0) + c1 * alpha * f'(0)
 *  - a curvature condition: |f'(alpha)| <= -c2 * f'(0)
 * The algorithm is further explained in Chapter 3 of:
 * "J. Nocedal, S.J. Wright, Numerical Optimization, Springer."
 * 
 * @author bruneli
 */
object StrongWolfe {
  /**
   * Configuration parameters for the Strong Wolfe step length search.
   *
   * @param maxIterLine maximum number of iterations per line search
   * @param maxIterZoom maximum number of iterations per line zoom
   * @param c1 sufficient decrease condition parameter
   * @param c2 curvature condition parameter
   * @param c3 parameter to extend the search interval
   */
  class StrongWolfeConfig(
    maxIterLine: Int = 10,
    maxIterZoom: Int = 10,
    val c1: Double = 1.0e-4,
    val c2: Double = 0.9,
    val c3: Double = 2.0) extends ConfigPars {
    val line = new ConfigPars(maxIter = maxIterLine)
    val zoom = new ConfigPars(maxIter = maxIterZoom)
  }
  implicit val defaultStrongWolfe: StrongWolfeConfig =
    new StrongWolfeConfig

  /**
   * Try to find an acceptable step length along a line 
   * satisfying the strong Wolfe conditions.
   *
   * @param f  scalar real-valued objective function
   * @param df derivative of the scalar objective function
   * @param pars algorithm configuration parameters
   * @return step length alphaStar or a Failure
   */
  def findStepLength(
    f:  Double => Double,
    df: Double => Double)(
    implicit pars: StrongWolfeConfig): Try[Double] = {

    val f0 = f(0.0)
    val df0 = df(0.0)
    
    def iterate(
      iter: Int,
      a0: Double, fa0: Double, dfa0: Double,
      a1: Double): Try[Double] = {
      if (iter >= pars.line.maxIter)
        Failure(throw new MaxIterException(
        		"Maximum number of iterations reached."))
      val fa1 = f(a1)
      val dfa1 = df(a1)
      // Test the sufficient decrease condition.
      // If it fails, zoom within that interval
      if ((fa1 > f0 + pars.c1 * a1 * df0) ||
          ((iter > 0) && (fa1 > fa0)))
        zoomStepLength(f, df, a0, a1)(pars)
      // Test the curvature condition
      else if (Math.abs(dfa1) <= -(pars.c2) * df0) Success(a1)
      // Check if high value has a positive derivative.
      // If so, zoom within that interval.
      else if (dfa1 >= 0.0) 
        zoomStepLength(f, df, a0, a1)(pars)
      // If none of the conditions are satisfied, try a new range
      else iterate(iter + 1, a1, fa1, dfa1, pars.c3 * a1)
    }
    
    iterate(0, 0.0, f0, df0, 1.0)
  }
  
  /**
   * Try to find an acceptable step length within an interval
   * satisfying the strong Wolfe conditions.
   *
   * @param f  scalar real-valued objective function
   * @param df derivative of the scalar objective function
   * @param a  interval lower bound
   * @param b  interval upper bound
   * @param pars algorithm configuration parameters
   * @return step length alphaStar
   */
  def zoomStepLength(
    f:  Double => Double,
    df: Double => Double,
    a:  Double,
    b:  Double)(
    implicit pars: StrongWolfeConfig): Try[Double] = {

    // Find minimum of a third order polynomial via 3 points
    def pol3Min(a: Double, fa: Double, dfa: Double,
                b: Double, fb: Double,
                c: Double, fc: Double) = {
      require((a != b) && (a != c) && (b != c),
              "a, b and c must be 3 distinctive points")
      // f(x) = c3 * (x - a)**3 + c2 * (x - a)**2 + c1 * (x - a) + c0
      val c0 = fa
      val c1 = dfa
      // With (b, fb) and (c, fc), solve 2 equations with 2 unknowns (c3, c2)
      val m12 = (b - a) * (b - a)
      val m11 = m12 * (b - a)
      val m22 = (c - a) * (c - a)
      val m21 = m22 * (c - a)
      val mdet = m11 * m22 - m12 * m21
      if (mdet == 0.0) -1.0
      else {
        val dfb = fb - c1 * (b - a) - c0
        val dfc = fc - c1 * (c - a) - c0
        val c2 = (m11 * dfc - m21 * dfb) / mdet
        val c3 = (m22 * dfb - m12 * dfc) / mdet
        // Find the minimum
        // df(x)/dx = 3 * c3 * (x - a)**2 + 2 * c2 * (x - a) + c1 = 0
        val delta = c2 * c2 - 3 * c1 * c3
        if ((c3 == 0.0) || (delta < 0.0)) -1.0
        else a + (Math.sqrt(delta) - c2) / (3 * c3)
      }
    }

    // Find minimum of a quadratic polynomial via 2 points
    def pol2Min(a: Double, fa: Double, dfa: Double,
                b: Double, fb: Double) = {
      require(a != b, "a and b must be distinctive points")
      // f(x) = c2 * (x - a)**2 + c1 * (x - a) + c0
      val c0 = fa
      val c1 = dfa
      val d = b - a
      val c2 = (fb - c1 * d - c0) / (d * d)
      if (c2 <= 0.0) -1.0
      else a - c1 / (2.0 * c2) // x_min = a - c1 / (2 * c2)
    }

    val f0 = f(0.0)
    val df0 = df(0.0)
    
    def iterate(
      iter: Int,
      a1: Double, f1: Double, df1: Double,
      a2: Double, f2: Double,
      a3: Double, f3: Double): Try[Double] = {
      if (iter >= pars.zoom.maxIter)
        Failure(throw new MaxIterException(
        		"Maximum number of iterations reached."))
      
      def findAlphaMin(
        order: Int, 
        xOld: Double): Double = order match {
        case 3 => {
          val xNew = 
            if (iter > 0) pol3Min(a1, f1, df1, a2, f2, a3, f3) 
            else -1.0
          if (xNew < a1 + 0.1 * (a2 - a1) ||
              xNew > a1 + 0.9 * (a2 - a1)) findAlphaMin(2, xNew)
          else xNew
        }
        case 2 => {
          val xNew = pol2Min(a1, f1, df1, a2, f2)
          if (xNew < a1 + 0.1 * (a2 - a1) ||
              xNew > a1 + 0.9 * (a2 - a1)) findAlphaMin(1, xNew)
          else xNew
        }
        case _ => 0.5 * (a2 - a1)
      }
      
      val astar  = findAlphaMin(3, 0.0)
      val fstar  = f(astar)
      val dfstar = df(astar)
      // Test strong Wolfe conditions
      if ((fstar <= f0 + pars.c1 * astar * df0) &&
          (Math.abs(dfstar) <= -(pars.c2) * df0)) Success(astar)
      else if ((fstar > f0 + (pars.c1) * astar * df0) ||
               (fstar > f1) ||
               (dfstar * (a2 - a1) >= 0.0))
        iterate(iter + 1, a1, f1, df1, astar, fstar, a2, f2)
      else
        iterate(iter + 1, astar, fstar, dfstar, a2, f2, a1, f1)
    }
    
    iterate(0, a, f(a), df(a), b, f(b), 0.0, 0.0)
  }
}