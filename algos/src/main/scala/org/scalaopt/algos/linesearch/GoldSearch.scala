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
 * Simple method to search a function minimum along a line.
 * 
 * The method is a variant of the bisection and therefore
 * does not rely on the computation of derivatives.
 * Example, bracket and then find a minimum within that interval:
 * {{{
 * scala> import org.scalaopt.algos.linesearch.GoldSearch._
 * scala> def f(x: Double) = math.pow(x - 2.0, 4.0) // Objective function
 * scala> val (a, b) = bracket(f, 0.0).get // Find Lower, upper bounds starting from 0
 * scala> minimize(f, a, b) // Find a minimum within that interval
 * }}}
 * 
 * @author bruneli
 */
object GoldSearch {
  /**
   * Configuration parameters for the Gold-Search algorithm.
   *
   * @param h step size to bracket a minimum along a line
   * @param tol tolerance error for a line search
   * @param maxIter maximum number of iterations to bracket a minimum
   */
  class GoldSearchConfig(
    val h: Double = 0.1,
    override val tol: Double = 1.0e-9,
    override val maxIter: Int = 100) extends ConfigPars(tol, maxIter) 

  /** Default configuration parameters for the Gold seach method */
  implicit val defaultGoldSearch = new GoldSearchConfig  
  
  /**
   * Bracket a local minimum from the user-supplied objective function
   *
   * @param f  scalar real-valued objective function
   * @param x1 starting value
   * @param pars configuration parameters
   * @return tuple2 with lower, upper bounds (if succeeding)
   */
  def bracket(
    f:  Double => Double,
    x1: Double)(
    implicit pars: GoldSearchConfig): Try[(Double, Double)] = {
    val c = (1.0 + math.sqrt(5.0)) / 2.0
    val x2 = x1 + (pars.h)
    val f2 = f(x2)
    
    // Increment x until value of f(x+h) exceeds f(x)
    def extend(x1: Double, x2: Double, f2: Double,
               h: Double, i: Int): Try[(Double, Double)] = {
      if (i >= (pars.maxIter))
        Failure(throw new MaxIterException(
        		"Maximum number of iterations reached."))
      val x3 = x2 + h
      val f3 = f(x3)
      if (f3 > f2)
        if (x3 > x1) Success((x1, x3)) else Success((x3, x1))
      else 
        extend(x2, x3, f3, c * h, i + 1)
    }
    
    // Increment x in the downhill direction
    if (f2 > f(x1)) {
      extend(x1, x1 - (pars.h), f(x1 - (pars.h)), -c * (pars.h), 0)
    } else {
      extend(x1, x2, f2, c * (pars.h), 0)
    }
  }
  
  /**
   * Search a local minimum of f within the interval [a, b]
   * 
   * This line search method is a variant of the bisection
   * method and does not require the computation of 
   * derivatives. It is used in the Powell search method.
   * The algorithm can be found in:
   * "J. Kiusalaas, Numerical Methods in Engineering with Python,
   *  Cambridge University Press".
   * 
   * @param f scalar real-valued objective function
   * @param a lower bound
   * @param b upper bound
   * @return tuple2 with (xmin, f(xmin))
   */
  def minimize(
    f: Double => Double,
    a: Double,
    b: Double)(
    implicit pars: GoldSearchConfig): (Double, Double) = {
    val r = (-1.0 + math.sqrt(5.0)) / 2.0 // Golden ratio
    val c = 1.0 - r
    val nIter = math.round(math.log((pars.tol) / math.abs(b - a)) / math.log(r))
    
    // telescope the search interval nIter times
    def telescope(
      x1: Double, f1: Double,
      x2: Double, f2: Double,
      a: Double, b: Double,
      i: Int): (Double, Double, Double, Double) =
      if (i == nIter)
        (x1, f1, x2, f2)
      else if (f1 > f2)
        telescope(x2, f2, c*x1 + r*b, f(c*x1 + r*b), x1, b, i + 1)
      else
        telescope(r*a + c*x2, f(r*a + c*x2), x1, f1, a, x2, i + 1)
        
    val (x1, f1, x2, f2) =
      telescope(r*a + c*b, f(r*a + c*b),
                c*a + r*b, f(c*a + r*b), a, b, 0)
    if (f1 < f2) (x1, f1)
    else (x2, f2)
  }
}