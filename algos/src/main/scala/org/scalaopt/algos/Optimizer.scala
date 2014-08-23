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

package org.scalaopt.algos

import scala.util.Try

/**
 * @author bruneli
 */
trait Optimizer

/**
 * Common set of configuration parameters.
 *
 * @param tol     tolerance error for convergence
 * @param maxIter maximum number of iterations
 * @param eps     finite differences step to evaluate derivatives
 */
class ConfigPars(
  val tol: Double = 1.0e-5,
  val maxIter: Int = 200,
  val eps: Double = 1.0e-8) {
  require(tol > 0.0, "tolerance error must be strictly positive.")
  require(maxIter > 0, "Maximum number of iterations must be > 0.")
  require(eps > 0.0, "epsilon must be strictly positive.")  
}

object ConfigPars {
  import scala.reflect.ClassTag

  /**
   * Check if configuration parameters are of type T
   * 
   * @param c configuration parameters
   * @return configuration parameters of type T
   */
  def checkConfig[T: ClassTag, C <: ConfigPars](
      c: ConfigPars): T = c match {
    case c: T => c
    case _ => throw new IllegalArgumentException(
          "Incorrect type of configuration parameters.")
  }
}

/**
 * Abstract class to define gradient based optimization methods.
 */
abstract class GradientMethod extends Optimizer {
  
  /**
   * Minimize an objective function acting on a vector of real values.
   * 
   * @param f    real-valued objective function
   * @param df   gradient of the real-valued objective function
   * @param x0   initial coordinates
   * @param pars algorithm configuration parameters
   * @return coordinates at a local minimum or failure
   */
  def minimize[C <: ConfigPars](
    f:  ObjectiveFunction,
    df: Coordinates => Coordinates,
    x0: Coordinates)(
    implicit pars: ConfigPars): Try[Coordinates]

  /**
   * Minimize an objective function acting on a vector of real values.
   *
   * For that function, the gradient is approximated via the
   * finite difference method using opt.eps .
   * 
   * @param f    real-valued objective function
   * @param x0   initial coordinates
   * @param pars algorithm configuration parameters
   * @return coordinates at a local minimum or failure
   */
  def minimize[C <: ConfigPars](
    f:  ObjectiveFunction,
    x0: Coordinates)(
    implicit pars: ConfigPars): Try[Coordinates] = {

    // Gradient from finite differences
    def dfFiniteDiff(x: Coordinates): Coordinates = {
      val fx: Double = f(x)
      for (i <- 0 until x.length) yield (f(x.updated(i, x(i) + pars.eps)) - fx) / pars.eps
    }

    minimize(f, dfFiniteDiff, x0)
  }
}

/**
 * Abstract class to define derivative-free optimization methods.
 */
abstract class DerivativeFreeMethod extends Optimizer {

  /**
   * Minimize an objective function acting on a vector of real values.
   * 
   * @param f    real-valued objective function
   * @param x0   initial coordinates
   * @param pars algorithm configuration parameters
   * @return coordinates at a local minimum or failure
   */
  def minimize[C <: ConfigPars](
    f:  ObjectiveFunction,
    x0: Coordinates)(
    implicit pars: C): Try[Coordinates]
}