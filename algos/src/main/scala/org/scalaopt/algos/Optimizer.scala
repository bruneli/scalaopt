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
 * An optimizer should implement at least one minimize method.
 *
 * @tparam C configuration parameters type
 * @author bruneli
 */
trait Optimizer[C <: ConfigPars] {

  val defaultConfig: C

  /**
   * Minimize an objective function acting on a vector of real values.
   *
   * @param f    real-valued objective function
   * @param x0   initial Variables
   * @param pars algorithm configuration parameters
   * @return Variables at a local minimum or failure
   */
  def minimize(
    f:  ObjectiveFunction,
    x0: Variables)(
    implicit pars: C): Try[Variables]

}

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
