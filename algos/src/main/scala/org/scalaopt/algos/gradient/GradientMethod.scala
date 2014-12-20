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

import scala.util.{Failure, Try}

/**
 * Abstract class to define gradient based optimization methods.
 *
 * @author bruneli
 */
abstract class GradientMethod[C <: ConfigPars] extends Optimizer[C] {

  /**
   * Minimize an objective function acting on a vector of real values.
   *
   * @param f    real-valued objective function
   * @param df   gradient of the real-valued objective function
   * @param x0   initial coordinates
   * @param pars algorithm configuration parameters
   * @return coordinates at a local minimum or failure
   */
  def minimizeWithGradient(
    f:  ObjectiveFunction,
    df: Coordinates => Coordinates,
    x0: Coordinates)(
    implicit pars: C): Try[Coordinates]

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
  override def minimize(
    f:  ObjectiveFunction,
    x0: Coordinates)(
    implicit pars: C): Try[Coordinates] = {

    minimizeWithGradient(f, dfFiniteDiff(f, pars.eps), x0)(pars)
  }

  /**
   * Minimize an objective function acting on a vector of real values
   * and on a set of data points in the form (X, y).
   * This method is not implemented.
   *
   * @param f    real-valued objective function acting on a vector on
   *             real-valued coordinates and real-valued observations X
   * @param data a set of points in the form (X, y)
   * @param x0   initial coordinates
   * @param pars algorithm configuration parameters
   * @return
   */
  override def minimize(
    f: ObjFunWithData,
    data: DataSet[Xy],
    x0: Coordinates)(
    implicit pars: C): Try[Coordinates] = {
    Failure(throw new UnsupportedOperationException(
      "Gradient based methods do not act directly on objective functions linked to data"))
  }

  /**
   * Compute the gradient of f at x with finite differences
   *
   * @param f   real-valued objective function
   * @param eps finite differences step to evaluate derivatives
   * @param x   coordinates
   * @return gradient of f evaluated at x
   */
  def dfFiniteDiff(f: ObjectiveFunction, eps: Double)(x: Coordinates): Coordinates = {
    val fx: Double = f(x)
    for (i <- 0 until x.length) yield (f(x.updated(i, x(i) + eps)) - fx) / eps
  }

}
