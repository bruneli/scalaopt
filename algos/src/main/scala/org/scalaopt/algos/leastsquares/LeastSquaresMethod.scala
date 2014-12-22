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

package org.scalaopt.algos.leastsquares

import org.scalaopt.algos._

import scala.util.{Failure, Try}

/**
 * Abstract class to define least squares methods.
 *
 * @author bruneli
 */
abstract class LeastSquaresMethod[C <: ConfigPars] extends Optimizer[C] {

  /**
   * Minimize an objective function acting on a vector of real values.
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
    Failure(throw new UnsupportedOperationException(
      "Least squares methods do not act directly on objective functions with no data"))
  }

  /**
   * Minimize an objective function acting on a vector of real values
   * and on a set of data points in the form (X, y)
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
    implicit pars: C): Try[Coordinates]

  /**
   * Minimize an objective function acting on a vector of real values
   * and on a set of data points in the form (X, y)
   *
   * @param f    real-valued objective function acting on a vector on
   *             real-valued coordinates and real-valued observations X
   * @param data a Seq of real valued points in the form (X, y)
   * @param x0   initial coordinates
   * @param pars algorithm configuration parameters
   * @return
   */
  def minimize(
    f: ObjFunWithData,
    data: Seq[Xy],
    x0: Coordinates)(
    implicit pars: C): Try[Coordinates] = {
    import org.scalaopt.algos.SeqDataSetConverter.SeqDataSet
    minimize(f, SeqDataSet(data), x0)(pars)
  }

}
