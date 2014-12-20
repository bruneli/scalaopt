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

package org.scalaopt.stdapps

import org.scalaopt.algos._
import org.scalaopt.algos.leastsquares._

/**
 * Statistics package using optimization algorithms to fit and predicts set of data.
 *
 * @author bruneli
 */
package object stats {

  /**
   * Fit a function func with parameters p to data points (X, y) with least squares.
   *
   * @param func   objective function taking as input parameters p and real data observations X
   * @param data   a set of points (X, y)
   * @param p0     initial parameter values
   * @param method a least squares method
   * @param config optimization method configuration parameters
   * @return fit results
   */
  def leastSquaresFit[C <: ConfigPars](
    func: ObjFunWithData,
    data: DataSet[Xy],
    p0: Coordinates,
    method: LeastSquaresMethod[C],
    config: Option[C] = None): FitResults = {
    implicit val pars = config.getOrElse(method.defaultConfig)
    val pOpt= method.minimize(func, data, p0).get
    new FitResults(pOpt, func)
  }

}
