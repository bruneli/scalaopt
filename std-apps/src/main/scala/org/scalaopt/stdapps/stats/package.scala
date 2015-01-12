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
import org.scalaopt.algos.derivativefree.NelderMead
import org.scalaopt.algos.leastsquares.{LeastSquaresMethod, LevenbergMarquardt}

/**
 * Statistics package using optimization algorithms to fit and predict set of data.
 *
 * @author bruneli
 */
package object stats {

  /**
   * Use result from a fit to predict y values for new data
   *
   * @param fit     results from a least squares or maximum likelihood fit
   * @param newData new data points X
   * @return set of points (X, y) with y being estimated from fit
   */
  def predict(fit: FitResults, newData: DataSet[Seq[Double]]) = {
    newData.map(x => (x, fit.predict(x)))
  }

  def predict(fit: FitResults, newData: Seq[Double]) = {
    newData.map(x => (x, fit.predict(x)))
  }

  /**
   * Fit a function with p parameters to data points (X, y) using the least squares method.
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
    method: LeastSquaresMethod[C] = LevenbergMarquardt,
    config: Option[C] = None): FitResults = {
    implicit val pars = config.getOrElse(method.defaultConfig)
    val pOpt= method.minimize(func, data, p0).get
    new FitResults(pOpt, func)
  }

  /**
   * Fit a probability density function with p parameters to data points X using the maximum likelihood principle.
   *
   * @param pdf    probability density function taking as input parameters p and real data observations X
   * @param data   a set of points X
   * @param p0     initial real parameter values
   * @param method an optimization method
   * @param config optimization method configuration parameters
   * @return fit results
   */
  def maxLikelihoodFit[C <: ConfigPars](
    pdf: ObjFunWithData,
    data: DataSet[Seq[Double]],
    p0: Coordinates,
    method: Optimizer[C] = NelderMead,
    config: Option[C] = None): FitResults = {
    implicit val pars = config.getOrElse(method.defaultConfig)

    val pOpt= method.minimize(negLogLikelihood(pdf, data), p0).get
    new FitResults(pOpt, pdf)
  }

  /**
   * Compute twice the negative log likelihood of pdf with data points X
   *
   * @param pdf  probability density function taking as input parameters p and real data observations X
   * @param data a set of points X
   * @param p    real parameter values
   * @return -2 times the log likelihood
   */
  def negLogLikelihood(
    pdf: ObjFunWithData,
    data: DataSet[Seq[Double]])(
    p: Coordinates): Double = {
    val value = data.aggregate(0.0)((sum, x) => sum - 2.0 * Math.log(pdf(p, x)), _ + _)
    value
  }

}
