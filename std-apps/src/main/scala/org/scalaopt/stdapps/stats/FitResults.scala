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

package org.scalaopt.stdapps.stats

import org.scalaopt.algos._

/**
 * Store results from a fit
 *
 * @param pOpt optimal parameters found
 * @param fObj objective function applying on data
 * @author bruneli
 */
class FitResults(pOpt: Variables, fObj: RegressionFunction) {

  /**
   * Predict the value of the objective function for new data x
   *
   * @param x vector of observed values
   * @return regression function evaluated at x with parameters obtained from the fit
   */
  def predict(x: Variables): Variables = fObj(pOpt, x)

  /**
   * Predict the value of the objective function at x
   *
   * @param x double value
   * @return regression function evaluated at x with parameters obtained from the fit
   */
  def predict(x: Double): Double = fObj(pOpt, Seq(x))(0)

}
