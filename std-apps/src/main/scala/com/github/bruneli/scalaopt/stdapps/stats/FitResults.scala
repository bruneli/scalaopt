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

package com.github.bruneli.scalaopt.stdapps.stats

import com.github.bruneli.scalaopt.core._
import com.github.bruneli.scalaopt.core.function.RegressionFunction
import com.github.bruneli.scalaopt.core.variable.{Input, Inputs, Output}

/**
 * Store results from a fit
 *
 * @param pOpt optimal parameters found
 * @param fObj objective function applying on data
 * @author bruneli
 */
class FitResults(pOpt: UnconstrainedVariablesType, fObj: RegressionFunction) {

  /**
   * Predict the value of the objective function for new data x
   *
   * @param x vector of observed values
   * @return regression function evaluated at x with parameters obtained from the fit
   */
  def predict(x: InputsType): OutputsType = {
    fObj(pOpt, x)
  }

  /**
   * Predict the value of the objective function at x
   *
   * @param x double value
   * @return regression function evaluated at x with parameters obtained from the fit
   */
  def predict(x: Input): Output = {
    fObj(pOpt, Inputs(x))(0)
  }

}
