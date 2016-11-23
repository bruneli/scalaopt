/*
 * Copyright 2016 Renaud Bruneliere
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

package com.github.bruneli.scalaopt.core.function
import com.github.bruneli.scalaopt.core.{InputsType, OutputsType, UnconstrainedVariablesType}

/**
 * General regression function based on a scala function
 *
 * @param f scala function of (parameters, inputs) to outputs
 * @author bruneli
 */
case class GeneralRegressionFunction(
  f: (UnconstrainedVariablesType, InputsType) => OutputsType) extends RegressionFunction {

  /**
   * Estimate the dependent variables Y given inputs X and unknown parameters P
   *
   * @param x vector of input variables
   * @param p vector of unknown parameters
   * @return vector of output variables
   */
  override def apply(p: UnconstrainedVariablesType, x: InputsType): OutputsType = {
    f(p, x)
  }

}
