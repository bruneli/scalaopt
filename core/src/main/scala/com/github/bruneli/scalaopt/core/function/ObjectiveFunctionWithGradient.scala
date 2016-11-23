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

import com.github.bruneli.scalaopt.core._
import com.github.bruneli.scalaopt.core.variable.UnconstrainedVariable

/**
 * Differentiable objective function with gradient passed as input
 *
 * @param f objective function and its gradient
 * @param config configuration parameters
 * @author bruneli
 */
case class ObjectiveFunctionWithGradient(
  f: (UnconstrainedVariablesType => Double, UnconstrainedVariablesType => UnconstrainedVariablesType),
  implicit val config: ConfigPars = new ConfigPars()) extends DifferentiableObjectiveFunction[UnconstrainedVariable] {

  def apply(x: UnconstrainedVariablesType) = f._1(x)

  override def gradient(x: UnconstrainedVariablesType) = f._2(x)

  override def dirder(x: UnconstrainedVariablesType, d: UnconstrainedVariablesType): Double = {
    gradient(x) dot d
  }

  override def dirHessian(
    x: UnconstrainedVariablesType,
    d: UnconstrainedVariablesType): UnconstrainedVariablesType = {
    val gradx = gradient(x)
    val gradxd = gradient(x + (d * config.eps))
    (gradxd - gradx) / config.eps
  }

}
