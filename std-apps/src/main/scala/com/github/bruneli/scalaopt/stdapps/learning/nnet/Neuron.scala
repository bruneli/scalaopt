/*
 * Copyright 2015 Renaud Bruneliere
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

package com.github.bruneli.scalaopt.stdapps.learning.nnet

import com.github.bruneli.scalaopt.core._
import com.github.bruneli.scalaopt.core.linalg.DenseVector
import com.github.bruneli.scalaopt.core.variable.{Input, Inputs, Output, UnconstrainedVariables}
import com.github.bruneli.scalaopt.stdapps.learning.nnet.activation.ActivationFunction

/**
 * Defines an artificial network neuron
 *
 * @author bruneli
 */
case class Neuron(
  layer: Int,
  index: Int,
  weights: UnconstrainedVariablesType,
  inputs: DenseVector[Input] = Inputs(),
  excitation: Double = Double.NaN,
  output: Output = Output(Double.NaN),
  target: Double = Double.NaN,
  residual: Double = Double.NaN,
  error: Double = Double.NaN) {

  def activate(inputs: InputsType, activationFunction: ActivationFunction): Neuron = {
    val excitation =
      if (weights.length == inputs.length + 1) {
        weights(0) + (inputs dot weights.force.tail)
      } else {
        throw new IllegalArgumentException(
          s"inputs ${inputs.length} do not have same size as weights ${weights.length - 1}")
      }
    this.copy(inputs = inputs.force, excitation = excitation, output = activationFunction(excitation))
  }

  def propagateError(target: Double, activationFunction: ActivationFunction): Neuron = {
    val activationDerivative = activationFunction.derivative(output)
    val lossDerivative = output - target
    val error = lossDerivative * activationDerivative
    this.copy(error = error, target = target, residual = lossDerivative)
  }

  def propagateError(neurons: Vector[Neuron], activationFunction: ActivationFunction): Neuron = {
    val activationDerivative = activationFunction.derivative(output)
    val lossDerivative = neurons map (neuron => neuron.error * neuron.weights(index + 1)) sum
    val error = lossDerivative * activationDerivative
    this.copy(error = error, residual = neurons.head.residual)
  }

  def gradient: UnconstrainedVariablesType = new UnconstrainedVariables(1.0 +: inputs.coordinates) * error

  def jacobian: UnconstrainedVariablesType = {
    if (residual.isNaN || residual == 0.0) {
      new UnconstrainedVariables(1.0 +: inputs.coordinates) * error
    } else {
      new UnconstrainedVariables(1.0 +: inputs.coordinates) * error / residual
    }
  }

}