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

package org.scalaopt.stdapps.learning.nnet

import org.scalaopt.algos._
import org.scalaopt.stdapps.learning.nnet.activation.ActivationFunction

/**
 * Defines an artificial network neuron
 *
 * @author bruneli
 */
case class Neuron(
  layer: Int,
  index: Int,
  weights: Variables,
  inputs: Variables = Nil,
  excitation: Double = Double.NaN,
  output: Double = Double.NaN,
  error: Double = Double.NaN) {

  def activate(inputs: Variables, activationFunction: ActivationFunction): Neuron = {
    val excitation =
      if (weights.size == inputs.size + 1) {
        weights.head + (inputs dot weights.tail)
      } else {
        throw new IllegalArgumentException(
          s"inputs ${inputs.size} do not have same size as weights ${weights.size - 1}")
      }
    this.copy(inputs = inputs, excitation = excitation, output = activationFunction(excitation))
  }

  def propagateError(target: Double, activationFunction: ActivationFunction): Neuron = {
    val activationDerivative = activationFunction.derivative(output)
    val lossDerivative = target - output
    val error = lossDerivative * activationDerivative
    this.copy(error = error)
  }

  def propagateError(neurons: Vector[Neuron], activationFunction: ActivationFunction): Neuron = {
    val activationDerivative = activationFunction.derivative(output)
    val lossDerivative = neurons map (neuron => neuron.error * neuron.weights(index)) sum
    val error = lossDerivative * activationDerivative
    this.copy(error = error)
  }

  def gradient: Variables = (1.0 +: inputs) * error

}