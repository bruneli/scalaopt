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
import org.scalaopt.stdapps.learning.nnet.activation._

/**
 * Feed-forward neural network.
 *
 * @param layers number of neurons per layer (from input to output included)
 * @param decay  parameter for weight decay
 * @param rang   initial random weights on [-rang, rang]
 *
 * @author bruneli
 */
class FeedforwardNeuralNetwork(
  layers: Vector[Int],
  decay: Double,
  rang: Double,
  innerFunction: ActivationFunction = LogisticFunction,
  outputFunction: ActivationFunction = LinearFunction) {

  require(layers.size > 0, "Neural network must have a least one layer.")

  private var network = initialNetwork

  def loss(weights: Coordinates): Double = 1.0

  def gradient(weights: Coordinates): Coordinates = Seq()

  private def initialNetwork: Network = Network(Vector.empty[List[Neuron]])

  private case class Network(layers: Vector[List[Neuron]]) {

    def forward(inputs: List[Double]): Network = {
      val activatedLayers = layers
        .zipWithIndex
        .tail
        .scanLeft(activateLayer(layers.head, inputs, layers.size == 1)) {
        case (previousLayer, currentLayerWithIndex) => {
          val (currentLayer, index) = currentLayerWithIndex
          val currentLayerInputs = previousLayer.map(_.output)
          activateLayer(currentLayer, currentLayerInputs, index == layers.size - 1)
        }
      }
      Network(activatedLayers)
    }

    def backward(targets: List[Double]): Network = {
      val errorPropagatedLayers = layers
        .init
        .scanRight(propagateErrorsOuterLayer(layers.last, targets))(
          propagateErrorsInnerLayer)
      Network(errorPropagatedLayers)
    }

    private def activateLayer(
      neurons: List[Neuron],
      inputs: List[Double],
      isOutputLayer: Boolean): List[Neuron] = {
      val activationFunction = if (isOutputLayer) outputFunction else innerFunction
      neurons.map(_.activate(inputs, activationFunction))
    }

    private def propagateErrorsOuterLayer(
      neurons: List[Neuron],
      targets: List[Double]): List[Neuron] = {
      neurons.zip(targets).map {
        case (neuron, target) => neuron.propagateError(target, outputFunction)
      }
    }

    private def propagateErrorsInnerLayer(
      neurons: List[Neuron],
      nextLayer: List[Neuron]): List[Neuron] = {
      neurons.map(_.propagateError(nextLayer, innerFunction))
    }

  }
}
