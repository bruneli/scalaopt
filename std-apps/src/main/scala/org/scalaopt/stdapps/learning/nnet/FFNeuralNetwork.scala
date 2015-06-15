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
import org.scalaopt.stdapps.learning.nnet.activation.{LinearFunction, LogisticFunction, ActivationFunction}

import scala.util.Random

/**
 * Define a generic feed-forward neural network
 *
 * @param layers         list of layers containing neurons
 * @param lossType       loss function used for the output layer
 * @param innerFunction  inner neurons activation function
 * @param outputFunction output layer activation function
 *
 * @author bruneli
 */
case class FFNeuralNetwork(
  layers: Vector[Vector[Neuron]],
  lossType: LossType.Value,
  innerFunction: ActivationFunction,
  outputFunction: ActivationFunction) {

  // TODO, with cross-entropy, outputFunction should be LogisticFunction

  def withWeights(weights: Variables): FFNeuralNetwork = {
    val nWeightsPerLayer = for (layer <- layers) yield layer.map(_.weights.size).sum[Int]
    require(nWeightsPerLayer.sum == weights.size,
      s"Number of weights provided ${weights.size} != ${nWeightsPerLayer.sum} required")
    val neurons = FFNeuralNetwork.splitWeights(nWeightsPerLayer, weights).zipWithIndex.map {
      case (weights, layer) =>
        FFNeuralNetwork.splitWeights(weights.size / layers(layer).size, weights).zipWithIndex.map {
          case (weights, index) => Neuron(layer, index, weights)
        }
    }
    FFNeuralNetwork(neurons, lossType, innerFunction, outputFunction)
  }

  def trainOn(data: DataSet[DataPoint]): FFNeuralNetworkTrainer = {
    new FFNeuralNetworkTrainer(this, data)
  }

  def outputs: Variables = layers.last.map(_.output)

  def weights: Variables = layers.flatMap(_.flatMap(_.weights))

  def gradient: Variables = layers.flatMap(_.flatMap(_.gradient))

  def jacobian: Variables = layers.flatMap(_.flatMap(_.jacobian))

  def loss: Double = lossType match {
    case LossType.MeanSquaredError => layers.last.map(_.residual).norm2
    case LossType.CrossEntropy =>
      if (layers.last.size == 1) {
        val target = layers.last.head.target
        val output = layers.last.head.output
        val entropy1 =
          if (target > 0.0) -target * Math.log(output / target) else 0.0
        val entropy0 =
          if (target < 1.0) -(1.0 - target) * Math.log((1.0 - output) / (1.0 - target)) else 0.0
        entropy0 + entropy1
      } else {
        val outputSum = layers.last.map(_.output).sum
        if (outputSum > 0.0) {
          layers.last.foldLeft(0.0) {
            case (entropy, neuron) =>
              if (neuron.output > 0.0) {
                entropy - neuron.target * Math.log(neuron.output / outputSum)
              } else {
                0.0
              }
          }
        } else {
          0.0
        }
      }
    case _ => throw new IllegalArgumentException(s"$lossType is not supported.")
  }

  def residual: Double =
    if (layers.last.size == 1) {
      layers.last.head.residual
    } else {
      layers.last.map(_.residual).norm
    }

  def forward(inputs: Variables): FFNeuralNetwork = {
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
    copy(layers = activatedLayers)
  }

  def backward(targets: Variables): FFNeuralNetwork = {
    val errorPropagatedLayers = layers
      .init
      .scanRight(propagateErrorsOuterLayer(layers.last, targets))(
        propagateErrorsInnerLayer)
    copy(layers = errorPropagatedLayers)
  }

  private def activateLayer(
    neurons: Vector[Neuron],
    inputs: Variables,
    isOutputLayer: Boolean): Vector[Neuron] = {
    val activationFunction = if (isOutputLayer) outputFunction else innerFunction
    if (isOutputLayer && neurons.size > 1) {
      lossType match {
        case LossType.CrossEntropy =>
          val excitedNeurons = neurons.map(_.activate(inputs, activationFunction))
          val maxExcitation = excitedNeurons.map(_.excitation).max
          val probs = excitedNeurons.map(neuron => activationFunction(neuron.excitation, maxExcitation))
          val sumProbs = probs.sum[Double]
          excitedNeurons.zip(probs).map {
            case (neuron, prob) => neuron.copy(output = prob / sumProbs)
          }
      }
    } else {
      neurons.map(_.activate(inputs, activationFunction))
    }
  }

  private def propagateErrorsOuterLayer(
    neurons: Vector[Neuron],
    targets: Variables): Vector[Neuron] = {
    lossType match {
      case LossType.MeanSquaredError =>
        neurons.zip(targets).map {
          case (neuron, target) => neuron.propagateError(target, outputFunction)
        }
      case LossType.CrossEntropy =>
        if (targets.size > 1) {
          val targetsSum = targets.sum[Double]
          neurons.zip(targets).map {
            case (neuron, target) =>
              neuron.copy(error = targetsSum * neuron.output - target, target = target)
          }
        } else {
          neurons.zip(targets).map {
            case (neuron, target) => neuron.copy(error = neuron.output - target, target = target)
          }
        }
      case _ => ???
    }
  }

  private def propagateErrorsInnerLayer(
    neurons: Vector[Neuron],
    nextLayer: Vector[Neuron]): Vector[Neuron] = {
    neurons.map(_.propagateError(nextLayer, innerFunction))
  }
}

object FFNeuralNetwork {

  /**
   * Generate a Feed-Forward Neural Network from a list with the number of neurons per layer
   * and random weights.
   *
   * @param layers         number of neurons per layer including the input and output layers
   * @param rang           range used to generate random weights
   * @param lossType       loss type (Mean-Square Errors or Cross-Entropy)
   * @param innerFunction  inner neurons activation function
   * @param outputFunction output layer activation function
   * @param random         random generator
   *
   * @return neural network
   */
  def apply(
    layers: Vector[Int],
    rang: Double,
    lossType: LossType.Value,
    innerFunction: ActivationFunction,
    outputFunction: ActivationFunction,
    random: Random = new Random(12345)): FFNeuralNetwork = {
    require(layers.size > 1, "Network must contain at least two layers (input + output)")
    val neurons = Range(1, layers.size).foldLeft((Vector.empty[Vector[Neuron]], 0))(generateLayer(layers, rang, random))._1
    FFNeuralNetwork(neurons, lossType, innerFunction, outputFunction)
  }

  /**
   * Generate a Feed-Forward Neural Network from a list with the number of neurons per layer
   * and weights.
   *
   * @param layers         number of neurons per layer including the input and output layers
   * @param weights        weights to be used by the different neurons
   * @param lossType       loss type (Mean-Square Errors or Cross-Entropy)
   * @param innerFunction  inner neurons activation function
   * @param outputFunction output layer activation function
   *
   * @return neural network
   */
  def apply(
    layers: Vector[Int],
    weights: Variables,
    lossType: LossType.Value,
    innerFunction: ActivationFunction,
    outputFunction: ActivationFunction): FFNeuralNetwork = {
    require(layers.size > 1, "Network must contain at least two layers (input + output)")
    val nWeightsPerLayer = countWeights(layers)
    require(nWeightsPerLayer.sum == weights.size,
      s"Number of weights provided ${weights.size} != ${nWeightsPerLayer.sum} required")
    val neurons = splitWeights(nWeightsPerLayer, weights).zipWithIndex.map {
      case (weights, layer) =>
        splitWeights(layers(layer) + 1, weights).zipWithIndex.map {
          case (weights, index) => Neuron(layer, index, weights)
        }
    }
    FFNeuralNetwork(neurons, lossType, innerFunction, outputFunction)
  }

  /**
   * Given a number of layers with x neurons each, evaluate the number of weights per layer.
   *
   * It assumes that for each layer one extra weight is added to add a constant term.
   *
   * @param layers vector with the number of neurons per layer
   * @return vector with the number of weights per layer
   */
  def countWeights(layers: Vector[Int]): Vector[Int] = {
    layers.tail.foldLeft((Vector.empty[Int], layers.head)) {
      case (previous: (Vector[Int], Int), current: Int) =>
        (previous._1 :+ (previous._2 + 1) * current, current)
    }._1
  }

  /**
   * Split a weight vector into multiple vector according to a vector specifying the number of weights per layer.
   *
   * @param numbers number of weights per layer
   * @param weights vector of weights
   * @return weights per layer
   */
  def splitWeights(numbers: Vector[Int], weights: Variables): Vector[Variables] = {
    numbers.foldLeft((Vector.empty[Variables], weights)) {
      case (previous, nWeights) =>
        val (thisLayerWeights, nextLayersWeights) = previous._2.splitAt(nWeights)
        (previous._1 :+ thisLayerWeights, nextLayersWeights)
    }._1
  }

  /**
   * Split a vector into peaces containing n-elements each
   *
   * @param number  number of weights per vector peace
   * @param weights vector of weights
   * @return a vector of vectors with n-weights per subvector
   */
  def splitWeights(number: Int, weights: Variables): Vector[Variables] = {
    if (weights.size <= number) {
      Vector(weights)
    } else {
      val (thisWeights, remainingWeights) = weights.splitAt(number)
      Vector(thisWeights) ++ splitWeights(number, remainingWeights)
    }
  }

  private def generateLayer(layers: Vector[Int], rang: Double, random: Random)(
    previous: (Vector[Vector[Neuron]], Int),
    currentLayer: Int) = {
    val (previousNeurons, previousLayer) = previous
    val newNeurons = previousNeurons :+ Range(0, layers(currentLayer)).toVector.map {
      index: Int => Neuron(currentLayer, index, randomWeights(layers(previousLayer) + 1, rang, random))
    }
    (newNeurons, currentLayer)
  }

  private def randomWeights(
    number: Int,
    rang: Double,
    random: Random): Variables = {
    for (i <- 1 to number) yield (random.nextDouble() - 0.5) * 2 * rang
  }

}

