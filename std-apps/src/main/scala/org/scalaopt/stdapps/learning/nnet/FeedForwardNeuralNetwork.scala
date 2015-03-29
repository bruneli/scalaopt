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
import org.scalaopt.algos.linalg.AugmentedRow
import org.scalaopt.stdapps.learning.nnet.activation._

import scala.util.Random

/**
 * Feed-forward neural network.
 *
 * @param layers number of neurons per layer (from input to output included)
 * @param decay  parameter for weight decay
 * @param rang   initial random weights on [-rang, rang]
 *
 * @author bruneli
 */
case class FeedForwardNeuralNetwork(
  data: DataSet[DataPoint],
  layers: Vector[Int],
  decay: Double = 0.0,
  rang: Double = 0.7,
  lossType: LossType.Value = LossType.MeanSquaredError,
  innerFunction: ActivationFunction = LogisticFunction,
  outputFunction: ActivationFunction = LinearFunction,
  xTol: Double = 1.0e-6,
  random: Random = new Random(12345)) extends MSEFunction {

  require(layers.size > 1, "Neural network must have a least two layers.")

  private var network = initialNetwork

  override def apply(weights: Variables) = {
    if (!isWeightsVectorUnchanged(weights)) network = Network(layers, weights)
    data.aggregate(0.0)(loss, _ + _)
  }

  override def gradient(weights: Variables): Variables = {
    if (!isWeightsVectorUnchanged(weights)) network = Network(layers, weights)
    data.aggregate((zeros(weights.size), 0.0))(backpropagate, sumJacobianResidual)._1
  }

  def apply(weights: Variables, x: Variables): Variables = {
    if (!isWeightsVectorUnchanged(weights)) network = Network(layers, weights)
    network.outputs
  }

  def residual(p: Variables, point: DataPoint): Double =
    if (isWeightsVectorUnchanged(p)) {
      val activatedNetwork = network.forward(point.x).backward(point.y)
      activatedNetwork.residual
    } else {
      network = Network(layers, p).forward(point.x).backward(point.y)
      network.residual
    }

  def jacobianAndResidual(p: Variables, point: DataPoint): (Variables, Double) =
    if (isWeightsVectorUnchanged(p)) {
      val activatedNetwork = network.forward(point.x).backward(point.y)
      (activatedNetwork.gradient, activatedNetwork.residual)
    } else {
      network = Network(layers, p).forward(point.x).backward(point.y)
      (network.gradient, network.residual)
    }

  def jacobianAndResidualsMatrix(p: Variables) = data.zipWithIndex.map(backpropagateRow)

  def randomWeights: Variables = {
    val numberOfWeights = Network.countWeights(layers).sum[Int]
    Network.randomWeights(numberOfWeights, rang)
  }

  private def initialNetwork: Network = Network(layers, rang)

  private def backpropagate(
    previous: (Variables, Double),
    point: DataPoint): (Variables, Double) = {
    val activatedNetwork = network.forward(point.x).backward(point.y)
    (previous._1 + activatedNetwork.gradient,
      previous._2 + activatedNetwork.residual)
  }

  private def backpropagateRow(pointWithIndex: (DataPoint, Long)): AugmentedRow = {
    val (point, index) = pointWithIndex
    val activatedNetwork = network.forward(point.x).backward(point.y)
    AugmentedRow(activatedNetwork.gradient, activatedNetwork.residual, index)
  }

  private def loss(zero: Double, point: DataPoint) = {
    network.forward(point.x).backward(point.y).loss
  }

  private def sumJacobianResidual(left: (Variables, Double), right: (Variables, Double)) = {
    (left._1 + right._1, left._2 + right._2)
  }

  private def isWeightsVectorUnchanged(weights: Variables) = {
    (weights - network.weights).norm2 < xTol * xTol
  }

  private case class Network(layers: Vector[Vector[Neuron]]) {

    def outputs: Variables = layers.last.map(_.output)

    def weights: Variables = layers.flatMap(_.flatMap(_.weights))

    def gradient: Variables = layers.flatMap(_.flatMap(_.gradient))

    def loss: Double = lossType match {
      case LossType.MeanSquaredError => layers.last.map(_.error).norm2
      case LossType.CrossEntropy =>
        if (layers.last.size == 1) {
          val target = layers.last.head.target
          val output = layers.last.head.output
          val entropy1 = if (target > 0.0) -target * Math.log(output / target) else 0.0
          val entropy0 = if (target < 1.0) -(1.0 - target) * Math.log((1.0 - output) / (1.0 - target)) else 0.0
          entropy0 + entropy1
        } else {
          val outputSum = layers.last.map(_.output).sum
          if (outputSum > 0.0) {
            layers.last.foldLeft(0.0) {
              case (entropy, neuron) =>
                if (neuron.output > 0.0) entropy - neuron.target * Math.log(neuron.output / outputSum) else 0.0
            }
          } else {
            0.0
          }
        }
      case _ => throw new IllegalArgumentException(s"$lossType is not supported.")
    }

    def residual: Double = layers.last.map(_.error).norm

    def forward(inputs: Variables): Network = {
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

    def backward(targets: Variables): Network = {
      val errorPropagatedLayers = layers
        .init
        .scanRight(propagateErrorsOuterLayer(layers.last, targets))(
          propagateErrorsInnerLayer)
      Network(errorPropagatedLayers)
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
            val probs = neurons.map(neuron => activationFunction(neuron.excitation, maxExcitation))
            val sumProbs = probs.sum[Double]
            neurons.zip(probs).map {
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
      if (targets.size > 1) {
        lossType match {
          case LossType.CrossEntropy =>
            val targetsSum = targets.sum[Double]
            neurons.zip(targets).map {
              case (neuron, target) => neuron.copy(error = targetsSum * neuron.output - target)
            }
          case _ => ???
        }
      } else {
        neurons.zip(targets).map {
          case (neuron, target) => neuron.propagateError(target, outputFunction)
        }
      }
    }

    private def propagateErrorsInnerLayer(
      neurons: Vector[Neuron],
      nextLayer: Vector[Neuron]): Vector[Neuron] = {
      neurons.map(_.propagateError(nextLayer, innerFunction))
    }
  }

  private object Network {

    def apply(layers: Vector[Int], rang: Double): Network = {
      require(layers.size > 1, "Network must contain at least two layers (input + output)")
      val neurons = Range(1, layers.size).foldLeft((Vector.empty[Vector[Neuron]], 0))(generateLayer(rang))._1
      Network(neurons)
    }

    def apply(layers: Vector[Int], weights: Variables): Network = {
      require(layers.size > 1, "Network must contain at least two layers (input + output)")
      val nWeightsPerLayer = countWeights(layers)
      require(nWeightsPerLayer.sum == weights.size,
        s"Number of weights provided ${weights.size} != ${nWeightsPerLayer.sum} required")
      val neurons = splitWeights(nWeightsPerLayer, weights).zipWithIndex.map {
        case (weights, layer) =>
          splitWeights(layers(layer), weights).zipWithIndex.map {
            case (weights, index) => Neuron(layer, index, weights)
          }
      }
      Network(neurons)
    }

    def generateLayer(rang: Double)(previous: (Vector[Vector[Neuron]], Int), currentLayer: Int) = {
      val (previousNeurons, previousLayer) = previous
      val newNeurons = previousNeurons :+ Range(0, layers(currentLayer)).toVector.map {
        index: Int => Neuron(currentLayer, index, randomWeights(previousLayer + 1, rang))
      }
      (newNeurons, currentLayer)
    }

    def randomWeights(number: Int, rang: Double): Variables = {
      for (i <- 1 to number) yield (random.nextDouble() - 0.5) * 2 * rang
    }

    def countWeights(layers: Vector[Int]): Vector[Int] = {
      layers.tail.foldLeft((Vector.empty[Int], layers.head)) {
        case (previous: (Vector[Int], Int), current: Int) =>
          (previous._1 :+ (previous._2 + 1) * current, current)
      }._1
    }

    def splitWeights(numbers: Vector[Int], weights: Variables): Vector[Variables] = {
      numbers.foldLeft((Vector.empty[Variables], weights)) {
        case (previous, nWeights) =>
          val (thisLayerWeights, nextLayersWeights) = previous._2.splitAt(nWeights)
          (previous._1 :+ thisLayerWeights, nextLayersWeights)
      }._1
    }

    def splitWeights(number: Int, weights: Variables): Vector[Variables] = {
      if (weights.size <= number) {
        Vector(weights)
      } else {
        val (thisWeights, remainingWeights) = weights.splitAt(number)
        Vector(thisWeights) ++ splitWeights(number, remainingWeights)
      }
    }
  }

}

object LossType extends Enumeration {
  val MeanSquaredError, CrossEntropy = Value
}