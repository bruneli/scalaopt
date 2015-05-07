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
import org.scalaopt.algos.SeqDataSetConverter._

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
case class FFNeuralNetworkTrainer(
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

  var network = initialNetwork

  override def apply(weights: Variables) = {
    if (!isWeightsVectorUnchanged(weights)) {
      network = FFNeuralNetwork(layers, weights, lossType, innerFunction, outputFunction)
    }
    data.aggregate(weights.norm2 * decay)(loss, _ + _)
  }

  override def gradient(weights: Variables): Variables = {
    if (!isWeightsVectorUnchanged(weights)) {
      network = FFNeuralNetwork(layers, weights, lossType, innerFunction, outputFunction)
    }
    data.aggregate((weights * 2.0 * decay, 0.0))(backpropagate, sumJacobianResidual)._1
  }

  override def dirder(weights: Variables, d: Variables): Double = {
    // TODO use backpropagation to compute the directional derivative
    if (!isWeightsVectorUnchanged(weights)) {
      network = FFNeuralNetwork(layers, weights, lossType, innerFunction, outputFunction)
    }
    gradient(weights) dot d
  }

  def apply(weights: Variables, x: Variables): Variables = {
    if (!isWeightsVectorUnchanged(weights)) {
      network = FFNeuralNetwork(layers, weights, lossType, innerFunction, outputFunction)
    }
    network.outputs
  }

  def residual(p: Variables, point: DataPoint): Double =
    if (isWeightsVectorUnchanged(p)) {
      val activatedNetwork = network.forward(point.x).backward(point.y)
      activatedNetwork.residual
    } else {
      network = FFNeuralNetwork(layers, p, lossType, innerFunction, outputFunction)
        .forward(point.x)
        .backward(point.y)
      network.residual
    }

  def jacobianAndResidual(p: Variables, point: DataPoint): (Variables, Double) =
    if (isWeightsVectorUnchanged(p)) {
      val activatedNetwork = network.forward(point.x).backward(point.y)
      (activatedNetwork.gradient, activatedNetwork.residual)
    } else {
      network = FFNeuralNetwork(layers, p, lossType, innerFunction, outputFunction)
        .forward(point.x)
        .backward(point.y)
      (network.gradient, network.residual)
    }

  def jacobianAndResidualsMatrix(p: Variables) = {
    if (!isWeightsVectorUnchanged(p)) {
      network = FFNeuralNetwork(layers, p, lossType, innerFunction, outputFunction)
    }
    if (decay > 0.0) {
      data.zipWithIndex.map(backpropagateRow) ++ penaltyMatrix(p)
    } else {
      data.zipWithIndex.map(backpropagateRow)
    }
  }

  private def initialNetwork: FFNeuralNetwork = {
    FFNeuralNetwork(layers, rang, lossType, innerFunction, outputFunction)
  }

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

  private def penaltyMatrix(p: Variables): DataSet[AugmentedRow] = {
    val penalty = Math.sqrt(decay)
    for (i <- 0 until p.size) yield {
      AugmentedRow(zeros(p.size).updated(i, penalty), 0.0, data.size + i)
    }
  }

}

object LossType extends Enumeration {
  val MeanSquaredError, CrossEntropy = Value
}