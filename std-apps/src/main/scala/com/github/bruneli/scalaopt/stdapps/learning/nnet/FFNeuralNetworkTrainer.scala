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
import com.github.bruneli.scalaopt.core.linalg.DenseVector._
import com.github.bruneli.scalaopt.core.linalg.AugmentedRow
import SeqDataSetConverter._
import com.github.bruneli.scalaopt.core.function.{ContinuousObjectiveFunction, MSEFunction}
import com.github.bruneli.scalaopt.core.variable._

import scala.util.{Random, Try}

/**
 * Feed-forward neural network.
 *
 * @param initialNetwork initial neural network
 * @param decay          parameter for weight decay
 *
 * @author bruneli
 */
case class FFNeuralNetworkTrainer(
  initialNetwork: FFNeuralNetwork,
  data: DataSet[DataPoint],
  decay: Double = 0.0,
  xTol: Double = 1.0e-6,
  random: Random = new Random(12345)) extends MSEFunction {

  val eps = 1.0e-8
  var network = initialNetwork

  def withMethod[A <: ContinuousObjectiveFunction[UnconstrainedVariable], B <: ConfigPars](
    method: Optimizer[UnconstrainedVariable, A, B],
    config: Option[B] = None): Try[FFNeuralNetwork] = {
    implicit val pars = config.getOrElse(method.defaultConfig)
    val objectiveFunction = this match {
      case f: A => f
      case _ => throw new ClassCastException("Incorrect type of objective function")
    }
    method.minimize(objectiveFunction, this.network.weights).map(network.withWeights)
  }

  def withDecay(decay: Double) = this.copy(decay = decay)

  override def apply(weights: UnconstrainedVariablesType) = {
    if (!isWeightsVectorUnchanged(weights)) network = network.withWeights(weights)
    data.aggregate(weights.norm2 / 2.0 * decay)(loss, _ + _) / data.size
  }

  override def gradient(weights: UnconstrainedVariablesType): UnconstrainedVariablesType = {
    if (!isWeightsVectorUnchanged(weights)) network = network.withWeights(weights)
    data.aggregate[(UnconstrainedVariablesType, Double)](
      (weights * decay, 0.0))(
      backpropagate, sumGradientResidual)._1 / data.size
  }

  override def dirder(weights: UnconstrainedVariablesType, d: UnconstrainedVariablesType): Double = {
    if (!isWeightsVectorUnchanged(weights)) network = network.withWeights(weights)
    gradient(weights) dot d
  }

  override def dirHessian(weights: UnconstrainedVariablesType, d: UnconstrainedVariablesType): UnconstrainedVariablesType = {
    val gradx = gradient(weights)
    val gradxd = gradient(weights + d * eps)
    (gradxd - gradx) / eps
  }

  def apply(weights: UnconstrainedVariablesType, x: InputsType): OutputsType = {
    if (!isWeightsVectorUnchanged(weights)) network = network.withWeights(weights)
    network.outputs
  }

  def residual(p: UnconstrainedVariablesType, point: DataPoint): Double =
    if (isWeightsVectorUnchanged(p)) {
      val activatedNetwork = network.forward(point.x).backward(point.y)
      activatedNetwork.residual
    } else {
      network = network.withWeights(p)
        .forward(point.x)
        .backward(point.y)
      network.residual
    }

  def jacobianAndResidual(p: UnconstrainedVariablesType,
                          point: DataPoint): (InputsType, Output) =
    if (isWeightsVectorUnchanged(p)) {
      val activatedNetwork = network.forward(point.x).backward(point.y)
      (Inputs(activatedNetwork.gradient), activatedNetwork.residual)
    } else {
      network = network.withWeights(p)
        .forward(point.x)
        .backward(point.y)
      (Inputs(network.gradient), network.residual)
    }

  def jacobianAndResidualsMatrix(p: UnconstrainedVariablesType) = {
    if (!isWeightsVectorUnchanged(p)) network = network.withWeights(p)
    if (decay > 0.0) {
      data.zipWithIndex.map(backpropagateRow) ++ penaltyMatrix(p)
    } else {
      data.zipWithIndex.map(backpropagateRow)
    }
  }

  private def backpropagate(
    previous: (UnconstrainedVariablesType, Double),
    point: DataPoint): (UnconstrainedVariablesType, Double) = {
    val activatedNetwork = network.forward(point.x).backward(point.y)
    (previous._1 + activatedNetwork.gradient,
      previous._2 + activatedNetwork.residual)
  }

  private def backpropagateRow(pointWithIndex: (DataPoint, Long)): AugmentedRow = {
    val (point, index) = pointWithIndex
    val activatedNetwork = network.forward(point.x).backward(point.y)
    AugmentedRow(Inputs(activatedNetwork.jacobian), activatedNetwork.residual, index)
  }

  private def loss(zero: Double, point: DataPoint) = {
    zero + network.forward(point.x).backward(point.y).loss
  }

  private def sumGradientResidual(left: (UnconstrainedVariablesType, Double),
                                  right: (UnconstrainedVariablesType, Double)) = {
    (left._1 + right._1, left._2 + right._2)
  }

  private def isWeightsVectorUnchanged(weights: UnconstrainedVariablesType): Boolean = {
    (weights - network.weights).norm2 < xTol * xTol
  }

  private def penaltyMatrix(p: UnconstrainedVariablesType): DataSet[AugmentedRow] = {
    val penalty = Math.sqrt(decay)
    for (i <- 0 until p.length) yield {
      AugmentedRow(zeros[Input](p.length).updated(i, penalty), 0.0, data.size + i)
    }
  }

}

object LossType extends Enumeration {
  val MeanSquaredError, CrossEntropy = Value
}