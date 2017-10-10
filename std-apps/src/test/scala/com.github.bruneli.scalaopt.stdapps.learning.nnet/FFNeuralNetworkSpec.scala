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

package org.scalaopt.stdapps.learning.nnet

import com.github.bruneli.scalaopt.core._
import com.github.bruneli.scalaopt.stdapps.learning.nnet.{FFNeuralNetwork, LossType}
import com.github.bruneli.scalaopt.stdapps.learning.nnet.activation.{LinearFunction, LogisticFunction}
import org.scalatest._
import org.scalatest.Matchers._
import LossType._
import com.github.bruneli.scalaopt.core.linalg.DenseVector
import com.github.bruneli.scalaopt.core.variable._

/**
 * @author bruneli
 */
class FFNeuralNetworkSpec extends FlatSpec with Matchers {

  "countWeights" should "count (24,15) weights for a network with (5,4,3) neurons" in {
    val weightsPerLayer = FFNeuralNetwork.countWeights(Vector(5, 4, 3))
    weightsPerLayer shouldBe Vector((5 + 1) * 4, (4 + 1) * 3)
  }

  "splitWeights" should "split 39 weights into 24 + 15 weights" in {
    val weights1 = DenseVector.fill[UnconstrainedVariable](24)(1.0)
    val weights2 = DenseVector.fill[UnconstrainedVariable](15)(2.0)
    val weightsPerLayer = FFNeuralNetwork.splitWeights(Vector(24, 15), weights1 ++ weights2)
    weightsPerLayer should have size 2
    weightsPerLayer(0).length shouldBe 24
    weightsPerLayer(0) shouldBe weights1
    weightsPerLayer(1).length shouldBe 15
    weightsPerLayer(1) shouldBe weights2
  }

  it should "split 40 weights into equal parts of 10" in {
    val weights = new UnconstrainedVariables((0 until 40).map(i => (i / 10).toDouble).toArray)
    val weightsPerNeuron = FFNeuralNetwork.splitWeights(10, weights)
    weightsPerNeuron should have size 4
    for ((weights, index) <- weightsPerNeuron.zipWithIndex) {
      weights shouldBe (1 to 10).map(i => index.toDouble).toVector
    }
  }

  "forward" should "propagate inputs up to the output layer" in {
    val neuronsPerLayer = Vector(3, 5, 1)
    val network = FFNeuralNetwork(neuronsPerLayer, 0.5, MeanSquaredError, LogisticFunction, LinearFunction)
    val weightsPerLayer = FFNeuralNetwork.countWeights(neuronsPerLayer)
    val weights = FFNeuralNetwork.splitWeights(weightsPerLayer, network.weights)
      .zipWithIndex
      .map { case (w, i) => FFNeuralNetwork.splitWeights(neuronsPerLayer(i) + 1, w) }
    val inputs = Inputs(1.0, 0.5, 1.5)
    val activatedNetwork = network.forward(inputs)
    // Hidden layer with sigmoid response
    val weights1 = weights(0)
    val outputs1 =
      for ((neuron, i) <- activatedNetwork.layers(0).zipWithIndex) yield {
      val excitation = weights1(i).force.head + (inputs dot weights1(i).force.tail)
      val output = LogisticFunction(excitation)
      neuron.excitation shouldBe excitation +- 1.0e-8
      neuron.output shouldBe output +- 1.0e-8
      (excitation, output)
    }
    // Output layer with linear response
    val weights2 = weights(1)
    for ((neuron, i) <- activatedNetwork.layers(1).zipWithIndex) {
      val outputsLayer1 = new Outputs(outputs1.map(_._2).toArray)
      val excitation = weights2(i).force.head + (outputsLayer1 dot weights2(i).force.tail)
      val output = LinearFunction(excitation)
      neuron.excitation shouldBe excitation +- 1.0e-8
      neuron.output shouldBe output +- 1.0e-8
    }
  }

  "backward" should "propagate target responses down to the inner layer" in {
    val neuronsPerLayer = Vector(3, 5, 1)
    val network = FFNeuralNetwork(neuronsPerLayer, 0.5, MeanSquaredError, LogisticFunction, LinearFunction)
    val weightsPerLayer = FFNeuralNetwork.countWeights(neuronsPerLayer)
    val weights = FFNeuralNetwork.splitWeights(weightsPerLayer, network.weights)
      .zipWithIndex
      .map { case (w, i) => FFNeuralNetwork.splitWeights(neuronsPerLayer(i) + 1, w) }
    val inputs = Inputs(1.0, 0.5, 1.5)
    val targets = Outputs(1.0)
    val finalNetwork = network.forward(inputs).backward(targets)
    // Propagate inputs in hidden layer
    val weights1 = weights(0)
    val outputs1 =
      for ((neuron, i) <- finalNetwork.layers(0).zipWithIndex) yield {
        val excitation = weights1(i).force.head + (inputs dot weights1(i).force.tail)
        LogisticFunction(excitation)
      }
    // Check gradient in output layer
    val weights2 = weights(1)
    val deltas = for ((neuron, i) <- finalNetwork.layers(1).zipWithIndex) yield {
      val outputsLayer1 = new Outputs(outputs1.toArray)
      val excitation = weights2(i).force.head + (outputsLayer1 dot weights2(i).force.tail)
      val output = LinearFunction(excitation)
      val delta = output - targets(0)
      for ((derivative, j) <- neuron.gradient.force.zipWithIndex) {
        val expDerivative = if (j == 0) delta else outputs1(j - 1) * delta
        derivative shouldBe expDerivative +- 1.0e-8
      }
      delta
    }
    // Propagate errors to the inner layer and check gradient
    for ((neuron, i) <- finalNetwork.layers(0).zipWithIndex) yield {
      val error = deltas(0) * weights2(0)(i + 1) * LogisticFunction.derivative(neuron.output)
      for ((derivative, j) <- neuron.gradient.force.zipWithIndex) {
        val expDerivative = if (j == 0) error else inputs(j - 1) * error
        derivative shouldBe expDerivative +- 1.0e-8
      }
    }
  }

  "gradient" should "be output - target when working with cross-entropy" in {
    val trueWeights = UnconstrainedVariables(0.1, -0.25, 0.5)
    val network = FFNeuralNetwork(
      Vector(2, 1),
      trueWeights,
      CrossEntropy,
      LogisticFunction,
      LogisticFunction)
    val inputs = Inputs(0.5, 1.5)
    val targets = Outputs(1.0)
    val eps = 1.0e-8
    val finalNetwork = network.forward(inputs).backward(targets)

    def output(x: InputsType) = {
      val net = (Input(1.0) +: x.force) dot trueWeights
      1.0 / (1.0 + Math.exp(-net))
    }
    def f(x: InputsType, y: Output, weights: UnconstrainedVariablesType = trueWeights) = {
      val net = (Input(1.0) +: x.force) dot weights
      val output = 1.0 / (1.0 + Math.exp(-net))
      val entropy0 = if (y > 0.0) -y * Math.log(output / y) else 0.0
      val entropy1 = if (y < 1.0) (1.0 - y) * Math.log((1.0 - output) / (1.0 - y)) else 0.0
      entropy0 + entropy1
    }
    def df(x: InputsType, y: Output) = {
      val error = output(x) - y
      (Input(1.0) +: x.force) * error
    }
    val gradient1 = finalNetwork.gradient
    val gradient2 = df(inputs, targets.head)
    val loss = f(inputs, targets.head)

    finalNetwork.outputs.coordinate(0) shouldBe output(inputs) +- 1.0e-5
    finalNetwork.loss shouldBe loss +- 1.0e-5
    for {(dx, i) <- gradient1.force.zip(gradient2.force).zipWithIndex
         (dx1, dx2) = dx} {
      val dw = trueWeights.updated(i, trueWeights(i) + eps)
      val dloss = f(inputs, targets.head, dw)
      val dx3 = (dloss - loss) / eps
      dx1.x shouldBe dx2.x +- 1.0e-5
      dx1.x shouldBe dx3 +- 1.0e-5
    }
  }

}
