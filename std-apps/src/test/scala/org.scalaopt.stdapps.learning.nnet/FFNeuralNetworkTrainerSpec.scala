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

import org.scalaopt.algos._
import org.scalaopt.algos.gradient.{ConjugateGradient, BFGS}
import org.scalaopt.algos.leastsquares.LevenbergMarquardt
import org.scalaopt.stdapps.learning.data.Iris
import org.scalaopt.stdapps.learning.nnet.activation._
import LossType._
import org.scalatest._
import org.scalatest.Matchers._

import scala.util.Random

/**
 * @author bruneli
 */
class FFNeuralNetworkTrainerSpec extends FlatSpec with Matchers {

  "jacobianAndResidualsMatrix" should "lead to same results as a SimpleMSEFunction" in {
    import SeqDataSetConverter._

    val random = new Random(12345)

    val trueWeights = Vector(0.1, 0.5, 0.7, 0.4, -0.5, 0.77, -0.1, 0.2, 0.2)
    val network = FFNeuralNetwork(
      Vector(2, 2, 1),
      trueWeights,
      MeanSquaredError,
      LogisticFunction,
      LinearFunction)

    val regressionFunc: (Variables, Variables) => Variables = {
      (weights, inputs) => network.withWeights(weights).forward(inputs).outputs
    }

    val data: DataSet[DataPoint] = for (i <- 1 to 6) yield {
      val x = for (j <- 1 to 2) yield random.nextGaussian()
      val y = network.forward(x).outputs
      DataPoint(x, y)
    }

    val w0 = Vector(-0.1, 0.6, -0.3, 0.6, -0.8, -0.7, +0.3, -0.1, 0.35)
    val trainer = network withWeights w0 trainOn data
    val mseFunction = SimpleMSEFunction(regressionFunc, data)

    val matrix1 = trainer.jacobianAndResidualsMatrix(w0)
    val matrix2 = mseFunction.jacobianAndResidualsMatrix(w0)

    for ((row1, row2) <- matrix1.collect().zip(matrix2.collect())) {
      Math.abs(row1.b) shouldBe Math.abs(row2.b) +- 1.0e-5
      for ((x, y) <- row1.a.zip(row2.a)) {
        Math.abs(x) shouldBe Math.abs(y) +- 1.0e-5
      }
    }
  }

  "neural network" should "train on data that mimics a network with BFGS" in {
    import SeqDataSetConverter._

    val random = new Random(12345)

    val trueWeights = Vector(0.1, 0.5, 0.7, 0.4, -0.5, 0.77, -0.1, 0.2, 0.2)
    //val trueWeights = Vector(0.1, 0.5, 0.7, 0.4)
    val network = FFNeuralNetwork(
      Vector(2, 2, 1),
      trueWeights,
      MeanSquaredError,
      LogisticFunction,
      LinearFunction)

    val data: DataSet[DataPoint] = for (i <- 1 to 100) yield {
      val x = for (j <- 1 to 2) yield random.nextGaussian()
      val y = network.forward(x).outputs
      DataPoint(x, y)
    }
    val w0 = Vector(-0.1, 0.6, -0.3, 0.6, -0.8, -0.7, +0.3, -0.1, 0.35)
    //val w0 = Vector(-0.1, 0.6, -0.3, 0.6)
    val trainedNetwork = network withWeights w0 trainOn data withMethod BFGS

    trainedNetwork should be a 'success
    (trainedNetwork.get.weights - trueWeights).norm should be <= BFGS.defaultConfig.tol
  }

  it should "train on data that mimics a network with CG" in {
    import SeqDataSetConverter._

    val random = new Random(12345)

    val trueWeights = Vector(0.1, 0.5, 0.7, 0.4)
    val network = FFNeuralNetwork(
      Vector(3, 1),
      trueWeights,
      MeanSquaredError,
      LogisticFunction,
      LinearFunction)

    val data: DataSet[DataPoint] = for (i <- 1 to 100) yield {
      val x = for (j <- 1 to 3) yield random.nextGaussian()
      val y = network.forward(x).outputs
      DataPoint(x, y)
    }
    val w0 = Vector(-0.1, 0.6, -0.3, 0.6)
    val cgConfig = ConjugateGradient.defaultConfig.copy(tol = 0.05)
    val trainedNetwork = network
      .withWeights(w0)
      .trainOn(data)
      .withMethod(ConjugateGradient, Some(cgConfig))

    trainedNetwork should be a 'success
    (trainedNetwork.get.weights - trueWeights).norm should be <= cgConfig.tol
  }

  it should "train on data that mimics a network with Levenberg-Marquardt" in {
    import SeqDataSetConverter._

    val random = new Random(12345)

    val trueWeights = Vector(0.1, 0.5, 0.7, 0.4, -0.5, 0.77, -0.1, 0.2, 0.2)
    val network = FFNeuralNetwork(
      Vector(2, 2, 1),
      trueWeights,
      MeanSquaredError,
      LogisticFunction,
      LinearFunction)

    val data: DataSet[DataPoint] = for (i <- 1 to 50) yield {
      val x = for (j <- 1 to 2) yield random.nextGaussian()
      val y = network.forward(x).outputs
      DataPoint(x, y)
    }
    val w0 = Vector(-0.1, 0.6, 0.6, 0.4, -0.6, 0.5, 0.0, 0.2, 0.35)
    val trainedNetwork = network withWeights w0 trainOn data withMethod LevenbergMarquardt
    val optWeights = trainedNetwork.get.weights

    val regressionFunc: (Variables, Variables) => Variables = {
      (weights, inputs) => network.withWeights(weights).forward(inputs).outputs
    }
    val mseFunction = SimpleMSEFunction(regressionFunc, data)
    val wOpt = LevenbergMarquardt.minimize(mseFunction, w0)(LevenbergMarquardt.defaultConfig)

    trainedNetwork should be a 'success
    (trainedNetwork.get.weights - trueWeights).norm should be <= LevenbergMarquardt.defaultConfig.tol
  }

  it should "train on the Iris data with the Levenberg-Marquardt method" in {
    val neuralNetwork = FFNeuralNetwork(
      Vector(Iris.nInputs, 5, Iris.nClasses),
      rang = 0.5,
      lossType = CrossEntropy,
      innerFunction = LogisticFunction,
      outputFunction = SoftMaxFunction)
    val optimalWeights = neuralNetwork.trainOn(Iris.data).withMethod(LevenbergMarquardt).map(_.weights)

    optimalWeights should be a 'success
  }

  it should "train on the Iris data with the BFGS method" in {
    val neuralNetwork = FFNeuralNetwork(
      Vector(Iris.nInputs, 5, Iris.nClasses),
      rang = 0.5,
      lossType = CrossEntropy,
      innerFunction = LogisticFunction,
      outputFunction = SoftMaxFunction)
    val optimalWeights = neuralNetwork.trainOn(Iris.data).withMethod(BFGS).map(_.weights)

    optimalWeights should be a 'success
    //optimalWeights.size shouldBe (5 * 6 + Iris.nClasses * 6)
  }

}
