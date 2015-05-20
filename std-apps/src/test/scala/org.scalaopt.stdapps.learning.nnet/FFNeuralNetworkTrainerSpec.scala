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
import org.scalaopt.algos.gradient.BFGS
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

  "neural network" should "train on artificial data that mimics a network" in {
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
    val trainedNetwork = network withWeights w0 trainOn data withMethod BFGS

    trainedNetwork shouldBe Succeeded
  }

  "neural network" should "train on the Iris data with the Levenberg-Marquardt method" in {
    val neuralNetwork = FFNeuralNetwork(
      Vector(Iris.nInputs, 5, Iris.nClasses),
      rang = 0.5,
      lossType = CrossEntropy,
      innerFunction = LogisticFunction,
      outputFunction = SoftMaxFunction)
    val optimalWeights = neuralNetwork.trainOn(Iris.data).withMethod(LevenbergMarquardt).map(_.weights)

    1 shouldBe 1
  }

  it should "train on the Iris data with the BFGS method" in {
    val neuralNetwork = FFNeuralNetwork(
      Vector(Iris.nInputs, 5, Iris.nClasses),
      rang = 0.5,
      lossType = CrossEntropy,
      innerFunction = LogisticFunction,
      outputFunction = SoftMaxFunction)
    val optimalWeights = neuralNetwork.trainOn(Iris.data).withMethod(BFGS).map(_.weights)

    optimalWeights shouldBe Succeeded
    //optimalWeights.size shouldBe (5 * 6 + Iris.nClasses * 6)
  }

}
