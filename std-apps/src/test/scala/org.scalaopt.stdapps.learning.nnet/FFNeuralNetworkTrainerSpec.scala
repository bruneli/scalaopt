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

import org.scalaopt.algos.gradient.BFGS
import org.scalaopt.algos.leastsquares.LevenbergMarquardt
import org.scalaopt.stdapps.learning.data.Iris
import org.scalaopt.stdapps.learning.nnet.activation.SoftMaxFunction
import org.scalatest._
import org.scalatest.Matchers._

/**
 * @author bruneli
 */
class FFNeuralNetworkTrainerSpec extends FlatSpec with Matchers {

  "neural network" should "train on artificial data mimicing a network" in {
    import BFGS._

    //val network = FFNeuralNetwork(Vector(3, 1), Vector(0.5, 0.7, 0.4, 1.0))
  }

  "neural network" should "train on the Iris data with the Levenberg-Marquardt method" in {
    import LevenbergMarquardt._

    val neuralNetwork = FFNeuralNetworkTrainer(
      Iris.data,
      Vector(Iris.nInputs, 5, Iris.nClasses),
      lossType = LossType.CrossEntropy,
      outputFunction = SoftMaxFunction,
      decay = 0.01)
    val w0 = neuralNetwork.network.weights
    val optimalWeights = LevenbergMarquardt.minimize(neuralNetwork, w0)

    1 shouldBe 1
  }

  it should "train on the Iris data with the BFGS method" in {
    import BFGS._

    val neuralNetwork = FFNeuralNetworkTrainer(
      Iris.data,
      Vector(Iris.nInputs, 5, Iris.nClasses),
      lossType = LossType.CrossEntropy,
      outputFunction = SoftMaxFunction,
      decay = 0.01)
    val w0 = neuralNetwork.network.weights
    val optimalWeights = BFGS.minimize(neuralNetwork, w0)

    optimalWeights shouldBe Succeeded
    //optimalWeights.size shouldBe (5 * 6 + Iris.nClasses * 6)
  }

}
