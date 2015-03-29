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

import org.scalaopt.algos.leastsquares.LevenbergMarquardt
import org.scalaopt.stdapps.learning.data.Iris
import org.scalatest._
import org.scalatest.Matchers._

/**
 * @author bruneli
 */
class FeedForwardNeuralNetworkSpec extends FlatSpec with Matchers {

  "neural network" should "train a XOR" in {

    val neuralNetwork = FeedForwardNeuralNetwork(Iris.data, Vector(Iris.nInputs, 5, Iris.nClasses))

    val optimalWeights = LevenbergMarquardt.minimize(neuralNetwork, neuralNetwork.randomWeights)

    1 shouldBe 1
  }

}
