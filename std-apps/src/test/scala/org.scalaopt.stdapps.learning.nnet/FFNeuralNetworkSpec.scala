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

import org.scalatest._
import org.scalatest.Matchers._

/**
 * @author bruneli
 */
class FFNeuralNetworkSpec extends FlatSpec with Matchers {

  "countWeights" should "count (24,15) weights for a network with (5,4,3) neurons" in {
    val weightsPerLayer = FFNeuralNetwork.countWeights(Vector(5, 4, 3))
    weightsPerLayer shouldBe Vector((5 + 1) * 4, (4 + 1) * 3)
  }

  "splitWeights" should "split 39 weights into 24 + 15 weights" in {
    val weights1 = (1 to 24).map(i => 1.0)
    val weights2 = (1 to 15).map(i => 2.0)
    val weightsPerLayer = FFNeuralNetwork.splitWeights(Vector(24, 15), weights1 ++ weights2)
    weightsPerLayer should have size 2
    weightsPerLayer(0) should have size 24
    weightsPerLayer(0) shouldBe weights1
    weightsPerLayer(1) should have size 15
    weightsPerLayer(1) shouldBe weights2
  }

  it should "split 40 weights into equal parts of 10" in {
    val weights = (0 until 40).map(i => (i / 10).toDouble)
    val weightsPerNeuron = FFNeuralNetwork.splitWeights(10, weights)
    weightsPerNeuron should have size 4
    for ((weights, index) <- weightsPerNeuron.zipWithIndex) {
      weights shouldBe (1 to 10).map(i => index.toDouble).toVector
    }
  }

}
