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

/**
 * Feed-forward neural network.
 *
 * @param layers number of neurons per layer (from input to output included)
 * @param decay  parameter for weight decay
 * @param rang   initial random weights on [-rang, rang]
 *
 * @author bruneli
 */
class FeedforwardNeuralNetwork(layers: Vector[Int], decay: Double, rang: Double) {

  def loss(weights: Coordinates): Double = 1.0

  def gradient(weights: Coordinates): Coordinates = Seq()

  private case class Network(neurons: List[List[Neuron]])
}
