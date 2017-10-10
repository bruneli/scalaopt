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

import com.github.bruneli.scalaopt.core.gradient.{BFGS, SteihaugCG}
import com.github.bruneli.scalaopt.core._
import com.github.bruneli.scalaopt.core.leastsquares.LevenbergMarquardt
import com.github.bruneli.scalaopt.stdapps.learning.data.Iris
import com.github.bruneli.scalaopt.stdapps.learning.nnet.{FFNeuralNetwork, LossType}
import com.github.bruneli.scalaopt.stdapps.learning.nnet.activation.{LinearFunction, LogisticFunction, SoftMaxFunction}
import LossType._
import com.github.bruneli.scalaopt.core.function.{ObjectiveFunctionFiniteDiffDerivatives, SimpleMSEFunction}
import com.github.bruneli.scalaopt.core.linalg.DenseVector
import com.github.bruneli.scalaopt.core.variable.{DataPoint, Input, Outputs, UnconstrainedVariables}
import org.scalatest._

import scala.util.Random

/**
 * @author bruneli
 */
class FFNeuralNetworkTrainerSpec extends FlatSpec with Matchers {

  "gradient" should "be same as a SimpleMSEFunction for an MSE loss" in {
    import SeqDataSetConverter._

    val random = new Random(12345)

    val trueWeights = UnconstrainedVariables(0.1, 0.5, 0.7, 0.4, -0.5, 0.77, -0.1, 0.2, 0.2)
    val network = FFNeuralNetwork(
      Vector(2, 2, 1),
      trueWeights,
      MeanSquaredError,
      LogisticFunction,
      LinearFunction)

    val regressionFunc: (UnconstrainedVariablesType, InputsType) => OutputsType = {
      (weights, inputs) => network.withWeights(weights).forward(inputs).outputs
    }

    val data: DataSet[DataPoint] = for (i <- 1 to 10) yield {
      val x = DenseVector.iterate(Input(random.nextGaussian()), 2)((x: Input) => Input(random.nextGaussian()))
      val y = network.forward(x).outputs
      DataPoint(x, y)
    }

    val w0 = UnconstrainedVariables(-0.1, 0.6, -0.3, 0.6, -0.8, -0.7, +0.3, -0.1, 0.35)
    val trainer = network withWeights w0 trainOn data
    val mseFunction = SimpleMSEFunction(regressionFunc, data)

    val gradient1 = trainer.gradient(w0)
    val gradient2 = mseFunction.gradient(w0)

    for ((ele1, ele2) <- gradient1.force.zip(gradient2.force)) {
      ele1.x shouldBe ele2.x +- 1.0e-5
    }

  }

  it should "be same as a SimpleFunctionFiniteDiffGradient for a cross-entropy loss" in {
    import SeqDataSetConverter._

    val random = new Random(12345)

    val trueWeights = UnconstrainedVariables(0.1, 0.5, 0.7, 0.4, -0.5, 0.77, -0.1, 0.2, 0.2)
    val network = FFNeuralNetwork(
      Vector(2, 2, 1),
      trueWeights,
      CrossEntropy,
      LogisticFunction,
      LogisticFunction)

    val data: DataSet[DataPoint] = for (i <- 1 to 10) yield {
      val x = DenseVector.iterate(Input(random.nextGaussian()), 2)((x: Input) => Input(random.nextGaussian()))
      val y = network.forward(x).outputs
      val target = if (y.norm >= 0.5) 1.0 else 0.0
      DataPoint(x, Outputs(target))
    }

    val objectiveFunc: UnconstrainedVariablesType => Double = {
      weights => {
        val trainedNetwork = network.withWeights(weights)
        def loss(sum: Double, point: DataPoint) = {
          sum + trainedNetwork.forward(point.x).backward(point.y).loss
        }
        data.aggregate(0.0)(loss, _ + _) / data.size
      }
    }

    val w0 = UnconstrainedVariables(-0.1, 0.6, -0.3, 0.6, -0.8, -0.7, +0.3, -0.1, 0.35)
    val trainer = network withWeights w0 trainOn data
    val objFunction = ObjectiveFunctionFiniteDiffDerivatives(objectiveFunc, BFGS.defaultConfig)

    val loss1 = trainer(w0)
    val loss2 = objFunction(w0)
    loss1 shouldBe loss2 +- 1.0e-5
    val gradient1 = trainer.gradient(w0)
    val gradient2 = objFunction.gradient(w0)

    for ((ele1, ele2) <- gradient1.force.zip(gradient2.force)) {
      ele1.x shouldBe ele2.x +- 1.0e-5
    }

  }

  "jacobianAndResidualsMatrix" should "gives same results as a SimpleMSEFunction for a linear function" in {
    import SeqDataSetConverter._

    val random = new Random(12345)

    val trueWeights = UnconstrainedVariables(0.1, 0.5, 0.7, 0.4, -0.5, 0.77, -0.1, 0.2, 0.2)
    val network = FFNeuralNetwork(
      Vector(2, 2, 1),
      trueWeights,
      MeanSquaredError,
      LogisticFunction,
      LinearFunction)

    val regressionFunc: (UnconstrainedVariablesType, InputsType) => OutputsType = {
      (weights, inputs) => network.withWeights(weights).forward(inputs).outputs
    }

    val data: DataSet[DataPoint] = for (i <- 1 to 6) yield {
      val x = DenseVector.iterate(Input(random.nextGaussian()), 2)((x: Input) => Input(random.nextGaussian()))
      val y = network.forward(x).outputs
      DataPoint(x, y)
    }

    val w0 = UnconstrainedVariables(-0.1, 0.6, -0.3, 0.6, -0.8, -0.7, +0.3, -0.1, 0.35)
    val trainer = network withWeights w0 trainOn data
    val mseFunction = SimpleMSEFunction(regressionFunc, data)

    val matrix1 = trainer.jacobianAndResidualsMatrix(w0)
    val matrix2 = mseFunction.jacobianAndResidualsMatrix(w0)

    for ((row1, row2) <- matrix1.collect().zip(matrix2.collect())) {
      Math.abs(row1.b) shouldBe Math.abs(row2.b) +- 1.0e-5
      for ((x, y) <- row1.a.force.zip(row2.a.force)) {
        Math.abs(x) shouldBe Math.abs(y) +- 1.0e-5
      }
    }
  }

  "neural network" should "retrieve an initial regression network with BFGS" in {
    import SeqDataSetConverter._

    val random = new Random(12345)

    val trueWeights = UnconstrainedVariables(0.1, 0.5, 0.7, 0.4, -0.5, 0.77, -0.1, 0.2, 0.2)
    val network = FFNeuralNetwork(
      Vector(2, 2, 1),
      trueWeights,
      MeanSquaredError,
      LogisticFunction,
      LinearFunction)

    val data: DataSet[DataPoint] = for (i <- 1 to 10000) yield {
      val x = DenseVector.iterate(Input(random.nextGaussian()), 2)((x: Input) => Input(random.nextGaussian()))
      val y = network.forward(x).outputs
      DataPoint(x, y)
    }
    val w0 = UnconstrainedVariables(-0.1, 0.6, 0.6, 0.4, -0.6, 0.5, 0.0, 0.2, 0.35)
    val trainedNetwork = network
      .withWeights(w0)
      .trainOn(data)
      .withMethod(BFGS, Some(BFGS.defaultConfig.copy(tol = 1.0e-6)))

    trainedNetwork should be a 'success
    val wOpt = trainedNetwork.get.weights
    val distBeforeFit = (w0 - trueWeights).norm
    val distAfterFit = (wOpt - trueWeights).norm
    distAfterFit / distBeforeFit should be <= 0.1
  }

  it should "retrieve an initial classification network with BFGS" in {
    import SeqDataSetConverter._

    val random = new Random(12345)

    val trueWeights = UnconstrainedVariables(0.1, 1.5, 0.4)
    val network = FFNeuralNetwork(
      Vector(2, 1),
      trueWeights,
      CrossEntropy,
      LogisticFunction,
      LogisticFunction)

    val data: DataSet[DataPoint] = for (i <- 1 to 10000) yield {
      val x = DenseVector.iterate(Input(random.nextGaussian()), 2)((x: Input) => Input(random.nextGaussian()))
      val p = network.forward(x).outputs.coordinate(0)
      val k = if (random.nextDouble() < p) 1.0 else 0.0
      DataPoint(x, Outputs(k))
    }
    val w0 = UnconstrainedVariables(-0.5, 0.6, 0.6)
    val trainedNetwork = network
      .withWeights(w0)
      .trainOn(data)
      .withMethod(BFGS, Some(BFGS.defaultConfig.copy(tol = 1.0e-5)))

    trainedNetwork should be a 'success
    val wOpt = trainedNetwork.get.weights

    val distBeforeFit = (w0 - trueWeights).norm
    val distAfterFit = (wOpt - trueWeights).norm
    distAfterFit / distBeforeFit should be <= 0.05
  }

  it should "retrieve an initial multi-classification network with BFGS" in {
    import SeqDataSetConverter._

    val random = new Random(12345)

    val trueWeights = UnconstrainedVariables(0.1, 0.5, 0.7, 0.4, -0.5, 0.77, -0.1, 0.2, 0.2)
    val network = FFNeuralNetwork(
      Vector(2, 3),
      trueWeights,
      CrossEntropy,
      LogisticFunction,
      SoftMaxFunction)

    val data: DataSet[DataPoint] = for (i <- 1 to 10000) yield {
      val x = DenseVector.iterate(Input(random.nextGaussian()), 2)((x: Input) => Input(random.nextGaussian()))
      val probs = network.forward(x).outputs.force
      val boundaries = probs.tail.scanLeft((0.0, probs.head)) {
        case (prev, prob) => (prev._2, prev._2 + prob)
      }
      val g = random.nextDouble()
      val y = boundaries.map { case (low, high) => if (g >= low && g < high) 1.0 else 0.0 }
      DataPoint(x, new Outputs(y.toArray))
    }
    val w0 = UnconstrainedVariables(-0.1, 0.6, 0.5, 0.6, -0.6, 0.5, 0.0, 0.3, 0.25)
    val trainedNetwork = network
      .withWeights(w0)
      .trainOn(data)
      .withMethod(BFGS, Some(BFGS.defaultConfig.copy(tol = 1.0e-5)))

    trainedNetwork should be a 'success
    val wOpt = trainedNetwork.get.weights

    val distBeforeFit = (w0 - trueWeights).norm
    val distAfterFit = (wOpt - trueWeights).norm
    distAfterFit / distBeforeFit should be < 1.0
  }

  it should "train on data that mimics a regression network with Steihaug CG" in {
    import SeqDataSetConverter._

    val random = new Random(12345)

    val trueWeights = UnconstrainedVariables(0.1, 0.5, 0.7, 0.4, -0.5, 0.77, -0.1, 0.2, 0.2)
    val network = FFNeuralNetwork(
      Vector(2, 2, 1),
      trueWeights,
      MeanSquaredError,
      LogisticFunction,
      LinearFunction)

    val data: DataSet[DataPoint] = for (i <- 1 to 100) yield {
      val x = DenseVector.iterate(Input(random.nextGaussian()), 2)((x: Input) => Input(random.nextGaussian()))
      val y = network.forward(x).outputs
      DataPoint(x, y)
    }
    val w0 = UnconstrainedVariables(0.1, 0.5, 0.7, 0.4, -0.5, 0.77, -0.05, 0.2, 0.2)
    val cgConfig = SteihaugCG.defaultConfig.copy(tol = 0.05)
    val trainedNetwork = network
      .withWeights(w0)
      .trainOn(data)
      .withMethod(SteihaugCG, Some(cgConfig))

    trainedNetwork should be a 'success
    (trainedNetwork.get.weights - trueWeights).norm should be <= cgConfig.tol
  }

  it should "train on data that mimics a regression network with Levenberg-Marquardt" in {
    import SeqDataSetConverter._

    val random = new Random(12345)

    val trueWeights = UnconstrainedVariables(0.1, 0.5, 0.7, 0.4, -0.5, 0.77, -0.1, 0.2, 0.2)
    val network = FFNeuralNetwork(
      Vector(2, 2, 1),
      trueWeights,
      MeanSquaredError,
      LogisticFunction,
      LinearFunction)

    val data: DataSet[DataPoint] = for (i <- 1 to 50) yield {
      val x = DenseVector.iterate(Input(random.nextGaussian()), 2)((x: Input) => Input(random.nextGaussian()))
      val y = network.forward(x).outputs
      DataPoint(x, y)
    }
    val w0 = UnconstrainedVariables(-0.1, 0.6, 0.6, 0.4, -0.6, 0.5, 0.0, 0.2, 0.35)
    val trainedNetwork = network withWeights w0 trainOn data withMethod LevenbergMarquardt
    val optWeights = trainedNetwork.get.weights

    val regressionFunc: (UnconstrainedVariablesType, InputsType) => OutputsType = {
      (weights, inputs) => network.withWeights(weights).forward(inputs).outputs
    }
    val mseFunction = SimpleMSEFunction(regressionFunc, data)
    val wOpt = LevenbergMarquardt.minimize(mseFunction, w0)(LevenbergMarquardt.defaultConfig)

    trainedNetwork should be a 'success
    (trainedNetwork.get.weights - trueWeights).norm should be <= LevenbergMarquardt.defaultConfig.tol
  }

  it should "retrieve an initial classification network with Levenberg-Marquardt" in {
    import SeqDataSetConverter._

    val random = new Random(12345)

    val trueWeights = UnconstrainedVariables(0.1, 1.5, 0.4)
    val network = FFNeuralNetwork(
      Vector(2, 1),
      trueWeights,
      CrossEntropy,
      LogisticFunction,
      LogisticFunction)

    val data: DataSet[DataPoint] = for (i <- 1 to 1000) yield {
      val x = DenseVector.iterate(Input(random.nextGaussian()), 2)((x: Input) => Input(random.nextGaussian()))
      val p = network.forward(x).outputs.coordinate(0)
      val k = if (random.nextDouble() < p) 1.0 else 0.0
      DataPoint(x, Outputs(k))
    }
    val w0 = UnconstrainedVariables(1.6, 1.6, 1.6)
    val trainedNetwork = network
      .withWeights(w0)
      .trainOn(data)
      .withMethod(LevenbergMarquardt)

    trainedNetwork should be a 'success
    val wOpt = trainedNetwork.get.weights

    val distBeforeFit = (w0 - trueWeights).norm
    val distAfterFit = (wOpt - trueWeights).norm
    distAfterFit / distBeforeFit should be <= 0.25
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
