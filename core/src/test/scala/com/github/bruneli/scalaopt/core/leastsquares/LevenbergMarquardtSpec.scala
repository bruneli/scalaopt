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

package com.github.bruneli.scalaopt.core.leastsquares

import com.github.bruneli.scalaopt.core._
import com.github.bruneli.scalaopt.core.function.RegressionFunction
import com.github.bruneli.scalaopt.core.variable._
import org.scalatest.{FlatSpec, Matchers, TryValues}

import scala.util.Random

/**
 * @author bruneli
 */
class LevenbergMarquardtSpec extends FlatSpec with Matchers with TryValues {

  import SeqDataSetConverter._
  import LevenbergMarquardt._

  val random = new Random(12345)

  "minimize" should "converge to its solution for a linear system" in {
    /** Linear regression, first variable is the intercept */
    val linear: RegressionFunction = {
      (p: UnconstrainedVariablesType, x: InputsType) => Outputs(p(0) + p.force.tail.inner(x))
    }

    val tol = 0.5
    val n = 10
    val m = 1000
    val p0 = new UnconstrainedVariables((80.0 +: (0 until n).map(_.toDouble)).toArray)

    val data: DataSet[DataPoint] =
      for (i <- 0 until m) yield {
        val x = randomVec(n, random)
        val y = Output(linear(p0, x)(0) + random.nextGaussian())
        DataPoint(x, y)
      }

    val pOpt = minimize((linear, data), p0)
    pOpt should be a 'success
    pOpt.get.length should be(n + 1)
    for ((estimated, expected) <- pOpt.get.force.zip(p0)) estimated.x shouldBe expected.x +- tol
  }

  it should "converge to its solution for a non-linear objective function" in {
    /** Exponential regression function */
    val exponential: RegressionFunction = {
      (x: UnconstrainedVariablesType, t: InputsType) => Outputs(x(0) * Math.exp(x(1) * t(0)))
    }
    val tol = 0.2
    val n = 10
    val x0 = UnconstrainedVariables(2.0, 1.0)
    val sigma = 0.1

    val data: DataSet[DataPoint] = for (i <- 0 until n) yield {
      val t = Inputs(i.toDouble / n)
      val y = Output(exponential(x0, t)(0) + sigma * random.nextGaussian())
      DataPoint(t, y)
    }

    val solution = minimize((exponential, data), UnconstrainedVariables(4.0, 0.5))
    solution should be a 'success
    solution.get(0).x shouldBe x0(0).x +- tol
    solution.get(1).x shouldBe x0(1).x +- tol

  }

  private def randomVec(n: Int, random: Random): Inputs = {
    val randomSeq = for (i <- 0 until n) yield random.nextDouble()
    new Inputs(randomSeq.toArray)
  }
}
