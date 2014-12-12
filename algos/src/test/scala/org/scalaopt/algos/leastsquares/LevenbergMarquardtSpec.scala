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

package org.scalaopt.algos.leastsquares

import org.scalaopt.algos._
import org.scalaopt.algos.gradient.ConjugateGradient.CGConfig
import org.scalatest.{Matchers, FlatSpec}

import scala.util.{Failure, Success, Random}

/**
 * @author bruneli
 */
class LevenbergMarquardtSpec extends FlatSpec with Matchers {
  import SeqDataSetConverter._
  import LevenbergMarquardt._

  val random = new Random(12345)

  "minimize" should "converge to its solution for a linear system" in {
    /** Linear objective function */
    def linear(p: Coordinates, x: Seq[Double]): Double = p.head + p.tail.zip(x).map { case (p, x) => p * x }.sum[Double]

    val tol = 0.5
    val n = 10
    val m = 1000
    val p0 = 80.0 +: (0 until n).map(_.toDouble)

    val data = for (i <- 0 until m) yield {
      val x = randomVec(n, random)
      val y = linear(p0, x) + random.nextGaussian()
      (x, y)
    }

    minimize(linear, data, p0) match {
      case Success(popt) => {
        popt.size shouldBe (n + 1)
        for ((estimated, expected) <- popt.zip(p0)) estimated shouldBe expected +- tol
      }
      case Failure(e) => assert(false)
    }
  }

  it should "throw an error if wrong configuration type" in {
    val wrongc = new CGConfig
    def f(p: Coordinates, x: Seq[Double]): Double = p.head + p.tail.zip(x).map { case (p, x) => p * x }.sum[Double]
    val p0 = Vector(10.0, 1.0, 2.0)
    val data = for (i <- 0 until 10) yield {
      val x = randomVec(2, random)
      (x, f(p0, x))
    }
    a [IllegalArgumentException] should be thrownBy {
      minimize(f, data, Vector(0.0, 0.0))(wrongc)
    }
  }

  private def randomVec(n: Int, random: Random) = for (i <- 0 until n) yield random.nextDouble()
}
