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

package org.scalaopt.algos.linear

import org.scalaopt.algos._
import org.scalaopt.algos.SeqDataSetConverter
import org.scalatest.{Matchers, FlatSpec}

import scala.util.{Failure, Success, Random}

/**
 * @author bruneli
 */
class LinearSpec extends FlatSpec with Matchers {

  import SeqDataSetConverter._

  "lm" should "provide the right solution when run with 11 unknowns and 1000 events" in {
    val random = new Random(12345)
    val tol = 0.5
    val n = 10
    val m = 1000
    val par0 = 80.0
    val pars = for (j <- 0 until n) yield (j / 1).toDouble
    val data = for (i <- 0 until m) yield {
      val x = randomVec(n, random)
      val y = par0 + x.zip(pars).map { case (x, p) => x*p }.sum + random.nextGaussian()
      DataPoint(x, Seq(y))
    }
    lm(data) match {
      case Success(solution) => {
        solution.size shouldBe (n + 1)
        for ((estimated, expected) <- solution.zip(par0 +: pars)) estimated shouldBe expected +- tol
      }
      case Failure(e) => assert(false)
    }
  }

  private def randomVec(n: Int, random: Random) = for (i <- 0 until n) yield random.nextDouble()
}
