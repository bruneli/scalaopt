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

package com.github.bruneli.scalaopt.sparkapps

import com.github.bruneli.scalaopt.core.linalg.{AugmentedRow, QR}
import com.github.bruneli.scalaopt.core.variable.Inputs
import org.apache.spark.SparkContext
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

import scala.util.Random

/**
 * @author bruneli
 */
class SparkDataSetSpec extends FlatSpec with Matchers with BeforeAndAfter {

  import SparkDataSetConverter._

  val tol = 1.0e-5

  // Augmented matrix representing the linear system AX=B to solve
  val ab = List(
    Array(2.0, 3.0, 1.0, 2.0),
    Array(3.0, 8.0, 1.0, 1.0),
    Array(4.0, 2.0, 9.0, 0.5))

  // Solution X
  val sol = List(2.5, -0.7, -0.9)

  val m = ab.zipWithIndex.map {
    case (row, i) => AugmentedRow(new Inputs(row.init), row.last, i.toLong)
  }

  var sc: SparkContext = _

  before {
    System.clearProperty("spark.driver.port")
    System.clearProperty("spark.hostPort")

    sc = new SparkContext("local[2]", "SparkDataSetSpec")
  }

  "++" should "perform the union of two data sets" in {
    val data1: SparkDataSet[Double] = sc.parallelize(List(1.0, 10.0, 5.0, 3.0))
    val data2: SparkDataSet[Double] = sc.parallelize(List(-20.0, 1.0))
    (data1 ++ data2).aggregate(0.0)(_ + _, _ + _) shouldBe 0.0 +- 1.0e-8
  }

  "spark data set" should "invert a matrix via QR decomposition" in {
    val rdd = sc.parallelize(m)
    val qr = QR(rdd, 3, pivoting = true)

    val r = List(
      List(-9.11043358, -3.1831635, -4.5003347),
      List(0.0, -8.1772532, -2.8951896),
      List(0.0, 0.0, 0.6040404))

    val qtb = List(-0.8232319, -1.5138969, 1.5101011)

    // Check pivoting
    List(3, 2, 1).zip(qr.ipvt).foreach {
      case (i, j) => (i - 1) shouldBe j
    }

    // Check R matrix agrees with R result (up to a sign)
    for (i <- 0 until 3) {
      for (j <- 0 until 3) {
        qr.R(i, j) shouldBe r(i)(j) +- tol
      }
    }

    // Check qtb is same (up to a sign)
    qtb.zip(qr.qtb.force.coordinates).foreach {
      case (x, y) => x shouldBe y +- tol
    }

    // Check solution is same
    sol.zip(qr.solution.force.coordinates).foreach {
      case (x, y) => x shouldBe y +- tol
    }
  }

  "spark data set" should "provide a solution when run with lm" in {
    val random = new Random(12345)
    val n = 10
    val pars = for (j <- 0 until n) yield (j / 10).toDouble
    val data = for (i <- 0 until 100) yield {
      val x = randomVec(n, random).toArray
      val y = 80.0 + x.zip(pars).map { case (x, p) => x*p }.sum + random.nextGaussian()
      AugmentedRow(new Inputs(1.0 +: x), y, i)
    }
    val rdd = sc.parallelize(data)
    val sol = QR(rdd, n + 1).solution
    sol.length shouldBe (n + 1)
  }

  after {
    sc.stop()
    sc = null

    System.clearProperty("spark.driver.port")
    System.clearProperty("spark.hostPort")
  }

  private def randomVec(n: Int, random: Random) = for (i <- 0 until n) yield random.nextDouble()
}