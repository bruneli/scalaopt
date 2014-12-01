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

package org.scalaopt.sparkapps

import org.apache.spark.SparkContext
import org.scalaopt.algos.linalg.{QR, AugmentedRow}
import org.scalatest.{Matchers, FlatSpec}

/**
 * @author bruneli
 */
class SparkDataSetSpec extends FlatSpec with Matchers {

  import SparkDataSetConverter._

  val tol = 1.0e-5

  // Augmented matrix representing the linear system AX=B to solve
  val ab = List(
    List(2.0, 3.0, 1.0, 2.0),
    List(3.0, 8.0, 1.0, 1.0),
    List(4.0, 2.0, 9.0, 0.5))

  // Solution X
  val sol = List(2.5, -0.7, -0.9)

  val m = ab.zipWithIndex.map {
    case (ab, i) => AugmentedRow(ab.init, ab.last, i.toLong)
  }

  val sparkContext = new SparkContext("local[1]", "test App")
  val sc = new SparkContext(sparkContext)
  val rdd = sc.parallelize(m)

  "spark data set" should "invert a matrix via QR decomposition" in {
    val qr = QR(rdd, 3, pivoting = true)

    val r = List(
      List(-9.11043358, -3.1831635, -4.5003347),
      List(0.0, -8.1772532, -2.8951896),
      List(0.0, 0.0, 0.6040404))

    val qtb = List(-0.8232319, -1.5138969, 1.5101011)

    // Check pivoting
    List(3, 2, 1).zip(qr.ipvt).map {
      case (i, j) => (i - 1) shouldBe j
    }

    // Check R matrix agrees with R result (up to a sign)
    for (i <- 0 until 3) {
      for (j <- 0 until 3) {
        qr.R(i, j) shouldBe r(i)(j) +- tol
      }
    }

    // Check qtb is same (up to a sign)
    qtb.zip(qr.qtb).map {
      case (x, y) => x shouldBe y +- tol
    }

    // Check solution is same
    sol.zip(qr.solution).map {
      case (x, y) => x shouldBe y +- tol
    }
  }
}
