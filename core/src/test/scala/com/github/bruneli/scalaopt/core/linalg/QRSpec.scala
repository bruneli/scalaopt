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

package com.github.bruneli.scalaopt.core.linalg

import com.github.bruneli.scalaopt.core.SeqDataSetConverter
import com.github.bruneli.scalaopt.core.variable.{Inputs, Output, Outputs, UnconstrainedVariables}
import org.scalatest.{FlatSpec, Matchers}

/**
 * @author bruneli
 */
class QRSpec extends FlatSpec with Matchers {

  import SeqDataSetConverter._

  val tol = 1.0e-5

  // Augmented matrix representing the linear system AX=B to solve
  val ab = List(
    Inputs(2.0, 3.0, 1.0) -> Output(2.0),
    Inputs(3.0, 8.0, 1.0) -> Output(1.0),
    Inputs(4.0, 2.0, 9.0) -> Output(0.5))

  // Solution X
  val sol = UnconstrainedVariables(2.5, -0.7, -0.9)

  val m = ab.zipWithIndex.map {
    case ((inputs, output), row) => AugmentedRow(inputs, output, row.toLong)
  }

  /**
   * > m = matrix(c(2,3,4,3,8,2,1,1,9), 3, 3)
   * > qr(m)
   * $qr
   *             [,1]       [,2]      [,3]
   * [1,] -5.3851648 -7.0564229 -7.613509
   * [2,]  0.5570860 -5.2160231  4.740036
   * [3,]  0.7427814 -0.6608145  1.602042
   *
   * $pivot
   * [1] 1 2 3
   *
   * > qr.qty(qr(m), c(2, 1, 0.5))
   * [1] -1.6712580 -0.6148164 -1.4418382
   *
   * > qr.solve(m, c(2, 1, 0.5))
   * [1]  2.5 -0.7 -0.9
   */
  "R from QR decomposition without pivoting" should "agree with R result" in {
    val qr = QR(m, 3, pivoting = false)

    val r = List(
      List(-5.3851648, -7.0564229, -7.613509),
      List(0.0, -5.2160231, 4.740036),
      List(0.0, 0.0, -1.602042))

    val qtb = Outputs(-1.6712580, -0.6148164, 1.4418382)

    // Check no pivoting has occurred
    (0 until 3).zip(qr.ipvt).foreach {
      case (i, j) => i shouldBe j
    }

    // Check R matrix agrees with R result (up to a sign)
    for (i <- 0 until 3) {
      for (j <- 0 until 3) {
        qr.R(i, j).x shouldBe r(i)(j) +- tol
      }
    }

    // Check qtb is same (up to a sign)
    qtb.force.zip(qr.qtb.force).foreach {
      case (x, y) => x.x shouldBe y.x +- tol
    }

    // Check solution is same
    sol.force.zip(qr.solution.force).foreach {
      case (x, y) => x.x shouldBe y.x +- tol
    }
  }

  /**
   * > m = matrix(c(2,3,4,3,8,2,1,1,9), 3, 3)
   * > qr(m, LAPACK=T)
   * $qr
   *             [,1]       [,2]       [,3]
   * [1,] -9.11043358 -3.1831635 -4.5003347
   * [2,]  0.09890773 -8.1772532 -2.8951896
   * [3,]  0.89016954 -0.2251146 -0.6040404
   *
   * $pivot
   * [1] 3 2 1
   *
   * > qr.qty(qr(m, LAPACK=T), c(2, 1, 0.5))
   *            [,1]
   * [1,] -0.8232319
   * [2,] -1.5138969
   * [3,] -1.5101011
   *
   * > qr.solve(qr(m, LAPACK=T), c(2, 1, 0.5))
   * [1]  2.5 -0.7 -0.9
   */
  "QR decomposition with pivoting" should "agree with R result" in {
    val qr = QR(m, 3, pivoting = true)

    val r = List(
      List(-9.11043358, -3.1831635, -4.5003347),
      List(0.0, -8.1772532, -2.8951896),
      List(0.0, 0.0, 0.6040404))

    val qtb = Outputs(-0.8232319, -1.5138969, 1.5101011)

    // Check pivoting
    List(3, 2, 1).zip(qr.ipvt).foreach {
      case (i, j) => (i - 1) shouldBe j
    }

    // Check R matrix agrees with R result (up to a sign)
    for (i <- 0 until 3) {
      for (j <- 0 until 3) {
        qr.R(i, j).x shouldBe r(i)(j) +- tol
      }
    }

    // Check qtb is same (up to a sign)
    qtb.force.zip(qr.qtb.force).foreach {
      case (x, y) => x.x shouldBe y.x +- tol
    }

    // Check solution is same
    sol.force.zip(qr.solution.force).foreach {
      case (x, y) => x.x shouldBe y.x +- tol
    }
  }

  "QR decomposition" should "fail if dataset is empty" in {
    val mat = List[AugmentedRow]()
    a [IllegalArgumentException] should be thrownBy {
      QR(mat, 3)
    }
  }

  "QR decomposition" should "fail if number of rows provided is insufficient" in {
    val mat = List(AugmentedRow(Inputs(1.0, 2.0, 3.0), Output(0.0), 0l))
    a [IllegalArgumentException] should be thrownBy {
      QR(mat, 3)
    }
  }
}
