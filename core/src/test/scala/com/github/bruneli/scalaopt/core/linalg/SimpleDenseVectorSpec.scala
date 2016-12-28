/*
 * Copyright 2016 Renaud Bruneliere
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

import com.github.bruneli.scalaopt.core.linalg.FromToDoubleConversions._
import com.github.bruneli.scalaopt.core.variable._
import org.apache.commons.math3.linear.Array2DRowRealMatrix
import org.scalatest.{FlatSpec, Matchers}

/**
 * @author bruneli
 */
class SimpleDenseVectorSpec extends FlatSpec with Matchers {

  import RealValue._

  val v1 = SimpleDenseVector(RealValue(3.0), RealValue(2.0))
  val v2 = SimpleDenseVector(RealValue(2.0), RealValue(1.0))

  "DenseVector" should "build a vector with three real values" in {

    val vector = SimpleDenseVector(RealValue(101.1), RealValue(-50.5), RealValue(70.5))

    vector.length shouldBe 3
    vector(0) shouldBe RealValue(101.1)
    vector(1) shouldBe RealValue(-50.5)
    vector(2) shouldBe RealValue(70.5)

  }

  "v1 ++ v2" should "concatenate the two vectors" in {

    val v = v1 ++ v2
    v shouldBe SimpleDenseVector(RealValue(3.0), RealValue(2.0), RealValue(2.0), RealValue(1.0))

  }

  "concatenating unconstrained variables with positive variables" should "produce a vector of continuous variables" in {

    val v = UnconstrainedVariables(-1.0, 2.0) ++ PositiveVariables(0.0, 3.0)

    v(0) shouldBe a[ContinuousVariable]
    v(2) shouldBe a[ContinuousVariable]

  }

  "take" should "return a DenseVector" in {

    val vector = SimpleDenseVector(RealValue(101.1), RealValue(-50.5), RealValue(70.5)).take(1)

    vector shouldBe a[DenseVector[_]]
    vector.length shouldBe 1
    vector(0) shouldBe RealValue(101.1)

  }

  "updated" should "return a DenseVector with one index modified" in {

    val v3 = v1.updated(0, 4.0)
    // v1 should be unchanged
    v1 shouldBe SimpleDenseVector(RealValue(3.0), RealValue(2.0))
    v3 shouldBe SimpleDenseVector(RealValue(4.0), RealValue(2.0))

  }

  "DenseVector(3, 2) + DenseVector(2, 1)" should "be DenseVector(5, 3)" in {
    v1 + v2 shouldBe SimpleDenseVector(RealValue(5.0), RealValue(3.0))
  }

  "DenseVector(3, 2) - DenseVector(2, 1)" should "be DenseVector(1, 1)" in {
    v1 - v2 shouldBe SimpleDenseVector(RealValue(1.0), RealValue(1.0))
  }

  "-DenseVector(3, 2)" should "be DenseVector(-3, -2)" in {
    -v1 shouldBe SimpleDenseVector(RealValue(-3.0), RealValue(-2.0))
  }

  "DenseVector(3, 2) * 2" should "be DenseVector(6, 4)" in {
    v1 * 2.0 shouldBe SimpleDenseVector(RealValue(6.0), RealValue(4.0))
  }

  "DenseVector(4, 2) / 2" should "be DenseVector(2, 1)" in {
    SimpleDenseVector(RealValue(4.0), RealValue(2.0)) / 2.0 shouldBe SimpleDenseVector(RealValue(2.0), RealValue(1.0))
  }

  "dot product of two DenseVectors" should "be as expected" in {
    (SimpleDenseVector(RealValue(1.0), RealValue(2.0)) dot SimpleDenseVector(RealValue(3.0), RealValue(4.0))) should be (1.0 * 3.0 + 2.0 * 4.0)
  }

  "norm of DenseVector(3, 4)" should "be 5" in {
    SimpleDenseVector(RealValue(3.0), RealValue(4.0)).norm shouldBe 5.0
  }

  "DenseVector(1, 2) outer DenseVector(1, 3)" should "be ((1, 3),(2, 6))" in {
    val m = new Array2DRowRealMatrix(Array(Array(1.0, 3.0), Array(2.0, 6.0)))
    (SimpleDenseVector(RealValue(1.0), RealValue(2.0)) outer SimpleDenseVector(RealValue(1.0), RealValue(3.0))) shouldBe m
  }

  "vector(3, 2.0)" should "create a vector of size 3 filled with 2" in {
    DenseVector.vector[RealValue](3, 2.0) shouldBe SimpleDenseVector(RealValue(2.0), RealValue(2.0), RealValue(2.0))
  }

  "zeros(3)" should "create a vector of size 3 filled with zeros" in {
    DenseVector.zeros[RealValue](3) shouldBe SimpleDenseVector(RealValue(0.0), RealValue(0.0), RealValue(0.0))
  }

  "ones(3)" should "create a vector of size 3 filled with ones" in {
    DenseVector.ones[RealValue](3) shouldBe SimpleDenseVector(RealValue(1.0), RealValue(1.0), RealValue(1.0))
  }

  "e(3, 1)" should "create a basis vector of dimension 3 along axis 1" in {
    DenseVector.e[RealValue](3, 1) shouldBe SimpleDenseVector(RealValue(0.0), RealValue(1.0), RealValue(0.0))
  }

  "gradient" should "compute the gradient of f in x" in {
    val f = (x: DenseVector[RealValue]) => (x dot x) + (x dot v2) + 2.0
    val df = (x: DenseVector[RealValue]) => x * 2.0 + v2

    // Gradient from finite differences in v1
    val grad1 = DenseVector.gradient(f, v1)
    // Analytics gradient in v1
    val grad2 = df(v1)

    grad1.length shouldBe 2
    grad1(0).x shouldBe grad2(0).x +- 1.0e-6
    grad1(1).x shouldBe grad2(1).x +- 1.0e-6
  }

  "min" should "build a new vector from two vectors with the minimum of each pair of elements" in {

    DenseVector.min(Constants(2.0, 2.0), Constants(1.0, 3.0)) shouldBe Constants(1.0, 2.0)

  }

  "max" should "build a new vector from two vectors with the minimum of each pair of elements" in {

    DenseVector.max(Constants(2.0, 2.0), Constants(1.0, 3.0)) shouldBe Constants(2.0, 3.0)

  }

}

case class RealValue(x: Double) extends AnyVal with ToDouble

object RealValue {

  implicit val RealValueFromDouble: FromDouble[RealValue] = (x: Double) => RealValue(x)

}