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

package org.scalaopt.algos

import org.scalatest._
import org.scalatest.Matchers._

/**
 * @author bruneli
 */
class RichCoordinatesSpec extends FlatSpec with Matchers {
  import org.jblas.DoubleMatrix

  "Vector(3, 2) + Vector(2, 1)" should "be Vector(5, 3)" in {
    (Vector(3.0, 2.0) + Vector(2.0, 1.0)) should be (Vector(5.0, 3.0))
  }
  
  "Vector(3, 2) - Vector(2, 1)" should "be Vector(1, 1)" in {
    (Vector(3.0, 2.0) - Vector(2.0, 1.0)) should be (Vector(1.0, 1.0))
  }
  
  "-Vector(3, 2)" should "be Vector(-3, -2)" in {
    (-Vector(3.0, 2.0)) should be (Vector(-3.0, -2.0))
  }
  
  "Vector(3, 2) * 2" should "be Vector(6, 4)" in {
    (Vector(3.0, 2.0) * 2.0) should be (Vector(6.0, 4.0))
  }
  
  "Vector(4, 2) / 2" should "be Vector(2, 1)" in {
    (Vector(4.0, 2.0) / 2.0) should be (Vector(2.0, 1.0))
  }
  
  "Vector(4, 2) / 0" should "throw an exception" in {
    a [IllegalArgumentException] should be thrownBy {
      Vector(4.0, 2.0) / 0.0
    }
  }
  
  "dot product of two vectors" should "be as expected" in {
    (Vector(1.0, 2.0) dot Vector(3.0, 4.0)) should be (1.0 * 3.0 + 2.0 * 4.0)
  }
  
  it should "throw an exception when dimensions are different" in {
    a [IllegalArgumentException] should be thrownBy {
      Vector(1.0, 2.0) dot Vector(3.0, 4.0, 5.0)
    }
  }

  "norm of Vector(3, 4)" should "be 5" in {
    (Vector(3.0, 4.0).norm) should be (5.0)
  }

  "Vector(1, 2).toMatrix" should "be DoubleMatrix(Array(1, 2))" in {
    (Vector(1.0, 2.0).toMatrix) should be (new DoubleMatrix(Array(1.0, 2.0)))
  }
  
  "Vector(1, 2).t" should "be DoubleMatrix(1, 2, 1.0, 2.0)" in {
    (Vector(1.0, 2.0).t) should be (new DoubleMatrix(1, 2, 1.0, 2.0))
  }
  
  "Vector(1, 2) outer Vector(1, 3)" should "be ((1, 3),(2, 6))" in {
    val m = new DoubleMatrix(2, 2, 1.0, 2.0, 3.0, 6.0)
    (Vector(1.0, 2.0) outer Vector(1.0, 3.0)) should be (m)
  }

}