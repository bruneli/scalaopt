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
class RichMatrixSpec extends FlatSpec with Matchers {
  import org.jblas.DoubleMatrix
  
  val m1 = new DoubleMatrix(2, 2, 4.0, 2.0, 3.0, 6.0)
  val m2 = new DoubleMatrix(2, 2, 1.0, 2.0, 2.0, 4.0)
  
  "m1 + m2" should "be ((5, 4), (5, 10))" in {
    val m3 = new DoubleMatrix(2, 2, 5.0, 4.0, 5.0, 10.0)
    (m1 + m2) should be (m3)
  }
  
  it should "throw an exception when m1 and m2 have different dimensions" in {
    val m3 = new DoubleMatrix(2, 1, 1.0, 1.0)
    a [IllegalArgumentException] should be thrownBy {
      m1 + m3
    }
  }
  
  "m1 - m2" should "be ((3, 0), (2, 2))" in {
    val m3 = new DoubleMatrix(2, 2, 3.0, 0.0, 1.0, 2.0)
    (m1 - m2) should be (m3)
  }
  
  it should "throw an exception when m1 and m2 have different dimensions" in {
    val m3 = new DoubleMatrix(2, 1, 1.0, 1.0)
    a [IllegalArgumentException] should be thrownBy {
      m1 - m3
    }
  }
  
  "-m1" should "be ((-4, -2),(-3, -6))" in {
    val m3 = new DoubleMatrix(2, 2, -4.0, -2.0, -3.0, -6.0)
    (-m1) should be (m3)
  }
  
  "m1 * 2" should "be ((8, 4),(6, 12))" in {
    val m3 = new DoubleMatrix(2, 2, 8.0, 4.0, 6.0, 12.0)
    (m1 * 2.0) should be (m3)
  }
  
  "m1 / 2" should "be ((2, 1),(1.5, 3))" in {
    val m3 = new DoubleMatrix(2, 2, 2.0, 1.0, 1.5, 3.0)
    (m1 / 2.0) should be (m3)
  }
  
  "m1 / 0" should "throw an exception" in {
    a [IllegalArgumentException] should be thrownBy {
      m1 / 0.0
    }
  }
  
  "m1 * m2" should "be ((10, 14), (20, 28))" in {
    val m3 = new DoubleMatrix(2, 2, 10.0, 14.0, 20.0, 28.0)
    (m1 * m2) should be (m3)
  }
  
  it should "throw an exception when columns(m1) != rows(m2)" in {
    val m3 = new DoubleMatrix(1, 2, 1.0, 1.0)
    a [IllegalArgumentException] should be thrownBy {
      m1 * m3
    }
  }

}