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

package com.github.bruneli.scalaopt.core.linesearch

import com.github.bruneli.scalaopt.core.MaxIterException

import scala.util.{Try, Success, Failure}
import org.scalatest._
import org.scalatest.Matchers._

class GoldSearchSpec extends FlatSpec with Matchers {
  import GoldSearch._
  
  "bracket" should "find a < 0 and b > 0" in {
    bracket(x => x * x, 4.0) match {
      case Success((a , b)) => {
        a should be < 0.0
        b should be > 0.0
      }
      case Failure(e) => assert(false)
    }
  }
  
  it should "throw an exception when reaching max number of iterations" in {
    a [MaxIterException] should be thrownBy {
      bracket(x => x, 4.0)
    }
  }
  
  "minimize" should "converge to x0" in {
    val tol: Double = 1.0e-9
    val x0: Double = 1.0
    def f(x: Double) = (x- x0) * (x - x0)
    val (xmin, fmin) = minimize(f, -4.0, 4.0)
    math.abs(xmin - x0) should be < tol
    f(xmin) should be < f(x0 + tol)
  }
}