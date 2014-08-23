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

package org.scalaopt.algos.linesearch

import org.scalatest.FunSuite
import org.scalaopt.algos.MaxIterException
import scala.util.{Try, Success, Failure}

class GoldSearchSuite extends FunSuite {
  import GoldSearch._
  
  test("Bracket a minimum") {
    bracket(x => x * x, 4.0) match {
      case Success((a , b)) => assert(a < 0.0 && b > 0.0)
      case Failure(e) => assert(false)
    }
  }
  
  test("Fails to bracket") {
    intercept[MaxIterException] {
      bracket(x => x, 4.0)
    }
  }
  
  test("Find a minimum") {
    val tol: Double = 1.0e-9
    val x0: Double = 1.0
    def f(x: Double) = (x- x0) * (x - x0)
    val (xmin, fmin) = minimize(f, -4.0, 4.0)
    assert(math.abs(xmin - x0) < tol)
    assert(f(xmin) < f(x0 + tol))
  }
}