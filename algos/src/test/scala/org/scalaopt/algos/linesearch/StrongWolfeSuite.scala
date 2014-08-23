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

class StrongWolfeSuite extends FunSuite {
  import StrongWolfe._

  test("Find quadratic polynomial minimum within an interval") {
    val tol: Double = 1.0e-9
    val x0: Double = 0.3
    def f(x: Double) = (x - x0) * (x - x0)
    def df(x: Double) = 2.0 * (x - x0)
    zoomStepLength(f, df, 0.0, 1.0) match {
      case Success(xmin) => {
        assert(math.abs(xmin - x0) < tol)
        assert(f(xmin) < f(x0 + tol))
      }
      case Failure(e) => assert(false)
    }
  }
  
  test("Find 3rd order polynomial minimum within an interval") {
    val tol: Double = 1.0e-9
    val x0: Double = 0.5
    def f(x: Double) = (x - x0) * (x - x0) + (x - x0) * (x - x0) * (x - x0)
    def df(x: Double) = 2.0 * (x - x0) + 3.0 * (x - x0) * (x - x0)
    val xmin = zoomStepLength(f, df, 0.0, 1.0)
    zoomStepLength(f, df, 0.0, 1.0) match {
      case Success(xmin) => {
        assert(math.abs(xmin - x0) < tol)
        assert(f(xmin) < f(x0 + tol))
      }
      case Failure(e) => assert(false)
    }
  }
  
  test("Fail to find minimum within an interval") {
    intercept[MaxIterException] {
      zoomStepLength(x => x, x => 1.0, 0.0, 1.0)
    }
  }
  
  test("Find quadratic polynomial step length") {
    val tol: Double = 1.0e-9
    val x0: Double = 0.3
    def f(x: Double) = (x - x0) * (x - x0)
    def df(x: Double) = 2.0 * (x - x0)
    findStepLength(f, df) match {
      case Success(xmin) => {
        assert(math.abs(xmin - x0) < tol)
        assert(f(xmin) < f(x0 + tol))
      }
      case Failure(e) => assert(false)
    }
  }
  
  test("Find 3rd order polynomial step length") {
    val tol: Double = 1.0e-9
    val x0: Double = 0.5
    def f(x: Double) = (x - x0) * (x - x0) + (x - x0) * (x - x0) * (x - x0)
    def df(x: Double) = 2.0 * (x - x0) + 3.0 * (x - x0) * (x - x0)
    findStepLength(f, df) match {
      case Success(xmin) => {
        assert(math.abs(xmin - x0) < tol)
        assert(f(xmin) < f(x0 + tol))
      }
      case Failure(e) => assert(false)
    }
  }
  
  test("Fail to find an approximate step length") {
    intercept[MaxIterException] {
      findStepLength(x => x, x => 1.0)
    }
  }
}