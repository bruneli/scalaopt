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

import org.scalaopt.algos._
import scala.util.{Success, Failure}
import org.scalatest._
import org.scalatest.Matchers._

class StrongWolfeSpec extends FlatSpec with Matchers {
  import StrongWolfe._

  "zoomStepLength" should "converge to minimum of 2d order polynomial" in {
    val tol = Seq(1.0e-9)
    val x0 = Seq(0.3)
    val pk = Seq(1.0)
    val f = (x: Variables) => (x - x0) dot (x - x0)
    val df = (x: Variables) => (x - x0) * 2.0
    val pt0 = LineSearchPoint(Seq(0.0), (f, df), pk)
    val pt1 = pt0.copy(x = Seq(1.0))
    zoomStepLength(0.0, pt0, 1.0, pt1, pt0) match {
      case Success(ptmin) => {
        (ptmin.x - x0).norm should be < tol.norm
        ptmin.fx should be < f(x0 + tol)
      }
      case Failure(e) => assert(false)
    }
  }

  it should "converge to minimum of 3rd order polynomial" in {
    val tol = Seq(1.0e-9)
    val x0 = Seq(0.5)
    val pk = Seq(1.0)
    val f = (x: Variables) => {
      val dx = (x - x0).norm
      dx * dx + Math.pow(dx, 3.0)
    }
    val df = (x: Variables) => {
      val dx = (x - x0).norm
      (x - x0) * 2.0 + pk * (3.0 * dx * dx)
    }
    val pt0 = LineSearchPoint(Seq(0.0), (f, df), pk)
    val pt1 = pt0.copy(x = Seq(1.0))
    zoomStepLength(0.0, pt0, 1.0, pt1, pt0) match {
      case Success(ptmin) => {
        (ptmin.x - x0).norm should be < tol.norm
        ptmin.fx should be < f(x0 + tol)
      }
      case Failure(e) => assert(false)
    }
  }
  
  it should "throw a MaxIterException if failing to converge" in {
    val x0 = Seq(0.3)
    val pk = Seq(1.0)
    val f = (x: Variables) => (x - x0).norm
    val df = (x: Variables) => pk
    val pt0 = LineSearchPoint(Seq(0.0), (f, df), pk)
    val pt1 = pt0.copy(x = Seq(1.0))
    a [MaxIterException] should be thrownBy {
      zoomStepLength(0.0, pt0, 1.0, pt1, pt0)
    }
  }
}