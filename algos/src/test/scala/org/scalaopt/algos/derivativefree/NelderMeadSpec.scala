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

package org.scalaopt.algos.derivativefree

import org.scalaopt.algos._
import scala.util.{Try, Success, Failure}
import org.scalatest._
import org.scalatest.Matchers._

class NelderMeadSpec extends FlatSpec with Matchers {
  import NelderMead._

  val x0 = Vector(0.5, 2.0)
  def fQuad(x: Coordinates): Double = (x - x0) dot (x - x0)

  val x1 = Vector(1.0, -1.0)
  val v1 = Vertex(x1, fQuad)
  val fx1 = fQuad(x1)
  
  "A Vertex" should "return fx equal to fx1" in {
    (v1.fx) should === (fx1)
  }

  val c  = new NelderMeadConfig(relDelta = 2.0)
  val s0 = Simplex(fQuad, x1)(c)
  
  "starting simplex" should "provide a list of shifted vertices" in {
    (s0.vertices(0).x) should === (v1.x)
    (s0.vertices(0).fx) should === (v1.fx)
    (s0.vertices(1).x) should === (Vector(v1.x(0) * c.relDelta, v1.x(1)))
    (s0.vertices(2).x) should === (Vector(v1.x(0), v1.x(1) * c.relDelta))
  }
  
  "minimum of fQuad" should "have converged" in {
    minimize(fQuad, x1) match {
      case Success(xmin) =>
        val d2 = (xmin zip x0).foldLeft(0.0) { 
          case (r, (xmin, x0)) => r + (xmin - x0) * (xmin - x0) 
        }
        d2 should be < (c.tol * c.tol)
      case Failure(e) => assert(false)
    }
  }
  
  it should "throw a MaxIterException when max number of iterations is reached" in {
    a [MaxIterException] should be thrownBy {
      minimize(x => x(0) + x(1), x1)
    }
  }

}