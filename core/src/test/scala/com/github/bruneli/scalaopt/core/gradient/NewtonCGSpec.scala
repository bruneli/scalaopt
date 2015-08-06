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

package com.github.bruneli.scalaopt.core.gradient

import com.github.bruneli.scalaopt.core.{Variables, MaxIterException}
import org.scalaopt.algos._
import org.scalatest.{Matchers, FlatSpec}

class NewtonCGSpec extends FlatSpec with Matchers {
  import NewtonCG._

  val x0 = Vector(0.5, 2.0)
  val fQuad = (x: Variables) => (x - x0) dot (x - x0)
  val dfQuad = (x: Variables) => (x - x0) * 2.0

  val config = new CGConfig(tol = 1.0e-6)

  "NewtonCG method" should "converge with fQuad and exact derivatives" in {
    val xOpt = minimize((fQuad, dfQuad), Vector(0.0, 0.0))
    xOpt shouldBe 'success
    val d = xOpt.get - x0
    (d dot d) should be < (config.tol * config.tol)
  }

  it should "converge with fQuad and approximate derivatives" in {
    val xOpt = minimize(fQuad, Vector(0.0, 0.0))
    xOpt shouldBe 'success
    val d = xOpt.get - x0
    (d dot d) should be < (config.tol * config.tol)
  }

  it should "throw an exception when reaching max number of iterations" in {
    a [MaxIterException] should be thrownBy {
      minimize((x: Variables) => x(0) + x(1), Vector(0.0, 0.0))
    }
  }

}
