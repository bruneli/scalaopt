/*
 * Copyright 2015 Renaud Bruneliere
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

package com.github.bruneli.scalaopt.core.linear

import org.scalatest.{Matchers, FlatSpec}
import com.github.bruneli.scalaopt.core._
import SimplexTableau.{min, max}

/**
 * @author bruneli
 */
class StandardSimplexSpec extends FlatSpec with Matchers {

  import StandardSimplex._

  "solveWith" should "find the minimum of a basic linear program with 2 variables and 1 constraint" in {

    val xMin = Vector(0.0, 1.0)

    val xOpt = min((x: Variables) => x(0) + x(1))
      .subjectTo(((x: Variables) => 0.5 * x(0) + 1.0 * x(1)) >= 1.0)
      .solveWith(StandardSimplex)

    xOpt shouldBe 'success
    for ((xObs, xExp) <- xOpt.get.zip(xMin)) {
      xObs shouldBe xExp +- 1.0e-8
    }

  }

  it should "find the maximum of a basic linear program with 2 variables and 2 constraints" in {

    val xMax = Vector(1.0, 2.0)

    val xOpt = max((x: Variables) => x(0) + x(1))
      .subjectTo(
        ((x: Variables) => x(0)) <= 1.0,
        ((x: Variables) => -x(0) + x(1)) <= 1.0
      )
      .solveWith(StandardSimplex)

    xOpt shouldBe 'success
    for ((xObs, xExp) <- xOpt.get.zip(xMax)) {
      xObs shouldBe xExp +- 1.0e-8
    }

  }

}
