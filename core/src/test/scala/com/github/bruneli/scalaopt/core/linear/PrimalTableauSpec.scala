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

import org.scalatest.{FlatSpec, Matchers}
import com.github.bruneli.scalaopt.core._
import PrimalTableau.{max, min}
import com.github.bruneli.scalaopt.core.variable.PositiveVariables

/**
 * @author bruneli
 */
class PrimalTableauSpec extends FlatSpec with Matchers {

  "min" should "build a tableau from a linear function without any constraint" in {

    val x0 = PositiveVariables(0.0, 0.0, 0.0)
    val tableau = min((x: ContinuousVariablesType) => 2.0 * x(0) + 1.0 * x(1) + 3.0 * x(2), x0)

    tableau.columns.size shouldBe 3
    tableau.numberOfConstraints shouldBe 0
    tableau.columns.collect().map(_.phase1Cost) should contain theSameElementsInOrderAs List(0.0, 0.0, 0.0)
    tableau.columns.collect().map(_.phase2Cost) should contain theSameElementsInOrderAs List(-2.0, -1.0, -3.0)
    tableau.rhs.phase1Cost shouldBe 0.0
    tableau.rhs.phase2Cost shouldBe 0.0

  }

  "max" should "build a tableau from a linear function without any constraint" in {

    val x0 = PositiveVariables(0.0, 0.0, 0.0)
    val tableau = max((x: ContinuousVariablesType) => 2.0 * x(0) + 1.0 * x(1) + 3.0 * x(2), x0)

    tableau.columns.size shouldBe 3
    tableau.numberOfConstraints shouldBe 0
    tableau.columns.collect().map(_.phase1Cost) should contain theSameElementsInOrderAs List(0.0, 0.0, 0.0)
    tableau.columns.collect().map(_.phase2Cost) should contain theSameElementsInOrderAs List(2.0, 1.0, 3.0)
    tableau.rhs.phase1Cost shouldBe 0.0
    tableau.rhs.phase2Cost shouldBe 0.0

  }

  "subjectTo" should "add a set of linear constraints to the tableau" in {

    val x0 = PositiveVariables(0.0, 0.0, 0.0)
    val tableau =
      min((x: ContinuousVariablesType) => 2.0 * x(0) + 1.0 * x(1) + 3.0 * x(2), x0)
        .subjectTo(
          ((x: ContinuousVariablesType) => x(0).x) ge 1.0,
          ((x: ContinuousVariablesType) => x(0) + x(1)) equ 3.0
        )

    // 4 columns because of the 3 decision variables + 1 slack variable introduced by inequality constraint
    tableau.columns.size shouldBe 4
    tableau.numberOfConstraints shouldBe 2
    tableau.columns.collect().map(_.phase1Cost) should contain theSameElementsInOrderAs List(0.0, 0.0, 0.0, 0.0)
    tableau.columns.collect().map(_.phase2Cost) should contain theSameElementsInOrderAs List(-2.0, -1.0, -3.0, 0.0)
    // the "greater equal" inequality constraint introduces an excess variable with a -1 value
    tableau.columns.collect().map(_.constrains(0).x) should contain theSameElementsInOrderAs List(1.0, 0.0, 0.0, -1.0)
    tableau.columns.collect().map(_.constrains(1).x) should contain theSameElementsInOrderAs List(1.0, 1.0, 0.0, 0.0)
    tableau.rhs.phase1Cost shouldBe 0.0
    tableau.rhs.phase2Cost shouldBe 0.0
    tableau.rhs.constrains.toVector.map(_.x) should contain theSameElementsInOrderAs Vector(1.0, 3.0)

  }

  it should "throw an exception when a constraint exceeds objective function size" in {

    val x0 = PositiveVariables(0.0, 0.0, 0.0)
    an[IllegalArgumentException] shouldBe thrownBy {
      min((x: ContinuousVariablesType) => 2.0 * x(0) + 1.0 * x(1) + 3.0 * x(2), x0)
        .subjectTo(
          ((x: ContinuousVariablesType) => x(0).x) ge 1.0,
          ((x: ContinuousVariablesType) => x(0) + x(1)) equ 3.0,
          ((x: ContinuousVariablesType) => x(2) + x(6)) le 5.0
        )
    }

  }
}
