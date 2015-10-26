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

package com.github.bruneli.scalaopt.core

import org.scalatest.{Matchers, FlatSpec}
import com.github.bruneli.scalaopt.core._

/**
 * @author bruneli
 */
class ConstraintSpec extends FlatSpec with Matchers {

  "LHConstaint" should "be convertible into an equality constraint" in {

    val c1 = ((x: Variables) => 2.0 * x(0) + 1.0 * x(1) - 3.0 * x(2)) equ 0.0
    val c2 = ((x: Variables) =>              2.0 * x(1) + 1.0 * x(2)) === 0.0

    c1 shouldBe a[Constraint]
    c1(Vector(0.0, 0.0, 0.0)) shouldBe true
    c1(Vector(1.0, 1.0, 1.0)) shouldBe true
    c1(Vector(1.0, 1.0, 1.0, 100.0)) shouldBe true
    c1(Vector(2.0, 1.0, 0.0)) shouldBe false
    an[IndexOutOfBoundsException] shouldBe thrownBy(c1(Vector(0.0, 0.0)))

    c2 shouldBe a[Constraint]
    c2(Vector(0.0, 0.0, 0.0)) shouldBe true
    c2(Vector(100.0, 1.0, -2.0)) shouldBe true
    c2(Vector(100.0, 1.0, -2.0, 100.0)) shouldBe true
    c2(Vector(2.0, 1.0, 0.0)) shouldBe false
    an[IndexOutOfBoundsException] shouldBe thrownBy(c2(Vector(0.0, 0.0)))

  }

  it should "be convertible into an <= inequality constraint" in {

    val c1 = ((x: Variables) => 2.0 * x(0) + 1.0 * x(1) - 3.0 * x(2)) le 0.0
    val c2 = ((x: Variables) =>              2.0 * x(1) + 1.0 * x(2)) <= 0.0

    c1 shouldBe a[Constraint]
    c1(Vector(0.0, 0.0, 0.0)) shouldBe true
    c1(Vector(1.0, 1.0, 2.0)) shouldBe true
    c1(Vector(1.0, 1.0, 1.0, 100.0)) shouldBe true
    c1(Vector(2.0, 1.0, 0.0)) shouldBe false
    an[IndexOutOfBoundsException] shouldBe thrownBy(c1(Vector(0.0, 0.0)))

    c2 shouldBe a[Constraint]
    c2(Vector(0.0, 0.0, 0.0)) shouldBe true
    c2(Vector(100.0, 1.0, -3.0)) shouldBe true
    c2(Vector(100.0, 1.0, -2.0, 100.0)) shouldBe true
    c2(Vector(2.0, 1.0, 0.0)) shouldBe false
    an[IndexOutOfBoundsException] shouldBe thrownBy(c2(Vector(0.0, 0.0)))

  }

  it should "be convertible into an >= inequality constraint" in {

    val c1 = ((x: Variables) => 2.0 * x(0) + 1.0 * x(1) - 3.0 * x(2)) ge 0.0
    val c2 = ((x: Variables) =>              2.0 * x(1) + 1.0 * x(2)) >= 0.0

    c1 shouldBe a[Constraint]
    c1(Vector(0.0, 0.0, 0.0)) shouldBe true
    c1(Vector(2.0, 1.0, 0.0)) shouldBe true
    c1(Vector(1.0, 1.0, 1.0, 100.0)) shouldBe true
    c1(Vector(1.0, 1.0, 2.0)) shouldBe false
    an[IndexOutOfBoundsException] shouldBe thrownBy(c1(Vector(0.0, 0.0)))

    c2 shouldBe a[Constraint]
    c2(Vector(0.0, 0.0, 0.0)) shouldBe true
    c2(Vector(2.0, 1.0, 0.0)) shouldBe true
    c2(Vector(100.0, 1.0, -2.0, 100.0)) shouldBe true
    c2(Vector(2.0, 1.0, -3.0)) shouldBe false
    an[IndexOutOfBoundsException] shouldBe thrownBy(c2(Vector(0.0, 0.0)))

  }

}
