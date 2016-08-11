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
import SeqDataSetConverter._
import ConstraintOperator._

/**
  * @author bruneli
  */
class StandardSimplexSpec extends FlatSpec with Matchers {

  import StandardSimplex._
  import StandardSimplexSpec._

  "solveWith" should "solve a basic linear program from a primal tableau with 2 variables and 1 constraint" in {

    val xMin = Vector(0.0, 1.0)

    val xOpt = PrimalTableau.min((x: Variables) => x(0) + x(1))
        .subjectTo(((x: Variables) => 0.5 * x(0) + 1.0 * x(1)) >= 1.0)
        .solveWith(StandardSimplex)

    xOpt shouldBe 'success
    for ((xObs, xExp) <- xOpt.get.zip(xMin)) {
      xObs shouldBe xExp +- 1.0e-8
    }

  }

  it should "solve a basic linear program from a dual tableau with 2 variables and 1 constraint" in {

    val xMin = Vector(0.0, 1.0)

    val xOpt = DualTableau.min((x: Variables) => x(0) + x(1))
        .subjectTo(((x: Variables) => 0.5 * x(0) + 1.0 * x(1)) >= 1.0)
        .solveWith(StandardSimplex)

    xOpt shouldBe 'success
    for ((xObs, xExp) <- xOpt.get.zip(xMin)) {
      xObs shouldBe xExp +- 1.0e-8
    }

  }

  it should "find the minimum of a basic linear program with no constraint" in {

    val xMin = Vector(0.0, 0.0)

    val xOpt = PrimalTableau.min((x: Variables) => x(0) + x(1))
        .solveWith(StandardSimplex)

    xOpt shouldBe 'success
    for ((xObs, xExp) <- xOpt.get.zip(xMin)) {
      xObs shouldBe xExp +- 1.0e-8
    }

  }

  it should "find the minimum of a basic linear program that is in negative plane" in {

    val xMin = Vector(-1.0, -0.5)

    val xOpt = PrimalTableau.min((x: Variables) => x(0) + x(1))
        .subjectTo(
          ((x: Variables) => 0.5 * x(0) + 1.0 * x(1)) >= -1.0,
          ((x: Variables) => x(0)) >= -1.0
        )
        .withNegativeVariables
        .solveWith(StandardSimplex)

    xOpt shouldBe 'success
    for ((xObs, xExp) <- xOpt.get.zip(xMin)) {
      xObs shouldBe xExp +- 1.0e-8
    }

  }

  it should "find the minimum of at intersection of two equations in negative plane" in {

    val xMin = Vector(0.0, -1.0)

    val xOpt = PrimalTableau.min((x: Variables) => x(0) + 2.0 * x(1))
        .subjectTo(
          ((x: Variables) => 1.0 * x(0)) >= -2.0,
          ((x: Variables) => 1.0 * x(0) + 1.0 * x(1)) >= -1.0,
          ((x: Variables) => 1.0 * x(0) - 1.0 * x(1)) <= 1.0
        )
        .withNegativeVariables
        .solveWith(StandardSimplex)

    xOpt shouldBe 'success
    for ((xObs, xExp) <- xOpt.get.zip(xMin)) {
      xObs shouldBe xExp +- 1.0e-8
    }

  }

  it should "find the maximum of a basic linear program with 2 variables and 2 constraints" in {

    val xMax = Vector(1.0, 2.0)

    val xOpt = PrimalTableau.max((x: Variables) => x(0) + x(1))
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

  it should "find the maximum of a linear program with the dual tableau" in {

    val xMax = Vector(1.0, 2.0)

    val xOpt = DualTableau.max((x: Variables) => x(0) + x(1))
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

  it should "fail to solve the linear program when constraints offer no solution" in {

    a[NoSolutionException] shouldBe thrownBy {
      PrimalTableau.max((x: Variables) => x(0) + x(1))
          .subjectTo(
            ((x: Variables) => x(0)) <= 1.0,
            ((x: Variables) => x(1)) <= 1.0,
            ((x: Variables) => x(0) + x(1)) >= 3.0
          )
          .solveWith(StandardSimplex)
    }

  }

  it should "throw an exception when program is unbounded from below" in {

    val xopt = PrimalTableau.min((x: Variables) => x(0) + x(1))
        .subjectTo(
          ((x: Variables) => x(0)) <= 1.0,
          ((x: Variables) => x(1)) <= 1.0
        )
        .withNegativeVariables
        .solveWith(StandardSimplex)
    xopt shouldBe 'failure

  }

  it should "find the market clearing volumes from an electricity pool auction" in {

    val nDemandOffers = electricityDemand("prices").size
    val nSupplyOffers = electricitySupply("prices").size
    val nOffers = nDemandOffers + nSupplyOffers

    // Electricity cannot be easily stored => total generation must be equal to the total load
    val balancing = LinearConstraint(ones(nDemandOffers) ++ vector(nSupplyOffers, -1.0), EQ, 0.0)

    // Upper bound on supplied energy per offer
    val upperBounds = (electricityDemand("energies") ++ electricitySupply("energies")).zipWithIndex.map {
      case (max, index) => LinearConstraint(e(nOffers, index), LE, max)
    }

    // maximize the social welfare defined as the area between consumption and generation bid ladders
    // given balancing and upper bound constraints
    val tableau = StandardSimplex.solve(
      PrimalTableau.max(electricityDemand("prices") ++ (electricitySupply("prices") * -1.0))
          .subjectTo(upperBounds.toSet + balancing))

    tableau shouldBe 'success

    // Select all demand offers with prices >= 37.5 and supply offers with prices <= 37.5
    // To match demand and supply, the last supply offer has only 55 MWh selected.
    val expectedClearing =
      Vector(250.0, 300.0, 120.0, 80.0, 40.0, 70.0, 60.0, 45.0, 30.0, 0.0, 0.0, 0.0,
        120.0, 50.0, 200.0, 400.0, 60.0, 50.0, 60.0, 55.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

    for ((selectedVolume, expectedVolume) <- tableau.get.solution.zip(expectedClearing)) {
      selectedVolume shouldBe expectedVolume +- 1.0e-8
    }
  }

  it should "find the market clearing prices from an electricity pool auction" in {

    val nDemandOffers = electricityDemand("prices").size
    val nSupplyOffers = electricitySupply("prices").size
    val nOffers = nDemandOffers + nSupplyOffers

    // Electricity cannot be easily stored => total generation must be equal to the total load
    val balancing = LinearConstraint(ones(nDemandOffers) ++ vector(nSupplyOffers, -1.0), EQ, 0.0)

    // Upper bound on supplied energy per offer
    val upperBounds = (electricityDemand("energies") ++ electricitySupply("energies")).zipWithIndex.map {
      case (max, index) => LinearConstraint(e(nOffers, index), LE, max)
    }

    // maximize the social welfare defined as the area between consumption and generation bid ladders
    // given balancing and upper bound constraints
    val tableau = StandardSimplex.solve(
      DualTableau.max(electricityDemand("prices") ++ (electricitySupply("prices") * -1.0))
          .subjectTo(upperBounds.toSet + balancing))

    tableau shouldBe 'success

    // Select all demand offers with prices >= 37.5 and supply offers with prices <= 37.5
    // To match demand and supply, the last supply offer has only 55 MWh selected.
    val expectedClearing =
      Vector(250.0, 300.0, 120.0, 80.0, 40.0, 70.0, 60.0, 45.0, 30.0, 0.0, 0.0, 0.0,
        120.0, 50.0, 200.0, 400.0, 60.0, 50.0, 60.0, 55.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

    for ((selectedVolume, expectedVolume) <- tableau.get.solution.zip(expectedClearing)) {
      selectedVolume shouldBe expectedVolume +- 1.0e-8
    }
  }
}

object StandardSimplexSpec {

  /** Energies (MWh) and prices (EUR/MWh) for each demand offer */
  lazy val electricityDemand: Map[String, Vector[Double]] = {
    Map("energies" -> Vector(250.0, 300.0, 120.0, 80.0, 40.0, 70.0, 60.0, 45.0, 30.0, 35.0, 25.0, 10.0),
      "prices" -> Vector(200.0, 110.0, 100.0, 90.0, 85.0, 75.0, 65.0, 40.0, 37.6, 30.0, 24.0, 15.0))
  }

  /** Energies (MWh) and prices (EUR/MWh) for each supply offer */
  lazy val electricitySupply: Map[String, Vector[Double]] = {
    Map("energies" -> Vector(120.0, 50.0, 200.0, 400.0, 60.0, 50.0, 60.0, 100.0, 70.0, 50.0, 70.0, 45.0, 50.0, 60.0, 50.0),
      "prices" -> Vector(0.0, 0.0, 15.0, 30.0, 32.5, 34.0, 36.0, 37.5, 39.0, 40.0, 60.0, 70.0, 100.0, 150.0, 200.0))
  }

}