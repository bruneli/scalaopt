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
import com.github.bruneli.scalaopt.core.linalg.DenseVector._
import com.github.bruneli.scalaopt.core.constraint.{LinearConstraint, LinearConstraintBuilder}
import com.github.bruneli.scalaopt.core.function.LinearContinuousObjectiveFunction
import com.github.bruneli.scalaopt.core.variable._

import scala.collection.immutable.TreeSet

/**
  * @author bruneli
  */
class StandardSimplexSpec extends FlatSpec with Matchers {

  import StandardSimplex._
  import StandardSimplexSpec._

  "solveWith" should "solve a basic linear program from a primal tableau with 2 variables and 1 constraint" in {

    val xMin = PositiveVariables(0.0, 1.0)

    val lp = PrimalTableau
        .given(PositiveVariables(0.0, 0.0))
        .minimize((x: ContinuousVariablesType) => x(0) + x(1))
        .subjectTo(((x: ContinuousVariablesType) => 0.5 * x(0) + 1.0 * x(1)) >= 1.0)

    val xOpt = lp.solveWith(StandardSimplex())(defaultConfig)

    xOpt shouldBe 'success
    for ((xObs, xExp) <- xOpt.get.force.zip(xMin)) {
      xObs.x shouldBe xExp.x +- 1.0e-8
    }

  }

  it should "solve a basic linear program from a dual tableau with 2 variables and 1 constraint" in {

    val xMin = PositiveVariables(0.0, 1.0)

    val xOpt = DualTableau
        .given(PositiveVariables(0.0, 0.0))
        .minimize((x: ContinuousVariablesType) => x(0) + x(1))
        .subjectTo(((x: ContinuousVariablesType) => 0.5 * x(0) + 1.0 * x(1)) >= 1.0)
        .solveWith(StandardSimplex())(defaultConfig)

    xOpt shouldBe 'success
    for ((xObs, xExp) <- xOpt.get.force.zip(xMin)) {
      xObs.x shouldBe xExp.x +- 1.0e-8
    }

  }

  it should "find the minimum of a basic linear program with no constraint" in {

    val xMin = Vector(0.0, 0.0)

    val xOpt = PrimalTableau
        .given(PositiveVariables(0.0, 0.0))
        .minimize((x: ContinuousVariablesType) => x(0) + x(1))
        .solveWith(StandardSimplex())(defaultConfig)

    xOpt shouldBe 'success
    for ((xObs, xExp) <- xOpt.get.force.zip(xMin)) {
      xObs.x shouldBe xExp +- 1.0e-8
    }

  }

  it should "find the minimum of a basic linear program that is in negative plane" in {

    val xMin = Vector(-1.0, -0.5)

    val variables = Variables(BoundedVariable(0.0, Some(-1.0), None), UnconstrainedVariable(0.0))
    val tableau = PrimalTableau
        .given(variables)
        .minimize((x: ContinuousVariablesType) => x(0) + x(1))
        .subjectTo(((x: ContinuousVariablesType) => 0.5 * x(0) + 1.0 * x(1)) >= -1.0)
    val xOpt = tableau
      .solveWith(StandardSimplex())(defaultConfig)

    xOpt shouldBe 'success
    for ((xObs, xExp) <- xOpt.get.force.zip(xMin)) {
      xObs.x shouldBe xExp +- 1.0e-8
    }

  }

  it should "find the minimum of at intersection of two equations in negative plane" in {

    val xMin = Vector(0.0, -1.0)

    val variables = Variables(BoundedVariable(0.0, Some(-2.0), None), UnconstrainedVariable(0.0))
    val xOpt = PrimalTableau
        .given(variables)
        .minimize((x: ContinuousVariablesType) => x(0) + 2.0 * x(1))
        .subjectTo(
          ((x: ContinuousVariablesType) => 1.0 * x(0) + 1.0 * x(1)) >= -1.0,
          ((x: ContinuousVariablesType) => 1.0 * x(0) - 1.0 * x(1)) <= 1.0
        )
        .solveWith(StandardSimplex())(defaultConfig)

    xOpt shouldBe 'success
    for ((xObs, xExp) <- xOpt.get.force.zip(xMin)) {
      xObs.x shouldBe xExp +- 1.0e-8
    }

  }

  it should "find the maximum of a basic linear program with 2 variables and 2 constraints" in {

    val xMax = Vector(1.0, 2.0)

    val variables = Variables(BoundedVariable(0.0, None, Some(1.0)), PositiveVariable(0.0))
    val xOpt = PrimalTableau
        .given(variables)
      .maximize((x: ContinuousVariablesType) => x(0) + x(1))
      .subjectTo(((x: ContinuousVariablesType) => -1.0 * x(0) + 1.0 * x(1)) <= 1.0)
      .solveWith(StandardSimplex())(defaultConfig)

    xOpt shouldBe 'success
    for ((xObs, xExp) <- xOpt.get.force.zip(xMax)) {
      xObs.x shouldBe xExp +- 1.0e-8
    }

  }

  it should "find the maximum of a linear program with the dual tableau" in {

    val xMax = Vector(1.0, 2.0)

    val xOpt = DualTableau
        .given(PositiveVariables(0.0, 0.0))
      .maximize((x: ContinuousVariablesType) => x(0) + x(1))
      .subjectTo(
        ((x: ContinuousVariablesType) => 1.0 * x(0)) <= 1.0,
        ((x: ContinuousVariablesType) => -x(0) + x(1)) <= 1.0
      )
      .solveWith(StandardSimplex())(defaultConfig)

    xOpt shouldBe 'success
    for ((xObs, xExp) <- xOpt.get.force.zip(xMax)) {
      xObs.x shouldBe xExp +- 1.0e-8
    }

  }

  it should "fail to solve the linear program when constraints offer no solution" in {

    val variables = Variables(
      BoundedVariable(0.0, Some(0.0), Some(1.0)),
      BoundedVariable(0.0, Some(0.0), Some(1.0)))

    a[NoSolutionException] shouldBe thrownBy {
      PrimalTableau
          .given(variables)
        .maximize((x: ContinuousVariablesType) => x(0) + x(1))
        .subjectTo(((x: ContinuousVariablesType) => x(0) + x(1)) >= 3.0)
        .solveWith(StandardSimplex())(defaultConfig)
    }

  }

  it should "throw an exception when program is unbounded from below" in {

    val variables = Variables(BoundedVariable(0.0, None, Some(1.0)), BoundedVariable(0.0, None, Some(1.0)))

    val xopt = PrimalTableau
        .given(variables)
      .minimize((x: ContinuousVariablesType) => x(0) + x(1))
      .solveWith(StandardSimplex())(defaultConfig)

    xopt shouldBe 'failure

  }

  it should "find the market clearing volumes from an electricity pool auction" in {

    implicit val fromDouble = ConversionsFromDouble.ContinuousVariableFromDouble

    val demand = electricityDemand
    val supply = electricitySupply
    //val demand = Map("prices" -> Vector(200.0), "energies" -> Vector(200.0))
    //val supply = Map("prices" -> Vector(100.0, 150.0), "energies" -> Vector(100.0, 200.0))

    val nDemandOffers = demand("prices").size
    val nSupplyOffers = supply("prices").size
    val nOffers = nDemandOffers + nSupplyOffers

    // Electricity cannot be easily stored => total generation must be equal to the total load
    val balancingCoefficients = ones[Constant](nOffers).mapWithIndex {
      (value, index) => if (index >= nDemandOffers) -1.0 else 1.0
    }
    val balancing = LinearConstraintBuilder[ContinuousVariable](balancingCoefficients).equ(0.0)

    // Upper bound on supplied energy per offer
    val upperBounds = (demand("energies") ++ supply("energies")).zipWithIndex.map {
      case (max, index) => LinearConstraintBuilder[ContinuousVariable](e[Constant](nOffers, index)).le(max)
    }

    // maximize the social welfare defined as the area between consumption and generation bid ladders
    // given balancing and upper bound constraints
    val variables = new PositiveVariables(Array.fill(nOffers)(0.0))
    val costVector = demand("prices") ++ supply("prices").map(_ * -1.0)
    val tableau = PrimalTableau
      .given(variables)
      .maximize(LinearContinuousObjectiveFunction[ContinuousVariable](Constants(costVector: _*)))
      .subjectTo(upperBounds :+ balancing: _*).create
    val optimum = StandardSimplex().solve(tableau)

    optimum shouldBe 'success

    // Select all demand offers with prices >= 37.5 and supply offers with prices <= 37.5
    // To match demand and supply, the last supply offer has only 55 MWh selected.
    val expectedClearing =
      Vector(250.0, 300.0, 120.0, 80.0, 40.0, 70.0, 60.0, 45.0, 30.0, 0.0, 0.0, 0.0,
        120.0, 50.0, 200.0, 400.0, 60.0, 50.0, 60.0, 55.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

    for ((selectedVolume, expectedVolume) <- optimum.get.coordinates.force.zip(expectedClearing)) {
      selectedVolume.x shouldBe expectedVolume +- 1.0e-8
    }
  }

  it should "find the market clearing prices from an electricity pool auction" in {

    implicit val fromDouble = ConversionsFromDouble.ContinuousVariableFromDouble

    //val demand = electricityDemand
    //val supply = electricitySupply
    val demand = Map("prices" -> Vector(200.0, 140.0), "energies" -> Vector(200.0, 50.0))
    val supply = Map("prices" -> Vector(100.0, 150.0, 175.0), "energies" -> Vector(100.0, 150.0, 20.0))

    val nDemandOffers = demand("prices").size
    val nSupplyOffers = supply("prices").size
    val nOffers = nDemandOffers + nSupplyOffers

    // Electricity cannot be easily stored => total generation must be equal to the total load
    val balancingCoefficients = ones[Constant](nOffers).mapWithIndex {
      (value, index) => if (index >= nDemandOffers) -1.0 else 1.0
    }
    val balancing = LinearConstraintBuilder[ContinuousVariable](balancingCoefficients).equ(0.0)

    // Upper bound on supplied energy per offer
    val upperBounds = (demand("energies") ++ supply("energies")).zipWithIndex.map {
      case (max, index) => LinearConstraintBuilder[ContinuousVariable](e[Constant](nOffers, index)).le(max)
    }

    // maximize the social welfare defined as the area between consumption and generation bid ladders
    // given balancing and upper bound constraints
    implicit val orderByVolume = Ordering.by[LinearConstraint[ContinuousVariable], Double](_.b)
    val constraints: TreeSet[LinearConstraint[ContinuousVariable]] = TreeSet(upperBounds :+ balancing: _*)
    val variables = new PositiveVariables(Array.fill(nOffers)(0.0))
    val costVector = demand("prices") ++ supply("prices").map(_ * -1.0)
    val objectiveFunction = LinearContinuousObjectiveFunction[ContinuousVariable](Constants(costVector: _*))
    val tableau = StandardSimplex().solveAllPhases(DualTableau.given(variables).maximize(objectiveFunction).subjectTo(constraints.toSeq: _*).create)

    tableau shouldBe 'success

    // Select all demand offers with prices >= 150.0 and supply offers with prices <= 150.0
    // To match demand and supply, the last supply offer has only 100 MWh selected.
    val expectedClearingVolumes = Vector(200.0, 0.0, 100.0, 100.0, 0.0)
    // First price is the clearing price, next are income per offer ordered by increasing volume
    val expectedClearingPrices = Vector(150.0, 0.0, 0.0, 50.0, 0.0, 50.0)

    for ((selectedVolume, expectedVolume) <- tableau.get.toTableau.dual.zip(expectedClearingVolumes)) {
      selectedVolume.x shouldBe expectedVolume +- 1.0e-8
    }
    for ((actualPrice, expectedPrice) <- tableau.get.toTableau.primal.zip(expectedClearingPrices)) {
      actualPrice.x shouldBe expectedPrice +- 1.0e-8
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