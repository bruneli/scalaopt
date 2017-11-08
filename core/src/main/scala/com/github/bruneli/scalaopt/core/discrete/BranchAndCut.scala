/*
 * Copyright 2016 Renaud Bruneliere
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

package com.github.bruneli.scalaopt.core.discrete

import com.github.bruneli.scalaopt.core.ConfigPars
import com.github.bruneli.scalaopt.core.ObjectiveType._
import com.github.bruneli.scalaopt.core.constraint._
import com.github.bruneli.scalaopt.core.function.ObjectiveFunction
import com.github.bruneli.scalaopt.core.variable.{ContinuousVariable, Variable}
import com.github.bruneli.scalaopt.core._
import com.github.bruneli.scalaopt.core.linalg.DenseVector

import scala.util.{Failure, Success, Try}

/**
  * Branch and cut algorithm used to solve mixed integer programs (MIP).
  *
  * @tparam A objective function type
  * @tparam B constraint type
  * @tparam RP relaxation program type
  * @tparam P integer program type
  * @tparam S relaxation program solver
  *
  * @author bruneli
  */
case class BranchAndCut[-C <: ConfigPars,
                        A <: ObjectiveFunction[Variable],
                        B <: Constraint[Variable],
                        RP <: CP[ContinuousVariable, _, _],
                        P <: MIP[A, B, RP],
                        S <: CPSolver[ContinuousVariable, RP, C]](
  relaxationProgramSolver: S,
  branchingMethod: BranchingStrategy)(
  implicit cbf: CanBuildCPFrom[ContinuousVariable, RP, RP]) extends CPSolver[Variable, P, C] {

  import BranchAndCut._

  def solve(program: P)(implicit pars: C): Try[Optimum[Variable]] = {

    def iterate(searchList: List[Node], best: Option[Optimum[Variable]]): Option[Optimum[Variable]] = searchList match {
      case Nil => best
      case h :: tl =>
        relaxationProgramSolver.solve(h.program) match {
          case Success(result) =>
            val node = h.copy(optimum = Some(result))
            if (program.acceptsAsFeasibleSolution(result.coordinates)) {
              if (best.isEmpty || node.hasBetterOptimumThan(best)) {
                val newBest = node.optimum.map(_.copy(
                  coordinates = program.adaptVariablesTo(result.coordinates)))
                iterate(tl.filter(_.hasBetterOptimumThan(node.optimum)), newBest)
              } else {
                iterate(tl, best)
              }
            } else {
              iterate(branch(program, node) ::: tl, best)
            }
          case Failure(_) =>
            iterate(tl, best)
        }
    }

    Try(iterate(List(Node(program.relaxationProgram)), None).getOrElse(throw new NoFeasibleSolution))
  }

  private def branch(program: P, result: Node): List[Node] = {
    branchingMethod.findCutVariable(program, result.optimum.get.coordinates) match {
      case Some(cut) =>
        val sol = result.optimum.get.coordinates
        val n = sol.length
        val x = sol(cut.index)
        val leftCut = cut.variable.floor(x)
        val rightCut = cut.variable.ceil(x)
        val leftConstraint: Option[LinearConstraint[ContinuousVariable]] = leftCut.collectFirst {
          case cutValue => LinearConstraintBuilder(DenseVector.e(n, cut.index)) <= cutValue
        }
        val rightConstraint: Option[LinearConstraint[ContinuousVariable]] = rightCut.collectFirst {
          case cutValue => LinearConstraintBuilder(DenseVector.e(n, cut.index)) >= cutValue
        }
        (leftConstraint.toList ::: rightConstraint.toList)
          .map(constraint => Node(cbf(result.program).addConstraint(constraint).create))
      case None => Nil
    }
  }

  case class Node(program: RP, optimum: Option[Optimum[ContinuousVariable]] = None) {

    def hasBetterOptimumThan(that: Option[Optimum[_]]): Boolean = {
      if (that.isEmpty) {
        optimum.isDefined
      } else {
        program.objective match {
          case MINIMIZE => this.optimum.exists(_.value < that.get.value)
          case MAXIMIZE => this.optimum.exists(_.value > that.get.value)
        }
      }
    }

  }

}

object BranchAndCut {

  class NoFeasibleSolution extends RuntimeException

}