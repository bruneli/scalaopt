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

package com.github.bruneli.scalaopt.core.constraint

import com.github.bruneli.scalaopt.core.ConfigPars
import com.github.bruneli.scalaopt.core.function.ObjectiveFunction
import com.github.bruneli.scalaopt.core.linalg.DenseVectorLike
import com.github.bruneli.scalaopt.core.variable.Variable

import scala.util.Try

/**
 * Operations used to build a constrained optimization problem
 *
 * @tparam To constrained program type
 *
 * @author bruneli
 */
trait CPBuilder[A <: Variable, +To <: CP[A, _, _]] {

  type Self <: CPBuilder[A, To]

  def minimize(objectiveFunction: ObjectiveFunction[A]): Self

  def maximize(objectiveFunction: ObjectiveFunction[A]): Self

  /**
   * Solve the constrained optimization problem
   *
   * @param method method used to solve the program
   * @param pars solver configuration parameters
   * @tparam C type of class holding method configuration parameters
   * @tparam S constrained optimization solver type
   * @return problem solution of failure
   */
  def solveWith[C <: ConfigPars, P >: To <: CP[A, _, _], S <: CPSolver[A, P, C]](
    method: S)(implicit pars: C): Try[DenseVectorLike[A]] = {
    method.solve(create).map(_.coordinates)
  }

  def addConstraint(constraint: Constraint[A]): Self

  def subjectTo(constraints: Constraint[A]*): Self

  def create: To

}

trait CanBuildCPFrom[A <: Variable, -From <: CP[A, _, _], +To <: CP[A, _, _]] {

  def apply(from: From): CPBuilder[A, To]

}