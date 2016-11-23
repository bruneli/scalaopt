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
import com.github.bruneli.scalaopt.core.linalg.DenseVector
import com.github.bruneli.scalaopt.core.variable.Variable

import scala.util.Try

/**
 * Operations used to build a constrained optimization problem
 *
 * @tparam A variable type
 * @tparam P constrained optimization problem type
 *
 * @author bruneli
 */
trait CPBuilderOps[A <: Variable, P <: CP[A, _, _]] {

  /**
   * Solve the constrained optimization problem
   *
   * @param method method used to solve the program
   * @param pars solver configuration parameters
   * @tparam C type of class holding method configuration parameters
   * @tparam S constrained optimization solver type
   * @return problem solution of failure
   */
  def solveWith[C <: ConfigPars, B >: P <: CP[A, _, _], S <: CPSolver[B, C]](method: S)(implicit pars: C): Try[DenseVector[A]]

  /**
   * Add a constraint to the problem to solve
   *
   * @param constraint constraint
   * @return problem augmented of one additional constraint
   */
  def addConstraint(constraint: Constraint[A]): P

  /**
   * Add a set of constraints to the problem
   *
   * @param constraints set of constraints
   * @return problem with new constraints
   */
  def subjectTo(constraints: Constraint[A]*): P

}
