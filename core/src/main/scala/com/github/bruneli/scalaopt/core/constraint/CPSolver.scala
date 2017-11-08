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

import com.github.bruneli.scalaopt.core.variable.Variable
import com.github.bruneli.scalaopt.core.{ConfigPars, Optimum}

import scala.util.Try

/**
 * Method used to solve a constrained optimization problem
 *
 * @tparam P constrained optimization problem type returned by this solver
 * @tparam C configuration parameters type
 * @author bruneli
 */
trait CPSolver[A <: Variable, -P <: CP[A, _, _], -C <: ConfigPars] {

  /**
   * Try to solve a constrained optimization problem
   *
   * @param program constrained optimization problem
   * @param pars solver configuration parameters
   * @return solution of the problem or failure
   */
  def solve(program: P)(implicit pars: C): Try[Optimum[A]]

}