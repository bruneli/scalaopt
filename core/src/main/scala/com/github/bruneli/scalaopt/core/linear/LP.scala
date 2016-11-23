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

package com.github.bruneli.scalaopt.core.linear

import com.github.bruneli.scalaopt.core.ConfigPars
import com.github.bruneli.scalaopt.core.constraint.{CP, CPBuilderOps, CPSolver, LinearConstraint}
import com.github.bruneli.scalaopt.core.function.LinearObjectiveFunction
import com.github.bruneli.scalaopt.core.linalg.DenseVector
import com.github.bruneli.scalaopt.core.variable.ContinuousVariable

import scala.util.Try

/**
 * Define a linear program as a continuous linear objective function and a set of
 * continuous linear constraints.
 *
 * @author bruneli
 */
trait LP
  extends CP[ContinuousVariable, LinearObjectiveFunction[ContinuousVariable], LinearConstraint[ContinuousVariable]]
    with CPBuilderOps[ContinuousVariable, LP] {

  /**
   * Solve the constrained optimization problem
   *
   * @param method method used to solve the program
   * @param pars solver configuration parameters
   * @tparam C type of class holding method configuration parameters
   * @return problem solution of failure
   */
  override def solveWith[C <: ConfigPars, B >: LP <: CP[ContinuousVariable, _, _], S <: CPSolver[B, C]](method: S)(implicit pars: C): Try[DenseVector[ContinuousVariable]] = {
    method.solve(this)(pars).map(_.solution)
  }

  def toTableau: SimplexTableau

}