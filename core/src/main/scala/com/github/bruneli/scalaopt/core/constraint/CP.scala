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

import com.github.bruneli.scalaopt.core.ObjectiveType
import com.github.bruneli.scalaopt.core.function.ObjectiveFunction
import com.github.bruneli.scalaopt.core.linalg.DenseVector
import com.github.bruneli.scalaopt.core.variable.Variable

/**
 * Define a constrained optimization problem as minimizing or maximazing an objective function
 * subject to constraints on the variables
 *
 * @tparam A variable type
 * @tparam B objective function type
 * @tparam C constraint type
 *
 * @author bruneli
 */
trait CP[A <: Variable, +B <: ObjectiveFunction[A], +C <: Constraint[A]] {

  /**
   * Get the problem objective: minimize or maximize
   */
  def objective: ObjectiveType

  /**
   * Get the objective function
   *
   * @return linear objective function
   */
  def objectiveFunction: B

  /**
   * Return the number of constraints
   */
  def numberOfConstraints: Int

  /**
   * Get a linear constraint
   *
   * @param i index of the constraint
   * @return linear constraint
   */
  def constraint(i: Int): C

  /**
   * Decision variables values
   */
  def solution: DenseVector[A]

}
