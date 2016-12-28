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

package com.github.bruneli.scalaopt.core.constraint

import com.github.bruneli.scalaopt.core.linalg.DenseVector
import com.github.bruneli.scalaopt.core.linalg.FromToDoubleConversions.FromDouble
import com.github.bruneli.scalaopt.core.variable.Variable

import scala.util.Try

/**
 * Define a constraint as a left operand, an operator and a right operand
 *
 * The left operand is a real-valued function acting on variables,
 * the right operand is a real-valued constant and the operator is an equality or an inequality.
 *
 * @tparam A optimization variable type
 *
 * @author bruneli
 */
trait Constraint[-A <: Variable] {

  /**
   * Check if constraint is satisfied in x
   *
   * @param x vector of variables
   * @return true if constraint is satisfied in x
   */
  def apply(x: DenseVector[A]): Boolean = operator(left(x), right)

  /**
   * Left operand function
   */
  def left: LeftOperand[A]

  /**
   * Constraint operator
   */
  def operator: ConstraintOperator

  /**
   * Right operand value
   */
  def right: Double

  /**
   * Try to transform a generic constraint into a linear constraint
   *
   * @param n size of the linear constraint (optional)
   * @return a linear constraint (of size n if specified) or a failure
   */
  def toLinearConstraint(n: Option[Int] = None)(
    implicit fromDouble: FromDouble[A]): Try[LinearConstraint[A]]

}
