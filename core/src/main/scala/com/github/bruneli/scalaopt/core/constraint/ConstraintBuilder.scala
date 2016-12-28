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

/**
 * Build a constraint from a function and an operator
 *
 * @author bruneli
 */
trait ConstraintBuilder[A <: Variable] {

  type B <: Constraint[A]

  /**
   * Build an equality constraint
   *
   * @param b right hand value
   * @return equality constraint
   */
  def equ(b: Double): B = build(EqualityOperator(), b)

  /** Alias of eq method */
  def ===(b: Double): B = equ(b)

  /**
   * Build an inequality constraint with an <= operator
   *
   * @param b right hand value
   * @return inequality constraint
   */
  def le(b: Double): B = build(LowerOrEqualOperator, b)

  /** Alias of le method */
  def <=(b: Double): B = le(b)

  /**
   * Build an inequality constraint with an >= operator
   *
   * @param b right hand value
   * @return inequality constraint
   */
  def ge(b: Double): B = build(GreaterOrEqualOperator, b)

  /** Alias of ge method */
  def >=(b: Double): B = ge(b)

  protected def build(operator: ConstraintOperator, right: Double): B

}
