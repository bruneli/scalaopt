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

package com.github.bruneli.scalaopt.core

/**
 * Define a constraint as a left hand real-valued function, an operator and a right hand side value
 *
 * @param c   left hand real-valued function
 * @param op  equality/inequality operator
 * @param b   right hand value
 * @param eps numerical precision to check an equality constraint
 *
 * @author bruneli
 */
case class Constraint(
  c: (Variables) => Double,
  op: ConstraintOperator.Value,
  b: Double,
  eps: Double = 1.0e-8) {

  /**
   * Check if constraint is satisfied in x
   *
   * @param x real-valued vector
   * @return true if constraint is satisfied in x
   */
  def apply(x: Variables): Boolean = op match {
    case ConstraintOperator.Eq => Math.abs(c(x) - b) <= eps
    case ConstraintOperator.Le => c(x) <= b
    case ConstraintOperator.Ge => c(x) >= b
  }

}

/**
 * Left hand part of a constraint
 * 
 * @param c real-valued constraint function returning a double 
 */
class LHConstraint(c: (Variables) => Double) {

  /**
   * Build an equality constraint
   * 
   * @param b right hand value
   * @return equality constraint
   */
  def equ(b: Double): Constraint = Constraint(c, ConstraintOperator.Eq, b)
  
  /** Alias of eq method */
  def ===(b: Double): Constraint = Constraint(c, ConstraintOperator.Eq, b)

  /**
   * Build an inequality constraint with an <= operator
   *
   * @param b right hand value
   * @return inequality constraint
   */
  def le(b: Double): Constraint = Constraint(c, ConstraintOperator.Le, b)

  /** Alias of le method */
  def <=(b: Double): Constraint = this.le(b)

  /**
   * Build an inequality constraint with an >= operator
   *
   * @param b right hand value
   * @return inequality constraint
   */
  def ge(b: Double): Constraint = Constraint(c, ConstraintOperator.Ge, b)

  /** Alias of ge method */
  def >=(b: Double): Constraint = this.ge(b)

}

/**
 * Enumerate the different type of constraint operators i.e. equal, lower equal, greater equal
 */
object ConstraintOperator extends Enumeration {

  val Eq, Le, Ge = Value

}