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
 * Objective function f(x) subject to constraints on the continuous variables x
 *
 * f and c_i are all smooth real-valued functions.
 *
 * @author bruneli
 */
trait ConstrainedObjectiveFunction extends ObjectiveFunction {

  override val isConstrained = true

  /**
   * Return the number of constraints
   */
  def numberOfConstraints: Int

  /**
   * Get a constraint
   *
   * @param i index of the constraint
   * @return constraint
   */
  def constraint(i: Int): Constraint

  /** Alias of the constraint method */
  def c(i: Int): Constraint = constraint(i)

}

class ConstrainedFunctionWithGradient(
  f: (Variables => Double, Variables => Variables),
  constraints: Vector[Constraint],
  implicit val pars: ConfigPars = new ConfigPars()) extends ConstrainedObjectiveFunction {

  def apply(x: Variables) = f._1(x)

  override def gradient(x: Variables) = f._2(x)

  override def dirder(x: Variables, d: Variables): Double = {
    gradient(x) dot d
  }

  override def dirHessian(x: Variables, d: Variables): Variables = {
    val gradx = gradient(x)
    val gradxd = gradient(x + d * pars.eps)
    (gradxd - gradx) / pars.eps
  }

  override def numberOfConstraints: Int = constraints.size

  override def constraint(i: Int): Constraint = constraints(i)
}

class ConstrainedFunctionFiniteDiffGradient(
  f: Variables => Double,
  constraints: Vector[Constraint],
  implicit val pars: ConfigPars = new ConfigPars()) extends ConstrainedObjectiveFunction {

  def apply(x: Variables) = f(x)

  override def gradient(x: Variables): Variables = {
    val fx: Double = this(x)
    for (i <- x.indices) yield (this(x.updated(i, x(i) + pars.eps)) - fx) / pars.eps
  }

  override def dirder(x: Variables, d: Variables): Double = {
    (this(x + d * pars.eps) - this(x)) / pars.eps
  }

  override def dirHessian(x: Variables, d: Variables): Variables = {
    val gradx = gradient(x)
    val gradxd = gradient(x + d * pars.eps)
    (gradxd - gradx) / pars.eps
  }

  override def numberOfConstraints: Int = constraints.size

  override def constraint(i: Int): Constraint = constraints(i)
}
