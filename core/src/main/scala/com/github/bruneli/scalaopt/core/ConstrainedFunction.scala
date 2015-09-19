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

import com.github.bruneli.scalaopt.core._
import org.apache.commons.math3.linear.RealMatrix

/**
 * Minimize a function f(x) subject to constraints on the continuous variables x
 *
 * The formulation adopted by this package to deal with constraints is:
 * min f(x)
 * subject to
 *  - c_i(x) = 0 if i is an equality
 *  - c_i(x) >= 0 if i is an inequality
 * f and c_i are all smooth real-valued functions.
 *
 * @author bruneli
 */
trait ConstrainedFunction extends ObjectiveFunction {

  /** Equality constraints */
  val equalities: Vector[Variables => Double]

  /** Inequality constraints */
  val inequalities: Vector[Variables => Double]

}

/**
 * Objective function and constrains used in linear programming
 *
 * The objective function is defined by c*x and the equality constraints by ax=b
 *
 * @param a real valued matrix of size m x n used for constraints
 * @param b real valued vector of size m equal to the number of constraints
 * @param c real valued vector of size n (cost vector) used to evaluated the objective function cx
 */
case class LinearProgramming(a: RealMatrix, b: Variables, c: Variables) extends ConstrainedFunction {

  val n = c.size
  val m = b.size
  require(a.getRowDimension == m, s"Number of rows in a should be equal to b size")
  require(a.getColumnDimension == n, s"Number of columns in a should be equal to c size")

  val equalities = for (i <- 0 until m) yield (x: Variables) => a.getRow(i).toSeq dot x - b
  val inequalities = Vector.empty[Variables => Double]

  /**
   * Evaluate the objective function for a given vector of variables
   *
   * @param x vector of variables
   * @return real-valued objective function at x
   */
  def apply(x: Variables): Double = c dot x

  /**
   * Gradient of f evaluated in x
   *
   * @param x vector of variables
   * @return gradient of f in x
   */
  override def gradient(x: Variables): Variables = c

}