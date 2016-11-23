/*
 * Copyright 2014 Renaud Bruneliere
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

package com.github.bruneli.scalaopt.core.variable

import com.github.bruneli.scalaopt.core._
import com.github.bruneli.scalaopt.core.function.DifferentiableObjectiveFunction
import com.github.bruneli.scalaopt.core.linalg.{DenseVector, SimpleDenseVector}

/**
 * Data point representation
 *
 * @param x independent inputs X
 * @param y dependent outputs Y
 */
case class DataPoint(x: InputsType, y: OutputsType)

object DataPoint {

  def apply(x: InputsType, y: Output): DataPoint = DataPoint(x, SimpleDenseVector(y))

  def apply(x: Input, y: Output): DataPoint = DataPoint(SimpleDenseVector(x), SimpleDenseVector(y))

}

/**
 * Define a point with a direction used in searches along a line.
 *
 * @param x coordinates of the point
 * @param f real-valued function acting on coordinates
 * @param d line search direction
 *
 * @author bruneli
 */
case class LineSearchPoint(
  x: UnconstrainedVariablesType,
  f: DifferentiableObjectiveFunction[UnconstrainedVariable],
  d: UnconstrainedVariablesType) {

  /** real-valued function f evaluated at x */
  lazy val fx = f(x)

  /** directional derivative at x along d */
  lazy val dfx = f.dirder(x, d)

  /** gradient df evaluated at x */
  lazy val grad = f.gradient(x)

  /** Hessian vector product with d */
  lazy val d2fxd = f.dirHessian(x, d)

  /**
   * Second order approximation of f evaluated in p.
   *
   * mk(p) = fx + gk p + pT Bk p
   * with Bk an approximate Hessian in x.
   */
  def m(p: UnconstrainedVariablesType, eps: Double = 1.0e-8): Double = {
    if (p.norm < eps) {
      fx
    } else {
      val pTBp = p dot f.dirHessian(x, p)
      fx + (grad dot p) + pTBp / 2.0
    }
  }
}
