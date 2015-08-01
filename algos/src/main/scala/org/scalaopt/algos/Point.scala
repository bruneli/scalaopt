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

package org.scalaopt.algos

/**
 * Data point representation
 *
 * @param x independent variables X
 * @param y dependent variables Y
 */
case class DataPoint(x: Variables, y: Variables)

object DataPoint {

  def apply(x: Variables, y: Double): DataPoint = DataPoint(x, Seq(y))

  def apply(x: Double, y: Double): DataPoint = DataPoint(Seq(x), Seq(y))

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
  x: Variables,
  f: ObjectiveFunction,
  d: Variables) {

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
  def m(p: Variables, eps: Double = 1.0e-8): Double = {
    if (p.norm < eps) {
      fx
    } else {
      val pTBp = p dot f.dirHessian(x, p)
      fx + (grad dot p) + pTBp / 2.0
    }
  }
}
