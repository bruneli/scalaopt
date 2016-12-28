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

import com.github.bruneli.scalaopt.core.linalg.FromToDoubleConversions.FromDouble
import com.github.bruneli.scalaopt.core.linalg.{DenseVector, SimpleDenseVector}
import com.github.bruneli.scalaopt.core.variable.{Constant, Constants, Variable}

import scala.util.{Failure, Success, Try}

/**
 * Left operand defining a linear real-valued function
 *
 * @param a linear coefficients
 * @tparam A optimization variable type
 * @author bruneli
 */
case class LinearLeftOperand[-A <: Variable](
  a: DenseVector[Constant]) extends LeftOperand[A] {

  /**
   * Evaluate the constraint left operand in x
   *
   * @param x vector of variables
   * @return left operand real-value in x
   */
  def apply(x: DenseVector[A]): Double = a dot x

  /**
   * Try to transform a generic constraint left operand into a linear constraint left operand
   *
   * @param n size of the linear constraint (optional)
   * @return a linear constraint left operand (of size n if specified) or a failure
   */
  override def toLinearConstraint(n: Option[Int] = None)(
    implicit fromDouble: FromDouble[A]): Try[LinearLeftOperand[A]] = n match {
    case Some(length) =>
      if (length < a.length) {
        Failure(throw new IllegalArgumentException(
          s"cannot reduce size of linear constraint from ${a.length} to $length"))
      } else {
        val extended = new Array[Double](length)
        var idx = 0
        while (idx < length) {
          if (idx < a.length) {
            extended(idx) = a.raw(idx)
          } else {
            extended(idx) = 0.0
          }
          idx += 1
        }
        Success(LinearLeftOperand(new Constants(extended)))
      }
    case None => Success(this)
  }

}
