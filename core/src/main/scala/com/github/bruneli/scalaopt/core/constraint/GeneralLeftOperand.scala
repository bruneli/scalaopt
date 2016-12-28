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

import com.github.bruneli.scalaopt.core._
import com.github.bruneli.scalaopt.core.linalg.FromToDoubleConversions.FromDouble
import com.github.bruneli.scalaopt.core.linalg.{DenseVector, SimpleDenseVector}
import com.github.bruneli.scalaopt.core.variable.{Constants, Variable, VariableFromDouble}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/**
 * Left operand defining a general real-valued function
 *
 * @param f real-valued function acting on a vector of variables
 * @tparam A optimization variable type
 * @author bruneli
 */
case class GeneralLeftOperand[-A <: Variable](
  f: DenseVector[A] => Double) extends LeftOperand[A] with VariableFromDouble {

  val MaxLinearConstraintRandomSize = 1000

  /**
   * Evaluate the constraint left operand in x
   *
   * @param x vector of variables
   * @return left operand real-value in x
   */
  def apply(x: DenseVector[A]): Double = f(x)

  /**
   * Try to transform a generic constraint left operand into a linear constraint left operand
   *
   * @param n size of the linear constraint (optional)
   * @return a linear constraint left operand (of size n if specified) or a failure
   */
  override def toLinearConstraint(n: Option[Int] = None)(
    implicit fromDouble: FromDouble[A]): Try[LinearLeftOperand[A]] = n match {
    case Some(length) =>
      Try(f(DenseVector.ones[A](length))).map {
        value =>
          val a = (0 until length).map(i => f(DenseVector.e[A](length, i)))
          LinearLeftOperand[A](new Constants(a.toArray))
      }
    case None =>
      @tailrec
      def iterate(size: Int): Try[LinearLeftOperand[A]] = {
        if (size >= MaxLinearConstraintRandomSize) {
          Failure(throw MaxIterException(
            s"Failed to build a linear constraint with size lower than $MaxLinearConstraintRandomSize"))
        } else {
          Try(f(DenseVector.ones[A](size))) match {
            case Success(value) =>
              val a = (0 until size).map(i => f(DenseVector.e[A](size, i)))
              Success(LinearLeftOperand[A](new Constants(a.toArray)))
            case Failure(e) => iterate(size + 1)
          }
        }
      }
      iterate(1)
  }

}
