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

package com.github.bruneli.scalaopt.core.function

import com.github.bruneli.scalaopt.core.MaxIterException
import com.github.bruneli.scalaopt.core.constraint.LinearLeftOperand
import com.github.bruneli.scalaopt.core.linalg.{DenseVector, DenseVectorLike, SimpleDenseVector}
import com.github.bruneli.scalaopt.core.linalg.FromToDoubleConversions.FromDouble
import com.github.bruneli.scalaopt.core.variable.{Constant, Constants, ContinuousVariable}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/**
 * Define a real-valued linear objective function acting on a vector of continuous variables
 *
 * @param cost cost vector representing the linear coefficients
 * @tparam A continuous variable type
 *
 * @author bruneli
 */
case class LinearContinuousObjectiveFunction[A <: ContinuousVariable](
  cost: DenseVector[Constant])
  extends LinearObjectiveFunctionLike[ContinuousVariable]
    with DifferentiableObjectiveFunction[A] {

  /**
   * Gradient of f evaluated in x
   *
   * By default, the gradient is estimated with finite differences.
   *
   * @param x vector of variables
   * @return gradient of f in x
   */
  override def gradient(x: DenseVectorLike[A]): DenseVectorLike[A] = {
    x.withValues(cost.coordinates)
  }

  /**
   * Evaluate the directional derivative of f in x
   *
   * By default, the derivative is estimated with finite differences.
   *
   * @param x vector of variables
   * @param d directional vector
   * @return directional derivative of f along d in x
   */
  override def dirder(x: DenseVectorLike[A], d: DenseVectorLike[A]): Double = {
    gradient(x) dot d
  }

  /**
   * Evaluate the vector product of the Hessian evaluated at x and a direction d
   *
   * @param x vector of variables
   * @param d directional vector
   * @return product of the Hessian in x times d
   */
  override def dirHessian(x: DenseVectorLike[A], d: DenseVectorLike[A]): DenseVectorLike[A] = {
    val zeros = Array.fill(cost.length)(0.0)
    d.withValues(zeros)
  }

  def toContinuous: LinearContinuousObjectiveFunction[ContinuousVariable] = {
    this.asInstanceOf[LinearContinuousObjectiveFunction[ContinuousVariable]]
  }

}

object LinearContinuousObjectiveFunction {

  val MaxLinearObjectiveFunctionRandomSize = 1000

  def apply[A <: ContinuousVariable : FromDouble](f: DenseVector[A] => Double): LinearContinuousObjectiveFunction[A] = {
    @tailrec
    def iterate(size: Int): LinearContinuousObjectiveFunction[A] = {
      if (size >= MaxLinearObjectiveFunctionRandomSize) {
        throw MaxIterException(
          s"Failed to build a linear objective function with size lower than $MaxLinearObjectiveFunctionRandomSize")
      } else {
        Try(f(DenseVector.ones[A](size))) match {
          case Success(value) =>
            val a = (0 until size).map(i => f(DenseVector.e[A](size, i)))
            LinearContinuousObjectiveFunction(new Constants(a.toArray))
          case Failure(e) => iterate(size + 1)
        }
      }
    }
    iterate(1)
  }

}