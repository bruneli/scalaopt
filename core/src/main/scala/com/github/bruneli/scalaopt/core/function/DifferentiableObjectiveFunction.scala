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

import com.github.bruneli.scalaopt.core.linalg.DenseVector
import com.github.bruneli.scalaopt.core.variable.ContinuousVariable

/**
 * Unconstrained continuous objective function with first and second order derivatives
 *
 * @tparam A continuous variable type
 * @author bruneli
 */
trait DifferentiableObjectiveFunction[A <: ContinuousVariable] extends ContinuousObjectiveFunction[A] {

  /**
   * Gradient of f evaluated in x
   *
   * By default, the gradient is estimated with finite differences.
   *
   * @param x vector of variables
   * @return gradient of f in x
   */
  def gradient(x: DenseVector[A]): DenseVector[A]

  /**
   * Evaluate the directional derivative of f in x
   *
   * By default, the derivative is estimated with finite differences.
   *
   * @param x vector of variables
   * @param d directional vector
   * @return directional derivative of f along d in x
   */
  def dirder(x: DenseVector[A], d: DenseVector[A]): Double

  /**
   * Evaluate the vector product of the Hessian evaluated at x and a direction d
   *
   * @param x vector of variables
   * @param d directional vector
   * @return product of the Hessian in x times d
   */
  def dirHessian(x: DenseVector[A], d: DenseVector[A]): DenseVector[A]

}
