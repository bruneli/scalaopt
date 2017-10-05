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

import com.github.bruneli.scalaopt.core.function.ObjectiveFunction
import com.github.bruneli.scalaopt.core.linalg.DenseVectorLike
import com.github.bruneli.scalaopt.core.variable.Variable

/**
 * Constrained optimization problem builder
 *
 * @author bruneli
 */
trait CPBuilder[A <: Variable, B <: ObjectiveFunction[A], C <: CP[A, B, _]] {

  /**
   * Minimize an objective function acting on a set of optimization variables
   */
  def min(objectiveFunction: B, variables: DenseVectorLike[A]): C

  /**
   * Maximize an objective function acting on a set of optimization variables
   */
  def max(objectiveFunction: B, variables: DenseVectorLike[A]): C

}
