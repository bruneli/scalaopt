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

import com.github.bruneli.scalaopt.core.linalg.DenseVectorLike
import com.github.bruneli.scalaopt.core.variable.{Variable, VariableFromDouble}

/**
 * Define an objective function as a real-valued function acting on a vector of variables
 *
 * Variables can be continuous or discrete, unconstrained or bounded.
 *
 * @tparam A optimization variable type
 * @author bruneli
 */
trait ObjectiveFunction[-A <: Variable] extends VariableFromDouble {

  /**
   * Evaluate the objective function for a given vector of variables
   *
   * @param x vector of variables
   * @return real-valued objective function at x
   */
  def apply(x: DenseVectorLike[A]): Double

}
