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

/**
 * Greater or equal inequality operator
 *
 * @author bruneli
 */
object GreaterOrEqualOperator extends ConstraintOperator {

  /**
   * Check if left and right operands satisfy the constraint defined by the operator
   *
   * @param left left operand real-value
   * @param right right operand real-value
   * @return true if constraint is satisfied
   */
  def apply(left: Double, right: Double): Boolean = left >= right

}