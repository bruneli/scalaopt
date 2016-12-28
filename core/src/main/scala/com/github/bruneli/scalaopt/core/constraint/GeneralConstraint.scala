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
import com.github.bruneli.scalaopt.core.linalg.FromToDoubleConversions.FromDouble
import com.github.bruneli.scalaopt.core.variable.Variable

import scala.util.Try

/**
 * Define a constraint applied to a general real-valued function of x
 *
 * @param left left operand real-valued function
 * @param operator constraint operator
 * @param right right operand real-valued constant
 * @tparam A optimization variable type
 * @author bruneli
 */
case class GeneralConstraint[-A <: Variable](
  left: GeneralLeftOperand[A],
  operator: ConstraintOperator,
  right: Double) extends Constraint[A] {

  /**
   * Try to transform a generic constraint into a linear constraint
   *
   * @param n size of the linear constraint (optional)
   * @return a linear constraint (of size n if specified) or a failure
   */
  override def toLinearConstraint(n: Option[Int])(
    implicit fromDouble: FromDouble[A]): Try[LinearConstraint[A]] = {
    left.toLinearConstraint(n)(fromDouble).map(left => LinearConstraint(left, operator, right))
  }

}
