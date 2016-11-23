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

import com.github.bruneli.scalaopt.core.linalg.DenseVector
import com.github.bruneli.scalaopt.core.linalg.FromToDoubleConversions.FromDouble
import com.github.bruneli.scalaopt.core.variable.Variable

/**
 * Class used to build a general constraint from an anonymous function
 *
 * @param function general real-valued function on x
 * @author bruneli
 */
case class GeneralConstraintBuilder[A <: Variable](
  function: DenseVector[A] => Double)(
  implicit fromDouble: FromDouble[A]) extends ConstraintBuilder[A, GeneralConstraint[A]] {

  override protected def build(operator: ConstraintOperator, right: Double): GeneralConstraint[A] = {
    GeneralConstraint(GeneralLeftOperand(function), operator, right)
  }

}
