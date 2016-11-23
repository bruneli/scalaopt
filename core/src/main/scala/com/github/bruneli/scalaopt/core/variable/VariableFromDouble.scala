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

package com.github.bruneli.scalaopt.core.variable

import com.github.bruneli.scalaopt.core.linalg.FromToDoubleConversions.FromDouble

/**
 * Implicitly convert a double to a Variable
 *
 * @author bruneli
 */
trait VariableFromDouble {

  implicit val VariableFromDouble: FromDouble[Variable] = {
    (x : Double) => Unknown(x)
  }

  implicit val ContinuousVariableFromDouble: FromDouble[ContinuousVariable] = {
    (x : Double) => Unknown(x)
  }

  implicit val UnconstrainedVariableFromDouble: FromDouble[UnconstrainedVariable] = {
    (x : Double) => UnconstrainedVariable(x)
  }

  implicit val PositiveVariableFromDouble: FromDouble[PositiveVariable] = {
    (x: Double) => PositiveVariable(x)
  }

  implicit val BinaryVariableFromDouble: FromDouble[BinaryVariable] = {
    (x: Double) => BinaryVariable(x >= 0.5)
  }

  implicit val InputFromDouble: FromDouble[Input] = {
    (x: Double) => Input(x)
  }

  implicit val OutputFromDouble: FromDouble[Output] = {
    (x: Double) => Output(x)
  }

  implicit val ConstantFromDouble: FromDouble[Constant] = {
    (x: Double) => Constant(x)
  }

}

object ConversionsFromDouble extends VariableFromDouble
