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

import com.github.bruneli.scalaopt.core.linalg.SimpleDenseVector

/**
 * Positive real-valued continuous variable
 * 
 * @author bruneli
 */
case class PositiveVariable(x: Double) extends AnyVal with ContinuousVariable {

  override def lower: Option[Double] = Some(0.0)

  override def upper: Option[Double] = None

  /** Build a new variable with x modified */
  override def build(x: Double): PositiveVariable = PositiveVariable(x)

}

/**
 * Vector of positive continuous variables
 */
class PositiveVariables(raw: Array[Double])
  extends SimpleDenseVector[PositiveVariable](raw)(ConversionsFromDouble.PositiveVariableFromDouble)

object PositiveVariables {

  def apply(vars: Double*): PositiveVariables = {
    new PositiveVariables(vars.toArray)
  }

}


