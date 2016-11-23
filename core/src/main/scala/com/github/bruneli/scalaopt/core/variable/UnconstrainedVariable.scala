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
 * Unconstrained real-valued continuous variable
 * 
 * @author bruneli
 */
case class UnconstrainedVariable(x: Double) extends AnyVal with ContinuousVariable {

  override def lower: Option[Double] = None

  override def upper: Option[Double] = None

  /** Build a new variable with x modified */
  override def build(x: Double): UnconstrainedVariable = UnconstrainedVariable(x)

}

/**
 * Vector of unconstrained continuous variables
 */
class UnconstrainedVariables(raw: Array[Double])
  extends SimpleDenseVector[UnconstrainedVariable](raw)(ConversionsFromDouble.UnconstrainedVariableFromDouble)

object UnconstrainedVariables {

  def apply(vars: Double*): UnconstrainedVariables = {
    new UnconstrainedVariables(vars.toArray)
  }

}

