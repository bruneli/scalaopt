/*
 * Copyright 2014 Renaud Bruneliere
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

package com.github.bruneli.scalaopt.core.linalg

import com.github.bruneli.scalaopt.core._
import com.github.bruneli.scalaopt.core.variable.{Input, Output, VariableFromDouble}

/**
 * Row to represent an augmented matrix used to solve a linear system AX = B
 *
 * @param a a row in the matrix
 * @param b solution for that row
 * @param i index of the row
 *
 * @author bruneli
 */
case class AugmentedRow(a: InputsType, b: Output, i: Long) extends VariableFromDouble {

  def +(that: AugmentedRow): AugmentedRow = {
    AugmentedRow(that.a + this.a, that.b.x + this.b.x, i)
  }

  override def toString = s"row $i (${a.force.mkString(", ")} | $b)"

}

object AugmentedRow extends VariableFromDouble {

  def apply(ab: (InputsType, Output),
    i: Long): AugmentedRow = {
    AugmentedRow(ab._1, ab._2, i)
  }

  def zeros(n: Int): AugmentedRow = {
    AugmentedRow(DenseVector.zeros[Input](n), 0.0, 0)
  }

}
