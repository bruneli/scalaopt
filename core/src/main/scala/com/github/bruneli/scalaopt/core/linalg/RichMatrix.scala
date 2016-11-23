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
import org.apache.commons.math3.linear.RealMatrix

/**
 * Enrich apache math3 RealMatrix with operators.
 *
 * @param m a matrix of Double values
 *   
 * @author bruneli
 */
class RichMatrix(m: RealMatrix) {

  /** Element-wise addition */
  def +(that: RealMatrix): RealMatrix =
    if (m.getRowDimension == that.getRowDimension &&
      m.getColumnDimension == that.getColumnDimension) {
      m.add(that)
    } else {
      throw new IllegalArgumentException(
        s"${asStr("this", m)} and ${asStr("that", that)} should have same dimensions")
    }

  /** Element-wise subtraction */
  def - (that: RealMatrix): RealMatrix =
    if (m.getRowDimension == that.getRowDimension &&
      m.getColumnDimension == that.getColumnDimension) {
      m.subtract(that)
    } else {
      throw new IllegalArgumentException(
        s"${asStr("this", m)} and ${asStr("that", that)} should have same dimensions")
    }

  /** Negative of a matrix */
  def unary_- : RealMatrix = m.scalarMultiply(-1.0)
  
  /** Multiplication by a scalar */
  def * (scalar: Double): RealMatrix = m.scalarMultiply(scalar)
  
  /** Division by a scalar */
  def / (scalar: Double): RealMatrix = 
    if (scalar == 0.0) {
      throw new IllegalArgumentException("scalar should be != 0.0")
    } else {
      m.scalarMultiply(1.0 / scalar)
    }

  /** Matrix-matrix multiplication */
  def * (that: RealMatrix): RealMatrix =
    if (m.getColumnDimension == that.getRowDimension) {
      m.multiply(that)
    } else {
      throw new IllegalArgumentException(
        s"${asStr("this", m)} and ${asStr("that", that)} should have same dimensions")
    }

  /** Matrix-Vector multiplication */
  def * (that: UnconstrainedVariablesType): UnconstrainedVariablesType =
    if (m.getColumnDimension == that.length) {
      m.operate(that.raw)
    } else {
      throw new IllegalArgumentException(
        s"Number of columms ${m.getColumnDimension} != vector dimension ${that.length}")
    }

  private def asStr(str: String, m: RealMatrix) =
    s"$str(${m.getRowDimension}, ${m.getColumnDimension})"

}