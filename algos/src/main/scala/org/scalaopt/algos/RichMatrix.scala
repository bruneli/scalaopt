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

package org.scalaopt.algos

import org.jblas.DoubleMatrix

/**
 * Enrich org.jblas.DoubleMatrix with operators.
 *
 * @param m a matrix of Double values
 *   
 * @author bruneli
 */
class RichMatrix(m: DoubleMatrix) {

  /** Element-wise addition */
  def +(that: DoubleMatrix): DoubleMatrix =
    if (m.rows == that.rows && m.columns == that.columns)
      m.add(that)
    else
      throw new IllegalArgumentException(
          "Matrices should have same dimensions")
  
  /** Element-wise subtraction */
  def - (that: DoubleMatrix): DoubleMatrix =
    if (m.rows == that.rows && m.columns == that.columns)
      m.sub(that)
    else
      throw new IllegalArgumentException(
          "Matrices should have same dimensions")

  /** Negative of a matrix */
  def unary_- : DoubleMatrix = m.neg
  
  /** Multiplication by a scalar */
  def * (scalar: Double): DoubleMatrix = m.mul(scalar)
  
  /** Division by a scalar */
  def / (scalar: Double): DoubleMatrix = 
    if (scalar == 0.0)
      throw new IllegalArgumentException("scalar should be != 0.0")
    else
      m.div(scalar)
  
  /** Matrix-matrix multiplication */
  def * (that: DoubleMatrix): DoubleMatrix =
    if (m.columns == that.rows)
      m.mmul(that)
    else
      throw new IllegalArgumentException(
          "Number of columns of m1 != number of rows of m2")
  
  /** Matrix-Vector multiplication */
  def * (that: Coordinates): Coordinates =
    if (m.columns == that.length)
      (m.mmul(that.toMatrix)).data
    else
      throw new IllegalArgumentException(
          "Number of columns of m1 != number of rows of m2")
  
}