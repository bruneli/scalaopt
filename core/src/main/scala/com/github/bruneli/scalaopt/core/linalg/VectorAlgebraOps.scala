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

package com.github.bruneli.scalaopt.core.linalg

import com.github.bruneli.scalaopt.core.linalg.FromToDoubleConversions.ToDouble
import org.apache.commons.math3.linear.RealMatrix

/**
 * Define operations on real-valued vector
 *
 * @tparam A vector type
 * @author bruneli
 */
trait VectorAlgebraOps[+A] {

  /** Add a constant value */
  def + (offset: Double): A

  /** Subtract a constant value */
  def - (offset: Double): A

  /** Negative of a vector */
  def unary_- : A

  /** Multiplication by scalar */
  def * (scalar: Double): A

  /** Division by scalar */
  def / (scalar: Double): A

  /** Multiplication by scalar */
  def *[B <: ToDouble] (scalar: B): A

  /** Division by scalar */
  def /[B <: ToDouble] (scalar: B): A

  /** Inner product of two vectors */
  def inner[B <: ToDouble](that: DenseVector[B]): Double

  /** Dot product of two vectors */
  def dot[B <: ToDouble](that: DenseVector[B]): Double

  /** L2 norm */
  def norm: Double = math.sqrt(norm2)

  /** L2 norm squared */
  def norm2: Double

  /** Conversion to a column matrix */
  def toMatrix: RealMatrix

  /** Transpose, conversion to a row matrix */
  def t: RealMatrix

  /** Outer product of two vectors */
  def outer[B <: ToDouble](that: DenseVector[B]): RealMatrix

}
