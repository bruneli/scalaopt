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
 * @author bruneli
 */
trait VectorAlgebraOps[+A <: ToDouble] {

  /** Vector type */
  type V[+T <: ToDouble]

  /** Element wise addition */
  def + [B <: ToDouble](that: V[B]): V[B]

  /** Element wise subtraction */
  def - [B <: ToDouble](that: V[B]): V[B]

  /** Add a constant value */
  def + (offset: Double): V[A]

  /** Subtract a constant value */
  def - (offset: Double): V[A]

  /** Negative of a vector */
  def unary_- : V[A]

  /** Multiplication by scalar */
  def * (scalar: Double): V[A]

  /** Division by scalar */
  def / (scalar: Double): V[A]

  /** Multiplication by scalar */
  def *[B <: ToDouble] (scalar: B): V[A]

  /** Division by scalar */
  def /[B <: ToDouble] (scalar: B): V[A]

  /** Inner product of two vectors */
  def inner[B <: ToDouble](that: V[B]): Double

  /** Dot product of two vectors */
  def dot[B <: ToDouble](that: V[B]): Double

  /** L2 norm */
  def norm: Double = math.sqrt(norm2)

  /** L2 norm squared */
  def norm2: Double

  /** Conversion to a column matrix */
  def toMatrix: RealMatrix

  /** Transpose, conversion to a row matrix */
  def t: RealMatrix

  /** Outer product of two vectors */
  def outer[B <: ToDouble](that: V[B]): RealMatrix

}
