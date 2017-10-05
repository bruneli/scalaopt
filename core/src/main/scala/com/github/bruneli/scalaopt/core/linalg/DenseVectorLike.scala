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
trait DenseVectorLike[+A <: ToDouble] {

  /** Vector type */
  type V <: DenseVectorLike[A]

  def length: Int

  def apply(i: Int): A

  def coordinate(i: Int): Double

  def force: DenseVector[A]

  /** Element wise addition */
  def +[B >: A <: ToDouble](that: DenseVectorLike[B]): DenseVectorLike[B]

  /** Element wise subtraction */
  def -[B >: A <: ToDouble](that: DenseVectorLike[B]): DenseVectorLike[B]

  /** Add a constant value */
  def + (offset: Double): V

  /** Subtract a constant value */
  def - (offset: Double): V

  /** Negative of a vector */
  def unary_- : V

  /** Multiplication by scalar */
  def * (scalar: Double): V

  /** Division by scalar */
  def / (scalar: Double): V

  /** Multiplication by scalar */
  def *[B >: A <: ToDouble] (scalar: B): DenseVectorLike[B]

  /** Division by scalar */
  def /[B >: A <: ToDouble] (scalar: B): DenseVectorLike[B]

  /** Inner product of two vectors */
  def inner[B <: ToDouble](that: DenseVectorLike[B]): Double = {
    var acc = 0.0
    var idx = 0
    while (idx < this.length) {
      acc += this.coordinate(idx) * that.coordinate(idx)
      idx += 1
    }
    acc
  }

  /** Dot product of two vectors */
  def dot[B <: ToDouble](that: DenseVectorLike[B]): Double = {
    this.inner(that)
  }

  /** L2 norm */
  def norm: Double = {
    math.sqrt(norm2)
  }

  /** L2 norm squared */
  def norm2: Double = {
    var acc = 0.0
    var idx = 0
    while (idx < this.length) {
      acc += this.coordinate(idx) * this.coordinate(idx)
      idx += 1
    }
    acc
  }

  /** Conversion to a column matrix */
  def toMatrix: RealMatrix

  /** Transpose, conversion to a row matrix */
  def t: RealMatrix = {
    this.toMatrix.transpose()
  }

  /** Outer product of two vectors */
  def outer[B <: ToDouble](that: DenseVectorLike[B]): RealMatrix = {
    this.toMatrix.multiply(that.t)
  }

  /** Directly update the underlying raw value of a vector */
  def updated(index: Int, elem: Double): V

  /** Map a function directly on raw values of the vector */
  def mapValues(f: Double => Double): V

  /** Map a function acting on tuple (value, index) of vector elements */
  def mapWithIndex(f: (Double, Int) => Double): V

  /**
    * Zip two vectors and map their pair of values into a new vector
    *
    * The size of the vector returned is the minimum size between this and that vectors
    *
    * @param that a vector of variable identical to the source vector
    * @param f a real-valued function acting on pairs of (this, that) vector elements
    * @return a vector of variable
    */
  def zipAndMap[B >: A <: ToDouble](that: DenseVectorLike[B],
                                    f: (Double, Double) => Double): DenseVectorLike[B]

  /** Build a new vector with values corresponding to indices i and j swapped */
  def swap(i: Int, j: Int): V

  def withValues(coordinates: Array[Double]): V = {
    newDenseVectorBuilder.withValues(coordinates)
  }

  def newDenseVectorBuilder: DenseVectorBuilder[V]

}
