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

import com.github.bruneli.scalaopt.core._
import com.github.bruneli.scalaopt.core.linalg.FromToDoubleConversions.{FromDouble, ToDouble}
import org.apache.commons.math3.linear.{MatrixUtils, RealMatrix}

import scala.collection.generic.CanBuildFrom
import scala.collection.{IndexedSeqLike, mutable}
import scala.collection.immutable.IndexedSeq

/**
 * Single dimension real-valued dense vector
 *
 * @author bruneli
 */
trait DenseVector[+A <: ToDouble]
  extends IndexedSeq[A]
    with IndexedSeqLike[A, DenseVector[A]]
    with DenseVectorLike[A] {
  self =>

  type V <: DenseVector[A]

  /** Representation of the vector as an array of double for quick operations */
  val coordinates: Array[Double]

  def length: Int = {
    coordinates.length
  }

  def coordinate(i: Int): Double = {
    coordinates(i)
  }

  def force: DenseVector[A] = {
    this
  }

  override protected[this] def newBuilder: mutable.Builder[A, DenseVector[A]] = {
    self.newBuilder
  }

  override def newDenseVectorBuilder: DenseVectorBuilder[V] = {
    self.newDenseVectorBuilder
  }

  /** Element wise addition */
  def +[B >: A <: ToDouble](that: DenseVectorLike[B]): DenseVector[B] = {
    val sum = new Array[Double](this.length)
    var idx = 0
    while (idx < sum.length) {
      sum(idx) = this.coordinate(idx) + that.coordinate(idx)
      idx += 1
    }
    newDenseVectorBuilder.withValues(sum)
  }

  /** Element wise subtraction */
  def -[B >: A <: ToDouble](that: DenseVectorLike[B]): DenseVector[B] = {
    val difference = new Array[Double](this.length)
    var idx = 0
    while (idx < difference.length) {
      difference(idx) = this.coordinate(idx) - that.coordinate(idx)
      idx += 1
    }
    newDenseVectorBuilder.withValues(difference)
  }

  /** Add a constant value */
  override def +(offset: Double): V = {
    val sum = new Array[Double](this.length)
    var idx = 0
    while (idx < sum.length) {
      sum(idx) = this.coordinates(idx) + offset
      idx += 1
    }
    newDenseVectorBuilder.withValues(sum)
  }

  /** Subtract a constant value */
  override def -(offset: Double): V = {
    this + (-offset)
  }

  /** Negative of a vector */
  override def unary_- : V = {
    val opposite = new Array[Double](this.length)
    var idx = 0
    while (idx < opposite.length) {
      opposite(idx) = -this.coordinates(idx)
      idx += 1
    }
    newDenseVectorBuilder.withValues(opposite)
  }

  /** Multiplication by scalar */
  override def *(scalar: Double): V = {
    val product = new Array[Double](this.length)
    var idx = 0
    while (idx < product.length) {
      product(idx) = this.coordinates(idx) * scalar
      idx += 1
    }
    newDenseVectorBuilder.withValues(product)
  }

  /** Division by scalar */
  override def /(scalar: Double): V = {
    val division = new Array[Double](this.length)
    var idx = 0
    while (idx < division.length) {
      division(idx) = this.coordinates(idx) / scalar
      idx += 1
    }
    newDenseVectorBuilder.withValues(division)
  }

  /** Multiplication by scalar */
  override def *[B >: A <: ToDouble](scalar: B): DenseVectorLike[B] = {
    this * scalar.x
  }

  /** Division by scalar */
  override def /[B >: A <: ToDouble](scalar: B): DenseVectorLike[B] = {
    this / scalar.x
  }

  /** Conversion to a column matrix */
  override def toMatrix: RealMatrix = MatrixUtils.createColumnRealMatrix(coordinates)

  /** Directly update the underlying raw value of a vector */
  def updated(index: Int, elem: Double): V = {
    val updated = new Array[Double](coordinates.length)
    var idx = 0
    while (idx < updated.length) {
      if (idx == index) {
        updated(idx) = elem
      } else {
        updated(idx) = coordinates(idx)
      }
      idx += 1
    }
    newDenseVectorBuilder.withValues(updated)
  }

  /** Map a function directly on raw values of the vector */
  def mapValues(f: Double => Double): V = {
    val updated = new Array[Double](coordinates.length)
    var idx = 0
    while (idx < updated.length) {
      updated(idx) = f(coordinates(idx))
      idx += 1
    }
    newDenseVectorBuilder.withValues(updated)
  }

  /** Map a function acting on tuple (value, index) of vector elements */
  def mapWithIndex(f: (Double, Int) => Double): V = {
    val updated = new Array[Double](coordinates.length)
    var idx = 0
    while (idx < updated.length) {
      updated(idx) = f(coordinates(idx), idx)
      idx += 1
    }
    newDenseVectorBuilder.withValues(updated)
  }

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
                                    f: (Double, Double) => Double): DenseVector[B] = {
    val result = new Array[Double](Math.min(this.length, that.length))
    var idx = 0
    while (idx < result.length) {
      result(idx) = f(this.coordinate(idx), that.coordinate(idx))
      idx += 1
    }
    newDenseVectorBuilder.withValues(result)
  }

  /** Build a new vector with values corresponding to indices i and j swapped */
  def swap(i: Int, j: Int): V = {
    val updated = new Array[Double](coordinates.length)
    var idx = 0
    while (idx < updated.length) {
      if (idx == i) {
        updated(idx) = coordinates(j)
      } else if (idx == j) {
        updated(idx) = coordinates(i)
      } else {
        updated(idx) = coordinates(idx)
      }
      idx += 1
    }
    newDenseVectorBuilder.withValues(updated)
  }

}

object DenseVector {

  implicit def canBuildFrom[A <: ToDouble : FromDouble]: CanBuildFrom[DenseVector[_], A, DenseVector[A]] = {
    new CanBuildFrom[DenseVector[_], A, DenseVector[A]] {
      def apply(from: DenseVector[_]) = SimpleDenseVector.newBuilder[A]

      def apply() = SimpleDenseVector.newBuilder[A]
    }
  }

  /** Create an n-vector of Variables filled with a constant value */
  def fill[A <: ToDouble : FromDouble](n: Int)(value: Double): DenseVector[A] = {
    SimpleDenseVector(Array.fill(n)(value))
  }

  /** Create a n-vector of Variables filled with zeros */
  def zeros[A <: ToDouble : FromDouble](n: Int): DenseVector[A] = fill(n)(0.0)

  /** Create an n-vector of Variables filled with ones */
  def ones[A <: ToDouble : FromDouble](n: Int): DenseVector[A] = fill(n)(1.0)

  /** Create an n-dimensional basis vector with i-th element set to 1 */
  def e[A <: ToDouble : FromDouble](n: Int, i: Int): DenseVector[A] = {
    val raw = new Array[Double](n)
    var j: Int = 0
    while (j < n) {
      raw(j) = if (i == j) 1.0 else 0.0
      j += 1
    }
    SimpleDenseVector(raw)
  }

  /**
   * Compute the gradient of f in x via finite differences
   *
   * @param f real-valued function on x
   * @param x vector of real values
   * @param eps precision used to compute finite differences
   * @tparam A variable type
   * @return gradient of f in x
   */
  def gradient[A <: ToDouble](
    f: DenseVectorLike[A] => Double,
    x: DenseVectorLike[A],
    eps: Double = 1.0e-8): DenseVectorLike[A] = {
    val fx = f(x)
    x.mapWithIndex((xi, i) => (f(x.updated(i, xi + eps)) - fx) / eps)
  }

  /**
   * Element-wise minimum of left and right vectors
   *
   * @param left real-valued vector
   * @param right real-valued vector
   * @tparam A vector element type
   * @return element-wise minimum of left and right vectors
   */
  def min[A <: ToDouble](
    left: DenseVectorLike[A],
    right: DenseVectorLike[A]): DenseVectorLike[A] = {
    left.zipAndMap(right, Math.min)
  }

  /**
   * Element-wise maximum of left and right vectors
   *
   * @param left real-valued vector
   * @param right real-valued vector
   * @tparam A vector element type
   * @return element-wise maximum of left and right vectors
   */
  def max[A <: ToDouble](
    left: DenseVectorLike[A],
    right: DenseVectorLike[A]): DenseVectorLike[A] = {
    left.zipAndMap(right, Math.max)
  }

  /**
   * Permute the elements of a vector
   *
   * Element i of the initial vector is moved to ipvt(i)
   *
   * @param ipvt pivot matrix used to permute vector elements
   * @param x initial vector
   * @tparam A vector element type
   * @return vector with permuted elements
   */
  def permute[A <: ToDouble](
    ipvt: Array[Int])(
    x: DenseVectorLike[A]): DenseVector[A] = {
    val xPivoted = new Array[Double](x.length)
    for (j <- 0 until x.length) xPivoted(ipvt(j)) = x.coordinate(j)
    x.force.newDenseVectorBuilder.withValues(xPivoted)
  }

  /**
   * Permute the elements of a vector applying the inverse transformation of ipvt
   *
   * Element ipvt(i) of the initial vector is moved to i
   *
   * @param ipvt pivot matrix used to permute vector elements
   * @param x initial vector
   * @tparam A vector element type
   * @return vector with permuted elements
   */
  def unpermute[A <: ToDouble](
    ipvt: Array[Int])(
    x: DenseVectorLike[A]): DenseVector[A] = {
    val xUnpivoted = new Array[Double](x.length)
    for (j <- 0 until x.length) xUnpivoted(j) = x.coordinate(ipvt(j))
    x.force.newDenseVectorBuilder.withValues(xUnpivoted)
  }

}
