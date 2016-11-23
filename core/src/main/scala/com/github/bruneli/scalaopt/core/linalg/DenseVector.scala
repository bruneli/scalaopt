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
    with VectorAlgebraOps[DenseVector[A]] {
  self =>

  /** Representation of the vector as an array of double for quick operations */
  val raw: Array[Double]

  override def length: Int = raw.length

  override protected[this] def newBuilder: mutable.Builder[A, DenseVector[A]] = {
    self.newBuilder
  }

  protected def build(updated: Array[Double]): DenseVector[A]

  /** Directly update the underlying raw value of a vector */
  def updated(index: Int, elem: Double): DenseVector[A] = {
    val updated = new Array[Double](raw.length)
    var idx = 0
    while (idx < updated.length) {
      if (idx == index) {
        updated(idx) = elem
      } else {
        updated(idx) = raw(idx)
      }
      idx += 1
    }
    build(updated)
  }

  /** Map a function directly on raw values of the vector */
  def mapValues(f: Double => Double): DenseVector[A] = {
    val updated = new Array[Double](raw.length)
    var idx = 0
    while (idx < updated.length) {
      updated(idx) = f(raw(idx))
      idx += 1
    }
    build(updated)
  }

  /** Map a function acting on tuple (value, index) of vector elements */
  def mapWithIndex(f: (Double, Int) => Double): DenseVector[A] = {
    val updated = new Array[Double](raw.length)
    var idx = 0
    while (idx < updated.length) {
      updated(idx) = f(raw(idx), idx)
      idx += 1
    }
    build(updated)
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
  def zipAndMap[B >: A <: ToDouble](that: DenseVector[B], f: (Double, Double) => Double): DenseVector[B] = {
    val result = new Array[Double](Math.min(this.length, that.length))
    var idx = 0
    while (idx < result.length) {
      result(idx) = f(this.raw(idx), that.raw(idx))
      idx += 1
    }
    that.build(result)
  }

  /** Build a new vector with values corresponding to indices i and j swapped */
  def swap(i: Int, j: Int): DenseVector[A] = {
    val updated = new Array[Double](raw.length)
    var idx = 0
    while (idx < updated.length) {
      if (idx == i) {
        updated(idx) = raw(j)
      } else if (idx == j) {
        updated(idx) = raw(i)
      } else {
        updated(idx) = raw(idx)
      }
      idx += 1
    }
    build(updated)
  }

  /** Change type of vector elements */
  def asVectorOf[B <: ToDouble : FromDouble]: DenseVector[B] = {
    SimpleDenseVector[B](raw)
  }

  /** Element wise addition */
  def +[B >: A <: ToDouble](that: DenseVector[B]): DenseVector[B] = {
    val sum = new Array[Double](this.length)
    var idx = 0
    while (idx < sum.length) {
      sum(idx) = this.raw(idx) + that.raw(idx)
      idx += 1
    }
    that.build(sum)
  }

  /** Element wise subtraction */
  def -[B >: A <: ToDouble](that: DenseVector[B]): DenseVector[B] = {
    val difference = new Array[Double](this.length)
    var idx = 0
    while (idx < difference.length) {
      difference(idx) = this.raw(idx) - that.raw(idx)
      idx += 1
    }
    that.build(difference)
  }

  /** Add a constant value */
  override def +(offset: Double): DenseVector[A] = {
    val sum = new Array[Double](this.length)
    var idx = 0
    while (idx < sum.length) {
      sum(idx) = this.raw(idx) + offset
      idx += 1
    }
    build(sum)
  }

  /** Subtract a constant value */
  override def -(offset: Double): DenseVector[A] = this + (-offset)

  /** Negative of a vector */
  override def unary_- : DenseVector[A] = {
    val negative = new Array[Double](this.length)
    var idx = 0
    while (idx < negative.length) {
      negative(idx) = -this.raw(idx)
      idx += 1
    }
    build(negative)
  }

  /** Multiplication by scalar */
  override def *(scalar: Double): DenseVector[A] = {
    val product = new Array[Double](this.length)
    var idx = 0
    while (idx < product.length) {
      product(idx) = this.raw(idx) * scalar
      idx += 1
    }
    build(product)
  }

  /** Division by scalar */
  override def /(scalar: Double): DenseVector[A] = {
    val division = new Array[Double](this.length)
    var idx = 0
    while (idx < division.length) {
      division(idx) = this.raw(idx) / scalar
      idx += 1
    }
    build(division)
  }

  /** Multiplication by scalar */
  override def *[B <: ToDouble](scalar: B): DenseVector[A] = this * scalar.x

  /** Division by scalar */
  override def /[B <: ToDouble](scalar: B): DenseVector[A] = this / scalar.x

  /** Inner product of two vectors */
  override def inner[B <: ToDouble](that: DenseVector[B]): Double = {
    var acc = 0.0
    var idx = 0
    while (idx < this.length) {
      acc += this.raw(idx) * that.raw(idx)
      idx += 1
    }
    acc
  }

  /** Dot product of two vectors */
  override def dot[B <: ToDouble](that: DenseVector[B]): Double = this.inner(that)

  /** L2 norm squared */
  override def norm2: Double = {
    var acc = 0.0
    var idx = 0
    while (idx < this.length) {
      acc += this.raw(idx) * this.raw(idx)
      idx += 1
    }
    acc
  }

  /** Conversion to a column matrix */
  override def toMatrix: RealMatrix = MatrixUtils.createColumnRealMatrix(raw)

  /** Transpose, conversion to a row matrix */
  override def t: RealMatrix = this.toMatrix.transpose()

  /** Outer product of two vectors */
  override def outer[B <: ToDouble](that: DenseVector[B]): RealMatrix = this.toMatrix * that.t

}

object DenseVector {

  implicit def canBuildFrom[A <: ToDouble : FromDouble]: CanBuildFrom[DenseVector[_], A, DenseVector[A]] = {
    new CanBuildFrom[DenseVector[_], A, DenseVector[A]] {
      def apply(from: DenseVector[_]) = SimpleDenseVector.newBuilder[A]

      def apply() = SimpleDenseVector.newBuilder[A]
    }
  }

  /** Create an n-vector of Variables filled with a constant value */
  def vector[A <: ToDouble : FromDouble](n: Int, value: Double): DenseVector[A] = {
    SimpleDenseVector(Array.fill(n)(value))
  }

  /** Create a n-vector of Variables filled with zeros */
  def zeros[A <: ToDouble : FromDouble](n: Int): DenseVector[A] = vector(n, 0.0)

  /** Create an n-vector of Variables filled with ones */
  def ones[A <: ToDouble : FromDouble](n: Int): DenseVector[A] = vector(n, 1.0)

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
    f: DenseVector[A] => Double,
    x: DenseVector[A],
    eps: Double = 1.0e-8): DenseVector[A] = {
    val grad = new Array[Double](x.length)
    val fx = f(x)
    var idx: Int = 0
    while (idx < x.length) {
      val xPlusDx = x.updated(idx, x.raw(idx) + eps)
      grad(idx) = (f(xPlusDx) - fx) / eps
      idx += 1
    }
    x.build(grad)
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
    left: DenseVector[A],
    right: DenseVector[A]): DenseVector[A] = {
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
    left: DenseVector[A],
    right: DenseVector[A]): DenseVector[A] = {
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
    x: DenseVector[A]): DenseVector[A] = {
    val xPivoted = new Array[Double](x.length)
    for (j <- 0 until x.length) xPivoted(ipvt(j)) = x.raw(j)
    x.build(xPivoted)
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
    x: DenseVector[A]): DenseVector[A] = {
    val xUnpivoted = new Array[Double](x.length)
    for (j <- 0 until x.length) xUnpivoted(j) = x.raw(ipvt(j))
    x.build(xUnpivoted)
  }

}
