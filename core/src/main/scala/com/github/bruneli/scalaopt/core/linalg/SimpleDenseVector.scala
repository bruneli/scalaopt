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

import com.github.bruneli.scalaopt.core.linalg.FromToDoubleConversions.{FromDouble, ToDouble}

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

/**
 * Dense vector of elements that can be represented uniquely by a double value
 *
 * @author bruneli
 */
case class SimpleDenseVector[A <: ToDouble](coordinates: Array[Double])(
  implicit fromDouble: FromDouble[A]) extends DenseVector[A] {

  override type V = SimpleDenseVector[A]

  override def apply(idx: Int): A = coordinates(idx)

  override protected[this] def newBuilder: mutable.Builder[A, SimpleDenseVector[A]] = {
    SimpleDenseVector.newBuilder
  }

  override def newDenseVectorBuilder: DenseVectorBuilder[V] = {
    new SimpleDenseVectorBuilder[A]
  }

}

object SimpleDenseVector {

  def apply[A <: ToDouble : FromDouble](points: A*): SimpleDenseVector[A] = {
    new SimpleDenseVector[A](points.map(_.x).toArray)
  }

  def newBuilder[A <: ToDouble : FromDouble]: mutable.Builder[A, SimpleDenseVector[A]] = {
    Vector.newBuilder[A].mapResult(vector => new SimpleDenseVector[A](vector.map(_.x).toArray))
  }

  implicit def canBuildFrom[A <: ToDouble : FromDouble]: CanBuildFrom[SimpleDenseVector[_], A, SimpleDenseVector[A]] = {
    new CanBuildFrom[SimpleDenseVector[_], A, SimpleDenseVector[A]] {
      def apply(from: SimpleDenseVector[_]) = newBuilder[A]

      def apply() = newBuilder[A]
    }
  }
  
}

class SimpleDenseVectorBuilder[A <: ToDouble](implicit fromDouble: FromDouble[A])
  extends DenseVectorBuilder[SimpleDenseVector[A]] {

  override def withValues(values: Array[Double]): SimpleDenseVector[A] = {
    new SimpleDenseVector[A](values)(fromDouble)
  }

}