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
case class SimpleDenseVector[+A <: ToDouble](
  raw: Array[Double])(
  implicit fromDouble: FromDouble[A]) extends DenseVector[A] {

  override def apply(idx: Int): A = raw(idx)

  override protected[this] def newBuilder: mutable.Builder[A, SimpleDenseVector[A]] = {
    SimpleDenseVector.newBuilder
  }

  override protected def build(updated: Array[Double]): DenseVector[A] = {
    SimpleDenseVector(updated)
  }

}

object SimpleDenseVector {

  def apply[A <: ToDouble : FromDouble](points: A*): SimpleDenseVector[A] = {
    new SimpleDenseVector[A](points.map(_.x).toArray)
  }

  def newBuilder[A <: ToDouble : FromDouble]: scala.collection.mutable.Builder[A, SimpleDenseVector[A]] = {
    Vector.newBuilder[A].mapResult(vector => new SimpleDenseVector[A](vector.map(_.x).toArray))
  }

  implicit def canBuildFrom[A <: ToDouble : FromDouble]: CanBuildFrom[SimpleDenseVector[_], A, SimpleDenseVector[A]] = {
    new CanBuildFrom[SimpleDenseVector[_], A, SimpleDenseVector[A]] {
      def apply(from: SimpleDenseVector[_]) = newBuilder[A]

      def apply() = newBuilder[A]
    }
  }
  
}
