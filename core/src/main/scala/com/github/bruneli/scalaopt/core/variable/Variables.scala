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

package com.github.bruneli.scalaopt.core.variable

import com.github.bruneli.scalaopt.core.linalg.{DenseVector, FromToDoubleConversions}
import com.github.bruneli.scalaopt.core.linalg.FromToDoubleConversions.{FromDouble, ToDouble}

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

/**
 * Vector of optimization variables that can be of different type
 * 
 * @author bruneli
 */
class Variables[+A <: Variable](variables: Vector[A]) extends DenseVector[A] {

  lazy val raw: Array[Double] = variables.map(_.x).toArray
  
  override def apply(idx: Int): A = variables(idx)

  override protected[this] def newBuilder: mutable.Builder[A, Variables[A]] = {
    Variables.newBuilder
  }

  override def withValues(updated: Array[Double]): DenseVector[A] = {
    new Variables[A](variables.zipWithIndex.map(
      tuple => tuple._1.build(updated(tuple._2)).asInstanceOf[A]))
  }

}

object Variables {

  def apply[A <: Variable](points: A*): Variables[A] = {
    new Variables[A](points.toVector)
  }

  def newBuilder[A <: Variable]: mutable.Builder[A, Variables[A]] = {
    Vector.newBuilder[A].mapResult(vector => new Variables[A](vector))
  }

  implicit def canBuildFrom[A <: Variable]: CanBuildFrom[Variables[_], A, Variables[A]] = {
    new CanBuildFrom[Variables[_], A, Variables[A]] {
      def apply(from: Variables[_]) = newBuilder[A]

      def apply() = newBuilder[A]
    }
  }

}