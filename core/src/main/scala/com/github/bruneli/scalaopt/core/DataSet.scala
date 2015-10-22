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

package com.github.bruneli.scalaopt.core

import scala.reflect.ClassTag

/**
 * An ordered list of elements that is used in the context of this library
 * to store observed values or intermediate results.
 *
 * @author bruneli
 */
trait DataSet[A] {

  /**
   * Returns a data set containing the elements from the left hand operand followed by the elements from the right
   * hand operand.
   *
   * @param that an other data set
   * @return the union of the two data sets
   */
  def ++(that: DataSet[A]): DataSet[A]

  /**
   * Aggregates the results of applying an operator to subsequent elements.
   *
   * @param z      the initial value for the accumulated result of the
   *               partition, i.e. the neutral element
   * @param seqop  an operator used to accumulate results within a partition
   * @param combop an associative operator used to combine results from
   *               different partitions
   * @tparam B     the type of accumulated results
   * @return the accumulated result
   */
  def aggregate[B: ClassTag](z: => B)(seqop: (B, A) => B, combop: (B, B) => B): B

  def collect(): Seq[A]
  
  def filter(p: A => Boolean): DataSet[A]
  
  /**
   * Applies a function f to all elements of the data set.
   * 
   * @param f the function that is applied for its side-effect to every element
   */
  def foreach(f: (A) => Unit): Unit

  def head: A

  /**
   * Builds a new data set by applying a function to all elements of this list.
   *
   * @param f the function to apply to each element.
   * @tparam B the element type of the returned collection.
   * @return a new data set resulting from applying the given function f
   *         to each element of this data set and collecting the results.
   */
  def map[B: ClassTag](f: (A) => B): DataSet[B]

  def maxBy(f: A => Double) = reduce {
    case (el1, el2) => if (f(el2) > f(el1)) el2 else el1
  }

  def minBy(f: A => Double) = reduce {
    case (el1, el2) => if (f(el2) < f(el1)) el2 else el1
  }

  def reduce(op: (A, A) => A): A

  def size: Long

  /**
   * Zips this sequence with its indices.
   *
   * @return A new sequence containing pairs consisting of all elements of
   *         this sequence paired with their index.
   */
  def zipWithIndex: DataSet[(A, Long)]

}