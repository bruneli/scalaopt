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

package org.scalaopt.algos.linalg

import scala.reflect.ClassTag

object SeqDataSetConverter {

  /**
   * A data set built from a scala sequence.
   *
   * @author bruneli
   */
  implicit class SeqDataSet[+A](v: Seq[A]) extends DataSet[A] {

    override def aggregate[B: ClassTag](z: => B)(
      seqop: (B, A) => B, combop: (B, B) => B): B = {
      v.aggregate(z)(seqop, combop)
    }
    
    override def collect(): Seq[A] = v
    
    override def filter(p: A => Boolean): DataSet[A] = v filter p

    override def foreach(f: (A) => Unit): Unit = v foreach f

    override def head: A = v head
    
    override def map[B: ClassTag](f: (A) => B): DataSet[B] = v map f

    override def zipWithIndex: DataSet[(A, Long)] = v.zipWithIndex.map {
      case (a, i) => (a, i.toLong)
    }

  }

}