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

package com.github.bruneli.scalaopt.sparkapps

import com.github.bruneli.scalaopt.core.{DataSet, SeqDataSetConverter}
import org.apache.spark.rdd.RDD
import SeqDataSetConverter.SeqDataSet

import scala.reflect.ClassTag

/**
 * Implicit class used to run numerical optimization methods on data sets stored in a spark RDD
 *
 * @author bruneli
 */
object SparkDataSetConverter {

  implicit class SparkDataSet[A : ClassTag](val rdd: RDD[A]) extends DataSet[A] {

    override def ++(that: DataSet[A]) = {
      val thatRDD = that match {
        case other: SparkDataSet[A] => other.rdd
        case _ => rdd.sparkContext.parallelize(that.collect())
      }
      new SparkDataSet(thatRDD.union(this.rdd))
    }

    override def aggregate[B: ClassTag](z: => B)(
      seqop: (B, A) => B, combop: (B, B) => B): B = {
      rdd.aggregate(z)(seqop, combop)
    }

    override def collect(): Seq[A] = rdd.collect()

    override def filter(p: A => Boolean): DataSet[A] = rdd filter p

    override def flatMap(f: (A) => TraversableOnce[A]): DataSet[A] = rdd flatMap f

    override def foreach(f: (A) => Unit): Unit = rdd foreach f

    override def head: A = rdd first()

    override def map[B: ClassTag](f: (A) => B): DataSet[B] = rdd map f

    override def size = rdd.count()

    def zip[B](that: DataSet[B]): DataSet[(A, B)] = that match {
      case other: SparkDataSet[B] => this.rdd.zip(other.rdd)
      case other: SeqDataSet[B] =>
        require(other.size >= rdd.size, "that size should be >= this size")
        this.zipWithIndex.map { case (left, index) => (left, other.collect()(index.toInt)) }
    }

    override def zipWithIndex: DataSet[(A, Long)] = rdd.mapPartitionsWithIndex(setIndicesInFirstPartition[A])

    private def setIndicesInFirstPartition[B](index: Int, iterator: Iterator[B]): Iterator[(B, Long)] =
      if (index == 0) {
        iterator.zipWithIndex.map {
          case (b, i) => (b, i.toLong)
        }
      } else {
        iterator.map { b: B => (b, Int.MaxValue.toLong) }
      }

    override def reduce(op: (A, A) => A): A = rdd reduce op

  }

}
