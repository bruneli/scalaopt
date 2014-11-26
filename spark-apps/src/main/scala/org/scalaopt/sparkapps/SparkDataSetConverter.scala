package org.scalaopt.sparkapps

import org.apache.spark.rdd.RDD
import org.scalaopt.algos.leastsquares.DataSet

import scala.reflect.ClassTag

/**
 * @author bruneli
 */
object SparkDataSetConverter {

  implicit class SparkDataSet[+A](rdd: RDD[A]) extends DataSet[A] {

    override def aggregate[B: ClassTag](z: => B)(
      seqop: (B, A) => B, combop: (B, B) => B): B = {
      rdd.aggregate(z)(seqop, combop)
    }

    override def collect(): Seq[A] = rdd.collect()

    override def filter(p: A => Boolean): DataSet[A] = rdd filter p

    override def foreach(f: (A) => Unit): Unit = rdd foreach f

    override def map[B: ClassTag](f: (A) => B): DataSet[B] = rdd map f

    override def zipWithIndex: DataSet[(A, Int)] = null

  }

}
