package org.scalaopt.sparkapps

import org.apache.spark.SparkContext
import org.scalaopt.algos.Coordinates

/**
 * @author bruneli
 */
object LinearModel {

  def nllr(mu: Double)(n: Int) = mu - n*Math.log(mu)

  def main(args: Array[String]) {
    val sc = new SparkContext()
    val rdd = sc.parallelize(1 to 10)
    println(s"Solving a linear model via QR decomposition.")
  }
}
