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

import com.github.bruneli.scalaopt.core.linalg.{AugmentedRow, QR}
import com.github.bruneli.scalaopt.core.variable.DataPoint

import scala.util.Try

/**
 * Linear model package.
 *
 * The only method of that package is lm that can be used to solve a linear equation.
 * {{{
 * scala> import scala.util.Random
 * scala> import com.github.bruneli.scalaopt.core._
 * scala> import linear.lm
 * scala> import SeqDataSetConverter._
 * scala> val random = new Random(12345)
 * scala> val beta0 = 80.0 +: (0 until 10).map(_.toDouble)
 * scala> val data = for (i <- 0 until 1000) yield {
 * scala>   val x = for (i <- 0 until 10) yield random.nextDouble()
 * scala>   val y = beta0.head + x.zip(beta0.tail).map { case (x, p) => x*p }.sum + random.nextGaussian()
 * scala>   DataPoint(x, y)
 * scala> }
 * scala> lm(data) // Should be close to beta0
 * }}}
 *
 * @author bruneli
 */
package object linear {

  /**
   * Try to solve a linear equation (in least-square sense) X Beta = Y via QR decomposition
   *
   * @param data      set of rows with X and y values
   * @param addOrigin add an origin to the linear formula
   * @return a successful solution Beta of the linear equation or failure
   */
  def lm(data: DataSet[DataPoint], addOrigin: Boolean = true): Try[UnconstrainedVariablesType] =
    Try {
      val n = if (addOrigin) data.head.x.size + 1 else data.head.x.size
      val ab = data.zipWithIndex.map {
        case (row, index) =>
          val a: InputsType = row.x
          // TODO if (addOrigin) Input(1.0) +: row.x else row.x
          AugmentedRow(a, row.y.head, index)
      }
      QR(ab, n).solution
    }

}
