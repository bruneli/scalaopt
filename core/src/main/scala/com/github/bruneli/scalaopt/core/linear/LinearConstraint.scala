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

package com.github.bruneli.scalaopt.core.linear

import com.github.bruneli.scalaopt.core._

/**
 * Define a linear constraint of the type Ax = b, Ax <= b
 *
 * @author bruneli
 */
case class LinearConstraint(a: DataSet[Double], b: Double) {

  def apply(x: Variables): Boolean = true

}

object LinearConstraint {

  import SeqDataSetConverter._

  def apply(f: Variables => Boolean, n: Int): LinearConstraint = {
    val b = f(zeros(n))
    val a = for (i <- 0 until n) yield {
      f(zeros(n).updated(i, 1.0))
    }
    new LinearConstraint(a, b)
  }

}
