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

package org.scalaopt.algos

import org.scalaopt.algos.linalg.{AugmentedRow, QR}

import scala.util.Try

/**
 * @author bruneli
 */
package object linear {

  def lm(data: DataSet[DataPoint], addOrigin: Boolean = true): Try[Variables] =
    Try {
      val n = if (addOrigin) data.head.x.size + 1 else data.head.x.size
      val ab = data.zipWithIndex.map {
        case (row, index) => {
          val a = if (addOrigin) 1.0 +: row.x else row.x
          AugmentedRow(a, row.y(0), index)
        }
      }
      QR(ab, n).solution
    }

}
