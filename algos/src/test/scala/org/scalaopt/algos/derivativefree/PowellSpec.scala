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

package org.scalaopt.algos.derivativefree

import org.scalaopt.algos._
import scala.util.{Try, Success, Failure}
import org.scalatest.FunSuite

class PowellSuite extends FunSuite {
  import Powell._
  
  val x0 = Vector(0.5, 2.0)
  def fQuad(x: Seq[Double]): Double = 
    (x - x0) dot (x - x0)

  val config = new PowellConfig(tol = 1.0e-6)
    
  test("Find minimum of fQuad") {
    val d = minimize(fQuad, Vector(0.0, 0.0)) match {
      case Success(xmin) => xmin - x0
      case Failure(e) => x0
    }
    assert((d dot d) < config.tol * config.tol)
  }
  
  test("Throw error if reaching max number of iterations") {
    intercept[MaxIterException] {
      minimize(x => x(0) + x(1), Vector(0.0, 0.0))
    }
  }
  
  test("Throw error if wrong configuration type") {
    import NelderMead.NelderMeadConfig
    val wrongc = new NelderMeadConfig
    intercept[IllegalArgumentException] {
      minimize(x => x(0) + x(1), Vector(0.0, 0.0))(wrongc)
    }
  }
}