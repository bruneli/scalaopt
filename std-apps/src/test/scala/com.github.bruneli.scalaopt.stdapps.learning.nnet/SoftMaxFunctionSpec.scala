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

package org.scalaopt.stdapps.learning.nnet.activation

import com.github.bruneli.scalaopt.stdapps.learning.nnet.activation.SoftMaxFunction
import org.scalatest.{Matchers, FlatSpec}

/**
 * @author bruneli
 */
class SoftMaxFunctionSpec extends FlatSpec with Matchers {

  "soft max function" should "give probabilities" in {
    val inputs = List(2.0, 3.0, 1.5, 4.5)
    inputs
      .map(input => (input, SoftMaxFunction.apply(input, inputs.max)))
      .foreach {
      case (input, output) => output shouldBe Math.exp(input - inputs.max) +- 0.001
    }
  }

}
