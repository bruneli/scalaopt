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

import com.github.bruneli.scalaopt.core.DataSet

/**
 * Define a tableau used with the Standard Simplex algorithm
 *
 * The linear programming problem:
 *
 * min cx
 * subject to
 * ax <= b,
 * x >= 0
 *
 * is rewritten in a matrix form:
 *
 * x1   x2   x2-   s1   s2   a1   a2   z
 * --------------------------------------
 * 0    0    0     0    0    -1   -1   w
 * -c1  -c2  c2    0    0    0    0    z
 * a11  a12  -a12  1    0    1    0    b1
 * a21  a22  -a22  0    1    0    1    b2
 *
 * with
 * - row1 = phase1 objective function
 * - row2 = phase2 objective function cx
 * - rows 3 & 4 = (inequality) constrains ax <= b
 * - x1 & x2 = decision variables
 * - x2- = additional variable in case x2 is not restricted to be >= 0 when defined
 * - s1 & s2 = additional slack variables to transform inequality constrains into equality constrains
 * - a1 & a2 = additional artificial variables to solve the phase1 problem
 *
 * @author bruneli
 */
case class SimplexTableau(columns: DataSet[TableauColumn], rhs: NonBasicVariable)

trait TableauColumn

case class BasicVariable(row: Int)

case class NonBasicVariable(phase1Cost: Double, phase2Cost: Double, constrains: Vector[Double])