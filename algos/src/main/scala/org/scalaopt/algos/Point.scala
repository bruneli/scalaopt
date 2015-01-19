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

/**
 * Define a point with coordinates and an associated derivable function.
 *
 * @param x  coordinates of the point
 * @param f  real-valued function acting on coordinates
 * @param df gradient of f
 *
 * @author bruneli
 */
case class Point(x: Coordinates, f: ObjectiveFunction, df: Coordinates => Coordinates) {

  /** real-valued function f evaluated at x */
  lazy val fx = f(x)

  /** gradient function df evaluated at x */
  lazy val dfx = df(x)

}
