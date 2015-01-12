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

package org.scalaopt

import org.apache.commons.math3.linear.RealMatrix

/**
 * Numerical Optimization Algorithms written in Scala.
 * 
 * All optimization algorithms contain a static method named minimize.
 * minimize takes as input a real objective function and starting coordinates.
 * Coordinates are represented by any scala collection of Double deriving from
 * Seq like commonly used Array, List, Vector,...
 * The real valued objective function can be any method taking as input a 
 * scala collection of Double and returning a Double.
 * The minimize function returns as output a Try[Coordinates] rather than
 * directly Coordinates to explicit the fact that all optimization algorithms
 * can fail to find a solution.
 * 
 * @author bruneli
 */
package object algos {

  /** Define Coordinates type as a sequence of Double values */
  type Coordinates = Seq[Double]
  
  /** Define an ObjectiveFunction type as a real-valued function */ 
  type ObjectiveFunction = Coordinates => Double

  /** Define an observed data pair as a real-valued vector X associated to an observed value y */
  type Xy = (Seq[Double], Double)
  
  /** Define a real-valued objective function applied to some observed data X */
  type ObjFunWithData = (Coordinates, Seq[Double]) => Double

  /** Implicit conversion of Seq[Double] to RichCoordinates */
  implicit def toRichCoordinates(v: Coordinates) = new RichCoordinates(v)

  /** Implicit conversion of RealMatrix to RichMatrix */
  implicit def toRichMatrix(m: RealMatrix) = new RichMatrix(m)

  /** Create an n-vector of coordinates filled with a constant value */
  def vector(n: Int, value: Double): Coordinates = (1 to n).map(i => value)
  
  /** Create a n-vector of coordinates filled with zeros */
  def zeros(n: Int): Coordinates = vector(n, 0.0)
  
  /** Create an n-vector of coordinates filled with ones */
  def ones(n: Int): Coordinates = vector(n, 1.0)

}