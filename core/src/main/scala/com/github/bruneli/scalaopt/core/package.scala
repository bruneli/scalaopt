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

package com.github.bruneli.scalaopt

import org.apache.commons.math3.linear.RealMatrix

/**
 * Numerical Optimization Algorithms written in Scala.
 *
 * All optimization algorithms contain a static method named minimize.
 * minimize takes as input a real objective function acting on variables.
 * Variables are represented by any scala collection of Double deriving from
 * Seq like commonly used Array, List, Vector,...
 * The real valued objective function can be any method taking as input a 
 * scala collection of Double and returning a Double.
 * The minimize function returns as output a Try[Variables] rather than
 * directly Variables to explicit the fact that all optimization algorithms
 * can fail to find a feasible solution.
 *
 * @author bruneli
 */
package object core {

  /** Define the vector of variables as a sequence of Double values */
  type Variables = Seq[Double]

  /** Function taking as input p unknown parameters and x observed values to give estimates of y dependent variables */
  type RegressionFunction = (Variables, Variables) => Variables

  /** Implicit conversion of a (function, gradient) tuple to an objective function */
  implicit def toFunctionWithGradient(f: (Variables => Double, Variables => Variables)): ObjectiveFunction = {
    new SimpleFunctionWithGradient(f)
  }

  /** Implicit conversion of a function to an objective function with finite differences derivatives */
  implicit def toFunctionWoGradient(f: Variables => Double): ObjectiveFunction = new SimpleFunctionFiniteDiffGradient(f)

  /** Implicit conversion of a regression function with set of data points to an MSE objective function */
  implicit def toMSEFunctionWoGradient(funcAndData: (RegressionFunction, DataSet[DataPoint])): MSEFunction = {
    new SimpleMSEFunction(funcAndData._1, funcAndData._2)
  }

  /** Implicit conversion of Seq[Double] to RichVariables */
  implicit def toRichVariables(v: Variables): RichVariables = new RichVariables(v)

  /** Implicit conversion of RealMatrix to RichMatrix */
  implicit def toRichMatrix(m: RealMatrix): RichMatrix = new RichMatrix(m)

  /** Implicit conversion of a real-valued function to the left hand side of a constraint */
  implicit def toLHConstraint(c: Variables => Double): LHConstraint = new LHConstraint(c)

  /** Implicit conversion of a tuple (x, y) to a DataPoint */
  implicit def toDataPointYVector(xy: (Variables, Variables)): DataPoint = DataPoint(xy._1, xy._2)

  implicit def toDataPointYScalar(xy: (Variables, Double)): DataPoint = DataPoint(xy._1, Vector(xy._2))

  def min(f: (Variables) => Double)(implicit pars: ConfigPars = new ConfigPars()): ObjectiveFunction = {
    new SimpleFunctionFiniteDiffGradient(f, pars)
  }

  def min(f: (Variables) => Double, df: (Variables) => Variables)(
    implicit pars: ConfigPars = new ConfigPars()): ObjectiveFunction = {
    new SimpleFunctionWithGradient((f, df), pars)
  }

  /** Create an n-vector of Variables filled with a constant value */
  def vector(n: Int, value: Double): Variables = (1 to n).map(i => value)

  /** Create a n-vector of Variables filled with zeros */
  def zeros(n: Int): Variables = vector(n, 0.0)

  /** Create an n-vector of Variables filled with ones */
  def ones(n: Int): Variables = vector(n, 1.0)

  /** Create an n-dimensional basis vector with i-th element set to 1 */
  def e(n: Int, i: Int): Variables = zeros(n).updated(i, 1.0)

}