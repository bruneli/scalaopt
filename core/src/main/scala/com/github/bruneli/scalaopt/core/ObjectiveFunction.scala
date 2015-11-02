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

/**
 * Define an objective function as a real-valued function acting on a vector of real variables
 *
 * @author bruneli
 */
trait ObjectiveFunction {

  /**
   * Constrained or unconstrained objective function
   */
  val isConstrained = false

  /**
   * Evaluate the objective function for a given vector of variables
   *
   * @param x vector of variables
   * @return real-valued objective function at x
   */
  def apply(x: Variables): Double

  /**
   * Gradient of f evaluated in x
   *
   * By default, the gradient is estimated with finite differences.
   *
   * @param x vector of variables
   * @return gradient of f in x
   */
  def gradient(x: Variables): Variables = ???

  /**
   * Evaluate the directional derivative of f in x
   *
   * By default, the derivative is estimated with finite differences.
   *
   * @param x vector of variables
   * @param d directional vector
   * @return directional derivative of f along d in x
   */
  def dirder(x: Variables, d: Variables): Double = ???

  /**
   * Evaluate the vector product of the Hessian evaluated at x and a direction d
   *
   * @param x vector of variables
   * @param d directional vector
   * @return product of the Hessian in x times d
   */
  def dirHessian(x: Variables, d: Variables): Variables = ???

  def subjectTo(constraints: Constraint*): ConstrainedObjectiveFunction = ???

}

class SimpleFunctionWithGradient(
  f: (Variables => Double, Variables => Variables),
  implicit val pars: ConfigPars = new ConfigPars()) extends ObjectiveFunction {

  def apply(x: Variables) = f._1(x)

  override def gradient(x: Variables) = f._2(x)

  override def dirder(x: Variables, d: Variables): Double = {
    gradient(x) dot d
  }

  override def dirHessian(x: Variables, d: Variables): Variables = {
    val gradx = gradient(x)
    val gradxd = gradient(x + d * pars.eps)
    (gradxd - gradx) / pars.eps
  }

  override def subjectTo(constraints: Constraint*) = {
    new ConstrainedFunctionWithGradient(f, constraints.toVector, pars)
  }

}

class SimpleFunctionFiniteDiffGradient(
  f: Variables => Double,
  implicit val pars: ConfigPars = new ConfigPars()) extends ObjectiveFunction {

  def apply(x: Variables) = f(x)

  override def gradient(x: Variables): Variables = {
    val fx: Double = this(x)
    for (i <- x.indices) yield (this(x.updated(i, x(i) + pars.eps)) - fx) / pars.eps
  }

  override def dirder(x: Variables, d: Variables): Double = {
    (this(x + d * pars.eps) - this(x)) / pars.eps
  }

  override def dirHessian(x: Variables, d: Variables): Variables = {
    val gradx = gradient(x)
    val gradxd = gradient(x + d * pars.eps)
    (gradxd - gradx) / pars.eps
  }

  override def subjectTo(constraints: Constraint*) = {
    new ConstrainedFunctionFiniteDiffGradient(f, constraints.toVector, pars)
  }

}

/**
 * An objective function dedicated to regression techniques
 */
trait ObjectiveFunctionForRegression extends ObjectiveFunction {

  /** Set of data points */
  val data: DataSet[DataPoint]

  /**
   * Regression function describing the relationship between dependent variables Y and X
   *
   * @param p vector of unknown parameters
   * @param x vector of observed values corresponding to variables X
   * @return estimates of Y
   */
  def apply(p: Variables, x: Variables): Variables

  /**
   * By default, cumulative loss over a set of data points
   *
   * @param p unknown parameters associated to the regression function
   * @return cumulative sum of loss over data points
   */
  def apply(p: Variables) = {
    def lossSum(sum: Double, dataPoint: DataPoint) = sum + loss(p, dataPoint)
    data.aggregate(0.0)(lossSum, _ + _)
  }

  /**
   * Evaluate the loss associated to a data point
   *
   * @param p unknown parameters associated to the regression function
   * @param data observed data point
   * @return loss
   */
  def loss(p: Variables, data: DataPoint): Double

}