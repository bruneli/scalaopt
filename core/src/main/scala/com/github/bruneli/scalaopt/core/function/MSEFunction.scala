package com.github.bruneli.scalaopt.core.function

import com.github.bruneli.scalaopt.core._
import com.github.bruneli.scalaopt.core.linalg.{AugmentedRow, DenseVector}
import com.github.bruneli.scalaopt.core.variable.{DataPoint, Input, Output, UnconstrainedVariable}

/**
 * Objective function relying on the Mean Squared Error expected loss function
 *
 * @author bruneli
 */
trait MSEFunction extends DifferentiableObjectiveFunction[UnconstrainedVariable] {

  /** Set of data points used to regress a set of unknown parameters */
  val data: DataSet[DataPoint]

  /**
   * Regression function describing the relationship between dependent variables Y and X
   * and characterized by p (unknown) parameters.
   *
   * @param p vector of unknown parameters
   * @param x vector of observed values corresponding to variables X
   * @return estimates of Y
   */
  def apply(p: UnconstrainedVariablesType, x: InputsType): OutputsType

  /**
   * By default, cumulative loss over a set of data points
   *
   * @param p unknown parameters associated to the regression function
   * @return cumulative sum of loss over data points
   */
  def apply(p: UnconstrainedVariablesType) = {
    def lossSum(sum: Double, xy: DataPoint) = sum + loss(p, xy)
    data.aggregate(0.0)(lossSum, _ + _) / data.size
  }

  /**
   * Evaluate the loss associated to a data point as (Y - f(p, X))**2 / 2
   *
   * @param p  vector of unknown parameters
   * @param xy an observed data point in the form (X, Y)
   * @return loss
   */
  def loss(p: UnconstrainedVariablesType, xy: DataPoint): Double = {
    val res = residual(p, xy)
    res * res / 2
  }

  /**
   * Compute the residual associated to a data point
   *
   * @param p  vector of unknown parameters
   * @param xy an observed data point in the form (X, Y)
   * @return the absolute difference between the observed and the estimated values of Y
   */
  def residual(p: UnconstrainedVariablesType, xy: DataPoint): Double

  /**
   * Simultaneously evaluate the Jacobian row and the residual associated to a data point
   *
   * @param p vector of unknown parameters
   * @param xy data point
   * @return tuple of a jacobian matrix row and the residual
   */
  def jacobianAndResidual(p: UnconstrainedVariablesType, xy: DataPoint): (InputsType, Output)

  /**
   * Evaluate the Jacobian & residuals of f in x0 for each data point
   *
   * @param p vector of unknown parameters wrt which derivatives are computed
   * @return the jacobian matrix & vector of residuals of f in p
   */
  def jacobianAndResidualsMatrix(p: UnconstrainedVariablesType): DataSet[AugmentedRow]

}

/**
 * Simple MSE function taking as input a regression function
 *
 * @param f    regression function taking p unknown parameters and x observed values
 *             as input to return y dependent estimates
 * @param data set of data points
 */
case class SimpleMSEFunction(
  f: RegressionFunction,
  data: DataSet[DataPoint],
  eps: Double = 1.0e-8) extends MSEFunction {

  /**
   * Evaluate the regression function
   *
   * @param p vector of unknown parameters
   * @param x vector of observed values X
   * @return estimates of Y
   */
  def apply(p: UnconstrainedVariablesType, x: InputsType): OutputsType = f(p, x)

  /**
   * Compute the residual associated to a data point
   *
   * @param x    unknown parameters
   * @param data a data point in the form (X, Y)
   * @return the absolute difference between observed and estimated values of Y
   */
  def residual(x: UnconstrainedVariablesType, data: DataPoint) = (data.y - f(x, data.x)).norm

  /**
   * Simultaneously evaluate the Jacobian row and the residual associated to a data point
   *
   * @param x0 vector of unknown variables
   * @param point data point
   * @return tuple of jacobian and residual
   */
  def jacobianAndResidual(
    x0: UnconstrainedVariablesType,
    point: DataPoint): (InputsType, Output) = {
    val f = (x: UnconstrainedVariablesType) => residual(x, point)
    val jacobian: InputsType = DenseVector.gradient(f, x0).asVectorOf[Input]
    val residual0 = residual(x0, point)
    (jacobian, residual0)
  }

  /**
   * Evaluate the Jacobian & residuals of f in x0 for each data point
   *
   * @param p vector of unknown parameters wrt which derivatives are computed
   * @return the jacobian matrix & vector of residuals of f in p
   */
  def jacobianAndResidualsMatrix(p: UnconstrainedVariablesType): DataSet[AugmentedRow] = {
    data.zipWithIndex map {
      case (xy, i) => AugmentedRow(jacobianAndResidual(p, xy), i)
    }
  }

  override def gradient(x: UnconstrainedVariablesType): UnconstrainedVariablesType = {
    DenseVector.gradient(this.apply, x)
  }

  override def dirder(x: UnconstrainedVariablesType, d: UnconstrainedVariablesType): Double = {
    (this(x + (d * eps)) - this(x)) / eps
  }

  override def dirHessian(x: UnconstrainedVariablesType, d: UnconstrainedVariablesType): UnconstrainedVariablesType = ???

}