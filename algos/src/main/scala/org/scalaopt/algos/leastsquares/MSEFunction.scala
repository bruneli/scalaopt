package org.scalaopt.algos.leastsquares

import org.scalaopt.algos._
import org.scalaopt.algos.linalg.AugmentedRow

/**
 * Objective function relying on the Mean Squared Error expected loss function
 *
 * @param f    regression function taking p unknown parameters and x observed values
 *             as input to return y dependent estimates
 * @param data set of data points
 *
 * @author bruneli
 */
case class MSEFunction(
  f: (Variables, Variables) => Variables,
  data: DataSet[DataPoint],
  val eps: Double = 1.0e-8) extends RegressionFunction {

  /**
   * Function to regress
   *
   * @param x vector of unknown variables
   * @param xObs vector of observed values
   * @return estimates of Y
   */
  def apply(x: Variables, xObs: Variables): Variables = f(x, xObs)

  /**
   * Sum of squared residuals for a given data point
   *
   * @param x vector of unknown variables
   * @param data observed data point
   * @return loss
   */
  def loss(x: Variables, data: DataPoint) = (data.y - f(x, data.x)) norm2

  /**
   * Compute the residual associated to a data point
   *
   * @param x    unknown parameters
   * @param data a data point in the form (X, Y)
   * @return the absolute difference between observed and estimated values of Y
   */
  def residual(x: Variables, data: DataPoint) = (data.y - f(x, data.x)) norm

  /**
   * Simultaneously evaluate the Jacobian row and the residual associated to a data point
   *
   * @param x0 vector of unknown variables
   * @param xy data point
   * @return tuple of jacobian and residual
   */
  def jacobianAndResidual(
    x0: Variables,
    xy: DataPoint): (Variables, Double) = {
    val residual0 = residual(x0, xy)
    val jacobian =
      for (j <- 0 until x0.length) yield {
        // Shift coordinate of jth-column by eps
        val x = x0.updated(j, x0(j) + eps)
        (residual(x, xy) - residual0) / eps
      }
    (jacobian, residual0)
  }

  /**
   * Evaluate the Jacobian & residuals of f in x0 for each data point
   *
   * @param x0   vector of unknown variables wrt which derivatives are computed
   * @return the jacobian & residual of f in p0
   */
  def jacobianAndResidualsMatrix(x0: Variables): DataSet[AugmentedRow] = {
    data.zipWithIndex map {
      case (xy, i) => AugmentedRow(jacobianAndResidual(x0, xy), i)
    }
  }
}

object MSEFunction {

  implicit def apply(fAndData: ((Variables, Variables) => Variables, IndexedSeq[DataPoint])): MSEFunction =
    MSEFunction(fAndData._1, SeqDataSetConverter.SeqDataSet(fAndData._2))

  //implicit def apply(fAndData: ((Variables, Variables) => Variables, DataSet[DataPoint])): MSEFunction =
  //  MSEFunction(fAndData._1, fAndData._2)

}