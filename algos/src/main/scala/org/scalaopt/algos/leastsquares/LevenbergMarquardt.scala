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

package org.scalaopt.algos.leastsquares

import org.scalaopt.algos._
import org.scalaopt.algos.linalg.{AugmentedRow, QR}
import scala.util.{Try, Success, Failure}

object LevenbergMarquardt {
  /**
   * Configuration parameters for the BFGS algorithm.
   *
   * @param tol tolerance on the scaled reduction of the norm of f
   * @param maxIter maximum number of iterations
   * @param eps finite differences step to evaluate derivatives
   * @param xTol xxx
   * @param gTol xxx
   * @param ratioTol xxx
   * @param stepBound xxx
   * @param epsmch machine precision
   * @param maxInnerIter xxx
   * @param maxStepLengthIter xxx
   */
  class LevenbergMarquardtConfig(
    override val tol: Double = 1.49012e-8,
    override val maxIter: Int = 10,
    override val eps: Double = 1.0e-8,
    val xTol: Double = 1.49012e-8,
    val gTol: Double = 0.0,
    val ratioTol: Double = 0.0001,
    val stepBound: Double = 100.0,
    val epsmch: Double = 2.22044604926e-16,
    val maxInnerIter: Int = 10,
    val maxStepLengthIter: Int = 10,
    val usePivoting: Boolean = false) extends ConfigPars(tol, maxIter, eps)

  implicit val defaultLevenbergMarquardt: LevenbergMarquardtConfig = new LevenbergMarquardtConfig

  def minimize[C <: ConfigPars](
    f: ObjFunWithData,
    data: DataSet[Xy],
    x0: Coordinates)(
    implicit c: ConfigPars): Try[Coordinates] = {

    // Check configuration parameters
    val pars = ConfigPars.checkConfig[LevenbergMarquardtConfig, C](c)

    val n = x0.length
    
    def iterate(
        outer: Int, 
        x0: Coordinates,
        diag0: Coordinates,
        xNorm0: Double,
        delta0: Double): Coordinates = {
      val qr = QR(jacobianMatrix(x0, f, data, pars.eps), x0.length, pars.usePivoting)
      val fNorm0 = qr.bNorm
      val (diag, xNorm, delta1) =
        updateDiagNormDelta(outer, x0, diag0, xNorm0, delta0, qr, pars.stepBound)
        
      // norm of the scaled gradient
      def scaledGradientNorm(gNorm0: Double, j: Int): Double = {
        val acNormj = qr.acNorms(qr.ipvt(j))
        if (acNormj == 0.0) {
          gNorm0
        } else {
          val sum = (0 until j).foldLeft(0.0) { 
            case (sum, i) => sum + qr.R(i, j) * qr.qtb(i) / fNorm0
          }
          Math.max(gNorm0, Math.abs(sum / acNormj))
        }
      }
      val gNorm = (0 until n).foldLeft(0.0)(scaledGradientNorm)

      val (x, fNorm, delta2, stoppingRule) =
        innerLoop(0, x0, qr, diag, delta1, 0.0, fNorm0)
       if (stoppingRule || outer >= pars.maxIter || gNorm <= pars.gTol || gNorm <= pars.epsmch) {
        x
      } else {
        iterate(outer + 1, x, diag, fNorm, delta2)
      }
    }

    def innerLoop(
      inner: Int,
      x0: Coordinates,
      qr: QR,
      diag: Coordinates,
      delta0: Double,
      par0: Double,
      fNorm0: Double): (Coordinates, Double, Double, Boolean) = {
      val (par, pk, xDiag) = lmPar(qr, diag, delta0, par0, pars.maxStepLengthIter)
      val xDiagNorm = xDiag.norm
      val x = x0 - pk
      val xNorm = x.norm
      def residualSquareSum(rss: Double, xy: Xy) = {
        val r = residual(x, xy, f)
        rss + r * r
      }
      val fNorm = Math.sqrt(data.aggregate(0.0)(residualSquareSum, _ + _))
      
      // Scaled predicted reduction and scaled directional derivative
      def multiply(predicted0: Coordinates, j: Int): Coordinates = {
        if (j == predicted0.length) {
          predicted0
        } else {
          val predicted = predicted0 - qr.R(j) * pk(qr.ipvt(j))
          multiply(predicted, j + 1)
        }
      }
      val predicted = multiply(zeros(diag.length), 0)
      val scaledPredictedNorm2 = predicted.norm2 / (fNorm0 * fNorm0)
      val scaledxDiagNorm2 = par * (xDiagNorm * xDiagNorm) / (fNorm0 * fNorm0)
      val predictedReduction = scaledPredictedNorm2 + 2.0 * scaledxDiagNorm2
      val directionalDerivative = -(scaledPredictedNorm2 + scaledxDiagNorm2)

      // Scaled actual reduction
      val actualReduction = if (fNorm < fNorm0) 1.0 - (fNorm * fNorm) / (fNorm0 * fNorm0) else -1.0

      // Ratio of the scaled actual reduction by the scaled predicted reduction
      val ratio = if (predictedReduction != 0.0) actualReduction / predictedReduction else 0.0

      // Update the Levenberg-Marquardt parameter and the step bound delta
      val (parNew, delta) =
        if (ratio > 0.25) {
          if (par != 0.0 && ratio < 0.75) (par, delta0) else (0.5 * par, 2.0 * xDiagNorm)
        } else {
          val temp1 =
            if (actualReduction >= 0.0) {
              0.5
            } else {
              0.5 * directionalDerivative / (directionalDerivative + 0.5 * actualReduction)
            }
          val temp2 = if (0.1 * fNorm >= fNorm0 || temp1 < 0.1) 0.1 else temp1
          (par / temp2, temp2 * Math.min(delta0, 10.0 * xDiagNorm))
        }

      // Compute the various stopping rules
      // Convergence tests
      val convergence1 =
        Math.abs(actualReduction) <= pars.tol &&
        predictedReduction <= pars.tol &&
        0.5 * ratio <= 1.0
      val convergence2 = delta <= pars.xTol * xNorm
      // Test for termination and stringent tolerances
      val termination1 =
        Math.abs(actualReduction) <= pars.epsmch &&
        predictedReduction <= pars.epsmch &&
        0.5 * ratio <= 1.0
      val termination2 = delta <= pars.epsmch * xNorm

      val stoppingRule = convergence1 || convergence2 || termination1 || termination2

      if (stoppingRule || inner >= pars.maxInnerIter || ratio >= 0.0001) {
        (x, fNorm, delta, stoppingRule)
      } else {
        innerLoop(inner + 1, x, qr, xDiag, delta, parNew, fNorm)
      }
    }

    Success(iterate(0, x0, Vector(), 0.0, 0.0))
  }
  
  /**
   * Compute the residual r = f(p, x) - y
   *
   * @param p  coordinates
   * @param xy vector of double X and expected result y
   * @param f  objective function
   * @return residual
   */
  def residual(
    p: Coordinates,
    xy: Xy,
    f: ObjFunWithData): Double = {
    val (x, y) = xy
    f(p, x) - y
  }

  /**
   * Evaluate the jacobian & residual of f in p0 for each data
   *
   * @param p0   coordinates wrt which derivatives are computed
   * @param f    objective function
   * @param data a list of points (X, y)
   * @return the jacobian & residual of f in p0
   */
  def jacobianMatrix(
    p0: Coordinates,
    f: ObjFunWithData,
    data: DataSet[Xy],
    eps: Double): DataSet[AugmentedRow] = {
    val n = p0.length

    // For each data row:
    //  - compute the residual f(p0, x) - y
    //  - compute the jacobian via finite difference.
    data.zipWithIndex map {
      case (xy, i) => {
        val r0 = residual(p0, xy, f)
        val jacobian =
          for (j <- 0 until p0.length) yield {
            // Shift coordinate of jth-column by eps
            val p = p0.updated(j, p0(j) + eps)
            val r = residual(p, xy, f)
            (r - r0) / eps
          }
        // Keep only track of indices for the first n rows
        val rowIndex = Math.min(i, n)
        AugmentedRow(jacobian, r0, rowIndex)
      }
    }
  }
  
  private def updateDiagNormDelta(
      iteration: Int, 
      x: Coordinates, 
      diag0: Coordinates, 
      xNorm0: Double,
      delta0: Double,
      qr: QR,
      stepBound: Double) =
    if (iteration == 0) {
      val diag = qr.acNorms map (norm => if (norm > 0.0) norm else 1.0)
      val xNorm = Math.sqrt(diag dot x)
      val delta = if (xNorm == 0.0) stepBound else stepBound * xNorm
      (diag, xNorm, delta)
    } else {
      val diag = diag0.zip(qr.acNorms) map { 
        case (d, norm) => Math.max(d, norm) 
      }
      (diag, xNorm0, delta0)
    }
    
  // Determine the Levenberg-Marquardt parameter
  def lmPar(
      qr: QR,
      diag: Coordinates, 
      delta: Double,
      par0: Double,
      maxStepLengthIter: Int): (Double, Coordinates, Coordinates) = {
    val n = diag.length
    val p1 = 0.1
    
    def gaussNewtonDirection(qtb0: Coordinates) = {
      // Take QtB values till first null R diagonal element
      val firstIdxZero = qr.rDiag.indexWhere(_ == 0.0)
      val nsing = if (firstIdxZero == -1) qr.rDiag.length - 1 else firstIdxZero - 1
      val qtb = qtb0.zipWithIndex.map {
        case (value, col) => if (col <= nsing) value else 0.0
      }
      
      def qtbMinusR(x: Coordinates, j: Int): Coordinates =
        if (j == -1 || x(j) == 0.0) {
          x
        } else {
          val alpha = x(j) / qr.rDiag(j)
          val rj = qr.R(j)
          //val xNew = (x - rj * alpha).updated(j, alpha)
          val xNew = x.updated(j, alpha).zipWithIndex.map { case (v, i) => if (i < j) v - rj(i) * alpha else v }
          qtbMinusR(xNew, j - 1)
        }
      
      val xUnpivoted = qtbMinusR(qtb, nsing)
      (qr.permute(xUnpivoted), nsing)
    }
    
    val (x, nsing) = gaussNewtonDirection(qr.qtb)
    val xdi = diag.zip(x) map { case (d, x) => d * x }
    val dxNorm = eNorm(xdi)
    val fp = dxNorm - delta
    
    if (fp <= p1 * delta) {
      (par0, x, xdi)
    } else {
      val parLow =
        if (nsing >= n) {
          val aux0 = for (i <- 0 until n) yield {
            val j = qr.ipvt(i)
            diag(j) * xdi(j) / dxNorm
          }
          def mul(aux: Coordinates, j: Int): Seq[Double] = {
            if (j == n) {
              aux
            } else {
              val (auxLower, auxUpper) = aux.splitAt(j - 1)
              val sum = auxLower inner qr.R(j).take(j - 1)
              mul(auxLower ++ auxUpper.updated(0, (aux(j) - sum) 
                  / qr.rDiag(j)), j + 1)
            }
          }
          val aux = mul(aux0, 0)
          fp / delta / (aux inner aux)
        } else {
          0.0
        }
      val aux = for (i <- 0 until n) yield {
        val sum = qr.R(i).take(i) inner qr.qtb.take(i)
        sum / diag(qr.ipvt(i))
      }
      val parUp = eNorm(aux) / delta

      def stepLength(
          iter: Int, 
          par0: (Double, Double, Double), 
          fp0: Double,
          maxStepLengthIter: Int): (Double, Coordinates, Coordinates) = {
        val aux = diag * Math.sqrt(par0._2)
        val (x, sDiag) = qrSolve(qr, diag)
        val xDiag = x.zip(sDiag).map { case (x, diag) => x * diag }
        val dxNorm = eNorm(xDiag) 
        val fp = dxNorm - delta
        if ((Math.abs(fp) <= p1 * delta) ||
            (par0._1 == 0.0 && fp <= fp0 && fp0 < 0.0) ||
            (iter == maxStepLengthIter)) {
          (par0._2, x, xDiag)
        } else {
          val aux0 = for (i <- 0 until n) yield {
            diag(qr.ipvt(i)) * xDiag(qr.ipvt(i)) / dxNorm
          }
          def multiply(j: Int, aux: Coordinates): Coordinates =
            if (j == n) {
              aux
            } else {
              val (auxLower, auxUpper) = aux.splitAt(j + 1)
              val auxj = auxLower(j) / sDiag(j)
              multiply(
                  j + 1, auxLower.updated(j, auxj) ++ 
                  (auxUpper - qr.R(j).drop(j + 1) / auxj))
            }
          val aux = multiply(0, aux0)
          val parc = fp / delta / (aux inner aux)
          val par = if (fp > 0.0) {
            val parLow = Math.max(par0._1, par0._2)
            (parLow, Math.max(parLow, par0._2 + parc), par0._3)
          } else {
            (par0._1, 
                Math.max(parLow, par0._2 + parc), 
                Math.min(par0._2, par0._3))
          }
          stepLength(iter + 1, par, fp, maxStepLengthIter)
        }
      }
      val parI = (parLow, Math.min(Math.max(par0, parLow), parUp), parUp)
      stepLength(0, parI, fp, maxStepLengthIter)
    }
  }
  
  /**
   * Solve a constrained linear system with the help of QR factorization.
   * 
   * Given an m by n matrix a, an n by n diagonal matrix d,
   * and an m-vector b, the problem is to determine an x which
   * solves the system
   *
   *     a*x = b  and  d*x = 0
   *
   * in the least squares sense.
   *
   * This subroutine completes the solution of the problem
   * if it is provided with the necessary information from the
   * QR factorization, with column pivoting, of a. That is, if
   * a*p = q*r, where p is a permutation matrix, q has orthogonal
   * columns, and r is an upper triangular matrix with diagonal
   * elements of non-increasing magnitude, then qrSolv expects
   * the full upper triangle of r, the permutation matrix p,
   * and the first n components of (q transpose)*b. The system
   * a*x = b, d*x = 0, is then equivalent to
   *
   *     r*z = q^T*b,  p^T*d*p*z = 0,
   *
   * where x = p*z. If this system does not have full rank,
   * then a least squares solution is obtained.
   * 
   */  
  def qrSolve(
      qr: QR,
      diag: Coordinates): (Coordinates, Coordinates) = {
    val n = diag.length
    
    def diagonalElimination(
        previous: (IndexedSeq[Coordinates], Coordinates, Coordinates),
        j: Int) = {
      val (s0, sDiag0, qtb0) = previous
      val jpvt = qr.ipvt(j)
      if (diag(jpvt) == 0.0) {
        (s0.updated(j, s0(j).updated(j, qr.R(j, j))), sDiag0.updated(j, s0(j)(j)), qtb0)
      } else {
        // Fill with zeros elements of sDiag where index is greater than j
        val sDiagInit: Seq[Double] =
          sDiag0.updated(j, diag(jpvt)).zipWithIndex.map { case (v, i) => if (i > j) 0.0 else v }
        val (sj, sDiag, qtb, qtbpj) =
          (j until n).foldLeft((s0(j), sDiagInit, qtb0, 0.0))(rotation)
        (s0.updated(j, sj.updated(j, qr.R(j, j))), sDiag.updated(j, sj(j)), qtb)
      }
    }
    
    def rotation(
        previous: (Coordinates, Coordinates, Coordinates, Double), 
        k: Int): (Coordinates, Coordinates, Coordinates, Double) = {
      val (sk0, sDiag0, qtb0, qtbpj0) = previous
      if (sDiag0(k) == 0.0) {
        previous
      } else {
        val (cos, sin) =
          if (Math.abs(sk0(k)) < Math.abs(sDiag0(k))) {
            val cot = sk0(k) / sDiag0(k)
            val sin = 1.0 / Math.sqrt(1.0 + cot * cot)
            (sin * cot, sin)
          } else {
            val tan = sDiag0(k) / sk0(k)
            val cos = 1.0 / Math.sqrt(1.0 + tan * tan)
            (cos, cos * tan)
          }
        val sk = for (i <- 0 until n) yield {
          if (i < k) sk0(i) else cos * sk0(i) + sin * sDiag0(i)
        }
        val sDiag = for (i <- 0 until n) yield {
          if (i <= k) sDiag0(i) else -sin * sk0(i) + cos * sDiag0(i)
        }
        val qtb = qtb0.updated(k, cos * qtb0(k) + sin * qtbpj0)
        val qtbpj = -sin * qtb0(k) + cos * qtbpj0 
        (sk, sDiag, qtb, qtbpj)
      }
    }
    
    // Eliminate the row of p^T*d*p*z = 0 with rotations
    // While doing it, update accordingly the matrix R and vector QtB
    // Resulting version of R is stored in S and QtB
    val s0: IndexedSeq[Coordinates] = for (j <- 0 until n) yield qr.R(j)
    val (s, sDiag, qtb) =
      (0 until n).foldLeft((s0, zeros(n), qr.qtb))(diagonalElimination)

    // Solve the modified triangular system s*z = qtb
    // If the system is singular, obtain a least square solution
    val firstIdxZero = sDiag.indexWhere(_ == 0.0)
    val nsing = if (firstIdxZero == -1) n - 1 else firstIdxZero - 1
    val qtbSingular = qtb.zipWithIndex.map { 
      case (value, col) => if (col <= nsing) value else 0.0
    }
    def solve(x0: Coordinates, j: Int): Coordinates = {
      if (j < 0) {
        x0
      } else {
        val (x0Lower, x0Upper) = x0.splitAt(j + 1)
        val sum = x0Upper inner s(j).drop(j + 1)
        val x = x0Lower.updated(j, (x0(j) - sum) / sDiag(j)) ++ x0Upper 
        solve(x, j - 1)
      }
    }
    val xUnpivoted = solve(qtbSingular, nsing)
    (qr.permute(xUnpivoted), sDiag)
  }

  def eNorm(v: Coordinates) = Math.sqrt(v map (x => x * x) sum)

}