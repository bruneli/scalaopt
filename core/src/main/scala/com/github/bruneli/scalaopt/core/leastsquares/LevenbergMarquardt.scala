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

package com.github.bruneli.scalaopt.core.leastsquares

import com.github.bruneli.scalaopt.core.linalg.{DenseVector, QR}
import com.github.bruneli.scalaopt.core._
import com.github.bruneli.scalaopt.core.function.MSEFunction
import com.github.bruneli.scalaopt.core.variable.{Input, UnconstrainedVariable, VariableFromDouble}

import scala.util.{Success, Try}

/**
 * Least squares solution of a non-linear problem with the Levenberg-Marquardt method.
 *
 * The Levenberg-Marquardt method is equivalent to the Gauss-Newton method but with
 * a trust region strategy meaning that beyond some distance delta, a gradient descent
 * method is used instead of a Gauss-Newton algorithm. The Gauss-Newton method is a
 * modified Newton's method with line search that simplifies via an approximation the
 * computation of the Hessian in case of quadratic Loss function (Least squares problem).
 * Example: find the best exponential curve passing through points
 * {{{
 * scala> import scala.util.Random
 * scala> import com.github.bruneli.scalaopt.core._
 * scala> import leastsquares.LevenbergMarquardt._
 * scala> import SeqDataSetConverter._
 * scala> val random = new Random(12345)
 * scala> val exponential = (x: Variables, t: Variables) => Seq(x(0) * Math.exp(x(1) * t(0)))
 * scala> val data: DataSet[DataPoint] = for (i <- 1 to 10) yield {
 *      |   val x = i / 10.0
 *      |   val y = 2.0 * Math.exp(0.5 * x) + 0.1 * random.nextGaussian()
 *      |   DataPoint(x, y)
 *      | }
 * scala> minimize((exponential, data), Vector(1.0, 1.0))
 * }}}
 *
 * Although modified, the code implemented below has been inspired from
 * the minpack package licensed under:
 *
 * Minpack Copyright Notice (1999) University of Chicago.  All rights reserved
 *
 * Redistribution and use in source and binary forms, with or
 * without modification, are permitted provided that the
 * following conditions are met:
 *
 * 1. Redistributions of source code must retain the above
 * copyright notice, this list of conditions and the following
 * disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above
 * copyright notice, this list of conditions and the following
 * disclaimer in the documentation and/or other materials
 * provided with the distribution.
 *
 * 3. The end-user documentation included with the
 * redistribution, if any, must include the following
 * acknowledgment:
 *
 * "This product includes software developed by the
 * University of Chicago, as Operator of Argonne National
 * Laboratory.
 *
 * Alternately, this acknowledgment may appear in the software
 * itself, if and wherever such third-party acknowledgments
 * normally appear.
 *
 * 4. WARRANTY DISCLAIMER. THE SOFTWARE IS SUPPLIED "AS IS"
 * WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT HOLDER, THE
 * UNITED STATES, THE UNITED STATES DEPARTMENT OF ENERGY, AND
 * THEIR EMPLOYEES: (1) DISCLAIM ANY WARRANTIES, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO ANY IMPLIED WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, TITLE
 * OR NON-INFRINGEMENT, (2) DO NOT ASSUME ANY LEGAL LIABILITY
 * OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR
 * USEFULNESS OF THE SOFTWARE, (3) DO NOT REPRESENT THAT USE OF
 * THE SOFTWARE WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS, (4)
 * DO NOT WARRANT THAT THE SOFTWARE WILL FUNCTION
 * UNINTERRUPTED, THAT IT IS ERROR-FREE OR THAT ANY ERRORS WILL
 * BE CORRECTED.
 *
 * 5. LIMITATION OF LIABILITY. IN NO EVENT WILL THE COPYRIGHT
 * HOLDER, THE UNITED STATES, THE UNITED STATES DEPARTMENT OF
 * ENERGY, OR THEIR EMPLOYEES: BE LIABLE FOR ANY INDIRECT,
 * INCIDENTAL, CONSEQUENTIAL, SPECIAL OR PUNITIVE DAMAGES OF
 * ANY KIND OR NATURE, INCLUDING BUT NOT LIMITED TO LOSS OF
 * PROFITS OR LOSS OF DATA, FOR ANY REASON WHATSOEVER, WHETHER
 * SUCH LIABILITY IS ASSERTED ON THE BASIS OF CONTRACT, TORT
 * (INCLUDING NEGLIGENCE OR STRICT LIABILITY), OR OTHERWISE,
 * EVEN IF ANY OF SAID PARTIES HAS BEEN WARNED OF THE
 * POSSIBILITY OF SUCH LOSS OR DAMAGES.
 *
 * @author bruneli
 */
object LevenbergMarquardt extends LeastSquaresMethod[LevenbergMarquardtConfig] with VariableFromDouble {

  implicit val defaultConfig: LevenbergMarquardtConfig = new LevenbergMarquardtConfig

  def minimize(
    f: MSEFunction,
    x0: UnconstrainedVariablesType)(
    implicit pars: LevenbergMarquardtConfig): Try[UnconstrainedVariablesType] = {

    val n = x0.length

    def iterate(
      outer: Int,
      x0: UnconstrainedVariablesType,
      diag0: UnconstrainedVariablesType,
      xNorm0: Double,
      delta0: Double): UnconstrainedVariablesType = {
      val qr = QR(f.jacobianAndResidualsMatrix(x0), x0.length, pars.usePivoting)
      val fNorm0 = qr.bNorm.x
      val (diag, xNorm, delta1) =
        updateDiagNormDelta(outer, x0, diag0, xNorm0, delta0, qr, pars.stepBound)

      // norm of the scaled gradient
      def scaledGradientNorm(gNorm0: Double, j: Int): Double = {
        val acNormj = qr.acNorms(qr.ipvt(j)).x
        if (acNormj == 0.0) {
          gNorm0
        } else {
          val sum = (0 until j).foldLeft(0.0) {
            case (acc, i) => acc + qr.R(i, j).x * qr.qtb(i).x / fNorm0
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
      x0: UnconstrainedVariablesType,
      qr: QR,
      diag: UnconstrainedVariablesType,
      delta0: Double,
      par0: Double,
      fNorm0: Double): (UnconstrainedVariablesType, Double, Double, Boolean) = {
      val (par, pk, xDiag) = lmPar(qr, diag, delta0, par0, pars.maxStepLengthIter)
      val xDiagNorm = xDiag.norm
      val x = x0 - pk
      val xNorm = x.norm
      val fNorm = Math.sqrt(f(x))

      // Scaled predicted reduction and scaled directional derivative
      def multiply(predicted0: InputsType, j: Int): InputsType = {
        if (j == predicted0.length) {
          predicted0
        } else {
          val predicted = predicted0 - qr.R(j) * pk(qr.ipvt(j)).x
          multiply(predicted, j + 1)
        }
      }
      val predicted = multiply(DenseVector.zeros[Input](diag.length), 0)
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

    Success(iterate(0, x0, DenseVector.zeros[UnconstrainedVariable](x0.length), 0.0, 0.0))
  }

  private def updateDiagNormDelta(
    iteration: Int,
    x: UnconstrainedVariablesType,
    diag0: UnconstrainedVariablesType,
    xNorm0: Double,
    delta0: Double,
    qr: QR,
    stepBound: Double): (UnconstrainedVariablesType, Double, Double) =
    if (iteration == 0) {
      val diag = qr.acNorms.mapValues {
        norm => if (norm > 0.0) norm else 1.0
      }.asVectorOf[UnconstrainedVariable]
      val xNorm = Math.sqrt(diag dot x)
      val delta = if (xNorm == 0.0) stepBound else stepBound * xNorm
      (diag, xNorm, delta)
    } else {
      val diag = DenseVector.max(diag0, qr.acNorms.asVectorOf[UnconstrainedVariable])
      (diag, xNorm0, delta0)
    }

  // Determine the Levenberg-Marquardt parameter
  def lmPar(
    qr: QR,
    diag: UnconstrainedVariablesType,
    delta: Double,
    par0: Double,
    maxStepLengthIter: Int): (Double, UnconstrainedVariablesType, UnconstrainedVariablesType) = {
    val n = diag.length
    val p1 = 0.1

    def gaussNewtonDirection(qtb0: UnconstrainedVariablesType) = {
      // Take QtB values till first null R diagonal element
      val firstIdxZero = qr.rDiag.indexWhere(_ == 0.0)
      val nsing = if (firstIdxZero == -1) qr.rDiag.length - 1 else firstIdxZero - 1
      val qtb = qtb0.mapWithIndex {
        (value: Double, col: Int) => if (col <= nsing) value else 0.0
      }

      def qtbMinusR(x: UnconstrainedVariablesType, j: Int): UnconstrainedVariablesType = {
        if (j == -1 || x(j).x == 0.0) {
          x
        } else {
          val alpha = x(j).x / qr.rDiag(j).x
          val rj = qr.R(j)
          //val xNew = (x - rj * alpha).updated(j, alpha)
          val xNew = x.updated(j, alpha).mapWithIndex {
            (v: Double, i: Int) => if (i < j) v - rj(i).x * alpha else v
          }
          qtbMinusR(xNew, j - 1)
        }
      }

      val xUnpivoted = qtbMinusR(qtb, nsing)
      (DenseVector.permute(qr.ipvt)(xUnpivoted), nsing)
    }

    val (x, nsing) = gaussNewtonDirection(qr.qtb.asVectorOf[UnconstrainedVariable])
    val xdi = diag.zipAndMap(x, _ * _)
    val dxNorm = eNorm(xdi)
    val fp = dxNorm - delta

    if (fp <= p1 * delta) {
      (par0, x, xdi)
    } else {
      val unpermutedDiag = DenseVector.unpermute(qr.ipvt)(diag)
      val parLow =
        if (nsing >= n) {
          val unpermutedXdi = DenseVector.unpermute(qr.ipvt)(xdi)
          val aux0 = unpermutedDiag
            .zipAndMap(unpermutedXdi, (l, r) => l * r / dxNorm)
            .asVectorOf[Input]
          def mul(aux: InputsType, j: Int): InputsType = {
            if (j == n) {
              aux
            } else {
              val sum = aux.take(j - 1) inner qr.R(j).take(j - 1)
              mul(aux.updated(j, (aux(j).x - sum) / qr.rDiag(j).x), j + 1)
            }
          }
          val aux = mul(aux0, 0)
          fp / delta / (aux inner aux)
        } else {
          0.0
        }
      val aux = unpermutedDiag.mapWithIndex {
        (diag, i) => (qr.R(i).take(i) inner qr.qtb.take(i)) / diag
      }
      val parUp = eNorm(aux) / delta

      def stepLength(
        iter: Int,
        par0: (Double, Double, Double),
        fp0: Double,
        maxStepLengthIter: Int): (Double, UnconstrainedVariablesType, UnconstrainedVariablesType) = {
        val (x, sDiag) = qrSolve(qr, diag)
        val xDiag = x.zipAndMap(sDiag, _ * _)
        val dxNorm = eNorm(xDiag)
        val fp = dxNorm - delta
        if ((Math.abs(fp) <= p1 * delta) ||
          (par0._1 == 0.0 && fp <= fp0 && fp0 < 0.0) ||
          (iter == maxStepLengthIter)) {
          (par0._2, x, xDiag)
        } else {
          val pivotedAux0 = diag.zipAndMap(xDiag, (x, y) => x * y / dxNorm)
          val aux0 = DenseVector.unpermute(qr.ipvt)(pivotedAux0)
          def multiply(j: Int, aux: UnconstrainedVariablesType): UnconstrainedVariablesType = {
            if (j == n) {
              aux
            } else {
              val auxj = aux(j).x / sDiag(j).x
              val rUpper = qr.R(j).drop(j + 1)
              val rUpperNorm2 = rUpper.norm2
              val auxUpdated = aux.mapWithIndex {
                (x, i) =>
                  if (i == j) {
                    auxj
                  } else if (x > j && rUpperNorm2 != 0.0) {
                    x - rUpper(i - j - 1).x / auxj
                  } else {
                    x
                  }
              }
              multiply(j + 1, auxUpdated)
            }
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
   * a*x = b  and  d*x = 0
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
   * r*z = q^T*b,  p^T*d*p*z = 0,
   *
   * where x = p*z. If this system does not have full rank,
   * then a least squares solution is obtained.
   *
   */
  def qrSolve(
    qr: QR,
    diag: UnconstrainedVariablesType): (UnconstrainedVariablesType, UnconstrainedVariablesType) = {
    val n = diag.length

    def diagonalElimination(
      previous: (IndexedSeq[InputsType], InputsType, OutputsType),
      j: Int): (IndexedSeq[InputsType], InputsType, OutputsType) = {
      val (s0, sDiag0, qtb0) = previous
      val jpvt = qr.ipvt(j)
      if (diag(jpvt).x == 0.0) {
        (s0.updated(j, s0(j).updated(j, qr.R(j, j).x)), sDiag0.updated(j, s0(j)(j).x), qtb0)
      } else {
        // Fill with zeros elements of sDiag where index is greater than j
        val sDiagInit: InputsType = sDiag0.updated(j, diag(jpvt).x).mapWithIndex {
          (v, i) => if (i > j) 0.0 else v
        }
        val (sj, sDiag, qtb, qtbpj) =
          (j until n).foldLeft((s0(j), sDiagInit, qtb0, 0.0))(rotation)
        (s0.updated(j, sj.updated(j, qr.R(j, j).x)), sDiag.updated(j, sj(j).x), qtb)
      }
    }

    def rotation(
      previous: (InputsType, InputsType, OutputsType, Double),
      k: Int): (InputsType, InputsType, OutputsType, Double) = {
      val (sk0, sDiag0, qtb0, qtbpj0) = previous
      if (sDiag0(k).x == 0.0) {
        previous
      } else {
        val (cos, sin) =
          if (Math.abs(sk0(k).x) < Math.abs(sDiag0(k).x)) {
            val cot = sk0(k).x / sDiag0(k).x
            val sin = 1.0 / Math.sqrt(1.0 + cot * cot)
            (sin * cot, sin)
          } else {
            val tan = sDiag0(k).x / sk0(k).x
            val cos = 1.0 / Math.sqrt(1.0 + tan * tan)
            (cos, cos * tan)
          }
        val sk = sk0.mapWithIndex {
          (s0, i) => if (i < k) s0 else cos * s0 + sin * sDiag0(i).x
        }
        val sDiag = sDiag0.mapWithIndex {
          (s0, i) => if (i <= k) sDiag0(i).x else -sin * s0 + cos * sDiag0(i).x
        }
        val qtb = qtb0.updated(k, cos * qtb0(k).x + sin * qtbpj0)
        val qtbpj = -sin * qtb0(k).x + cos * qtbpj0
        (sk, sDiag, qtb, qtbpj)
      }
    }

    // Eliminate the row of p^T*d*p*z = 0 with rotations
    // While doing it, update accordingly the matrix R and vector QtB
    // Resulting version of R is stored in S and QtB
    val s0: IndexedSeq[InputsType] = for (j <- 0 until n) yield qr.R(j)
    val (s, sDiag, qtb) =
      (0 until n).foldLeft((s0, DenseVector.zeros[Input](n), qr.qtb))(diagonalElimination)

    // Solve the modified triangular system s*z = qtb
    // If the system is singular, obtain a least square solution
    val firstIdxZero = sDiag.indexWhere(_ == 0.0)
    val nsing = if (firstIdxZero == -1) n - 1 else firstIdxZero - 1
    val qtbSingular = qtb.mapWithIndex {
      (value, col) => if (col <= nsing) value else 0.0
    }
    def solve(x0: UnconstrainedVariablesType, j: Int): UnconstrainedVariablesType = {
      if (j < 0) {
        x0
      } else {
        val sum = x0.drop(j + 1) inner s(j).drop(j + 1)
        val x = x0.updated(j, (x0(j).x - sum) / sDiag(j).x)
        solve(x, j - 1)
      }
    }
    val xUnpivoted = solve(qtbSingular.asVectorOf[UnconstrainedVariable], nsing)
    (DenseVector.permute(qr.ipvt)(xUnpivoted), sDiag.asVectorOf[UnconstrainedVariable])
  }

  /** Euclidean norm of vector */
  def eNorm(v: UnconstrainedVariablesType): Double = v.norm

}

/**
 * Configuration parameters for the BFGS algorithm.
 *
 * @param tol               tolerance on the scaled reduction of the norm of f
 * @param maxIter           maximum number of iterations for the outer loop
 * @param eps               finite differences step to evaluate derivatives
 * @param xTol              tolerance on the norm of x reported to trust region size
 * @param gTol              tolerance on the scaled gradient norm
 * @param ratioTol          tolerance on the actual versus predicted reduction ratio
 * @param stepBound         step bound factor used to define delta, the trust region size
 * @param epsmch            machine precision
 * @param maxInnerIter      maximum number of iterations for the inner loop
 * @param maxStepLengthIter maximum number of iterations for the step length loop
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