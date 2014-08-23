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
import scala.util.{Try, Success, Failure}
import org.jblas.DoubleMatrix

object LevenbergMarquardt {
  
  val fTol = 1.49012e-8
  val xTol = fTol
  val gTol = 0.0
  val eps = 1.0e-8
  val stepBound = 100.0
  
  type ObjFunWithData = (Coordinates, Seq[Double]) => Double
  type DataSet = Vector[(Vector[Double], Double)]
  type Jacobian = Vector[Vector[Double]]

  def minimize(
      f: ObjFunWithData,
      data: DataSet,
      x0: Coordinates): Try[Coordinates] = {
    
    val n = x0.length
    
    def iterate(
        outer: Int, 
        x0: Coordinates,
        diag0: Coordinates,
        xNorm0: Double,
        delta0: Double): Coordinates = {
      val fJac = jacobian(x0, f, data)
      val qrObj = qrFac(fJac)
      val r0 = data map (xy => residual(x0, xy, f))
      val fNorm0 = eNorm(r0)
      val qtf = qtr(qrObj, r0)
      val (diag, xNorm, delta) = 
        updateDiagNormDelta(outer, x0, diag0, xNorm0, delta0, qrObj)
        
      // norm of the scaled gradient
      def scaledGradientNorm(gNorm0: Double, j: Int): Double = {
        val acNormj = qrObj.acNorm(qrObj.ipvt(j))
        if (acNormj == 0.0) {
          gNorm0
        } else {
          val sum = (0 until j).foldLeft(0.0) { 
            case (sum, i) => sum + qrObj.r(i, j) * qtf(i) / fNorm0
          }
          Math.max(gNorm0, Math.abs(sum / acNormj))
        }
      }
      val gNorm = (0 until n).foldLeft(0.0)(scaledGradientNorm)
        
      val (x, fNorm, delta2, ratio) = 
        innerLoop(0, x0, qrObj, diag, qtf, 0.0, 0.0, fNorm0)
      if (ratio < 0.0001) {
        iterate(outer + 1, x, diag, fNorm, delta2)
      } else {
        x
      }
    }

    def innerLoop(
      inner: Int,
      x0: Coordinates,
      qrObj: QRObject,
      diag: Coordinates,
      qtf: Vector[Double],
      delta0: Double,
      par0: Double,
      fNorm0: Double): (Coordinates, Double, Double, Double) = {
      val (par, pk, xDiag) = lmPar(qrObj, diag, qtf, delta0, par0)
      val x = data map (xy => residual(x0 - pk, xy, f))
      
      // Predicted reduction
      def multiply(predicted0: Coordinates, j: Int): Coordinates = {
        if (j == predicted0.length) {
          predicted0
        } else {
          val (lowerPre, upperPre) = predicted0.splitAt(j)
          val predicted = 
            (lowerPre - qrObj.r(j).take(j) * pk(j)) ++ upperPre
          multiply(predicted, j + 1)
        }
      }
      val predicted = multiply((1 to x.length).map(i => 0.0).toVector, 0)
      
      val fNorm = Math.sqrt(x inner x)
      val reduction = 1.0 - (fNorm * fNorm) / (fNorm0 * fNorm0)
      val delta = 2.0 * Math.sqrt(xDiag inner xDiag)
      val ratio = 0.0
      if (reduction <= fTol || delta <= xTol * fNorm) {
        (x, fNorm, delta, ratio)
      } else {
        innerLoop(inner + 1, x, qrObj, xDiag, qtf, delta, par, fNorm)
      }
    }

    Success(iterate(0, x0, Vector(), 0.0, 0.0))
  }
  
  def residual(
      p: Coordinates, 
      xy: (Coordinates, Double), 
      f: ObjFunWithData): Double = {
    val (x, y) = xy
    y - f(p, x)
  }
  
  // Compute the Jacobian J
  def jacobian(
      x0: Coordinates, 
      f: ObjFunWithData, 
      data: DataSet): Jacobian = {
    val r0 = data map (xy => residual(x0, xy, f))
    (for (i <- 0 until x0.length) yield {
      val x = x0.updated(i, x0(i) + eps)
      val r = data map (xy => residual(x, xy, f))
      (r zip r0) map { case (x, x0) => (x - x0) / eps }
    }).toVector
  }
  
  // QR factorization of J
  def qrFac(jacobian: Jacobian): QRObject = {

    def indexColumnLargestNorm(k: Int, rDiag: Vector[Double]) = {
      rDiag.zipWithIndex.drop(k).foldLeft((rDiag(k), k)) { 
        case (r, c) => if (c._1 > r._1) c else r 
      }._2
    }
    
    def swapColumns(jacobian: Jacobian, ipvt: Vector[Int], i: Int, j: Int) = {
      (jacobian.updated(i, jacobian(j)).updated(j, jacobian(i)),
       ipvt.updated(i, ipvt(j)).updated(j, ipvt(i)))
    }
    
    def houseHolder(qrObj0: QRObject, j: Int) = {
      
      // Find the column with largest norm and swap it with the j-th column
      val kmax = indexColumnLargestNorm(j, qrObj0.rDiag)
      val (a, ipvt) = 
        if (kmax == j) 
          (qrObj0.jacobian, qrObj0.ipvt) 
        else
          swapColumns(qrObj0.jacobian, qrObj0.ipvt, j, kmax)
          
      // Compute the norm of the j-th column and renormalize it 
      val ajNorm = eNorm(a(j).drop(j - 1)) * Math.signum(a(j)(j))
      val ajUpperRows = a(j).take(j - 1)
      val ajj = a(j)(j) / ajNorm + 1
      val ajLowerRows = ajj +: (a(j).drop(j) / ajNorm)

      // Scale all columns on the right of aj
      def updateColumn(ak: Vector[Double]) = {
        val (akUpperRows, akLowerRows) = ak.splitAt(j - 1)
        val alpha = (akLowerRows dot ajLowerRows) / ajj
        akUpperRows ++ (akLowerRows - ajLowerRows * alpha)
      }
      val aRightColumns = a.drop(j).map(updateColumn)
      
      // Update rDiag
      def rDiagUpdate(rk: (Double, Int)) = {
        val (r, k) = rk
        val akj = aRightColumns(k)(j)
        r * Math.sqrt(Math.max(0.0, 1 - akj * akj / r / r))
      }
      val rDiag = 
        ((qrObj0.rDiag.take(j - 1) :+ (-ajNorm)) ++ 
            qrObj0.rDiag.drop(j).zipWithIndex.map(rDiagUpdate))
      
      // Construct the new matrix by merging the modified columns
      QRObject(
          qrObj0.acNorm,
          rDiag,
          (a.take(j - 1) :+ (ajUpperRows ++ ajLowerRows)) ++ aRightColumns,
          ipvt,
          j)
    }
    
    val n = jacobian.length
    val acNorm = jacobian map eNorm
    val ipvt = (0 until n).toVector
    (0 until n).foldLeft(QRObject(acNorm, acNorm, jacobian, ipvt, -1))(houseHolder)
  }
  
  def qtr(qrObj: QRObject, r: Vector[Double]) = {
    
    def qtrColumn(qtr: Vector[Double], j: Int) = {
      val ajj = qrObj.jacobian(j)(j)
      if (ajj != 0.0) {
        val ajLowerRows = qrObj.jacobian(j).drop(j - 1)
        val (qtrUpperRows, qtrLowerRows) = qtr.splitAt(j - 1)
        val alpha = (qtrLowerRows dot ajLowerRows) / ajj
        qtrUpperRows ++ (qtrLowerRows * (1.0 - alpha))
      } else {
        qtr
      }
    }
    
    (0 until r.length).toVector.foldLeft(r)(qtrColumn)
  }
  
  def updateDiagNormDelta(
      iteration: Int, 
      x: Coordinates, 
      diag0: Coordinates, 
      xNorm0: Double,
      delta0: Double,
      qrObj: QRObject) =
    if (iteration == 0) {
      val diag = qrObj.acNorm map (norm => if (norm > 0.0) norm else 1.0)
      val xNorm = eNorm(diag.zip(x).map { case (d, x) => d * x })
      (diag, xNorm, stepBound * xNorm)
    } else {
      val diag = diag0.zip(qrObj.acNorm) map { 
        case (d, norm) => Math.max(d, norm) 
      }
      (diag, xNorm0, delta0)
    }
    
  // Determine the Levenberg-Marquardt parameter
  def lmPar(
      qrObj: QRObject,  
      diag: Coordinates, 
      qtr: Vector[Double],
      delta: Double,
      par0: Double): (Double, Coordinates, Coordinates) = {
    
    val n = diag.length
    val p1 = 0.1
    
    def gaussNewtonDirection(qtr0: Vector[Double]) = {
      // Take qtr values till first null R diagonal element
      val firstIdxZero = qrObj.rDiag.indexWhere(_ == 0.0)
      val n = if (firstIdxZero == -1) qrObj.rDiag.length else firstIdxZero
      val qtr = qtr0.zipWithIndex.map { 
        case (value, col) => if (col < n) value else 0.0
      }
      
      def qtrMinusR(x: Seq[Double], col: Int): Seq[Double] =
        if (col == 0 || x(col) == 0.0) {
          x
        } else {
          val alpha = x(col) / qrObj.rDiag(col)
          val (xLower, xUpper) = x.splitAt(col)
          val rLower = qrObj.jacobian(col).take(col - 1)
          val xNew = (xLower - rLower * alpha) ++ xUpper.updated(0, alpha) 
          qtrMinusR(xNew, col - 1)
        }        
      
      val xUnpivoted = qtrMinusR(qtr, n)
      (qrObj.ipvt.map(index => xUnpivoted(index)), n)
    }
    
    val (x, nsing) = gaussNewtonDirection(qtr)
    val xdi = diag.zip(x) map { case (d, x) => d * x }
    val dxNorm = eNorm(xdi)
    val fp = dxNorm - delta
    
    if (fp <= p1 * delta) {
      (0.0, x, xdi)
    } else {
      val parLow =
        if (nsing >= n) {
          val aux0 = for (i <- 0 until n) yield {
            val j = qrObj.ipvt(i)
            diag(j) * xdi(j) / dxNorm
          }
          def mul(aux: Coordinates, j: Int): Seq[Double] = {
            if (j == n) {
              aux
            } else {
              val (auxLower, auxUpper) = aux.splitAt(j - 1)
              val sum = auxLower inner qrObj.r(j).take(j - 1)
              mul(auxLower ++ auxUpper.updated(0, (aux(j) - sum) 
                  / qrObj.rDiag(j)), j + 1)
            }
          }
          val aux = mul(aux0, 0)
          fp / delta / (aux inner aux)
        } else {
          0.0
        }
      val aux = for (i <- 0 until n) yield {
        val sum = qrObj.r(i).take(i) inner qtr.take(i)
        sum / diag(qrObj.ipvt(i))
      }
      val parUp = eNorm(aux) / delta
      
      val nIter = 10
      def stepLength(
          iter: Int, 
          par0: (Double, Double, Double), 
          fp0: Double): (Double, Coordinates, Coordinates) = {
        val aux = diag * Math.sqrt(par0._2)
        val (x, sDiag) = qrSolve(qrObj, diag, qtr)
        val xDiag = x.zip(sDiag).map { case (x, diag) => x * diag }
        val dxNorm = eNorm(xDiag) 
        val fp = dxNorm - delta
        if ((Math.abs(fp) <= p1 * delta) ||
            (par0._1 == 0.0 && fp <= fp0 && fp0 < 0.0) ||
            (iter == nIter)) {
          (par0._2, x, xDiag)
        } else {
          val aux0 = for (i <- 0 until n) yield {
            diag(qrObj.ipvt(i)) * xDiag(qrObj.ipvt(i)) / dxNorm
          }
          def multiply(j: Int, aux: Coordinates): Coordinates =
            if (j == n) {
              aux
            } else {
              val (auxLower, auxUpper) = aux.splitAt(j)
              val auxj = auxLower(j) / sDiag(j)
              multiply(
                  j + 1, auxLower.updated(j, auxj) ++ 
                  (auxUpper - qrObj.r(j).drop(j) / auxj))
            }
          val aux = multiply(0, aux0.toVector)
          val parc = fp / delta / (aux inner aux)
          val par = if (fp > 0.0) {
            val parLow = Math.max(par0._1, par0._2)
            (parLow, Math.max(parLow, par0._2 + parc), par0._3)
          } else {
            (par0._1, 
                Math.max(parLow, par0._2 + parc), 
                Math.min(par0._2, par0._3))
          }
          stepLength(iter + 1, par, fp)
        }
      }
      val parI = (parLow, Math.min(Math.max(par0, parLow), parUp), parUp)
      stepLength(0, parI, fp)
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
      qrObj: QRObject,
      diag: Seq[Double], 
      qtb0: Seq[Double]): (Coordinates, Coordinates) = {
    val n = qrObj.rDiag.length
    
    def diagonalElimination(
        previous: (Vector[Coordinates], Coordinates, Coordinates), 
        j: Int) = {
      val (s0, sDiag0, qtb0) = previous
      val jpvt = qrObj.ipvt(j)
      if (diag(jpvt) == 0.0) {
        (s0.updated(j, s0(j).updated(j, qrObj.rDiag(j))), sDiag0.updated(j, s0(j)(j)), qtb0)
      } else {
        val sDiag0 = diag(jpvt) +: zeros(n - j - 1)
        val (sj, sDiag, qtb, qtbpj) = 
          (j until n).foldLeft((s0(j), sDiag0, qtb0, 0.0))(rotation)
        (s0.updated(j, sj), sDiag, qtb)
      }
    }
    
    def rotation(
        previous: (Coordinates, Coordinates, Coordinates, Double), 
        k: Int) = {
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
    // While doing it, update accordingly the matrix r and vector qtb
    // Resulting version of r is stored in s and qtb
    val s0 = for (j <- 0 until n) yield qrObj.r(j)
    val (s, sDiag, qtb) = 
      (0 until n).foldLeft((s0.toVector, zeros(n), qtb0))(diagonalElimination)

    // Solve the modified triangular system s*z = qtb
    // If the system is singular, obtain a least square solution
    val firstIdxZero = sDiag.indexWhere(_ == 0.0)
    val nsing = if (firstIdxZero == -1) n else firstIdxZero
    val qtbSingular = qtb.zipWithIndex.map { 
      case (value, col) => if (col < nsing) value else 0.0
    }
    def solve(x0: Coordinates, j: Int): Coordinates = {
      if (j < 0) {
        x0
      } else {
        val (x0Lower, x0Upper) = x0.splitAt(j + 1)
        val sum = x0Upper inner s(j).drop(j)
        val x = x0Lower.updated(j, (x0(j) - sum) / sDiag(j)) ++ x0Upper 
        solve(x, j - 1)
      }
    }
    val xUnpivoted = solve(qtbSingular, nsing)
    val x = for (j <- 0 until n) yield xUnpivoted(qrObj.ipvt(j))
    (x, sDiag)
  }

  def eNorm(v: Seq[Double]) = Math.sqrt(v map (x => x * x) sum)
  
  case class QRObject(
      acNorm: Vector[Double],
      rDiag: Vector[Double],
      jacobian: Jacobian,
      ipvt: Vector[Int],
      j: Int) {
    
    lazy val n = ipvt.length
  
    def r(col: Int) = jacobian(col).take(n - 1) :+ rDiag(col)
    
    def r(col: Int, row: Int) =
      if (col == row) {
        rDiag(col)
      } else if (row < col) {
        jacobian(col)(row)
      } else {
        jacobian(row)(col)
      }
    
  }
  
}