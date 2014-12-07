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

package org.scalaopt.algos.linalg

import org.scalaopt.algos._

/**
 * Results from the rank-revealing QR decomposition of the 
 * equation system AX = B.
 *
 * @param r       an n x n upper triangular matrix
 * @param rDiag   an n vector for fast access to diagonal elements of R
 * @param qtb     a vector with the first n rows resulting 
 *                from the product Qt * B
 * @param ipvt    a permutation matrix represented by an n-vector
 * @param acNorms initial column norms of matrix A
 * @param bNorm   norm of vector B
 *
 * @author bruneli
 */
class QR(
    r: Seq[Coordinates],
    val rDiag: Coordinates,
    val qtb: Coordinates, 
    val ipvt: Vector[Int],
    val acNorms: Coordinates,
    val bNorm: Double) {
  
  /** Number of columns or rows in R */
  val n = rDiag.length

  require(n > 0, "QR decomposition is empty")
  require(n == r.length, "R is not a square matrix")
  
  /**
   * Rij element of matrix R
   * 
   * @param i row index
   * @param j column index
   * @return matrix element Rij
   */
  def R(i: Int, j: Int): Double =
    if (i < 0 || i >= n) {
      throw new IllegalArgumentException(s"Row $i is out of (0, $n) bounds")
    } else if (j < 0 || j >= n) {
      throw new IllegalArgumentException(s"Column $j is out of (0, $n) bounds")      
    } else if (i == j) {
      rDiag(i)
    } else if (i < j) {
      r(i)(j)
    } else {
      0.0
    }
  
  /**
   * Return the column j of matrix R
   * 
   * @param j column index
   * @return an n-vector with content of column j
   */
  def R(j: Int): Coordinates =
    if (j < 0 || j >= n) {
      throw new IllegalArgumentException(s"Column $j is out of (0, $n) bounds")
    } else {
      r.zipWithIndex.map { 
        case (row, i) => 
          if (i > j) {
            0.0
          } else if (i == j) {
            rDiag(j)
          } else {
            row(j)
          }
      }
    }
  
  /**
   * Return the solution of the linear equation system
   * decomposed to R and QtB.
   */
  lazy val solution: Coordinates = {
    val firstIdxZero = rDiag.indexWhere(_ == 0.0)
    val nsing = if (firstIdxZero == -1) n - 1 else firstIdxZero
    val qtbSingular = qtb.zipWithIndex.map { 
      case (value, col) => if (col <= nsing) value else 0.0
    }
    def solve(x0: Coordinates, j: Int): Coordinates = {
      if (j < 0) {
        x0
      } else {
        val (x0Lower, x0Upper) = x0.splitAt(j + 1)
        val sum = x0Upper inner r(j).drop(j + 1)
        val x = x0Lower.updated(j, (x0(j) - sum) / rDiag(j)) ++ x0Upper 
        solve(x, j - 1)
      }
    }
    permute(solve(qtbSingular, nsing))
  }
  
  /**
   * Permute the solution of a linear system back to its initial order
   */
  def permute(xUnpivoted: Coordinates): Coordinates = {
    var x = new Array[Double](n)
    for (j <- 0 until n) x(ipvt(j)) = xUnpivoted(j)
    x.toSeq    
  }
  
  /**
   * Permute the solution of a linear system to its QR system
   */
  def unpermute(x: Coordinates) = for (j <- 0 until n) x(ipvt(j))
  
}

object QR {

  /**
   * Perform the rank-revealing QR decomposition of matrix A
   * and compute the product Qt * B.
   *
   * @param ab       an augmented m * (n+1) matrix representing
   *                 the linear equation system A X = B
   * @param n        the number of columns of matrix A
   * @param pivoting perform a pivoting of columns while decomposing
   * @return an object with the matrix R, the vector QtB and
   *         the permutation matrix
   */
  def apply(
      ab: DataSet[AugmentedRow], 
      n: Int, 
      pivoting: Boolean = true): QR = {

    val abNorms =
      ab.aggregate(zeros(n + 1))(columnsNorm, _ + _).map(Math.sqrt(_))

    // Initialize the pivot matrix
    val ipvt = (0 until n).toVector

    // Perform progressively householder transformations on columns
    val qrObj = 
      (0 until n).foldLeft(QRObject(abNorms.init, ab, ipvt))(houseHolder(n, pivoting))
      
    val reducedMat = qrObj.ab.filter(row => row.i < n).collect().sortBy(row => row.i)
    val r = reducedMat.map(row => row.a)
    val qtb = reducedMat.map(row => row.b)
      
    new QR(r, qrObj.rDiag, qtb, qrObj.ipvt, abNorms.init, abNorms.last)
  }
  
  /**
   * Solve a linear equation system AX=B via QR decomposition
   * 
   * @param ab the augmented matrix of the linear system
   * @param n  the number of columns
   * @return the solution of the linear system in a least squares sense
   */
  def solve(ab: DataSet[AugmentedRow], n: Int): Coordinates = {
    val qr = QR(ab, n)
    qr.solution
  }

  /**
   * Perform the Householder transformation of column j
   */
  private def houseHolder(n: Int, pivoting: Boolean)(
      qrObj0: QRObject, j: Int): QRObject = {

    val (ab0, ipvt) =
      if (pivoting) {
        // Find the column with largest norm and swap it with the j-th column
        val kmax = indexColumnLargestNorm(j, qrObj0.rDiag)
        if (kmax == j) {
          (qrObj0.ab, qrObj0.ipvt)
        } else {
          swapColumns(qrObj0.ab, qrObj0.ipvt, j, kmax)
        }
      } else {
        // Keep current ordering of columns
        (qrObj0.ab, qrObj0.ipvt)
      }
        
    // Compute the dot products of column j with columns k >= j
    // taking into account only rows with index >= j.
    // At same time, retrieve values of row ajk (j=1,n).
    val (dotProducts, rowj) =
      ab0.aggregate((AugmentedRow.zeros(n), AugmentedRow.zeros(n)))(
        columnsDotProduct(j.toLong, j), sumResults)

    // Norm of the j-th column used to scale all the remaining columns
    val ajNorm = 
      Math.sqrt(dotProducts.a(j) + rowj.a(j) * rowj.a(j)) * Math.signum(rowj.a(j))

    val ajj = if (ajNorm != 0.0) rowj.a(j) / ajNorm + 1.0 else 0.0

    // Rescale content of a given row
    def updateRow(i: Long, j: Int, aij: Double)(
      xk: (Double, Int)): Double = {
      val (aik, k) = xk
      if (k == j) {
        aij
      } else if (k > j) {
        val alpha = dotProducts.a(k) / ajNorm / ajj + rowj.a(k)
        aik - alpha * aij
      } else {
        aik
      }
    }

    // Rescale rows below j and columns beyond j
    def updateColumn(j: Int)(row: AugmentedRow): AugmentedRow =
      if (row.i >= j.toLong) {
        val aij =
          if (row.i == j.toLong) ajj else row.a(j) / ajNorm
        // Rescale the solution column by the same factor as the other columns
        val alpha = dotProducts.b / ajNorm / ajj + rowj.b
        val qtb = row.b - alpha * aij
        AugmentedRow(row.a.zipWithIndex.map(updateRow(row.i, j, aij)), qtb, row.i)
      } else {
        row
      }

    // Update the diagonal index of R for a given column
    def rDiagUpdate(ajUpdatedRow: Coordinates)(
        rk: (Double, Int)): Double = {
      val (r, k) = rk
      if (k > j) {
        val akj = ajUpdatedRow(k)
        r * Math.sqrt(Math.max(0.0, 1 - akj * akj / r / r))
      } else {
        r
      }
    }

    val (ab, rDiag) =
      if (ajNorm == 0.0) {
        (ab0, qrObj0.rDiag.updated(j, 0.0))
      } else {
        val ajUpdatedRow =
          rowj.a.zipWithIndex.map(updateRow(j, j, ajj / ajNorm + 1.0))

        val rDiag1 =
          qrObj0.rDiag.zipWithIndex.map(rDiagUpdate(ajUpdatedRow)).updated(j, -ajNorm)

        (ab0.map(updateColumn(j)), rDiag1)
      }

    QRObject(rDiag, ab, ipvt)
  }

  /**
   * Square the content of all columns and sum it
   *
   * @param sum current value of the squared sum
   * @param row row information
   * @return an updated sum including row information
   */
  private def columnsNorm(
      sum: Coordinates, 
      row: AugmentedRow): Coordinates = {
    sum + ((row.a map (x => x * x)) :+ row.b * row.b)
  }

  private def indexColumnLargestNorm(k: Int, rDiag: Seq[Double]) = {
    rDiag.zipWithIndex.drop(k).foldLeft((rDiag(k), k)) {
      case (r, c) => if (c._1 > r._1) c else r
    }._2
  }

  /**
   * Swap column i with column j
   */
  private def swapColumns(ab: DataSet[AugmentedRow], ipvt: Vector[Int],
    i: Int, j: Int): (DataSet[AugmentedRow], Vector[Int]) = {
    (ab map swapColumns(i, j), ipvt.updated(i, ipvt(j)).updated(j, ipvt(i)))
  }

  private def swapColumns(i: Int, j: Int)(row: AugmentedRow): AugmentedRow = {
    AugmentedRow(row.a.updated(i, row.a(j)).updated(j, row.a(i)),
      row.b, row.i)
  }

  /**
   * Perform the dot product of j-th column with other columns starting
   * from row i. At the same time, keep the content of row j.
   *
   * @param i   minimal row index used to compute the dot product
   * @param j   reference column index
   * @param sum current value of the dot product sum
   * @param row row information
   * @return an updated sum including row information
   */
  private def columnsDotProduct(i: Long, j: Int)(
    sum: (AugmentedRow, AugmentedRow),
    row: AugmentedRow): (AugmentedRow, AugmentedRow) = {
    if (row.i == j) {
      (sum._1, row)
    } else if (row.i > i) {
      val xj = row.a(j)
      val newSum = sum._1.a + (row.a.zipWithIndex map {
        case (x, k) => if (k >= j) x * xj else x
      })
      (AugmentedRow(newSum, sum._1.b + xj * row.b, row.i), sum._2)
    } else {
      sum
    }
  }

  /**
   * Sum results when aggregating dot products of columns
   */
  private def sumResults(
    result1: (AugmentedRow, AugmentedRow),
    result2: (AugmentedRow, AugmentedRow)): (AugmentedRow, AugmentedRow) = {
    (result1._1 + result2._1, result1._2 + result2._2)
  }

  /**
   * Private class used to perform the QR factorization
   *
   * @param rDiag R diagonal elements
   * @param ab    augmented matrix
   * @param ipvt  pivot matrix
   */
  private case class QRObject(
    rDiag: Coordinates,
    ab: DataSet[AugmentedRow],
    ipvt: Vector[Int])

}