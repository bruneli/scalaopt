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

package com.github.bruneli.scalaopt.core.linalg

import com.github.bruneli.scalaopt.core._
import com.github.bruneli.scalaopt.core.variable._

/**
 * Results from the QR decomposition of the equation system AX = B.
 *
 * @param r       an n x n upper triangular matrix
 * @param rDiag   an n vector for fast access to diagonal elements of R
 * @param qtb     a vector with the first n rows resulting 
 *                from the product Qt * B
 * @param ipvt    a permutation matrix represented by an n-vector
 * @param acNorms initial column norms of matrix A
 * @param bNorm   norm of vector B
 * @author bruneli
 */
class QR(
  r: Array[InputsType],
  val rDiag: InputsType,
  val qtb: OutputsType,
  val ipvt: Array[Int],
  val acNorms: InputsType,
  val bNorm: Output) extends VariableFromDouble {

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
  def R(i: Int, j: Int): Input = {
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
  }

  /**
   * Return the column j of matrix R
   *
   * @param j column index
   * @return an n-vector with content of column j
   */
  def R(j: Int): InputsType = {
    if (j < 0 || j >= n) {
      throw new IllegalArgumentException(s"Column $j is out of (0, $n) bounds")
    } else {
      val column = r.zipWithIndex.map {
        case (row, i) =>
          if (i > j) {
            0.0
          } else if (i == j) {
            rDiag(j).x
          } else {
            row(j).x
          }
      }
      new SimpleDenseVector[Input](column)
    }
  }

  /**
   * Return the solution of the linear equation system
   * decomposed to R and QtB.
   */
  lazy val solution: UnconstrainedVariablesType = {
    val firstIdxZero = rDiag.force.indexWhere(_ == 0.0)
    val nsing = if (firstIdxZero == -1) n - 1 else firstIdxZero
    val qtbSingular = qtb.mapWithIndex {
      (value: Double, col: Int) => if (col <= nsing) value else 0.0
    }
    def solve(x0: UnconstrainedVariablesType, j: Int): UnconstrainedVariablesType = {
      if (j < 0) {
        x0
      } else {
        val sum = x0.force.drop(j + 1) inner r(j).force.drop(j + 1)
        val x = x0.updated(j, (x0(j).x - sum) / rDiag(j).x)
        solve(x, j - 1)
      }
    }
    DenseVector.permute[UnconstrainedVariable](ipvt)(
      solve(new UnconstrainedVariables(qtbSingular.force.coordinates), nsing))
  }

}

object QR extends VariableFromDouble {

  /**
   * Perform the QR decomposition with or without pivoting of matrix A
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

    val abNorms: AugmentedRow =
      doSqrt(ab.aggregate(AugmentedRow.zeros(n + 1))(columnsNorm, _ + _))

    // Initialize the pivot matrix
    val ipvt = (0 until n).toArray

    // Perform progressively householder transformations on columns
    val qrObj: QRObject = (0 until n).foldLeft(
      QRObject(abNorms.a, ab, ipvt))(
      houseHolder(n, pivoting))

    val reducedMat = qrObj.ab.filter(row => row.i < n).collect().sortBy(row => row.i)
    val r = reducedMat.map(row => row.a).toArray
    val qtb = SimpleDenseVector(reducedMat.map(_.b): _*)

    new QR(r, qrObj.rDiag, qtb, qrObj.ipvt, abNorms.a, abNorms.b)
  }

  /**
   * Solve a linear equation system AX=B via QR decomposition
   *
   * @param ab the augmented matrix of the linear system
   * @param n  the number of columns
   * @return the solution of the linear system in a least squares sense
   */
  def solve(ab: DataSet[AugmentedRow], n: Int): UnconstrainedVariablesType = {
    val qr = QR(ab, n)
    qr.solution
  }

  /**
   * Perform the Householder transformation of column j
   */
  private def houseHolder(
    n: Int, pivoting: Boolean)(
    qrObj0: QRObject, j: Int): QRObject = {

    val (ab0, ipvt) =
      if (pivoting) {
        // Find the column with largest norm and swap it with the j-th column
        val kMax = indexColumnLargestNorm(j, qrObj0.rDiag)
        if (kMax == j) {
          (qrObj0.ab, qrObj0.ipvt)
        } else {
          swapColumns(qrObj0.ab, qrObj0.ipvt, j, kMax)
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
    Math.sqrt(dotProducts.a(j).x + rowj.a(j).x * rowj.a(j).x) * Math.signum(rowj.a(j).x)

    val ajj = if (ajNorm != 0.0) rowj.a(j).x / ajNorm + 1.0 else 0.0

    // Rescale content of a given row
    def updateRow(i: Long, j: Int, aij: Double)(
      aik: Double, k: Int): Double = {
      if (k == j) {
        aij
      } else if (k > j) {
        val alpha = dotProducts.a(k).x / ajNorm / ajj + rowj.a(k).x
        aik - alpha * aij
      } else {
        aik
      }
    }

    // Rescale rows below j and columns beyond j
    def updateColumn(j: Int)(row: AugmentedRow): AugmentedRow = {
      if (row.i >= j.toLong) {
        val aij = if (row.i == j.toLong) ajj else row.a(j).x / ajNorm
        // Rescale the solution column by the same factor as the other columns
        val alpha = dotProducts.b.x / ajNorm / ajj + rowj.b.x
        val qtb = row.b.x - alpha * aij
        AugmentedRow(row.a.mapWithIndex(updateRow(row.i, j, aij)), qtb, row.i)
      } else {
        row
      }
    }

    // Update the diagonal index of R for a given column
    def rDiagUpdate(ajUpdatedRow: InputsType)(r: Double, k: Int): Double = {
      if (k > j) {
        val akj = ajUpdatedRow(k).x
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
          rowj.a.mapWithIndex(updateRow(j, j, ajj / ajNorm + 1.0))

        val rDiag1 =
          qrObj0.rDiag.mapWithIndex(rDiagUpdate(ajUpdatedRow)).updated(j, -ajNorm)

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
    sum: AugmentedRow,
    row: AugmentedRow): AugmentedRow = {
    sum + AugmentedRow(row.a.mapValues(x => x * x), row.b.x * row.b.x, row.i)
  }

  private def doSqrt(row: AugmentedRow): AugmentedRow = {
    AugmentedRow(row.a.mapValues(Math.sqrt), Math.sqrt(row.b.x), row.i)
  }

  private def indexColumnLargestNorm(k: Int, rDiag: InputsType): Int = {
    rDiag.force.zipWithIndex.drop(k).foldLeft((rDiag(k), k)) {
      case (r, c) => if (c._1.x > r._1.x) c else r
    }._2
  }

  /**
   * Swap column i with column j
   */
  private def swapColumns(ab: DataSet[AugmentedRow], ipvt: Array[Int],
    i: Int, j: Int): (DataSet[AugmentedRow], Array[Int]) = {
    (ab map swapColumns(i, j), ipvt.updated(i, ipvt(j)).updated(j, ipvt(i)))
  }

  private def swapColumns(i: Int, j: Int)(row: AugmentedRow): AugmentedRow = {
    AugmentedRow(row.a.swap(i, j), row.b, row.i)
  }

  /**
   * Perform the dot product of j-th column with other columns starting
   * from row i. At the same time, keep the content of row j.
   *
   * @param i minimal row index used to compute the dot product
   * @param j reference column index
   * @param sumAndRowj current value of the dot product sum and row j
   * @param row row information
   * @return an updated sum including row information
   */
  private def columnsDotProduct(i: Long, j: Int)(
    sumAndRowj: (AugmentedRow, AugmentedRow),
    row: AugmentedRow): (AugmentedRow, AugmentedRow) = {
    val (sum, rowj) = sumAndRowj
    if (row.i == j) {
      (sum, row)
    } else if (row.i > i) {
      val xj = row.a(j)
      val newSum = sum.a + row.a.mapWithIndex {
        (x: Double, k: Int) => if (k >= j) x * xj.x else x
      }
      (AugmentedRow(newSum, sum.b.x + xj.x * row.b.x, row.i), rowj)
    } else {
      sumAndRowj
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
    rDiag: InputsType,
    ab: DataSet[AugmentedRow],
    ipvt: Array[Int])

}