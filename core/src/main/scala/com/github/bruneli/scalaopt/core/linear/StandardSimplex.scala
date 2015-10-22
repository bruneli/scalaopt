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

package com.github.bruneli.scalaopt.core.linear

import com.github.bruneli.scalaopt.core.{MaxIterException, Variables, ConfigPars, Optimizer}

import scala.annotation.tailrec
import scala.util.{Try, Success, Failure}

/**
 * Standard primal simplex method
 *
 * @author bruneli
 */
object StandardSimplex extends Optimizer[SimplexTableau, StandardSimplexConfig] {

  override val defaultConfig: StandardSimplexConfig = StandardSimplexConfig()

  /**
   * Minimize an objective function acting on a vector of real values.
   *
   * @param tableau real-valued objective function
   * @param x0      initial Variables
   * @param pars    algorithm configuration parameters
   * @return Variables at a local minimum or failure
   */
  override def minimize(
    tableau: SimplexTableau, x0: Variables)(
    implicit pars: StandardSimplexConfig): Try[Variables] = {
    solvePhase1(pars)(tableau) flatMap solvePhase2(pars) map(_.solution)
  }

  /**
   * Find the column with the smallest cost and set its row index to the row with the smallest min ratio
   *
   * @param phase   look at simplex phase 1 or phase 2 cost
   * @param tableau simplex tableau
   * @return column with the smallest cost
   */
  def pivotColumn(phase: SimplexPhase.Value)(tableau: SimplexTableau) = {
    val pivotCandidate = phase match {
      case SimplexPhase.Phase1 =>
        tableau.columns.minBy(_.phase1Cost)
      case SimplexPhase.Phase2 =>
        tableau.columns.filter(_.columnType != ColumnType.ArtificialVariable).minBy(_.phase2Cost)
    }
    minRatioRow(pivotCandidate, tableau.rhs)
  }

  /**
   * Find the row index with the minimum ratio b_i / A_(i,j)
   *
   * @param pivotColumn pivot column
   * @param rhs         equality constrains right hand side
   * @return pivot column with row index corresponding to minimum ratio
   */
  def minRatioRow(pivotColumn: TableauColumn, rhs: TableauColumn) = {
    val minRatioIdx = pivotColumn.constrains.zip(rhs.constrains).map(computeRatio).zipWithIndex.minBy(_._1)._2
    pivotColumn.copy(row = minRatioIdx)
  }

  def solvePhase1(pars: StandardSimplexConfig)(tableau: SimplexTableau): Try[SimplexTableau] = {
    iterate(0, tableau, SimplexPhase.Phase1, pars)
  }

  def solvePhase2(pars: StandardSimplexConfig)(tableau: SimplexTableau): Try[SimplexTableau] = {
    iterate(0, tableau, SimplexPhase.Phase2, pars)
  }

  private def computeRatio(values: (Double, Double)) = {
    val (row, rhs) = values
    if (row <= 0.0) Double.PositiveInfinity else rhs / row
  }

  @tailrec
  private def iterate(
    i: Int,
    tableau: SimplexTableau,
    simplexPhase: SimplexPhase.Value,
    pars: StandardSimplexConfig): Try[SimplexTableau] = {
    if (tableau.isOptimal(pars.eps)) {
      Success(tableau)
    } else if (i >= pars.maxIter) {
      Failure(throw new MaxIterException(s"Simplex $simplexPhase: number of iterations exceeds ${pars.maxIter}"))
    } else {
      val column = pivotColumn(simplexPhase)(tableau)
      iterate(i + 1, tableau.pivot(column), simplexPhase, pars)
    }
  }

}

case class StandardSimplexConfig(override val tol: Double = 1.49012e-8,
                                 override val maxIter: Int = 10,
                                 override val eps: Double = 1.0e-8) extends ConfigPars(tol, maxIter, eps)

/**
 * Define the two different simplex phases
 */
object SimplexPhase extends Enumeration {

  /** Phase1: find a basic feasible solution */
  val Phase1 = Value(0)

  /** Phase2: find the optimal basic feasible solution */
  val Phase2 = Value(1)

}