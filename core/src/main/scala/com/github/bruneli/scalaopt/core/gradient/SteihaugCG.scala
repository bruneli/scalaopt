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

package com.github.bruneli.scalaopt.core.gradient

import com.github.bruneli.scalaopt.core._

import scala.annotation.tailrec
import scala.util.{Success, Failure, Try}

/**
 * Trust region Newton Conjugate Gradient method from Steihaug.
 *
 * {{{
 * scala> import com.github.bruneli.scalaopt.core._
 * scala> import gradient.SteihaugCG._
 * scala> minimize((x: Variables) => x dot x, Vector(2.0, 4.0)) // Approximate derivatives
 * scala> minimize(((x: Variables) => x dot x, (x: Variables) => x * 2.0), Vector(2.0, 4.0)) // Exact derivatives
 * }}}
 *
 * @author bruneli
 */
object SteihaugCG extends Optimizer[ObjectiveFunction, SteihaugCGConfig] {

  implicit val defaultConfig: SteihaugCGConfig = new SteihaugCGConfig()

  /**
   * Minimize an objective function acting on a vector of real values.
   *
   * @param f    real-valued objective function
   * @param x0   initial Variables
   * @param pars algorithm configuration parameters
   * @return Variables at a local minimum or failure
   */
  override def minimize(
    f: ObjectiveFunction,
    x0: Variables)(
    implicit pars: SteihaugCGConfig): Try[Variables] = {

    def iterate(k: Int, deltak: Double, ptk: LineSearchPoint): Try[Variables] = {
      if (k >= pars.maxIter)
        Failure(throw new MaxIterException(
          "Maximum number of iterations reached."))

      val dfxNorm = ptk.grad.norm
      val epsk = Math.min(0.5, Math.sqrt(dfxNorm)) * dfxNorm
      val z0 = zeros(ptk.x.size)
      val r0 = ptk.grad
      val d0 = -r0
      val ptkpp = searchDirection(0, ptk.copy(d = d0), z0, r0, deltak, epsk)

      val actualReduction = ptk.fx - ptkpp.fx
      val predictedReduction = ptk.fx - ptk.m(ptkpp.d)
      val rhok = actualReduction / predictedReduction

      val deltakpp =
        if (rhok < 0.25) {
          deltak / 4.0
        } else {
          if (rhok > 3.0 / 4.0 && Math.abs(ptkpp.d.norm - deltak) < pars.eps) {
            Math.min(2.0 * deltak, pars.deltaMax)
          } else {
            deltak
          }
        }

      if (ptkpp.grad.norm < pars.tol) {
        Success(ptkpp.x)
      } else {
        iterate(k + 1, deltakpp, if (rhok > pars.eta) ptkpp else ptk)
      }
    }

    iterate(0, pars.delta0, LineSearchPoint(x0, f, zeros(x0.size)))
  }

  @tailrec
  private def searchDirection(
    j: Int,
    ptk: LineSearchPoint,
    zj: Variables,
    rj: Variables,
    deltak: Double,
    epsk: Double): LineSearchPoint = {
    if ((ptk.d dot ptk.d2fxd) <= 0.0) {
      val pkpp = pkFromTrustRegionEdge(zj, ptk, deltak)
      ptk.copy(x = ptk.x + pkpp, d = pkpp)
    } else {
      val alphaj = (rj dot rj) / (ptk.d dot ptk.d2fxd)
      val zjpp = zj + ptk.d * alphaj
      if (zjpp.norm > deltak) {
        val pkpp = pkFromTrustRegionEdge(zj, ptk, deltak)
        ptk.copy(x = ptk.x + pkpp, d = pkpp)
      } else {
        val rjpp = rj + ptk.d2fxd * alphaj
        if (rjpp.norm < epsk) {
          ptk.copy(x = ptk.x + zjpp, d = zjpp)
        } else {
          val betajpp = (rjpp dot rjpp) / (rj dot rj)
          val djpp = -rjpp + ptk.d * betajpp
          searchDirection(j + 1, ptk.copy(d = djpp), zjpp, rjpp, epsk, deltak)
        }
      }
    }
  }

  /**
   * Find pk = zj + tau dj that minimizes
   * mk(pk) = fk - dj pk + 1/2 pkT B pk
   * and satisfies norm(pk) = deltak the trust region size.
   *
   * In practice use (zj + tau dj)T (zj + tau dj) = deltak * deltak
   * to solve a second order equation.
   */
  def pkFromTrustRegionEdge(zj: Variables, ptk: LineSearchPoint, deltak: Double) = {
    val dj = ptk.d
    val a = dj dot dj
    val b = zj dot dj
    val c = (zj dot zj) - deltak * deltak
    val disc = b * b - a * c
    val tauPos = (-b + Math.sqrt(disc)) / a
    val tauNeg = (-b - Math.sqrt(disc)) / a
    val pkPos = zj + ptk.d * tauPos
    val pkNeg = zj + ptk.d * tauNeg
    val mkPos = ptk.m(pkPos)
    val mkNeg = ptk.m(pkNeg)
    val m0 = ptk.fx
    if (ptk.m(pkPos) < ptk.m(pkNeg)) pkPos else pkNeg
  }

}

/**
 * Configuration parameters for the Steihaug conjugate gradient algorithm.
 *
 * @param tol tolerance error for convergence
 * @param maxIter maximum number of iterations
 * @param eps finite differences step to evaluate derivatives
 * @param delta0 initial trust region radius
 * @param deltaMax maximum trust region radius
 * @param eta minimal actual reduction over predicted reduction
 */
case class SteihaugCGConfig(
  override val tol: Double = 1.0e-5,
  override val maxIter: Int = 200,
  override val eps: Double = 1.0e-8,
  delta0: Double = 1.0,
  deltaMax: Double = 1.0e5,
  eta: Double = 0.2) extends ConfigPars(tol, maxIter, eps)