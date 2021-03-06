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

package com.github.bruneli.scalaopt.core.derivativefree

import com.github.bruneli.scalaopt.core._
import com.github.bruneli.scalaopt.core.function.ContinuousObjectiveFunction
import com.github.bruneli.scalaopt.core.variable.UnconstrainedVariable

import scala.util.{Failure, Success, Try}

/**
 * Implements the Nelder-Mead optimization algorithm.
 *
 * This algorithm is also commonly named the Downhill-Simplex method.
 * It does not require the computation of derivatives and is therefore
 * relatively robust wrt noise.
 * Example, find the minimum of a quadratic convex function
 * {{{
 * scala> import com.github.bruneli.scalaopt.core._
 * scala> import derivativefree.NelderMead._
 * scala> minimize((x: Variables) => x dot x, Vector(2.0, 4.0))
 * }}}
 *
 * @author bruneli
 */
object NelderMead extends DerivativeFreeMethod[NelderMeadConfig] {

  implicit val defaultConfig: NelderMeadConfig = new NelderMeadConfig

  /**
   * Minimize a real-valued continuous objective function acting on a vector of unconstrained variables.
   *
   * @param f    real-valued continuous objective function
   * @param x0   initial vector of unconstrained variables
   * @param pars algorithm configuration parameters
   * @return Variables of a local minimum if it converged
   */
  override def minimize(
    f: ContinuousObjectiveFunction[UnconstrainedVariable],
    x0: UnconstrainedVariablesType)(
    implicit pars: NelderMeadConfig): Try[UnconstrainedVariablesType] = {

    // Stop when required precision is reached or an exception is raised
    def stoppingRule(s: Try[Simplex]): Boolean = s match {
      case Success(s) => s.move.norm / math.sqrt(s.vertices.length) < pars.tol
      case Failure(e) => throw e
    }

    // From an initial simplex, modify it till the stopping rule is satisfied
    from(Try(Simplex(f, x0)(pars))) find stoppingRule match {
      case Some(s) => s.map(_.vtxMin.x)
      case None => Failure(
        new RuntimeException("Stopping rule is never satisfied."))
    }
  }

  /**
   * Simplex vertex with Variables and an associated function
   *
   * @constructor creates a new vertex from variables and a function
   * @param x vertex real-valued Variables
   * @param f real-valued objective function acting on x
   */
  case class Vertex(x: UnconstrainedVariablesType, f: ContinuousObjectiveFunction[UnconstrainedVariable]) {

    /** Value of function f in x */
    val fx = f(x)

    /** Vertex norm */
    def norm = this.x.norm

    /** Number of dimensions */
    def length = this.x.length

    /**
     * Creates a new vertex by shifting the i-th coordinate.
     *
     * @param i        coordinate index
     * @param relDelta relative shift of coordinate i
     * @param absDelta absolute shift of coordinate i
     * @return new vertex with coordinate i shifted
     */
    def shift(
      i: Int,
      relDelta: Double,
      absDelta: Double): Vertex = {
      val xNew = if (x.coordinate(i) == 0.0) {
        x.coordinate(i) + absDelta
      } else {
        x.coordinate(i) * relDelta
      }
      Vertex(x.updated(i, xNew), f)
    }
  }

  /**
   * A simplex used in the Nelder-Mead algorithm.
   *
   * @constructor create a new simplex made of n+1 vertices
   * @param vertices   collection of vertices
   * @param barycenter barycenter of the vertices
   * @param iter       iteration number
   * @param moveName   name of the simplex transform move
   * @param pars       configuration parameters
   */
  class Simplex(
    val vertices: Vector[Vertex],
    val barycenter: UnconstrainedVariablesType,
    val iter: Int,
    val moveName: String,
    pars: NelderMeadConfig) {
    require(vertices.length > 1,
      "There must be at least 2 vertices")
    require(vertices.length == vertices(0).x.length + 1,
      "Number of vertices must be equal to number of dimensions + 1")
    require(iter >= 0,
      "iteration number must be positive")

    /** Minimum vertex */
    val vtxMin = vertices.head

    /** Maximum vertex */
    val vtxMax = vertices.last

    /**
     * Simplex barycenter found when omitting one vertex.
     *
     * When removing one point from the barycenter, the weight of
     * each point must be scaled by (n+1)/n with n the number of
     * dimensions.
     *
     * @param vtx Vertex omitted during the barycenter computation
     * @return new barycenter found when omitting one vertex
     */
    def barycenter(vtx: Vertex): UnconstrainedVariablesType = {
      barycenter * (1.0 + 1.0 / barycenter.length) - (vtx.x / vtx.length)
    }

    /**
     * Move vector used to create a new vertex.
     *
     * The move vector is defined as the difference between
     * the barycenter found by omitting the maximum vertex 
     * and the maximum vertex.
     */
    val move: UnconstrainedVariablesType = barycenter(vtxMax) - vtxMax.x

    /**
     * Reflect the maximum vertex wrt the barycenter found by
     * omitting it from the simplex.
     */
    def reflection: Option[Simplex] = {
      val vtxNew = Vertex(vtxMax.x + move * pars.cReflection, vtxMax.f)
      if (vtxNew.fx <= vtxMin.fx) expansion(vtxNew)
      else if (vtxNew.fx <= vtxMax.fx) Some(update(vtxNew, "Reflection"))
      else None
    }

    /**
     * Same as reflection except move is expanded.
     */
    def expansion(vtxOld: Vertex): Option[Simplex] = {
      val vtxNew = Vertex(vtxOld.x + move * pars.cExpansion, vtxOld.f)
      if (vtxNew.fx <= vtxMin.fx) Some(update(vtxNew, "Expansion"))
      else Some(update(vtxOld, "Reflection"))
    }

    /**
     * Contrary to reflection, generate a new vertex inside 
     * the current simplex.
     */
    def contraction: Option[Simplex] = {
      val vtxNew = Vertex(vtxMax.x + move * pars.cContraction, vtxMax.f)
      if (vtxNew.fx <= vtxMax.fx) Some(update(vtxNew, "Contraction"))
      else None
    }

    /**
     * Shrink the whole simplex toward its current minimum.
     */
    def shrinkage: Option[Simplex] = {
      def shrink(v: Vertex) =
        Vertex(vtxMin.x + (v.x - vtxMin.x) * pars.cShrink, vtxMin.f)
      val verticesNew =
        vertices.map(shrink).sortWith(Simplex.increasingValue)
      val barycenterNew = Simplex.barycenter(verticesNew)
      Some(new Simplex(
        verticesNew,
        barycenterNew,
        iter + 1,
        "Shrinkage",
        pars))
    }

    /**
     * Modify a Simplex such as new Simplex is closer to a minimum.
     *
     * The algorithm recursively tries different simplex modifications
     * (reflection, expansion, contraction) wrt the current maximum
     * vertex. If no improvement is found in any of the three different
     * moves, the whole simplex is shrinked toward its current minimum. 
     */
    def next: Try[Simplex] =
    if (iter >= pars.maxIter * vertices.length)
      Failure(throw MaxIterException(
        "Maximum number of iterations reached."))
    else
      Success(reflection.orElse(contraction).orElse(shrinkage).get)

    private def update(vtxNew: Vertex, moveName: String): Simplex = {
      val barycenterNew =
        barycenter + (vtxNew.x - vtxMax.x) / (vtxMax.length + 1)
      val verticesNew =
        if (vtxNew.fx < vtxMin.fx)
          vtxNew +: vertices.init
        else if (vtxNew.fx > vertices.init.last.fx)
          vertices.init :+ vtxNew
        else {
          val index = vertices.indexWhere(_.fx >= vtxNew.fx)
          (vertices.take(index) :+ vtxNew) ++ vertices.init.drop(index)
        }
      new Simplex(verticesNew, barycenterNew, iter + 1, moveName, pars)
    }
  }

  object Simplex {
    /**
     * Generate n+1 vertices from an initial vertex with n-Variables
     *
     * The starting simplex contains x0. The other n vertices are
     * generated by applying x0 + b * ei (i = 1, 2,... n) where
     * ei is the unit vector along the i-est axis. b is proportional
     * to x0 if x0 is non zero, equal to deltaAbs otherwise. 
     *
     * @param f  real-valued objective function
     * @param x0 starting Variables
     * @return Simplex made of n+1 vertices
     */
    def apply(
      f: ContinuousObjectiveFunction[UnconstrainedVariable],
      x0: UnconstrainedVariablesType)(implicit pars: NelderMeadConfig): Simplex = {
      val v0 = Vertex(x0, f)

      val vertices = {
        for (i <- 0 to v0.length) yield {
          if (i == 0) v0 else v0.shift(i - 1, pars.relDelta, pars.absDelta)
        }
      }.toVector.sortWith(increasingValue)

      new Simplex(vertices, barycenter(vertices), 0, "Starting", pars)
    }

    /**
     * Rank two vertices according to their associated function value.
     */
    def increasingValue(v1: Vertex, v2: Vertex): Boolean = v1.fx < v2.fx

    /**
     * Compute the barycenter of a set of vertices.
     */
    def barycenter(vertices: Vector[Vertex]): UnconstrainedVariablesType = {
      vertices.tail.foldLeft[UnconstrainedVariablesType](
        vertices.head.x / (vertices.head.length + 1)) {
        case (r, c) => r + c.x / (c.length + 1.0)
      }
    }

  }

  /**
   * Generate an infinite sequence of successive simplices
   *
   * @param s initial simplex
   * @return a stream of possible simplices
   */
  def from(s: Try[Simplex]): Stream[Try[Simplex]] =
  s #:: from(s flatMap (_ next))

}

/**
 * Configuration options for the Nelder-Mead algorithm.
 *
 * @param tol          tolerance error for convergence
 * @param maxIter      maximum number of iterations per dimension
 * @param cReflection  reflection coefficient
 * @param cExpansion   expansion coefficient
 * @param cContraction contraction coefficient
 * @param cShrink      simplex reduction when shrinking it
 * @param relDelta     relative distance used to generate a new vertex
 * @param absDelta     absolute distance used to generate a new vertex
 */
case class NelderMeadConfig(
  override val tol: Double = 1.0e-5,
  override val maxIter: Int = 200,
  cReflection: Double = 2.0,
  cExpansion: Double = 1.0,
  cContraction: Double = 0.5,
  cShrink: Double = 0.5,
  relDelta: Double = 0.05,
  absDelta: Double = 0.00025) extends ConfigPars(tol, maxIter)