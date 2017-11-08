package com.github.bruneli.scalaopt.core.discrete

import com.github.bruneli.scalaopt.core.constraint.{CP, Constraint}
import com.github.bruneli.scalaopt.core.function.ObjectiveFunction
import com.github.bruneli.scalaopt.core.linalg.DenseVectorLike
import com.github.bruneli.scalaopt.core.variable.{ContinuousVariable, DiscreteVariable, Variable, Variables}

/**
  * Mixed Integer Program
  *
  * @tparam A objective function type
  * @tparam B constraint type
  * @tparam R relaxation program type
  * @author bruneli
  */
trait MIP[A <: ObjectiveFunction[Variable],
          B <: Constraint[Variable],
          R <: CP[ContinuousVariable, _, _]] extends CP[Variable, A, B] {
  self =>

  val matchingPrecision: Double

  lazy val discreteVariableIndices = getDiscreteVariableIndices()

  def variables: Variables[Variable]

  def relaxationProgram: R

  def adaptVariablesTo(solution: DenseVectorLike[Variable]): Variables[Variable] = {
    variables.mapWithIndex((x, i) => solution.coordinate(i))
  }

  def acceptsAsFeasibleSolution(solution: DenseVectorLike[ContinuousVariable]): Boolean = {
    discreteVariableIndices.forall(
      i => isDiscrete(solution(i), variables(i).asInstanceOf[DiscreteVariable]))
  }

  private def isDiscrete(candidate: Variable,
                         variable: DiscreteVariable): Boolean = {
    val dxLow = variable.floor(candidate).map(candidate.x - _.x)
    val dxHigh = variable.ceil(candidate).map(_.x - candidate.x)
    dxLow.exists(_ < matchingPrecision) || dxHigh.exists(_ < matchingPrecision)
  }

  private def getDiscreteVariableIndices(): Seq[Int] = {
    for {(variable, index) <- variables.zipWithIndex
         if variable.isInstanceOf[DiscreteVariable]} yield {
      index
    }
  }

}
