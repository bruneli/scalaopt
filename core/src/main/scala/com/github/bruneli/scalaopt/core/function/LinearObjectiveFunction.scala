package com.github.bruneli.scalaopt.core.function

import com.github.bruneli.scalaopt.core.linalg.{DenseVector, DenseVectorLike}
import com.github.bruneli.scalaopt.core.variable.{Constant, ContinuousVariable, Variable}

trait LinearObjectiveFunctionLike[A <: Variable] extends ObjectiveFunction[A] {

  val cost: DenseVector[Constant]

  /**
    * Evaluate the objective function for a given vector of variables
    *
    * @param x vector of variables
    * @return real-valued objective function at x
    */
  override def apply(x: DenseVectorLike[A]): Double = {
    cost dot x
  }

  def toContinuous: LinearContinuousObjectiveFunction[ContinuousVariable]

}

/**
  * Define a real-valued linear objective function acting on a vector of variables
  *
  * @param cost cost vector representing the linear coefficients
  * @tparam A variable type
  *
  * @author bruneli
  */
case class LinearObjectiveFunction[A <: Variable](cost: DenseVector[Constant]) extends LinearObjectiveFunctionLike[A] {

  def toContinuous: LinearContinuousObjectiveFunction[ContinuousVariable] = {
    LinearContinuousObjectiveFunction(cost)
  }

}
