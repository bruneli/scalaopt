package com.github.bruneli.scalaopt.core.discrete

import com.github.bruneli.scalaopt.core.linalg.DenseVectorLike
import com.github.bruneli.scalaopt.core.variable.{DiscreteVariable, Variable}

trait BranchingStrategy {

  def findCutVariable(program: MIP[_, _, _],
                      relaxationSolution: DenseVectorLike[Variable]): Option[BranchingCut]

}

trait BranchingCut {

  val variable: DiscreteVariable

  val index: Int

}