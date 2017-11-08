package com.github.bruneli.scalaopt.core.discrete

import com.github.bruneli.scalaopt.core.linalg.DenseVectorLike
import com.github.bruneli.scalaopt.core.variable.{DiscreteVariable, Variable, Variables}

class MostExtremeBranching extends BranchingStrategy {

  import MostExtremeBranching._

  def findCutVariable(program: MIP[_, _, _],
                      relaxationSolution: DenseVectorLike[Variable]): Option[MostExtremeBranchingCut] = {
    program.discreteVariableIndices.foldLeft[Option[MostExtremeBranchingCut]](None)(updateCutVariable(relaxationSolution, program.variables))
  }

  private def updateCutVariable(solution: DenseVectorLike[Variable], variables: Variables[Variable])(
    previous: Option[MostExtremeBranchingCut], index: Int): Option[MostExtremeBranchingCut] = {
    val discreteVariable = variables(index).asInstanceOf[DiscreteVariable]
    val dxLow = discreteVariable.floor(solution(index)).map(solution(index).x - _.x).getOrElse(0.0)
    val dxUp = discreteVariable.ceil(solution(index)).map(_.x - solution(index).x).getOrElse(0.0)
    val dx = Math.max(dxLow, dxUp)
    if (previous.isEmpty || dx > previous.get.dx) {
      Some(MostExtremeBranchingCut(discreteVariable, index, dx))
    } else {
      previous
    }
  }

}

object MostExtremeBranching {

  case class MostExtremeBranchingCut(variable: DiscreteVariable, index: Int, dx: Double) extends BranchingCut

}
