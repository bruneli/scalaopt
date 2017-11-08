package com.github.bruneli.scalaopt.core.discrete

import com.github.bruneli.scalaopt.core.{ObjectiveType, Optimum}
import com.github.bruneli.scalaopt.core.constraint.{LinearConstraint, LinearLeftOperand}
import com.github.bruneli.scalaopt.core.function.{LinearObjectiveFunction, ObjectiveFunction}
import com.github.bruneli.scalaopt.core.linalg.DenseVectorLike
import com.github.bruneli.scalaopt.core.linear.DualTableau
import com.github.bruneli.scalaopt.core.variable.{Constants, Variable, Variables}

/**
  * Mixed Integer Linear Program
  */
case class MILP(variables: Variables[Variable],
                relaxationProgram: DualTableau,
                matchingPrecision: Double = 1e-5) extends MIP[LinearObjectiveFunction[Variable], LinearConstraint[Variable], DualTableau] {

  /**
    * Get the problem objective: minimize or maximize
    */
  override def objective: ObjectiveType = relaxationProgram.objective

  /**
    * Get the objective function
    *
    * @return linear objective function
    */
  override def objectiveFunction: LinearObjectiveFunction[Variable] = {
    val phase2Costs = relaxationProgram.columns.map(_.phase2Cost).collect().toArray
    LinearObjectiveFunction(new Constants(phase2Costs))
  }

  /**
    * Return the number of constraints
    */
  override def numberOfConstraints = relaxationProgram.numberOfConstraints

  /**
    * Get a linear constraint
    *
    * @param i index of the constraint
    * @return linear constraint
    */
  override def constraint(i: Int): LinearConstraint[Variable] = {
    LinearConstraint(LinearLeftOperand(relaxationProgram.constraint(i).left.a), relaxationProgram.constraint(i).operator, relaxationProgram.constraint(i).right)
  }

}
