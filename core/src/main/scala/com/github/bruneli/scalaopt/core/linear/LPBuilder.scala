package com.github.bruneli.scalaopt.core.linear

import com.github.bruneli.scalaopt.core._
import com.github.bruneli.scalaopt.core.constraint._
import com.github.bruneli.scalaopt.core.function.LinearContinuousObjectiveFunction
import com.github.bruneli.scalaopt.core.variable.ContinuousVariable

trait LPBuilder[P <: LP] extends CPBuilder[ContinuousVariable, P] {

  type Self <: LPBuilder[P]

  def addLinearConstraint(constraint: LinearConstraint[ContinuousVariable]): Self

  def addConstraint(constraint: Constraint[ContinuousVariable]): Self = {
    val linearConstraint = constraint.toLinearConstraint()(ContinuousVariableFromDouble)
      .getOrElse(throw new IllegalArgumentException(s"could not express constraint $constraint as a linear constraint"))
    addLinearConstraint(linearConstraint)
  }

  def subjectTo(constraints: Constraint[ContinuousVariable]*): Self = {
    constraints.tail.foldLeft[Self](addConstraint(constraints.head))(addConstraintTo)
  }

  protected def addConstraintTo(previousBuilder: Self,
                                constraint: Constraint[ContinuousVariable]): Self

}
