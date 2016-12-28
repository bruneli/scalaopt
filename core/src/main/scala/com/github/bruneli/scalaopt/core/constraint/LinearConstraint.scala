/*
 * Copyright 2016 Renaud Bruneliere
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

package com.github.bruneli.scalaopt.core.constraint

import com.github.bruneli.scalaopt.core.linalg.FromToDoubleConversions.FromDouble
import com.github.bruneli.scalaopt.core.linalg.DenseVector
import com.github.bruneli.scalaopt.core.variable.{Constant, Constants, Variable}

import scala.util.{Success, Try}

/**
 * Define a constraint applied to a linear real-valued function of x
 *
 * @param left left operand real-valued function
 * @param operator constraint operator
 * @param right right operand real-valued constant
 * @tparam A optimization variable type
 * @author bruneli
 */
case class LinearConstraint[-A <: Variable](
  left: LinearLeftOperand[A],
  operator: ConstraintOperator,
  right: Double) extends Constraint[A] {

  /** Alias for the linear coefficients */
  def a: DenseVector[Constant] = left.a

  /** Alias for the real-valued constant linked to the right operand of the constraint */
  def b: Double = right

  /**
   * Try to resize a linear constraint
   *
   * It fails if the requested size is smaller than the current size.
   *
   * @param n size of the linear constraint (optional)
   * @return a linear constraint (of size n if specified) or a failure
   */
  override def toLinearConstraint(n: Option[Int] = None)(
    implicit fromDouble: FromDouble[A]): Try[LinearConstraint[A]] = n match {
    case Some(length) =>
      this.left.toLinearConstraint(n).map(left => LinearConstraint(left, operator, right))
    case None =>
      Success(this)
  }

  /**
   * Transform a general linear constraint into an n-dimensional linear equality constraint
   *
   * The initial constraint must have a number of dimensions smaller or equal to the new number of dimensions
   *
   * @param n     number of dimensions of the new constraint left hand side
   * @param slack position of the slack variable (optional, but mandatory for an initial inequality constraint)
   * @return an n-dimensional linear equality constraint
   */
  def toEquality(n: Int, slack: Option[Int] = None): LinearConstraint[A] = {
    val m = this.a.size
    require(n > m || operator.isInstanceOf[EqualityOperator] && n == m,
      s"Size of the new linear constraint $n should be >= initial constraint size $m")
    require(operator.isInstanceOf[EqualityOperator] || slack.isDefined,
      s"Every inequality constraint must specify a slack variable position")
    require(slack.isEmpty || slack.get >= m && slack.get < n,
      s"Slack variable position ${slack.get} must be >= $m and < $n")
    val a = new Array[Double](n)
    var idx = 0
    while (idx < n) {
      if (idx < m) {
        a(idx) = this.a(idx).x
      } else if (slack.isDefined && slack.get == idx) {
        operator match {
          case LowerOrEqualOperator =>
            // Add a slack variable to have equality satisfied
            a(idx) = 1.0
          case GreaterOrEqualOperator =>
            // Add an excess variable to have equality satisfied
            a(idx) = -1.0
          case _ =>
            throw new IllegalArgumentException(s"Equality constraint should not use slack variable")
        }
      } else {
         a(idx) = 0.0
      }
      idx += 1
    }
    val left = LinearLeftOperand[A](new Constants(a))
    LinearConstraint(left, EqualityOperator(), b)
  }

  /**
   * Produce a new linear constraint with a positive right-hand-side value
   */
  def withPositiveRhs: LinearConstraint[A] = {
    if (b < 0.0) {
      val inverseOp = operator match {
        case LowerOrEqualOperator => GreaterOrEqualOperator
        case EqualityOperator(eps) => operator
        case GreaterOrEqualOperator => LowerOrEqualOperator
      }
      LinearConstraint(LinearLeftOperand(-a), inverseOp, -b)
    } else {
      this
    }
  }

}
