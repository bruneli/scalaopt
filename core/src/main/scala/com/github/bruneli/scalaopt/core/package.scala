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

package com.github.bruneli.scalaopt

import com.github.bruneli.scalaopt.core.constraint.GeneralConstraintBuilder
import com.github.bruneli.scalaopt.core.function._
import com.github.bruneli.scalaopt.core.linalg.FromToDoubleConversions.{FromDouble, ToDouble}
import com.github.bruneli.scalaopt.core.linalg.{DenseVector, DenseVectorLike, RichMatrix, SimpleDenseVector}
import com.github.bruneli.scalaopt.core.variable._
import org.apache.commons.math3.linear.RealMatrix

/**
 * Numerical Optimization Algorithms written in Scala.
 *
 * All optimization algorithms contain a static method named minimize.
 * minimize takes as input a real objective function acting on variables.
 * Variables are represented by any scala collection of Double deriving from
 * Seq like commonly used Array, List, Vector,...
 * The real valued objective function can be any method taking as input a 
 * scala collection of Double and returning a Double.
 * The minimize function returns as output a Try[Variables] rather than
 * directly Variables to explicit the fact that all optimization algorithms
 * can fail to find a feasible solution.
 *
 * @author bruneli
 */
package object core extends VariableFromDouble {

  /** Define the vector of variables as an Array of abstract Variable objects */
  type RealVectorType = DenseVectorLike[ToDouble]
  type UnconstrainedVariablesType = DenseVectorLike[UnconstrainedVariable]
  type PositiveVariablesType = DenseVectorLike[PositiveVariable]
  type ContinuousVariablesType = DenseVectorLike[ContinuousVariable]
  type InputsType = DenseVectorLike[Input]
  type OutputsType = DenseVectorLike[Output]

  /** Implicit conversion from a ToDouble derived class to its Double value */
  implicit def DoubleFromToDouble[A <: ToDouble](x: A): Double = x.x

  /** Implicit conversion of an array of double to a dense vector of variables */
  implicit def toUnconstrainedVariables(raw: Array[Double]): UnconstrainedVariablesType = {
    new SimpleDenseVector[UnconstrainedVariable](raw)
  }
  implicit def toInputs(raw: Array[Double]): InputsType = {
    new SimpleDenseVector[Input](raw)
  }
  implicit def toOutputs(raw: Array[Double]): OutputsType = {
    new SimpleDenseVector[Output](raw)
  }

  /** Implicit conversion of a (function, gradient) tuple to an objective function */
  implicit def toFunctionWithGradient(
    f: (UnconstrainedVariablesType => Double,
      UnconstrainedVariablesType => UnconstrainedVariablesType)): DifferentiableObjectiveFunction[UnconstrainedVariable] = {
    ObjectiveFunctionWithGradient(f)
  }

  /** Implicit conversion of a function to an objective function with finite differences derivatives */
  implicit def toFunctionWoGradient(
    f: UnconstrainedVariablesType => Double): DifferentiableObjectiveFunction[UnconstrainedVariable] = {
    ObjectiveFunctionFiniteDiffDerivatives(f)
  }

  /** Implicit conversion of a function acting on parameters and inputs to a regression function */
  implicit def toRegressionFunction(
    f: (UnconstrainedVariablesType, InputsType) => OutputsType): RegressionFunction = {
    GeneralRegressionFunction(f)
  }

  /** Implicit conversion of a regression function with a set of data points to an MSE objective function */
  implicit def toMSEFunctionWoGradient(funcAndData: (RegressionFunction, DataSet[DataPoint])): MSEFunction = {
    SimpleMSEFunction(funcAndData._1, funcAndData._2)
  }

  /** Implicit conversion of a function to a linear objective function */
  implicit def toLinearObjectiveFunction[A <: ContinuousVariable : FromDouble](
    f: DenseVector[A] => Double): LinearObjectiveFunction[A] = {
    LinearObjectiveFunction(f)
  }
  implicit def toLinearObjectiveFunction(
    f: ContinuousVariablesType => Double): LinearObjectiveFunction[ContinuousVariable] = {
    LinearObjectiveFunction(f)(ContinuousVariableFromDouble)
  }

  /** Implicit conversion of RealMatrix to RichMatrix */
  implicit def toRichMatrix(m: RealMatrix): RichMatrix = new RichMatrix(m)

  /** Implicit conversion of a real-valued function to the left operand of a constraint */
  implicit def toConstraintBuilder[A <: Variable : FromDouble](c: DenseVector[A] => Double): GeneralConstraintBuilder[A] = {
    new GeneralConstraintBuilder[A](c)
  }
  implicit def toConstraintBuilder(c: ContinuousVariablesType => Double): GeneralConstraintBuilder[ContinuousVariable] = {
    new GeneralConstraintBuilder[ContinuousVariable](c)
  }

  /** Implicit conversion of a tuple (x, y) to a DataPoint */
  implicit def toDataPointYVector(xy: (InputsType, OutputsType)): DataPoint = {
    DataPoint(xy._1, xy._2)
  }
  implicit def toDataPointYScalar(xy: (InputsType, Output)): DataPoint = {
    DataPoint(xy._1, xy._2)
  }

}