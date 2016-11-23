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

package com.github.bruneli.scalaopt.core.variable

import com.github.bruneli.scalaopt.core.linalg.FromToDoubleConversions.ToDouble

/**
 * Define the different type of optimization variable supported
 *
 * @author bruneli
 */
trait Variable extends Any with ToDouble {

  /** Lower bound on variable value if any */
  def lower: Option[Double]

  /** Upper bound on variable value if any */
  def upper: Option[Double]

  /** Convert variable into a continuous variable */
  def toContinuousVariable: ContinuousVariable

  /** Build a new variable with x modified */
  def build(x: Double): Variable

}

/**
 * Optimization variable with an unknown type
 */
case class Unknown(x: Double) extends AnyVal with ContinuousVariable {

  override def lower: Option[Double] = None

  override def upper: Option[Double] = None

  /** Build a new variable with x modified */
  override def build(x: Double): Unknown = Unknown(x)

}

sealed trait Continuous extends Any

sealed trait Discrete extends Any

/**
 * Real-valued continuous variable
 */
trait ContinuousVariable extends Any with Variable with Continuous {

  override def toContinuousVariable: ContinuousVariable = this

}

/**
 * Real-valued continuous variable bounded from below and/or above
 *
 * @param x variable value
 * @param lower lower bound on its value (optional)
 * @param upper upper bound on its value (optional)
 */
case class BoundedVariable(
  x: Double,
  lower: Option[Double],
  upper: Option[Double]) extends ContinuousVariable {

  /** Build a new variable with x modified */
  override def build(x: Double): BoundedVariable = this.copy(x = x)

}

trait DiscreteVariable extends Any with Variable with Discrete {

  override def toContinuousVariable: ContinuousVariable = {
    BoundedVariable(this.x, this.lower, this.upper)
  }

  /** Find the closest discrete variable with a value lower or equal to v */
  def floor(v: Variable): Option[DiscreteVariable]

  /** Find the closest discrete variable with a value greater or equal to v */
  def ceil(v: Variable): Option[DiscreteVariable]

}

/**
 * Discrete level
 */
case class Level(x: Double) extends AnyVal

/**
 * Discrete real-valued variable defined by a finite set of possible levels
 */
case class DiscreteLevelsVariable(
  level: Level, levels: Array[Level]) extends DiscreteVariable {

  val x = level.x

  lazy val sortedLevels = levels.sortBy(_.x)

  override def lower: Option[Double] = sortedLevels.headOption.map(_.x)

  override def upper: Option[Double] = sortedLevels.lastOption.map(_.x)

  override def floor(v: Variable): Option[DiscreteVariable] = {
    sortedLevels.takeWhile(_.x <= v.x).lastOption.map(level => this.copy(level = level))
  }

  override def ceil(v: Variable): Option[DiscreteVariable] = {
    sortedLevels.find(_.x >= v.x).map(level => this.copy(level = level))
  }

  /** Build a new variable with x modified */
  override def build(x: Double): DiscreteLevelsVariable = {
    val lvl = sortedLevels.find(_.x >= x).getOrElse(sortedLevels.head)
    this.copy(level = lvl)
  }

}

/**
 * Integer variable
 */
case class IntegerVariable(
  i: Int,
  min: Option[Int] = None,
  max: Option[Int] = None) extends DiscreteVariable {

  val x = i.toDouble

  override def lower: Option[Double] = min.map(_.toDouble)

  override def upper: Option[Double] = max.map(_.toDouble)

  override def floor(v: Variable): Option[DiscreteVariable] = {
    if (lower.isDefined && v.x < lower.get) {
      None
    } else {
      Some(IntegerVariable(Math.floor(v.x).toInt, min, max))
    }
  }

  override def ceil(v: Variable): Option[DiscreteVariable] = {
    if (upper.isDefined && v.x > upper.get) {
      None
    } else {
      Some(IntegerVariable(Math.ceil(v.x).toInt, min, max))
    }
  }

  /** Build a new variable with x modified */
  override def build(x: Double): IntegerVariable = {
    if (x - Math.floor(x) >= 0.5) {
      this.copy(i = Math.ceil(x).toInt)
    } else {
      this.copy(i = Math.floor(x).toInt)
    }
  }

}

/**
 * Binary variable
 */
case class BinaryVariable(b: Boolean) extends DiscreteVariable {

  val x = if (b) 1.0 else 0.0

  override def lower: Option[Double] = Some(0.0)

  override def upper: Option[Double] = Some(1.0)

  override def floor(v: Variable): Option[DiscreteVariable] = {
    Some(BinaryVariable(false))
  }

  override def ceil(v: Variable): Option[DiscreteVariable] = {
    Some(BinaryVariable(true))
  }

  /** Build a new variable with x modified */
  override def build(x: Double): BinaryVariable = {
    BinaryVariable(x >= 0.5)
  }

}