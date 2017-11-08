package com.github.bruneli.scalaopt.core

import com.github.bruneli.scalaopt.core.linalg.DenseVectorLike
import com.github.bruneli.scalaopt.core.variable.Variable

case class Optimum[+A <: Variable](coordinates: DenseVectorLike[A], value: Double)
