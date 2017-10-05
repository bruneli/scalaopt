package com.github.bruneli.scalaopt.core.linalg

/**
  * A double vector builder
  *
  * @author bruneli
  */
trait DenseVectorBuilder[+To] {

  def withValues(values: Array[Double]): To

}

trait CanBuildDenseVectorFrom[-From, +To] {

  def apply(from: From): DenseVectorBuilder[To]

}
