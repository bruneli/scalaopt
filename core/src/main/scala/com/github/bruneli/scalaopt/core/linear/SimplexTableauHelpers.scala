package com.github.bruneli.scalaopt.core.linear

import com.github.bruneli.scalaopt.core.DataSet
import com.github.bruneli.scalaopt.core.constraint.{ConstraintOperator, GreaterOrEqualOperator, LowerOrEqualOperator}
import com.github.bruneli.scalaopt.core.linalg.DenseVector
import com.github.bruneli.scalaopt.core.variable.Constant

trait SimplexTableauHelpers {

  protected def addSlackVariable(n: Int,
                                 m0: Int,
                                 op: ConstraintOperator)(
                                 i: Int): TableauColumn = {
    val isSlack = i == (n - 1) && (op == LowerOrEqualOperator || op == GreaterOrEqualOperator)
    val isBasic = isSlack && op == LowerOrEqualOperator
    val row = if (isBasic) m0 else -1
    TableauColumn(
      0.0,
      0.0,
      DenseVector.zeros[Constant](m0),
      i,
      isSlack = isSlack,
      isBasic = isBasic,
      row = row)
  }

  protected def getNegativeColumn(columns: DataSet[TableauColumn],
                                  constraintTypes: Vector[ConstraintOperator]): TableauColumn = {
    val initialColumn = TableauColumn(0.0, 0.0, DenseVector.zeros(constraintTypes.size), 0)
    val isPositiveColumn = (column: TableauColumn) => !column.isSlack && !column.isArtificial
    val seqOp = (previous: TableauColumn, current: TableauColumn) => {
      previous + current.negate
    }
    columns
      .filter(isPositiveColumn)
      .aggregate(initialColumn)(seqOp, _ + _)
      .copy(column = -1, isSlack = false, isArtificial = false, isBasic = false)
  }

}
