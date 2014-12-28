package com.rklaehn.interval

import spire.implicits._

object IntervalSeqBooleanAlgebraCheck
  extends BooleanAlgebraSpecification[IntervalSeq[Long]]("IntervalSet.BooleanAlgebra") {

  val algebra = IntervalSeqAlgebra.booleanAlgebra[Long]

  val eq = IntervalSeqAlgebra.booleanAlgebra[Long]

  def arb = IntervalSeqArbitrary.arbitrary
}
