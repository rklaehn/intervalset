package com.rklaehn.interval
import org.scalacheck.Prop._

object IntervalSetBooleanAlgebraCheck
  extends BooleanAlgebraSpecification[IntervalSet[Long]]("IntervalSet.BooleanAlgebra") {

  def algebra = IntervalSetAlgebra.booleanAlgebra

  def eq = IntervalSetAlgebra.booleanAlgebra

  def arb = IntervalSetArbitrary.arbitrary
}
