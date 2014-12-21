package com.rklaehn.interval

import org.scalacheck.Test.Parameters
import org.scalacheck.{Prop, Properties, Gen, Arbitrary}
import spire.algebra.{Eq, BooleanAlgebra}
import spire.std.any._
import org.scalacheck.Prop._
import spire.syntax.all._

object IntervalSetBooleanAlgebraProperties
  extends BooleanAlgebraSpecification[IntervalSet[Long]]("IntervalSet.BooleanAlgebra") {

  def algebra = IntervalSetAlgebra.booleanAlgebra

  def eq = IntervalSetAlgebra.booleanAlgebra

  def arb = IntervalSetArbitrary.arbitrary

}
