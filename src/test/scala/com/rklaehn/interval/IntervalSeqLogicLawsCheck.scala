package com.rklaehn.interval

import org.scalacheck.Properties
import spire.implicits._
import spire.laws.LogicLaws

object IntervalSeqLogicLawsCheck extends Properties("IntervalSeq") with AddProperties {

  val algebra = IntervalSeqAlgebra.booleanAlgebra[Long]

  val arb = IntervalSeqArbitrary.arbitrary

  addProperties("LogicLaws", LogicLaws(algebra, arb).bool(algebra))
}
