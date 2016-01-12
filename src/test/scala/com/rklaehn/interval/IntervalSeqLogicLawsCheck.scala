package com.rklaehn.interval

import org.scalacheck.Properties
import spire.implicits._
import spire.laws.LogicLaws

object IntervalSeqLogicLawsCheck extends Properties("IntervalSeq") with AddProperties {

  val algebra = IntervalSeq.algebra[Int]

  val arb = IntervalSeqArbitrary.arbitrary

  addProperties("LogicLaws", LogicLaws(algebra, arb).bool(algebra))
}
