package com.rklaehn.interval

import org.scalacheck.Properties
import org.scalacheck.Prop._
import spire.implicits._

object BooleanIntervalsSetCheck extends Properties("BooleanIntervalsSetConsistentWithIntervalSeq") {

  implicit val arb = IntervalSeqArbitrary.arbitrary

  private def toIntervalsSet(x: IntervalSeq[Long]): IntervalsSet[Long, Boolean] =
    x.intervals.map(IntervalsSet[Long, Boolean](_, true)).foldLeft(IntervalsSet.empty[Long, Boolean])(_ ^ _)

  private def toIntervalSeq(x: IntervalsSet[Long, Boolean]): IntervalSeq[Long] =
    x.intervals.filter(_._2).foldLeft(IntervalSeq.empty[Long]) { case (x, y) ⇒ x ^ IntervalSeq(y._1) }

  property("roundtrip") = forAll { a: IntervalSeq[Long] ⇒
    val as = toIntervalsSet(a)
    val a1 = toIntervalSeq(as)
    a == a1
  }

  property("xor") = forAll { (a: IntervalSeq[Long], b: IntervalSeq[Long]) ⇒
    val reference = a ^ b
    val as = toIntervalsSet(a)
    val bs = toIntervalsSet(b)
    val result = toIntervalSeq(as ^ bs)
    result == reference
  }

  property("and") = forAll { (a: IntervalSeq[Long], b: IntervalSeq[Long]) ⇒
    val reference = a & b
    val as = toIntervalsSet(a)
    val bs = toIntervalsSet(b)
    val result = toIntervalSeq(as & bs)
    result == reference
  }

  property("or") = forAll { (a: IntervalSeq[Long], b: IntervalSeq[Long]) ⇒
    val reference = a | b
    val as = toIntervalsSet(a)
    val bs = toIntervalsSet(b)
    val result = toIntervalSeq(as | bs)
    result == reference
  }
}


