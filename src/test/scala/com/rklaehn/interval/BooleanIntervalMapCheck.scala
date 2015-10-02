package com.rklaehn.interval

import org.scalacheck.Properties
import org.scalacheck.Prop._
import spire.implicits._

object BooleanIntervalMapCheck extends Properties("BooleanIntervalsSetConsistentWithIntervalSeq") {

  implicit val arb = IntervalSeqArbitrary.arbitrary

  private def toIntervalsSet(x: IntervalSeq[Long]): IntervalMap[Long, Boolean] =
    x.intervals.map(IntervalMap[Long, Boolean](_, true)).foldLeft(IntervalMap.empty[Long, Boolean])(_ | _)

  private def toIntervalSeq(x: IntervalMap[Long, Boolean]): IntervalSeq[Long] =
    x.entries.filter(_._2).foldLeft(IntervalSeq.empty[Long]) { case (x, y) ⇒ x ^ IntervalSeq(y._1) }

  property("negate") = forAll { a: IntervalSeq[Long] ⇒
    val reference = ~a
    val as = toIntervalsSet(a)
    val rs = ~as
    val a1 = toIntervalSeq(rs)
    reference == a1
  }

  property("roundtrip") = forAll { a: IntervalSeq[Long] ⇒
    val as = toIntervalsSet(a)
    val a1 = toIntervalSeq(as)
    a == a1
  }

  property("xor") = forAll { (a: IntervalSeq[Long], b: IntervalSeq[Long]) ⇒
    val reference = a ^ b
    val as = toIntervalsSet(a)
    val bs = toIntervalsSet(b)
    val rs = as ^ bs
    val result = toIntervalSeq(rs)
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


