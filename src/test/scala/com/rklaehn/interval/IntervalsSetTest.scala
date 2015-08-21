package com.rklaehn.interval

import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalatest.FunSuite
import spire.implicits._

class IntervalsSetTest extends FunSuite {

  test("constant") {
    val a = IntervalsSet.empty[Int, Int]
    val b = IntervalsSet.constant[Int, Int](0)
    assert((a ^ b) == b)
    assert((a | b) == b)
    assert((a & b) == a)
    assert((b ^ b) == a)
  }

  test("simple") {
    val a = IntervalsSet.empty[Int, Int]
    val b = IntervalsSet.below(0, 0)
    assert((a ^ b) == b)
    assert((a | b) == b)
    assert((a & b) == a)
    assert((b ^ b) == a)
  }
}

object IntervalsSetCheck extends Properties("IntervalsSetConsistentWithIntervalSeq") {

  implicit val arb = IntervalSeqArbitrary.arbitrary

  private def toIntervalsSet(x: IntervalSeq[Long], v: Int): IntervalsSet[Long, Int] =
    x.intervals.map(IntervalsSet(_, v)).foldLeft(IntervalsSet.empty[Long, Int])(_ ^ _)

  private def toIntervalSeq(x: IntervalsSet[Long, Int], v: Int): IntervalSeq[Long] =
    x.intervals.filter(_._2.contains(v)).foldLeft(IntervalSeq.empty[Long]) { case (x, y) ⇒ x ^ IntervalSeq(y._1) }

  property("roundtrip") = forAll { a: IntervalSeq[Long] ⇒
    val as = toIntervalsSet(a, 1)
    val a1 = toIntervalSeq(as, 1)
    a == a1
  }

  property("xor") = forAll { (a: IntervalSeq[Long], b: IntervalSeq[Long]) ⇒
    val reference = a ^ b
    val as = toIntervalsSet(a, 1)
    val bs = toIntervalsSet(b, 1)
    val result = toIntervalSeq(as ^ bs, 1)
    result == reference
  }

  property("and") = forAll { (a: IntervalSeq[Long], b: IntervalSeq[Long]) ⇒
    val reference = a & b
    val as = toIntervalsSet(a, 1)
    val bs = toIntervalsSet(b, 1)
    val result = toIntervalSeq(as & bs, 1)
    result == reference
  }

  property("or") = forAll { (a: IntervalSeq[Long], b: IntervalSeq[Long]) ⇒
    val reference = a | b
    val as = toIntervalsSet(a, 1)
    val bs = toIntervalsSet(b, 1)
    val result = toIntervalSeq(as | bs, 1)
    result == reference
  }
}