package com.rklaehn.interval

import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalatest.FunSuite
import spire.implicits._

import scala.collection.immutable.SortedSet

class IntervalsSetTest extends FunSuite {

  implicit object IntIsValue extends IntervalsSet.Value[Int] {
    def zero = 0

    def isZero(x: Int) = x == 0

    def andNot(a: Int, b: Int) = a & (~b)

    def or(a: Int, b: Int) = a | b

    def and(a: Int, b: Int) = a & b

    def xor(a: Int, b: Int) = a ^ b
  }

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

  implicit object IntIsValue extends IntervalsSet.Value[Int] {
    def zero = 0

    def isZero(x: Int) = x == 0

    def andNot(a: Int, b: Int) = a & (~b)

    def or(a: Int, b: Int) = a | b

    def and(a: Int, b: Int) = a & b

    def xor(a: Int, b: Int) = a ^ b
  }

  implicit val arb = IntervalSeqArbitrary.arbitrary

  private def toIntervalsSet(x: IntervalSeq[Long], v: Int): IntervalsSet[Long, Int] =
    x.intervals.map(IntervalsSet[Long, Int](_, v)).foldLeft(IntervalsSet.empty[Long, Int])(_ ^ _)

  private def toIntervalSeq(x: IntervalsSet[Long, Int], v: Int): IntervalSeq[Long] =
    x.intervals.filter(_._2 == v).foldLeft(IntervalSeq.empty[Long]) { case (x, y) ⇒ x ^ IntervalSeq(y._1) }

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