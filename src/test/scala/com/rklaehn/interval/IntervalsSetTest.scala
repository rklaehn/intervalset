package com.rklaehn.interval

import org.scalatest.FunSuite
import spire.implicits._
import spire.math.Rational

class IntervalsSetTest extends FunSuite {

  test("constant") {
    val a = IntervalsSet.empty[Int, Boolean]
    val b = IntervalsSet.constant[Int, Boolean](true)
    assert((a ^ b) == b)
    assert((a | b) == b)
    assert((a & b) == a)
    assert((b ^ b) == a)
  }

  test("simple") {
    val a = IntervalsSet.empty[Int, Boolean]
    val b = IntervalsSet.below(0, true)
    assert((a ^ b) == b)
    assert((a | b) == b)
    assert((a & b) == a)
    assert((b ^ b) == a)
  }

  test("check") {
    def toIntervalsSet(x: IntervalSeq[Rational]): IntervalsSet[Rational, Boolean] =
      x.intervals.map(IntervalsSet[Rational, Boolean](_, true)).foldLeft(IntervalsSet.empty[Rational, Boolean])(_ | _)

    def toIntervalSeq(x: IntervalsSet[Rational, Boolean]): IntervalSeq[Rational] =
      x.intervals.filter(_._2).foldLeft(IntervalSeq.empty[Rational]) { case (x, y) ⇒ x ^ IntervalSeq(y._1) }

    val a = IntervalSeq("[13, ∞)")
    val b = IntervalSeq("[19, ∞)")
    val r = a ^ b
    val r1 = toIntervalsSet(a) ^ toIntervalsSet(b)
    println(r)
    println(r1)
  }
}

