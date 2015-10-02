package com.rklaehn.interval

import org.scalatest.FunSuite
import spire.implicits._
import spire.math.Rational

class IntervalsSetTest extends FunSuite {

  test("constant") {
    val a = IntervalMap.empty[Int, Boolean]
    val b = IntervalMap.constant[Int, Boolean](true)
    assert((a ^ b) == b)
    assert((a | b) == b)
    assert((a & b) == a)
    assert((b ^ b) == a)
  }

  test("simple") {
    val a = IntervalMap.empty[Int, Boolean]
    val b = IntervalMap.below(0, true)
    assert((a ^ b) == b)
    assert((a | b) == b)
    assert((a & b) == a)
    assert((b ^ b) == a)
  }

  test("check") {
    def toIntervalsSet(x: IntervalSeq[Rational]): IntervalMap[Rational, Boolean] =
      x.intervals.map(IntervalMap[Rational, Boolean](_, true)).foldLeft(IntervalMap.empty[Rational, Boolean])(_ | _)

    def toIntervalSeq(x: IntervalMap[Rational, Boolean]): IntervalSeq[Rational] =
      x.entries.filter(_._2).foldLeft(IntervalSeq.empty[Rational]) { case (x, y) ⇒ x ^ IntervalSeq(y._1) }

    val a = IntervalSeq("[13, ∞)")
    val b = IntervalSeq("[19, ∞)")
    val r = a ^ b
    val r1 = toIntervalsSet(a) ^ toIntervalsSet(b)
    println(r)
    println(r1)
  }
}

