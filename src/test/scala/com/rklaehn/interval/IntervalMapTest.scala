package com.rklaehn.interval

import org.scalatest.FunSuite
import spire.implicits._
import spire.math.Interval

class IntervalMapTest extends FunSuite {
  test("IntervalMap[Int, Bool]") {
    val a = IntervalMap.FromBool.above(1, true)
    val b = IntervalMap.FromBool.below(1, true)
    val c = a | b
    val d = ~a
    assert(a.entries.toSeq === Seq(Interval.atOrBelow(1) -> false, Interval.above(1) -> true))
    assert(c.entries.head === Interval.below(1) -> true)
    assert(c.entries.last === Interval.above(1) -> true)
    assert(d.entries.head === Interval.atOrBelow(1) -> true)
    assert((a & b) == IntervalMap.FromBool.zero[Int, Boolean])
  }

  test("equalsWrongType") {
    assert(IntervalMap.FromBool.zero[Int, Boolean] != "foo")
  }
}
