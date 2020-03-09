package com.rklaehn.interval

import org.scalatest.funsuite.AnyFunSuite
import spire.implicits._
import spire.math.Interval

class IntervalMapTest extends AnyFunSuite {
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

  test("apply") {
    val t = IntervalMap.FromBool.above(0, true) | IntervalMap.FromBool.below(-100, true)
    assert(t(0) == false)
    assert(t.below(-1) == false)
    assert(t.at(-1) == false)
    assert(t.above(-1) == false)
    assert(t.below(0) == false)
    assert(t.at(0) == false)
    assert(t.above(0) == true)
    assert(t.below(1) == true)
    assert(t.at(1) == true)
    assert(t.above(1) == true)

    val u = IntervalMap.FromBool.above(0, true)
    assert(u(0) == false)
    assert(u.below(-1) == false)
    assert(u.at(-1) == false)
    assert(u.above(-1) == false)
    assert(u.below(0) == false)
    assert(u.at(0) == false)
    assert(u.above(0) == true)
    assert(u.below(1) == true)
    assert(u.at(1) == true)
    assert(u.above(1) == true)
  }

  test("step") {
    assert(IntervalMap.step2(0, 0, 0, 0, 1, 1, 1) == IntervalMap.step1(0, 1, 1, 1))
    assert(IntervalMap.step2(0, 0, 1, 1, 1, 1, 1) == IntervalMap.step1(0, 0, 1, 1))
  }

  test("FromMonoid") {
    import IntervalMap.FromMonoid._
    val z = empty[Int, String]
    assert(below(0, "") == z)
    assert(atOrBelow(0, "") == z)
    assert(point(0, "") == z)
    assert(hole(0, "") == z)
    assert(above(0, "") == z)
    assert(atOrAbove(0, "") == z)
    assert(apply(Interval(0, 1), "") == z)
    assert(apply(Interval.empty[Int], "x") == z)
    assert(apply[Int, String]() == z)
    assert(apply(Interval(0, 2), "x") == apply(Interval(0, 1) -> "x", Interval.openLower(1, 2) -> "x"))
    assert(hole(0, "x").at(0) == "")
  }
  test("FromBool") {
    import IntervalMap.FromBool._
    val z = zero[Int, Boolean]
    val o = one[Int, Boolean]
    assert(below(0, false) == z)
    assert(atOrBelow(0, false) == z)
    assert(point(0, false) == z)
    assert(hole(0, false) == z)
    assert(above(0, false) == z)
    assert(atOrAbove(0, false) == z)
    assert(~o == z)
    assert(apply(Interval.empty[Int], true) == z)
    assert(hole(0, true) == ~point(0, true))
  }
}
