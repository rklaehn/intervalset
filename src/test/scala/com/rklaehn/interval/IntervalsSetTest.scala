package com.rklaehn.interval

import org.scalatest.FunSuite
import spire.implicits._

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
}

