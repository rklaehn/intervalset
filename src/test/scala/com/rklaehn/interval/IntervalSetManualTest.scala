package com.rklaehn.interval

import org.junit.Assert._
import org.junit.Test

class IntervalSetManualTest {

  import IntervalTrie._
  import IntervalSet._

  @Test
  def leafOperationTest(): Unit = {
    val a = above(1L)
    val b = atOrAbove(1L)
    val c = point(1L)
    val d = hole(1L)
    assertEquals(atOrAbove(1L), a | b)
    assertEquals(above(1L), a & b)
    assertEquals(point(1L), a ^ b)
    assertEquals(atOrAbove(1L), a | c)
    assertEquals(zero[Long], a & c)
    assertEquals(atOrAbove(1L), a ^ c)
    assertEquals(hole(1L), a | d)
    assertEquals(above(1L), a & d)
    assertEquals(below(1L), a ^ d)
  }
}
