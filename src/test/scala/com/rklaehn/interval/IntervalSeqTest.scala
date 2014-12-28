package com.rklaehn.interval

import org.junit.Assert._
import org.junit.Test
import spire.implicits._

class IntervalSeqTest {

  import IntervalSeq._

  //  @Test
  //  def testIntervalUnion() : Unit = {
  //    val a = Interval("[14, 44)")
  //    val b = Interval("(76, ∞)")
  //    val r1 = Interval("[14, ∞)")
  //    val r2 = a union b
  //    println(r1)
  //    println(r2)
  //    require(r1 == r2)
  //  }

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

  /*
  @Test
  def parseTest(): Unit = {
    val atext = "[46]"
//    val atext = "(-∞, 34);(61, 69];(69, ∞)"
    val b = IntervalSet(atext)
    val btext = b.toString
    println(s"$atext $btext")
  }
  */

  @Test
  def atIsSameAsApplyTest(): Unit = {
    val is = above(1)
    is.at(1) == is.apply(1)
  }


  @Test
  def equalsDifferentTypeTest(): Unit = {
    val is = above(1)
    is != "DOH!"
  }

  @Test
  def subsetOfTest(): Unit = {
    assertTrue(above(1).isSupersetOf(above(1)))
    assertTrue(atOrAbove(1).isSupersetOf(above(1)))
    assertFalse(above(1).isSupersetOf(atOrAbove(1)))

    assertFalse(above(1).isProperSupersetOf(above(1)))
    assertTrue(atOrAbove(1).isProperSupersetOf(above(1)))
    assertFalse(above(1).isProperSupersetOf(atOrAbove(1)))
  }

  @Test
  def algebraTest(): Unit = {
    val algebra = IntervalSeqAlgebra.booleanAlgebra[Long]
    val a = IntervalSeq.above(1L)
    val b = IntervalSeq.below(1L)
    assertEquals(a ^ b, algebra.xor(a, b))
  }
}
