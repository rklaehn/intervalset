package com.rklaehn.interval

import org.junit.Assert._
import org.junit.Test
import spire.algebra.Order
import spire.math.Interval.Closed
import spire.math._

class IntervalSetTest {

  import IntervalSet._

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

  @Test(expected = classOf[NumberFormatException])
  def parseLargeIntegerTest(): Unit = {
    val text = "[1000000000000000000000000]"
    IntervalSet(text)
  }

  @Test
  def predefinedTypesTest(): Unit = {
    def testEdge[T:IntervalSetElement](value:T) : Unit = {
      implicit val order : Order[T] = implicitly[IntervalSetElement[T]].ops
      assertEquals(IndexedSeq(value), IntervalSet.above(value).edges.toIndexedSeq)
      assertEquals(Interval.above(value).toString, IntervalSet.above(value).toString)
    }

    def testInterval[T:IntervalSetElement](min:T, max:T) : Unit = {
      implicit val ops = implicitly[IntervalSetElement[T]].ops
      val interval = Interval.fromBounds(Closed(min), Closed(max))
      val intervalSet = IntervalSet(interval)
      assertEquals(IndexedSeq(min,max), intervalSet.edges.toIndexedSeq)
      assertEquals(interval.toString, intervalSet.toString)

    }

    //Int
    testEdge(1.toByte)
    testEdge(-1.toByte)
    testInterval(Byte.MinValue, Byte.MaxValue)

    //Short
    testEdge(1.toShort)
    testEdge(-1.toShort)
    testInterval(Short.MinValue, Short.MaxValue)

    //Int
    testEdge(1)
    testEdge(-1)
    testInterval(Int.MinValue, Int.MaxValue)

    //Long
    testEdge(1L)
    testEdge(-1L)
    testInterval(Long.MinValue, Long.MaxValue)

    //Float
    testEdge(1.0f)
    testEdge(-1.0f)
    testInterval(Float.MinValue, Float.MaxValue)

    //Double
    testEdge(1.0)
    testEdge(-1.0)
    testInterval(Double.MinValue, Double.MaxValue)

    //UByte
    testEdge(UByte(1))
    testInterval(UByte.MinValue, UByte.MaxValue)

    //UShort
    testEdge(UShort(1))
    testInterval(UShort.MinValue, UShort.MaxValue)

    //UInt
    testEdge(UInt(1))
    testInterval(UInt.MinValue, UInt.MaxValue)

    //ULong
    testEdge(ULong(1))
    testInterval(ULong.MinValue, ULong.MaxValue)
  }

  @Test(expected = classOf[IllegalArgumentException])
  def doubleRejectsNaNTest(): Unit = {
    IntervalSet.above(Double.NaN)
  }

  @Test(expected = classOf[IllegalArgumentException])
  def floatRejectsNaNTest(): Unit = {
    IntervalSet.above(Float.NaN)
  }

  @Test
  def atIsSameAsApplyTest(): Unit = {
    val is = IntervalSet.above(1)
    is.at(1) == is.apply(1)
  }

  @Test
  def subsetOfTest(): Unit = {
    assertTrue(IntervalSet.above(1).isSupersetOf(IntervalSet.above(1)))
    assertTrue(IntervalSet.atOrAbove(1).isSupersetOf(IntervalSet.above(1)))
    assertFalse(IntervalSet.above(1).isSupersetOf(IntervalSet.atOrAbove(1)))

    assertFalse(IntervalSet.above(1).isProperSupersetOf(IntervalSet.above(1)))
    assertTrue(IntervalSet.atOrAbove(1).isProperSupersetOf(IntervalSet.above(1)))
    assertFalse(IntervalSet.above(1).isProperSupersetOf(IntervalSet.atOrAbove(1)))
  }

  @Test(expected = classOf[IllegalArgumentException])
  def concatExceptionTest(): Unit = {
    IntervalTrie.concat(IntervalTrie.Leaf(0,false, false), IntervalTrie.Leaf(0,false, false))
  }

  @Test
  def bothUnapplyTest(): Unit = {
    assertEquals(None, IntervalSet.Both.unapply(IntervalTrie.Leaf(0,false,false)))
  }

  @Test
  def algebraTest(): Unit = {
    val algebra = IntervalSetAlgebra.booleanAlgebra[Long]
    val a = IntervalSet.above(1L)
    val b = IntervalSet.below(1L)
    assertEquals(a ^ b, algebra.xor(a, b))
  }
}
