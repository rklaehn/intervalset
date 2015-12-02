package com.rklaehn.interval

import org.scalatest.FunSuite
import spire.algebra.Order
import spire.math.interval.Closed
import spire.math._

class IntervalTrieTest extends FunSuite {

  import IntervalTrie._

  test("leafOperation") {
    val a = above(1L)
    val b = atOrAbove(1L)
    val c = point(1L)
    val d = hole(1L)
    assert(atOrAbove(1L) === (a | b))
    assert(above(1L) === (a & b))
    assert(point(1L) === (a ^ b))
    assert(atOrAbove(1L) === (a | c))
    assert(empty[Long] === (a & c))
    assert(atOrAbove(1L) === (a ^ c))
    assert(hole(1L) === (a | d))
    assert(above(1L) === (a & d))
    assert(below(1L) === (a ^ d))
  }

  test("parseLargeInteger") {
    intercept[NumberFormatException] {
      val text = "[1000000000000000000000000]"
      IntervalTrie(text)
    }
  }

  test("predefinedTypes") {

    def testEdge[T: Element](value: T): Unit = {
      implicit val order: Order[T] = implicitly[Element[T]].ops
      assert(IndexedSeq(value) === IntervalTrie.above(value).edges.toIndexedSeq)
      assert(Interval.above(value).toString === IntervalTrie.above(value).toString)
    }

    def testInterval[T: Element](min: T, max: T): Unit = {
      implicit val ops = implicitly[Element[T]].ops
      val interval = Interval.fromBounds(Closed(min), Closed(max))
      val intervalSet = IntervalTrie(interval)
      assert(IndexedSeq(min, max) === intervalSet.edges.toIndexedSeq)
      assert(interval.toString === intervalSet.toString)
    }

    //Byte
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

    //Char
    testEdge(Char.MinValue)
    testEdge(Char.MaxValue)
    testInterval(Char.MinValue, Char.MaxValue)
  }

  test("doubleRejectsNaN") {
    intercept[IllegalArgumentException] {
      IntervalTrie.above(Double.NaN)
    }
  }

  test("floatRejectsNaN") {
    intercept[IllegalArgumentException] {
      IntervalTrie.above(Float.NaN)
    }
  }

  test("atIsSameAsApply") {
    val is = IntervalTrie.above(1)
    assert(is.at(1) === is.apply(1))
  }

  test("subsetOf") {
    assert(IntervalTrie.above(1).isSupersetOf(IntervalTrie.above(1)))
    assert(IntervalTrie.atOrAbove(1).isSupersetOf(IntervalTrie.above(1)))
    assert(!IntervalTrie.above(1).isSupersetOf(IntervalTrie.atOrAbove(1)))

    assert(!IntervalTrie.above(1).isProperSupersetOf(IntervalTrie.above(1)))
    assert(IntervalTrie.atOrAbove(1).isProperSupersetOf(IntervalTrie.above(1)))
    assert(!IntervalTrie.above(1).isProperSupersetOf(IntervalTrie.atOrAbove(1)))
  }

  test("concatException") {
    intercept[IllegalArgumentException] {
      Tree.concat(Tree.Leaf(0, false, false), Tree.Leaf(0, false, false))
    }
  }

  test("algebra") {
    val algebra = IntervalTrieAlgebra.booleanAlgebra[Long]
    val a = IntervalTrie.above(1L)
    val b = IntervalTrie.below(1L)
    assert((a ^ b) === algebra.xor(a, b))
  }

  test("charAdditiveMonoid") {
    import IntervalTrie.CharElement.ops._
    assert(0 === zero.toInt)
    assert('a' === plus('a', zero))
  }

  test("iteratorAfterEnd") {
    intercept[NoSuchElementException] {
      val all = IntervalTrie.empty[Int]
      val it = all.intervalIterator
      it.next()
    }
  }
}
