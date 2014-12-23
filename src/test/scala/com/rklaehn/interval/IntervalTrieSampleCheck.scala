package com.rklaehn.interval

import org.scalacheck.Properties
import org.scalacheck.Prop._
import spire.syntax.all._
import spire.std.any._

object IntervalTrieSampleCheck extends Properties("IntervalSet.Sample") {

  // this will resolve to the Arbitrary instance for Boolean from scalacheck
  implicit def arb = IntervalSetArbitrary.arbitrary

  // a test that works by sampling the result at all relevant places and checks consistency with the boolean operation
  def unarySampleTest(a:IntervalSet[Long], r:IntervalSet[Long], op:Boolean => Boolean) = {
    val support = a.edges.toArray.sorted.distinct
    support.forall { value =>
      val sameBefore = r.below(value) === op(a.below(value))
      val sameAt = r.at(value) === op(a.at(value))
      val sameAfter = r.above(value) === op(a.above(value))
      sameBefore & sameAt & sameAfter
    }
  }

  // a test that works by sampling the result at all relevant places and checks consistency with the boolean operation
  def binarySampleTest(a:IntervalSet[Long], b:IntervalSet[Long], r:IntervalSet[Long], op:(Boolean, Boolean) => Boolean) = {
    val support = (a.edges ++ b.edges).toArray.sorted.distinct
    support.forall { value =>
      val sameBefore = r.below(value) === op(a.below(value), b.below(value))
      val sameAt = r.at(value) === op(a.at(value), b.at(value))
      val sameAfter = r.above(value) === op(a.above(value), b.above(value))
      sameBefore & sameAt & sameAfter
    }
  }

  // a test that works by sampling the result at all relevant places and checks consistency with the boolean operation
  def trinarySampleTest(a:IntervalSet[Long], b:IntervalSet[Long], c:IntervalSet[Long], r:IntervalSet[Long], op:(Boolean, Boolean, Boolean) => Boolean) = {
    val support = (a.edges ++ b.edges ++ c.edges).toArray.sorted.distinct
    support.forall { value =>
      val sameBefore = r.below(value) === op(a.below(value), b.below(value), c.below(value))
      val sameAt = r.at(value) === op(a.at(value), b.at(value), c.at(value))
      val sameAfter = r.above(value) === op(a.above(value), b.above(value), c.above(value))
      sameBefore & sameAt & sameAfter
    }
  }

  property("sample_not") = forAll { a: IntervalSet[Long] =>
    unarySampleTest(a, ~a, ~_)
  }

  property("sample_and") = forAll { (a: IntervalSet[Long], b: IntervalSet[Long]) =>
    binarySampleTest(a, b, a & b, _ & _)
  }

  property("sample_or") = forAll { (a: IntervalSet[Long], b: IntervalSet[Long]) =>
    binarySampleTest(a, b, a | b, _ | _)
  }

  property("sample_xor") = forAll { (a: IntervalSet[Long], b: IntervalSet[Long]) =>
    binarySampleTest(a, b, a ^ b, _ ^ _)
  }

  property("toStringParse") = forAll { a: IntervalSet[Long] =>
    val aText = a.toString
    val b = IntervalSet(aText)
    a == b
  }

  property("isContiguous") = forAll { a: IntervalSet[Long] =>
    a.isContiguous == (a.intervals.size <= 1)
  }

  property("hull") = forAll { a: IntervalSet[Long] =>
    val hullSet = IntervalSet(a.hull)
    val outside = ~hullSet
    val nothingOutside = (a & outside) == IntervalSet.zero[Long]
    val allInside = a.intervals.forall(i => hullSet.isSupersetOf(IntervalSet(i)))
    nothingOutside & allInside
  }

  /**
   * Check optimized intersects method against naive implementation using &
   */
  property("intersects/intersection") = forAll { (a: IntervalSet[Long], b: IntervalSet[Long]) =>
    val r1 = a intersects b
    val r2 = !(a & b).isEmpty
    r1 == r2
  }

  /**
   * Check optimized isSupersetOf method against naive implementation using &
   */
  property("isSupersetOf/intersection") = forAll { (a: IntervalSet[Long], b: IntervalSet[Long]) =>
    val r1 = a isSupersetOf b
    val r2 = (a & b) == b
    r1 == r2
  }

  property("isSupersetOf") = forAll { (a: IntervalSet[Long], x: Long) =>
    val b = a & IntervalSet.atOrAbove(x)
    a isSupersetOf b
  }

  property("disjoint") = forAll { (s: IntervalSet[Long], x: Long) =>
    val a = s & IntervalSet.below(x)
    val b = s & IntervalSet.atOrAbove(x)
    !(a intersects b)
  }
}