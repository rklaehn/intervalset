package scala.collection.immutable

import org.scalacheck.Properties
import org.scalacheck.Prop._
import spire.syntax.all._
import spire.std.any._

object IntervalTrieSampleProperties extends Properties("TreeValueSet2.Sample") {

  import IntervalSetBooleanAlgebra.algebra
  import IntervalSetTestOps._

  // this will resolve to the Arbitrary instance for Boolean from scalacheck
  implicit def arb = IntervalSetArbitrary.arbitrary

  // a test that works by sampling the result at all relevant places and checks consistency with the boolean operation
  def unarySampleTest(a:IntervalSet[Long], r:IntervalSet[Long], op:Boolean => Boolean) = {
    val support = a.support.toArray.sorted.distinct
    support.forall { value =>
      val sameBefore = r.below(value) === op(a.below(value))
      val sameAt = r.at(value) === op(a.at(value))
      val sameAfter = r.above(value) === op(a.above(value))
      sameBefore & sameAt & sameAfter
    }
  }

  // a test that works by sampling the result at all relevant places and checks consistency with the boolean operation
  def binarySampleTest(a:IntervalSet[Long], b:IntervalSet[Long], r:IntervalSet[Long], op:(Boolean, Boolean) => Boolean) = {
    val support = (a.support ++ b.support).toArray.sorted.distinct
    support.forall { value =>
      val sameBefore = r.below(value) === op(a.below(value), b.below(value))
      val sameAt = r.at(value) === op(a.at(value), b.at(value))
      val sameAfter = r.above(value) === op(a.above(value), b.above(value))
      sameBefore & sameAt & sameAfter
    }
  }

  // a test that works by sampling the result at all relevant places and checks consistency with the boolean operation
  def trinarySampleTest(a:IntervalSet[Long], b:IntervalSet[Long], c:IntervalSet[Long], r:IntervalSet[Long], op:(Boolean, Boolean, Boolean) => Boolean) = {
    val support = (a.support ++ b.support).toArray.sorted.distinct
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

//  property("equality") = forAll { a: IntervalTrie =>
//    ~a === negate(a)
//  }
//
//  property("toStringParse") = forAll { a: IntervalTrie =>
//    val atext = IntervalTrie.format(a)
//    val b = IntervalTrie.parse(atext)
//    val btext = IntervalTrie.format(b)
//    a == b
//  }
}