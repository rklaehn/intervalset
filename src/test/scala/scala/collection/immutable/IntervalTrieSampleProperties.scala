package scala.collection.immutable

import org.scalacheck.Properties
import org.scalacheck.Prop._
import spire.syntax.all._
import spire.std.any._

object IntervalTrieSampleProperties extends Properties("TreeValueSet2.Sample") {

  import IntervalTrieTestOps._

  import IntervalTrieBooleanAlgebra.algebra

  // this will resolve to the Arbitrary instance for Boolean from scalacheck
  implicit def arb = IntervalTrieArbitrary.arbitrary

  // a test that works by sampling the result at all relevant places and checks consistency with the boolean operation
  def unarySampleTest(a:IntervalTrie, r:IntervalTrie, op:Boolean => Boolean) = {
    val support = a.support.toArray.sorted.distinct
    support.forall { value =>
      val sameBefore = (value == 0) || (r.before(value) === op(a.before(value)))
      val sameAt = r.at(value) === op(a.at(value))
      val sameAfter = r.after(value) === op(a.after(value))
      sameBefore & sameAt & sameAfter
    }
  }

  // a test that works by sampling the result at all relevant places and checks consistency with the boolean operation
  def binarySampleTest(a:IntervalTrie, b:IntervalTrie, r:IntervalTrie, op:(Boolean, Boolean) => Boolean) = {
    val support = (a.support ++ b.support).toArray.sorted.distinct
    support.forall { value =>
      val sameBefore = (value == 0) || (r.before(value) === op(a.before(value), b.before(value)))
      val sameAt = r.at(value) === op(a.at(value), b.at(value))
      val sameAfter = r.after(value) === op(a.after(value), b.after(value))
      if(!sameBefore)
        println("huh")
      sameBefore & sameAt & sameAfter
    }
  }

  // a test that works by sampling the result at all relevant places and checks consistency with the boolean operation
  def trinarySampleTest(a:IntervalTrie, b:IntervalTrie, c:IntervalTrie, r:IntervalTrie, op:(Boolean, Boolean, Boolean) => Boolean) = {
    val support = (a.support ++ b.support).toArray.sorted.distinct
    support.forall { value =>
      val sameBefore = (value == 0) || (r.before(value) === op(a.before(value), b.before(value), c.before(value)))
      val sameAt = r.at(value) === op(a.at(value), b.at(value), c.at(value))
      val sameAfter = r.after(value) === op(a.after(value), b.after(value), c.after(value))
      sameBefore & sameAt & sameAfter
    }
  }

  def test = {
    import IntervalTrie._
    val a = zero merge point(29) merge point(51) merge point(88)
    val b = zero merge startAt(52)
    val na = (a flip true)
    val r = (a flip true) | b
    val r0 = negate(a) | b
    println("a   =" + IntervalTrie.format(a))
    println("!a  =" + IntervalTrie.format(na))
    println("b   =" + IntervalTrie.format(b))
    println("!a|b=" + IntervalTrie.format(r))
    println("!a|b=" + IntervalTrie.format(r0))

    println("a   =" + a)
    println("!a  =" + na)
    println("b   =" + b)
    println("!a|b=" + r)
    println("!a|b=" + r0)
    val ok = binarySampleTest(a, b, r, !_ | _)
    println(ok)
    ok
  }

  property("sample_not") = forAll { a: IntervalTrie =>
    unarySampleTest(a, ~a, ~_)
  }

  property("sample_and") = forAll { (a: IntervalTrie, b: IntervalTrie) =>
    binarySampleTest(a, b, a & b, _ & _)
  }

  property("sample_or") = forAll { (a: IntervalTrie, b: IntervalTrie) =>
    binarySampleTest(a, b, a | b, _ | _)
  }

  property("sample_xor") = forAll { (a: IntervalTrie, b: IntervalTrie) =>
    binarySampleTest(a, b, a ^ b, _ ^ _)
  }

  property("equality") = forAll { a: IntervalTrie =>
    ~a === negate(a)
  }
}
