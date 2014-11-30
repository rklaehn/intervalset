package scala.collection.immutable

import org.scalacheck.{Gen, Arbitrary}
import spire.syntax.all._

object IntervalTrieArbitrary {
  import IntervalTrie._
  import IntervalTrieTestOps._
  import IntervalTrieBooleanAlgebra.algebra

  def interval(a:Long, ai:Boolean, b:Long, bi:Boolean) : IntervalTrie = {
    val result = if(a<b) {
      // a normal interval
      val start = Leaf(a, ai, true)
      val end = Leaf(b, bi, false)
      zero merge start merge end
    } else if(a>b) {
      // a hole
      val start = Leaf(b, bi, false)
      val end = Leaf(a, ai, true)
      one merge start merge end
    } else {
      if(ai ^ bi)
        zero merge Leaf(a, true, false)
      else
        one merge Leaf(a, false, true)
    }
    result
  }

  def makeProfileXor(support:Array[Long], included:Array[Boolean]) : IntervalTrie = {
    require(support.length == included.length)
    val result = (support.indices.dropRight(1) by 2).foldLeft(zero : IntervalTrie) {
      case (current, i) =>
        val a = support(i)
        val ai = included(i)
        val b = support(i+1)
        val bi = included(i+1)
        current ^ interval(a, ai, b, bi)
    }
    result
  }

  def makeProfile(start: Boolean, support: Array[Long], change: Array[Int]): IntervalTrie = {
    require(support.distinct.sorted.toIndexedSeq == support.toIndexedSeq)
    require(support.length == change.length)
    require(change.forall(x => x >= 0 && x < 3))
    def one = IntervalTrie.one.asInstanceOf[Leaf]
    def zero = IntervalTrie.zero.asInstanceOf[Leaf]
    val cuts = Array.newBuilder[Leaf]
    if (support.isEmpty || support.head != 0) {
      cuts += (if (start) one else zero)
    }
    var current = start
    support zip (change) map { case (value, change) =>
      val cut = (current, change) match {
        case (false, 0) => Leaf(value, true, true)
        case (false, 1) => Leaf(value, false, true)
        case (false, 2) => Leaf(value, true, false)
        case (true, 0) => Leaf(value, false, false)
        case (true, 1) => Leaf(value, true, false)
        case (true, 2) => Leaf(value, false, true)
        case _ => ???
      }
      current = cut.after
      cuts += cut
    }
    val result = IntervalTrie(cuts.result(): _*)
    //    println(result)
    //    println(TreeValueSet2.format(result))
    result
  }

  private def randomProfile(min: Long, max: Long, count: Int): Gen[IntervalTrie] = {
    for {
      start <- Gen.oneOf(false, true)
      randomLongs <- Gen.resize(count, Gen.containerOf[Array, Long](Gen.choose(min, max)))
      support = randomLongs.sorted.distinct
      change <- Gen.containerOfN[Array, Int](support.length, Gen.choose(0, 2))
    } yield
      makeProfile(start, support, change.take(support.length))
  }

  private def randomProfileXor(min: Long, max: Long, count: Int): Gen[IntervalTrie] = {
    for {
      support <- Gen.resize(count, Gen.containerOf[Array, Long](Gen.choose(min, max)))
      included <- Gen.containerOfN[Array, Boolean](support.length, Gen.oneOf(false, true))
    } yield
      makeProfileXor(support, included)
  }

  private def randomProfileGen(size:Int) = Gen.frequency(
    1 -> zero,
    1 -> one,
    // 30 -> randomProfile(0, 100, size)
    30 -> randomProfileXor(0, 100, size)
  )

//  val arbs = {
//    val gen = randomProfileGen(1000)
//    val p = Gen.Parameters.default.withSize(10000).withRng(new Random(0))
//    val result = (0 until 100).flatMap(_ => gen(p)).toArray
//    println(result(0))
//    result
//  }
//
//  val arbitrary = Arbitrary(Gen.oneOf(arbs))

  val arbitrary = Arbitrary(randomProfileGen(10))
}
