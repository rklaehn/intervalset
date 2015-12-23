package com.rklaehn.interval

import org.scalacheck.Properties
import org.scalacheck.Prop._
import spire.implicits._

import scala.collection.immutable.SortedSet

object SetIntervalMapCheck extends Properties("SetIntervalsSetConsistentWithIntervalSeq") {

  implicit def setEq[T] = spire.optional.genericEq.generic[Set[T]]

  implicit def sortedSet[T] = spire.optional.genericEq.generic[SortedSet[T]]

  implicit val arb = IntervalSeqArbitrary.arbitrary

  private def toIntervalMap(x: IntervalSeq[Int], value: Int): IntervalMap[Int, SortedSet[Int]] =
    IntervalMap.FromBool(x.intervals.toSeq.map(_ → SortedSet(value)): _*)

  private def toIntervalSeq(x: IntervalMap[Int, SortedSet[Int]], value: Int): IntervalSeq[Int] =
    x.entries.filter(_._2.contains(value)).foldLeft(IntervalSeq.empty[Int]) { case (x, y) ⇒ x ^ IntervalSeq(y._1) }

  def toIntervalsSet(m: Map[Int, IntervalSeq[Int]]): IntervalMap[Int, SortedSet[Int]] = {
    val empty = IntervalMap.FromBool.zero[Int, SortedSet[Int]]
    m.foldLeft(empty) {
      case (a, (v, i)) ⇒
        a ^ toIntervalMap(i, v)
    }
  }

  def binarySampleTest(
    am: Map[Int, IntervalSeq[Int]],
    bm: Map[Int, IntervalSeq[Int]],
    op: (IntervalMap[Int, SortedSet[Int]], IntervalMap[Int, SortedSet[Int]]) ⇒ IntervalMap[Int, SortedSet[Int]],
    reference: (IntervalSeq[Int], IntervalSeq[Int]) ⇒ IntervalSeq[Int]
  ): Boolean = {
    val as = toIntervalsSet(am)
    val bs = toIntervalsSet(bm)
    val rs = op(as, bs)
    (am.keys ++ bm.keys).forall { k ⇒
      val ai = am.getOrElse(k, IntervalSeq.empty[Int])
      val bi = bm.getOrElse(k, IntervalSeq.empty[Int])
      val ri = toIntervalSeq(rs, k)
      reference(ai, bi) == ri
    }
  }

  def unarySampleTest(
    am: Map[Int, IntervalSeq[Int]],
    op: IntervalMap[Int, SortedSet[Int]] ⇒ IntervalMap[Int, SortedSet[Int]],
    reference: IntervalSeq[Int] ⇒ IntervalSeq[Int]
  ): Boolean = {
    val as = toIntervalsSet(am)
    val rs = op(as)
    am.keys.forall { k ⇒
      val ai = am.getOrElse(k, IntervalSeq.empty[Int])
      val ri = toIntervalSeq(rs, k)
      reference(ai) == ri
    }
  }

  property("roundtrip") = forAll { xm: Map[Int, IntervalSeq[Int]] ⇒
    val combined = toIntervalsSet(xm)
    xm.forall {
      case (v, i) ⇒
        i == toIntervalSeq(combined, v)
    }
  }

  property("xor") = forAll { (am: Map[Int, IntervalSeq[Int]], bm: Map[Int, IntervalSeq[Int]]) ⇒
    binarySampleTest(am, bm, _ ^ _, _ ^ _)
  }

  property("and") = forAll { (am: Map[Int, IntervalSeq[Int]], bm: Map[Int, IntervalSeq[Int]]) ⇒
    binarySampleTest(am, bm, _ & _, _ & _)
  }

  property("or") = forAll { (am: Map[Int, IntervalSeq[Int]], bm: Map[Int, IntervalSeq[Int]]) ⇒
    binarySampleTest(am, bm, _ | _, _ | _)
  }
}
