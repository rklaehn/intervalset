package com.rklaehn.interval

import org.scalacheck.Properties
import org.scalacheck.Prop._
import spire.implicits._

import scala.collection.immutable.SortedSet

object SetIntervalMapCheck extends Properties("SetIntervalsSetConsistentWithIntervalSeq") {

  implicit val arb = IntervalSeqArbitrary.arbitrary

  private def toIntervalMap(x: IntervalSeq[Long], value: Int): IntervalMap[Long, SortedSet[Int]] =
    IntervalMap(x.intervals.toSeq.map(_ → SortedSet(value)): _*)

  private def toIntervalSeq(x: IntervalMap[Long, SortedSet[Int]], value: Int): IntervalSeq[Long] =
    x.entries.filter(_._2.contains(value)).foldLeft(IntervalSeq.empty[Long]) { case (x, y) ⇒ x ^ IntervalSeq(y._1) }

  def toIntervalsSet(m: Map[Int, IntervalSeq[Long]]): IntervalMap[Long, SortedSet[Int]] = {
    val empty = IntervalMap.empty[Long, SortedSet[Int]]
    m.foldLeft(empty) {
      case (a, (v, i)) ⇒
        a ^ toIntervalMap(i, v)
    }
  }

  def binarySampleTest(
    am: Map[Int, IntervalSeq[Long]],
    bm: Map[Int, IntervalSeq[Long]],
    op: (IntervalMap[Long, SortedSet[Int]], IntervalMap[Long, SortedSet[Int]]) ⇒ IntervalMap[Long, SortedSet[Int]],
    reference: (IntervalSeq[Long], IntervalSeq[Long]) ⇒ IntervalSeq[Long]
  ): Boolean = {
    val as = toIntervalsSet(am)
    val bs = toIntervalsSet(bm)
    val rs = op(as, bs)
    (am.keys ++ bm.keys).forall { k ⇒
      val ai = am.getOrElse(k, IntervalSeq.empty[Long])
      val bi = bm.getOrElse(k, IntervalSeq.empty[Long])
      val ri = toIntervalSeq(rs, k)
      reference(ai, bi) == ri
    }
  }

  def unarySampleTest(
    am: Map[Int, IntervalSeq[Long]],
    op: IntervalMap[Long, SortedSet[Int]] ⇒ IntervalMap[Long, SortedSet[Int]],
    reference: IntervalSeq[Long] ⇒ IntervalSeq[Long]
  ): Boolean = {
    val as = toIntervalsSet(am)
    val rs = op(as)
    (am.keys).forall { k ⇒
      val ai = am.getOrElse(k, IntervalSeq.empty[Long])
      val ri = toIntervalSeq(rs, k)
      reference(ai) == ri
    }
  }

  property("roundtrip") = forAll { xm: Map[Int, IntervalSeq[Long]] ⇒
    val combined = toIntervalsSet(xm)
    xm.forall {
      case (v, i) ⇒
        i == toIntervalSeq(combined, v)
    }
  }

  property("xor") = forAll { (am: Map[Int, IntervalSeq[Long]], bm: Map[Int, IntervalSeq[Long]]) ⇒
    binarySampleTest(am, bm, _ ^ _, _ ^ _)
  }

  property("and") = forAll { (am: Map[Int, IntervalSeq[Long]], bm: Map[Int, IntervalSeq[Long]]) ⇒
    binarySampleTest(am, bm, _ & _, _ & _)
  }

  property("or") = forAll { (am: Map[Int, IntervalSeq[Long]], bm: Map[Int, IntervalSeq[Long]]) ⇒
    binarySampleTest(am, bm, _ | _, _ | _)
  }
}
