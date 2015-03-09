package com.rklaehn.rope

import org.scalacheck.{Gen, Arbitrary, Properties}
import org.scalacheck.Prop._
import spire.implicits._
import spire.math._

object RopeCheck extends Properties("Rope") {

  property("concat/apply") = forAll { a:Array[Array[Int]] =>
    val nea = a.filterNot(_.isEmpty)
    if(nea.isEmpty)
      true
    else {
      val rope = nea.map(Rope.apply).reduce(_ ++ _)
      val concat = nea.reduce(_ ++ _)
      println(rope.length + " " + rope.depth)
      rope.length == concat.length && concat.indices.forall(i => concat(i) == rope(i))
    }
  }

  property("concat/apply with rebalance") = forAll { a:Array[Array[Int]] =>
    val nea = a.filterNot(_.isEmpty)
    if(nea.isEmpty)
      true
    else {
      val items = a.flatten.map(i => Array(i))
      val rope = items.map(Rope.apply).reduce(_ ++ _)
      val concat = nea.reduce(_ ++ _)
      rope.length == concat.length && concat.indices.forall(i => concat(i) == rope(i))
    }
  }

  property("search") = forAll { r:SortedRope =>
    val rope = r.rope
    val indices = 0 until rope.length
    def ordered = indices.init.forall(i => rope(i) < rope(i + 1))
    def normalSearch = indices.forall { i =>
      val key = rope(i)
      val sr = rope.search(0, rope.length, key)
      sr == SearchResult(i)
    }
    def sliceSearch = indices.forall { i =>
      val key = rope(i)
      val sr = Slice(rope).search(key)
      sr == SearchResult(i)
    }
    def normalSearchBefore = indices.forall { i =>
      val key = rope(i) - 1
      val sr = rope.search(0, rope.length, key)
      sr == SearchResult.before(i)
    }
    def sliceSearchBefore = indices.forall { i =>
      val key = rope(i) - 1
      val sr = Slice(rope).search(key)
      sr == SearchResult.before(i)
    }
    def normalSearchAfter = indices.forall { i =>
      val key = rope(i) + 1
      val sr = rope.search(0, rope.length, key)
      sr == SearchResult.before(i + 1)
    }
    def sliceSearchAfter = indices.forall { i =>
      val key = rope(i) + 1
      val sr = Slice(rope).search(key)
      sr == SearchResult.before(i + 1)
    }
    ordered &&
      normalSearch &&
      sliceSearch &&
      normalSearchBefore &&
      sliceSearchBefore &&
      normalSearchAfter &&
      sliceSearchAfter
  }

  property("search2") = forAll { r:SortedRope =>
    val rope = r.rope
    val indices = 0 until rope.length
    def ordered = indices.init.forall(i => rope(i) < rope(i + 1))
    def normalSearch(w: Int) = indices.forall { i =>
      val key = rope(i)
      val from = max(i - w, 0)
      val until = min(i + w + 1, rope.length)
      val sr = rope.search(from, until, key)
      sr == SearchResult(i)
    }
    def normalSearchBefore(w: Int) = indices.forall { i =>
      val key = rope(i) - 1
      val from = max(i - w, 0)
      val until = min(i + w + 1, rope.length)
      val sr = rope.search(from, until, key)
      sr == SearchResult.before(i)
    }
    def normalSearchAfter(w: Int) = indices.forall { i =>
      val key = rope(i) + 1
      val from = max(i - w, 0)
      val until = min(i + w + 1, rope.length)
      val sr = rope.search(from, until, key)
      sr == SearchResult.before(i + 1)
    }
    ordered &&
      normalSearch(4) &&
      normalSearch(1) &&
      normalSearch(0) &&
      normalSearchAfter(0) &&
      normalSearchBefore(0)
  }

  case class SortedRope(rope:Rope[Int])

  implicit lazy val arbitrarySortedRope: Arbitrary[SortedRope] = Arbitrary {
    for {
      values <- Gen.nonEmptyContainerOf[Array, Int](Arbitrary.arbInt.arbitrary)
      sorted = values.map(_ * 2).sorted.distinct
      splits <- Gen.containerOf[Array, Int](Gen.choose(0, sorted.size - 1))
      sortedSplits = (splits :+ 0 :+ sorted.length).sorted.distinct
    } yield {
      val slices =
        for(i <- sortedSplits.indices.init)
        yield sorted.slice(sortedSplits(i), sortedSplits(i + 1))
      val rope = slices.map(Rope.apply).reduce(_ ++ _)
      SortedRope(rope)
    }
  }
}
