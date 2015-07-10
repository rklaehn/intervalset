package com.rklaehn.interval

import spire.algebra.{AdditiveMonoid, Order, Monoid}

import scala.annotation.tailrec

import spire.implicits._

object ArrayReducerTest extends App {
  val cache = ArrayReducer.Cache.simple[Int, Int]
  val a = (0 until 100).toArray
  val mid: (Int, Int) => Int = (a, b) => (a + b) / 2
  def sum = ArrayReducer.reduce[Int, Int](a, 0, 100, 10, identity, mid, cache)(implicitly[Order[Int]], implicitly[AdditiveMonoid[Int]].additive)
  println(sum)
  println(sum)
}

abstract class Reducer[CC, S] extends ((Int, Int) => S) {

  def a: CC

  def from: Int

  def until: Int

  def split(from: Int, to: Int): Int

  def at(i: Int): S

  def empty: S

  def combine(a: S, b: S): S

  def minW: Int

  def linear(i0: Int, i1: Int): S = {
    var r = empty
    var i = i0
    while(i < i1) {
      r = combine(r, at(i))
      i += 1
    }
    r
  }

  def applyCached(from: Int, to: Int): S = apply(from, to)

  def apply(from: Int, until: Int): S = {
    val w = until - from
    if(w < minW) linear(from, until)
    else {
      val mid = split(from, until)
      // only call through the cache if both bounds have been produced split
      combine(
        if(from == this.from) apply(from, mid) else applyCached(from, mid),
        if(until == this.until) apply(mid, until) else applyCached(mid, until)
      )
    }
  }

  def result: S = apply(from, until)
}

object ArrayReducer {

  type Cache[CC, S] = (Int, Int, CC, (Int, Int) => S) => S

  object Cache {
    def none[CC, S]: Cache[CC, S] = (from, until, _, f) => f(from, until)

    def simple[E, S]: Cache[Array[E], S] = new ((Int, Int, Array[E], (Int, Int) => S) => S) {

      val cache = scala.collection.mutable.Map.empty[Seq[E], S]

      def apply(from: Int, until: Int, a: Array[E], f: (Int, Int) => S) = {
        cache.getOrElseUpdate(a.toSeq, {
          println(s"Miss $from $until")
          f(from, until)
        })
      }
    }
  }

  def reduce[E: Order, S: Monoid]
  (
    a: Array[E],
    from: Int,
    until: Int,
    minW: Int,
    single: E => S,
    pivot: (E, E) => E,
    cache: Cache[Array[E], S]
    ): S = {
    val r = new ArrayReducer[E, S](a, from, until, minW, single, pivot, cache)
    r.result
  }

  private class ArrayReducer[E, S](
    val a: Array[E],
    val from:Int,
    val until: Int,
    val minW: Int,
    single: E => S,
    pivot: (E,E) => E,
    cache: ArrayReducer.Cache[Array[E], S]
    )(implicit m: Monoid[S], o: Order[E]) extends Reducer[Array[E], S] {

    def empty = m.id

    def combine(a: S, b: S) = m.op(a, b)

    def at(i: Int) = single(a(i))

    override def applyCached(from: Int, until: Int) =
      cache(from, until, a.slice(from, until), this)

    def split(i0: Int, i1: Int): Int =
      if (i0 == i1) i0
      else {
        val p = pivot(a(i0), a(i1 - 1))
        val i = binarySearch(p, i0, i1)
        if (i >= 0) i else -i - 1
      }

    def binarySearch(key: E, i0: Int, i1: Int): Int = {
      @tailrec
      def binarySearch0(low: Int, high: Int): Int =
        if (low <= high) {
          val mid = (low + high) >>> 1
          val midVal = a(mid)
          val c = o.compare(midVal, key)
          if (c < 0)
            binarySearch0(mid + 1, high)
          else if (c > 0)
            binarySearch0(low, mid - 1)
          else
            mid
        } else -(low + 1)
      binarySearch0(i0, i1 - 1)
    }
  }

}