package com.rklaehn.interval

import spire.algebra.{Monoid, Order}

import spire.implicits._
import Order.ordering
import spire.math.Interval
import spire.math.interval.{EmptyBound, Open, Unbound, Closed}
import scala.collection.immutable.{HashSet, SortedMap}
import StableSortedTree2._

object IntervalsTrie {

  private implicit def valueMonoid[V: Order]: Monoid[SortedMap[V, Int]] = new Monoid[SortedMap[V, Int]] {
    def id = SortedMap.empty[V, Int]

    def op(x: SortedMap[V, Int], y: SortedMap[V, Int]): SortedMap[V, Int] = if(x.size < y.size) op(y, x) else {
      y.foldLeft(x) { case (r, (k, count)) =>
        val count1 = count + r.getOrElse(k, 0)
        if(count1 != 0) r.updated(k, count1)
        else r - k
      }
    }
  }

  def apply[K: Partitioner, V: Order](elems: (Interval[K], V)*) = {
    if(elems.isEmpty) empty[K, V]
    else elems.map { case (k,v) => single(k, v) }.reduce(_ merge _)
  }

  def empty[K: Partitioner, V: Order]: IntervalsTrie[K, V] =
    new IntervalsTrie[K, V](SortedMap.empty, null)

  def single[K: Partitioner, V: Order](i: Interval[K], v: V): IntervalsTrie[K, V] = {
    implicit val vm = valueMonoid[V]
    val inc = SortedMap(v -> 1)
    val dec = SortedMap(v -> -1)
    val none = SortedMap.empty[V, Int]
    def point(k: K): IntervalsTrie[K, V] =
      new IntervalsTrie[K, V](none, Leaf(k, inc, none))
    def below(k: K): IntervalsTrie[K, V] =
      new IntervalsTrie[K, V](inc, Leaf(k, dec, dec))
    def atOrBelow(k: K): IntervalsTrie[K, V] =
      new IntervalsTrie[K, V](inc, Leaf(k, none, dec))
    def above(k: K): IntervalsTrie[K, V] =
      new IntervalsTrie[K, V](none, Leaf(k, none, inc))
    def atOrAbove(k: K): IntervalsTrie[K, V] =
      new IntervalsTrie[K, V](none, Leaf(k, inc, inc))
    def fromTo(a: K, ai:Boolean, b: K, bi: Boolean) =
      new IntervalsTrie[K, V](none,
        merge(
          Leaf(a, if(ai) inc else none, inc),
          Leaf(b, if(bi) none else dec, dec)
        )
      )
    def all = new IntervalsTrie[K, V](inc, null)
    i.fold {
      case (Closed(a), Closed(b)) if a == b => point(a)
      case (Unbound(), Open(x)) => below(x)
      case (Unbound(), Closed(x)) => atOrBelow(x)
      case (Open(x), Unbound()) => above(x)
      case (Closed(x), Unbound()) => atOrAbove(x)
      case (Closed(a), Closed(b)) => fromTo(a, true, b, true)
      case (Closed(a), Open(b)) => fromTo(a, true, b, false)
      case (Open(a), Closed(b)) => fromTo(a, false, b, true)
      case (Open(a), Open(b)) => fromTo(a, false, b, false)
      case (Unbound(), Unbound()) => all
      case (EmptyBound(), EmptyBound()) => empty[K, V]
    }
  }
}

class IntervalsTrie[K, V] private (private val initial: SortedMap[V, Int], private val root: Node[K, SortedMap[V, Int]])(implicit p: Partitioner[K], m: Monoid[SortedMap[V, Int]]) {

  implicit def order = p.o

  def values: Set[V] =
    StableSortedTree2.elements(root).foldLeft(initial.keys.to[HashSet]) { case (s, (_, before, after)) =>
      s ++ before.keys ++ after.keys
    }

  def at(k: K): SortedMap[V, Int] = {
    def delta(node: Node[K, SortedMap[V, Int]]): SortedMap[V, Int] = node match {
      case x:Branch[K, SortedMap[V, Int]] =>
        if(k < x.p) delta(x.l)
        else m.op(x.l.delta, delta(x.r))
      case x:Leaf[K, SortedMap[V, Int]] =>
        val kp = k.compare(x.p)
        if(kp < 0) m.id
        else if(kp == 0) x.before
        else x.delta
      case _ => m.id
    }
    m.op(initial, delta(root))
  }

  def merge(that: IntervalsTrie[K, V]) = {
    val root1 = StableSortedTree2.merge(this.root, that.root)
    val initial1 = m.op(this.initial, that.initial)
    new IntervalsTrie[K, V](initial1, root1)
  }

  def elements: Traversable[(K, SortedMap[V, Int], SortedMap[V, Int])] = StableSortedTree2.elements(root)
}