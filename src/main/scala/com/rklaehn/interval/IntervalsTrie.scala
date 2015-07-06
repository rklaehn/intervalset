package com.rklaehn.interval

import spire.algebra.{Monoid, Order}

import spire.implicits._
import Order.ordering
import spire.math.Interval
import spire.math.interval.{EmptyBound, Open, Unbound, Closed}
import scala.collection.immutable.HashSet
import StableSortedTree2._

import scala.reflect.ClassTag

object IntervalsTrie {

  private implicit def valueXorMonoid[V: Order]: Monoid[HashSet[V]] = new Monoid[HashSet[V]] {
    def id = HashSet.empty[V]

    def op(x: HashSet[V], y: HashSet[V]): HashSet[V] = (x diff y) union (y diff x)
  }

  def apply[K: Partitioner, V: Order](elems: (Interval[K], V)*) = {
    if(elems.isEmpty) empty[K, V]
    else elems.map { case (k,v) => single(k, v) }.reduce(_ xor _)
  }

  def empty[K: Partitioner, V: Order]: IntervalsTrie[K, V] =
    new IntervalsTrie[K, V](HashSet.empty, null)

  def single[K: Partitioner, V: Order](i: Interval[K], v: V): IntervalsTrie[K, V] = {
    implicit val vm = valueXorMonoid[V]
    val sv = HashSet(v)
    val none = HashSet.empty[V]
    def point(k: K): IntervalsTrie[K, V] =
      new IntervalsTrie[K, V](none, Leaf(k, sv, none))
    def below(k: K): IntervalsTrie[K, V] =
      new IntervalsTrie[K, V](sv, Leaf(k, sv, sv))
    def atOrBelow(k: K): IntervalsTrie[K, V] =
      new IntervalsTrie[K, V](sv, Leaf(k, none, sv))
    def above(k: K): IntervalsTrie[K, V] =
      new IntervalsTrie[K, V](none, Leaf(k, none, sv))
    def atOrAbove(k: K): IntervalsTrie[K, V] =
      new IntervalsTrie[K, V](none, Leaf(k, sv, sv))
    def fromTo(a: K, ai:Boolean, b: K, bi: Boolean) =
      new IntervalsTrie[K, V](none,
        merge(
          Leaf(a, if(ai) sv else none, sv),
          Leaf(b, if(bi) none else sv, sv)
        )
      )
    def all = new IntervalsTrie[K, V](sv, null)
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

class IntervalsTrie[K, V] private (val belowAll: HashSet[V], private val root: Node[K, HashSet[V]])(implicit p: Partitioner[K], m: Monoid[HashSet[V]]) {

  implicit def order = p.o

  def intervals(v: V)(implicit c: ClassTag[K]): IntervalSeq[K] = {
    val belowAll = this.belowAll(v)
    val values = Array.newBuilder[K]
    val kinds = Array.newBuilder[Byte]
    var current = belowAll
    this.elements.foreach { case (k, below, delta) =>
      val changeBelow = below.contains(v)
      val changeDelta = delta.contains(v)
      if(changeBelow || changeDelta) {
        val kind = (changeBelow ^ current, changeDelta ^ current) match {
          case (false, false) => IntervalSeq.K00
          case (true, false) => IntervalSeq.K10
          case (false, true) => IntervalSeq.K01
          case (true, true) => IntervalSeq.K11
        }
        values += k
        kinds += kind
      }
      current ^= changeDelta
    }
    new IntervalSeq[K](belowAll, values.result(), kinds.result(), p.o)
  }

  def values: HashSet[V] =
    StableSortedTree2.elements(root).foldLeft(belowAll.to[HashSet]) { case (s, (_, before, delta)) =>
      s ++ before ++ delta
    }

  def at(k: K): HashSet[V] = {
    def delta(node: Node[K, HashSet[V]]): HashSet[V] = node match {
      case x:Branch[K, HashSet[V]] =>
        if(k < x.p) delta(x.l)
        else m.op(x.l.sign, delta(x.r))
      case x:Leaf[K, HashSet[V]] =>
        val kp = k.compare(x.p)
        if(kp < 0) m.id
        else if(kp == 0) x.at
        else x.sign
      case _ => m.id
    }
    m.op(belowAll, delta(root))
  }

  def truncate(min: K, max: K): IntervalsTrie[K, V] = {
    val tree1 = StableSortedTree2.truncate(root, IntervalSeq(Interval.open(min, max)))
    val atmin = StableSortedTree2.Leaf(min, at(min), at(min))
    val atmax = StableSortedTree2.Leaf(max, m.id, at(max))
    val tree2 = StableSortedTree2.merge(tree1, atmin)
    val tree3 = StableSortedTree2.merge(tree2, atmax)
    new IntervalsTrie(m.id, tree3)
  }

  def xor(that: IntervalsTrie[K, V]) = {
    val root1 = StableSortedTree2.merge(this.root, that.root)
    val initial1 = m.op(this.belowAll, that.belowAll)
    new IntervalsTrie[K, V](initial1, root1)
  }

  def elements: Traversable[(K, HashSet[V], HashSet[V])] = StableSortedTree2.elements(root)
}