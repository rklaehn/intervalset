package com.rklaehn.interval

import spire.implicits._
import spire.algebra._
import spire.math
import Order.ordering

import scala.collection.AbstractTraversable
import scala.collection.immutable.SortedMap

class IntervalMap[K, V] private (private val initial: SortedMap[V, Int], private val root: StableSortedSeq.Node[K, SortedMap[V, Int]])(implicit p: StableSortedSeq.Partitioner[K], m: Monoid[SortedMap[V, Int]]) {

  implicit def order = p.o

  def value: SortedMap[V, Int] = StableSortedSeq.v[SortedMap[V, Int]](root)

  def at(k: K): SortedMap[V, Int] = {
    def delta(node: StableSortedSeq.Node[K, SortedMap[V, Int]]): SortedMap[V, Int] = node match {
      case x:StableSortedSeq.Branch[K, SortedMap[V, Int]] =>
        if(k < x.p) delta(x.l)
        else m.op(x.l.v, delta(x.r))
      case x:StableSortedSeq.Leaf[K, SortedMap[V, Int]] =>
        if(k < x.p) m.id
        else x.v
      case _ => m.id
    }
    m.op(initial, delta(root))
  }

  def merge(that: IntervalMap[K, V]) = {
    val root1 = StableSortedSeq.merge(this.root, that.root)
    val initial1 = m.op(this.initial, that.initial)
    new IntervalMap[K, V](initial1, root1)
  }

  def elements: Traversable[(K, SortedMap[V, Int])] = StableSortedSeq.elements(root)
}

object IntervalMap {

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

  def empty[K: StableSortedSeq.Partitioner, V: Order]: IntervalMap[K, V] =
    new IntervalMap[K, V](SortedMap.empty[V, Int], null)

  def fromTo[K: StableSortedSeq.Partitioner, V: Order](min: K, max: K, v: V): IntervalMap[K, V] = {
    val d0 = StableSortedSeq.single[K, SortedMap[V, Int]](min, SortedMap(v -> 1))
    val d1 = StableSortedSeq.single[K, SortedMap[V, Int]](max, SortedMap(v -> -1))
    val tree = StableSortedSeq.merge[K, SortedMap[V, Int]](d0, d1)
    new IntervalMap[K, V](SortedMap.empty[V, Int], tree)
  }
}

class SortedSet[K: StableSortedSeq.Partitioner, V: Monoid] private (private val root: StableSortedSeq.Node[K, V]) {

  def value: V = StableSortedSeq.v[V](root)

  def merge(that: SortedSet[K, V]): SortedSet[K, V] = {
    val root1 = StableSortedSeq.merge(this.root, that.root)
    new SortedSet[K, V](root1)
  }

  def isEmpty: Boolean = root eq null

  def keys: Traversable[K] = StableSortedSeq.keys(root)
}

object SortedSet {

  def empty[K: StableSortedSeq.Partitioner, V: Monoid]: SortedSet[K, V] = new SortedSet[K, V](null)

  def single[K: StableSortedSeq.Partitioner, V: Monoid](k: K, v: V) = new SortedSet[K, V](StableSortedSeq.single(k, v))
}

object StableSortedSeq {

  sealed trait Partitioner[K] {
    def partition(a: K, b: K): (K, K)
    def g: AdditiveAbGroup[K]
    def o: Order[K]
  }

  implicit val LongPartitioner: Partitioner[Long] = new Partitioner[Long] {
    def partition(a: Long, b: Long): (Long, Long) = {
      if(a == b) (a, 0L)
      else if((a.signum < 0) != (b.signum < 0)) (0L, Long.MaxValue)
      else {
        val bit = java.lang.Long.highestOneBit(a ^ b)
        val mask = ~(bit - 1)
        (math.max(a, b) & mask, bit)
      }
    }

    val g = implicitly[AdditiveAbGroup[Long]]

    val o = implicitly[Order[Long]]
  }

  def single[K: Partitioner, V](key: K, value: V): Node[K, V] =
    Leaf(key, value)

  def keys[K, V](s: Node[K, V]): Traversable[K] = {
    new AbstractTraversable[K] {
      def foreach0[U](n: AnyRef, f: K => U): Unit = n match {
        case l: (K, V) => f(l._1)
        case Branch(_, _, l, r) =>
          foreach0(l, f)
          foreach0(r, f)
        case _ =>
      }

      def foreach[U](f: (K) => U) = foreach0(s, f)
    }

  }

  def values[K, V](s: Node[K, V]): Traversable[V] = {
    new AbstractTraversable[V] {
      def foreach0[U](n: Node[K, V], f: V => U): Unit = n match {
        case Leaf(_, v) => f(v)
        case Branch(_, _, l, r) =>
          foreach0(l, f)
          foreach0(r, f)
        case _ =>
      }

      def foreach[U](f: (V) => U) = foreach0(s, f)
    }

  }

  def elements[K, V](s: Node[K, V]): Traversable[(K, V)] = {
    new AbstractTraversable[(K, V)] {
      def foreach0[U](n: AnyRef, f: ((K, V)) => U): Unit = n match {
        case l: Leaf[K, V] => f((l.p, l.v))
        case Branch(_, _, l, r) =>
          foreach0(l, f)
          foreach0(r, f)
        case _ =>
      }

      def foreach[U](f: ((K, V)) => U) = foreach0(s, f)
    }

  }

  def structuralEquals[K: Eq, V: Eq](a: Node[K, V], b: Node[K, V]): Boolean = (a, b) match {
    case (a: Branch[K, V], b: Branch[K, V]) =>
      a.p == b.p && a.hw == b.hw && structuralEquals[K, V](a.l, b.l) && structuralEquals[K, V](a.r, b.r)
    case (a: Leaf[K, V], b: Leaf[K, V]) =>
      a.p === b.p && a.v === b.v
    case _ => false
  }

  @inline
  private[interval] def v[V: Monoid](x: Node[_, V]): V = x match {
    case x:Leaf[_, V] => x.v
    case x:Branch[_, V] => x.v
    case _ => implicitly[Monoid[V]].id
  }

  def merge[K: Partitioner, V: Monoid](a: Node[K, V], b: Node[K, V]): Node[K, V] =
    new Merger[K, V].apply(a, b)

  class Merger[K, V](implicit p: Partitioner[K], v: Monoid[V]) {

    implicit def additive: AdditiveAbGroup[K] = p.g

    implicit def order: Order[K] = p.o

    def combine(a: V, b: V): V = v.op(a, b)

    def nodeAbove(l: Node[K, V], r: Node[K, V]): Branch[K, V] = {
      require(l.p < r.p)
      val (p1, hw1) = p.partition(l.p, r.p)
      Branch(p1, hw1, l, r)
    }

    def withL(n: Branch[K, V], l1: Node[K, V]): Branch[K, V] = if(l1 eq null) n else {
//      require(l1.p + l1.hw <= n.p)
//      require(l1.p - l1.hw >= n.p - n.hw)
      n.copy(l = l1)
    }

    def withR(n: Branch[K, V], r1: Node[K, V]): Branch[K, V] = if(r1 eq null) n else {
//      require(r1.p - r1.hw >= n.p)
//      require(r1.p + r1.hw <= n.p + n.hw)
      n.copy(r = r1)
    }

    def apply(a: Node[K, V], b: Node[K, V]): Node[K, V] = (a, b) match {
      case (a: Branch[K, V], b: Branch[K, V]) =>
        val p_ab = a.p compare b.p
        if (p_ab == 0) {
          // nodes have exactly the same pivot
          // just merge them
          // a   |
          // b   |
          val p1 = a.p
          val l1 = apply(a.l, b.l)
          val r1 = apply(a.r, b.r)
          val hw1 = math.max(a.hw, b.hw)
          Branch(p1, hw1, l1, r1)
        } else if (p_ab < 0) {
          // a is below b
          // a |
          // b     |
          val hw_ab = a.hw compare b.hw
          if (hw_ab == 0) {
            // they have the same half width, so they are guaranteed not to overlap
            // we can just create a node above the two
            nodeAbove(a, b)
          } else if (hw_ab < 0) {
            // a is smaller than b and to the left of b
            if (a.p - a.hw >= b.p - b.hw)
              withL(b, apply(a, b.l))
            else
              nodeAbove(a, b)
          } else {
            // b is smaller than a and to the right of a
            if (a.p + a.hw >= b.p + b.hw)
              withR(a, apply(a.r, b))
            else
              nodeAbove(a, b)
          }
        } else {
          // a is above b
          // a     |
          // b |
          val hw_ab = a.hw compare b.hw
          if (hw_ab == 0) {
            // they have the same half width, so they are guaranteed not to overlap
            // we can just create a node above the two
            nodeAbove(b, a)
          } else if (hw_ab < 0) {
            // a is smaller than b and to the right of b
            if (a.p + a.hw <= b.p + b.hw)
              withR(b, apply(a, b.r))
            else
              nodeAbove(b, a)
          } else {
            // b is smaller than a and to the left of a
            if (a.p - a.hw <= b.p - b.hw)
              withL(a, apply(a.l, b))
            else
              nodeAbove(b, a)
          }
        }
      case (a: Branch[K, V], b: Leaf[K, V]) =>
        val p_ab = a.p compare b.p
        if (p_ab <= 0) {
          // b.p >= a.p
          if (a.p + a.hw > b.p)
            withR(a, apply(a.r, b))
          else
            nodeAbove(a, b)
        } else {
          // b.p < a.p
          if (a.p - a.hw <= b.p)
            withL(a, apply(a.l, b))
          else
            nodeAbove(b, a)
        }
      case (a: Leaf[K, V], b: Branch[K, V]) =>
        val p_ab = a.p compare b.p
        if (p_ab >= 0) {
          if(a.p < b.p + b.hw)
            withR(b, apply(a, b.r))
          else
            nodeAbove(b, a)
        } else {
          // a.p < b.p
          if (a.p >= b.p - b.hw)
            withL(b, apply(a, b.l))
          else
            nodeAbove(a, b)
        }
      case (a: Leaf[K, V], b: Leaf[K, V]) =>
        val p_ab = a.p compare b.p
        if(p_ab == 0)
          Leaf(a.p, combine(a.v, b.v))
        else if(p_ab < 0)
          nodeAbove(a, b)
        else
          nodeAbove(b, a)
      case (a, null) => a
      case (null, b) => b
      case _ => null
    }
  }

  sealed abstract class Node[K, V] {
    def p: K

    def v: V
  }

  case class Branch[K, V](p: K, hw: K, l: Node[K, V], r: Node[K, V])(implicit m: Monoid[V]) extends Node[K, V] {

    lazy val v = m.op(l.v, r.v)
  }

  case class Leaf[K, V](p: K, v: V) extends Node[K, V]
}
