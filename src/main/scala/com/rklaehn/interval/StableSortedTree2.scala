package com.rklaehn.interval

import spire.implicits._
import spire.algebra._
import spire.math
import spire.math.Interval

import scala.collection.AbstractTraversable
import scala.collection.immutable.HashSet

object StableSortedTree2 {

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

  def single[K: Partitioner, V: Monoid](key: K,  before: V, after: V): Node[K, V] =
    Leaf(key, before, after)

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

  def elements[K, V](s: Node[K, V]): Traversable[(K, V, V)] = {
    new AbstractTraversable[(K, V, V)] {
      def foreach0[U](n: AnyRef, f: ((K, V, V)) => U): Unit = n match {
        case l: Leaf[K, V] => f((l.p, l.at, l.sign))
        case Branch(_, _, l, r) =>
          foreach0(l, f)
          foreach0(r, f)
        case _ =>
      }

      def foreach[U](f: ((K, V, V)) => U) = foreach0(s, f)
    }

  }

  def truncate[K, V](a: Node[K, V], i: IntervalSeq[K])(implicit xor: Monoid[V]): Node[K, V] = a match {
    case a: Leaf[K, V] =>
      val at_p = i.at(a.p)
      val above_p = i.above(a.p)
      if(at_p && above_p)
        a
      else if(at_p || above_p) {
        val above0 = xor.op(a.sign, a.at)
        val at1 = if (at_p) a.at else xor.id
        val above1 = if (above_p) above0 else xor.id
        val sign1 = xor.op(at1, above1)
        Leaf(a.p, at1, sign1)
      } else
        null
    case a: Branch[K, V] =>
      val l1 = truncate(a.l, i)
      val r1 = truncate(a.r, i)
      if((l1 eq a.l) && (r1 eq a.r))
        a
      else {
        val l1_defined = l1 ne null
        val r1_defined = r1 ne null
        if(l1_defined && r1_defined) a.copy(l = l1, r = r1)
        else if(l1_defined) l1
        else if(r1_defined) r1
        else null
      }
  }

  def structuralEquals[K: Eq, V: Eq](a: Node[K, V], b: Node[K, V]): Boolean = (a, b) match {
    case (a: Branch[K, V], b: Branch[K, V]) =>
      a.p == b.p && a.hw == b.hw && structuralEquals[K, V](a.l, b.l) && structuralEquals[K, V](a.r, b.r)
    case (a: Leaf[K, V], b: Leaf[K, V]) =>
      a.p === b.p && a.at === b.at && a.sign === b.sign
    case _ => false
  }

  @inline
  private[interval] def v[V: Monoid](x: Node[_, V]): V = x match {
    case x:Leaf[_, V] => x.sign
    case x:Branch[_, V] => x.sign
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
          Leaf(a.p, combine(a.at, b.at), combine(a.sign, b.sign))
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

    def sign: V
  }

  case class Branch[K, V](p: K, hw: K, l: Node[K, V], r: Node[K, V])(implicit m: Monoid[V]) extends Node[K, V] {

    lazy val sign = m.op(l.sign, r.sign)
  }

  case class Leaf[K, V](p: K, at: V, sign: V) extends Node[K, V]
}