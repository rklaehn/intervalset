package com.rklaehn.interval

import spire.implicits._
import spire.algebra.{AdditiveAbGroup, Order}
import spire.math

import scala.collection.AbstractTraversable

sealed trait StableSortedSeq[K, V]

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

  def single[K: Partitioner, V](key: K, value: V): StableSortedSeq[K, V] =
    Node(key, implicitly[Partitioner[K]].g.additive.id, null, null, value)

  def keys[K, V](s: StableSortedSeq[K, V]): Traversable[K] = {
    new AbstractTraversable[K] {
      def foreach0[U](n: Node[K, V], f: K => U): Unit = {
        if ((n.l eq null) && (n.r eq null))
          f(n.p)
        else {
          foreach0(n.l, f)
          foreach0(n.r, f)
        }
      }

      def foreach[U](f: (K) => U) = foreach0(s.asInstanceOf[Node[K, V]], f)
    }

  }

  def merge[K: Partitioner, V](a: StableSortedSeq[K, V], b: StableSortedSeq[K, V]): StableSortedSeq[K, V] =
    new Merger[K, V].apply(a.asInstanceOf[Node[K, V]], b.asInstanceOf[Node[K, V]])

  class Merger[K, V](implicit p: Partitioner[K]) {

    implicit def additive: AdditiveAbGroup[K] = p.g

    implicit def order: Order[K] = p.o

    def combine(a: V, b: V): V = a

    def nodeAbove(l:  Node[K, V], r: Node[K, V]): Node[K, V] = {
      require(l.p < r.p)
      val v1 = combine(l.v, r.v)
      val (p1, hw1) = p.partition(l.p, r.p)
      Node(p1, hw1, l, r, v1)
    }

    def withL(n: Node[K, V], l1: Node[K, V]): Node[K, V] = if(l1 eq null) n else {
      require(l1.p + l1.hw <= n.p)
      require(l1.p - l1.hw >= n.p - n.hw)
      n.copy(l = l1, v = combine(l1.v, n.r.v))
    }

    def withR(n: Node[K, V], r1: Node[K, V]): Node[K, V] = if(r1 eq null) n else {
      require(r1.p - r1.hw >= n.p)
      require(r1.p + r1.hw <= n.p + n.hw)
      n.copy(r = r1, v = combine(n.l.v, r1.v))
    }

    def apply(a: Node[K, V], b: Node[K, V]): Node[K, V] = if(a eq null) b else if(b eq null) a else {
      val p_ab = a.p compare b.p
      if (p_ab == 0) {
        if(a.hw.isZero && !b.hw.isZero) {
          withR(b, apply(a, b.r))
        } else if(b.hw.isZero && !a.hw.isZero) {
          withR(a, apply(a.r, b))
        } else {
          // nodes have exactly the same pivot
          // just merge them
          // a   |
          // b   |
          val p1 = a.p
          val l1 = apply(a.l, b.l)
          val r1 = apply(a.r, b.r)
          val v1 = combine(a.v, b.v)
          val hw1 = math.max(a.hw, b.hw)
          Node(p1, hw1, l1, r1, v1)
        }
      } else if (p_ab < 0) {
        // a is below b
        // a |
        // b     |
        val hw_ab = a.hw compare b.hw
        if(hw_ab == 0) {
          // they have the same half width, so they are guaranteed not to overlap
          // we can just create a node above the two
          nodeAbove(a, b)
        } else if(hw_ab < 0) {
          // a is smaller than b and to the left of b
          if(a.p - a.hw >= b.p - b.hw)
            withL(b, apply(a, b.l))
          else
            nodeAbove(a, b)
        } else {
          // b is smaller than a and to the right of a
          if(b.hw.isZero) {
            if(a.p + a.hw > b.p)
              withR(a, apply(a.r, b))
            else
              nodeAbove(a, b)
          } else {
            if (a.p + a.hw >= b.p + b.hw)
              withR(a, apply(a.r, b))
            else
              nodeAbove(a, b)
          }
        }
      } else {
        // a is above b
        // a     |
        // b |
        val hw_ab = a.hw compare b.hw
        if(hw_ab == 0) {
          // they have the same half width, so they are guaranteed not to overlap
          // we can just create a node above the two
          nodeAbove(b, a)
        } else if(hw_ab < 0) {
          if(a.hw.isZero) {
            // a is smaller than b and to the right of b
            if (a.p < b.p + b.hw)
              withR(b, apply(a, b.r))
            else
              nodeAbove(b, a)
          } else {
            // a is smaller than b and to the right of b
            if (a.p + a.hw <= b.p + b.hw)
              withR(b, apply(a, b.r))
            else
              nodeAbove(b, a)
          }
        } else {
          // b is smaller than a and to the left of a
          if(a.p - a.hw <= b.p - b.hw)
            withL(a, apply(a.l, b))
          else
            nodeAbove(b, a)
        }
      }
    }
  }

  case class Node[K, V](p: K, hw: K, l: Node[K, V], r: Node[K, V], v: V) extends StableSortedSeq[K, V]
}
