package com.rklaehn.interval

import language.implicitConversions
import spire.algebra.{Eq, Bool, Order}
import spire.math.Interval
import spire.math.interval._
import spire.algebra.Order.ordering
import spire.implicits._

import scala.collection.AbstractTraversable
import scala.collection.immutable.SortedSet

sealed abstract class IntervalMap[K, V] { lhs ⇒
  import IntervalMap._

  def belowAll: V

  def unary_~(implicit x: Bool[V]): IntervalMap[K, V] =
    negate[K, V](this)

  def ^(rhs: IntervalMap[K, V])(implicit o: Order[K], v: Value[V]): IntervalMap[K, V] =
    new Xor[K, V](lhs, rhs).result

  def &(rhs: IntervalMap[K, V])(implicit o: Order[K], v: Value[V]): IntervalMap[K, V] =
    new And[K, V](lhs, rhs).result

  def |(rhs: IntervalMap[K, V])(implicit o: Order[K], v: Value[V]): IntervalMap[K, V] =
    new Or[K, V](lhs, rhs).result

  def entries(implicit ev0: Order[K], ev1: Value[V]): Traversable[(Interval[K], V)]

  def values(implicit v: Value[V]): V
}

object IntervalMap {

  trait Value[V] extends Eq[V] {

    def zero: V

    def isOne(x: V): Boolean

    def isZero(x: V): Boolean

    def xor(a: V, b: V): V

    def and(a: V, b: V): V

    def or(a: V, b: V): V

    def andNot(a: V, b: V): V
  }

  object Value {

    def apply[V: Value]: Value[V] = implicitly[Value[V]]

    implicit object booleanIsValue extends Value[Boolean] {

      def eqv(x: Boolean, y: Boolean) = x == y

      def zero = false

      def andNot(a: Boolean, b: Boolean) = a & (!b)

      def or(a: Boolean, b: Boolean) = a | b

      def and(a: Boolean, b: Boolean) = a & b

      def xor(a: Boolean, b: Boolean) = a ^ b

      def isZero(x: Boolean) = !x

      def isOne(x: Boolean) = x
    }

    implicit def setIsValue[T]: Value[Set[T]] = new Value[Set[T]] {

      def eqv(x: Set[T], y: Set[T]) = x == y

      val zero = Set.empty[T]

      def andNot(a: Set[T], b: Set[T]) = a diff b

      def or(a: Set[T], b: Set[T]) = a union b

      def and(a: Set[T], b: Set[T]) = a intersect b

      def xor(a: Set[T], b: Set[T]) = (a diff b) union (b diff a)

      def isZero(x: Set[T]) = x.isEmpty

      def isOne(x: Set[T]) = false
    }

    implicit def sortedSetIsValue[T: Order]: Value[SortedSet[T]] = new Value[SortedSet[T]] {

      def eqv(x: SortedSet[T], y: SortedSet[T]) = x == y

      val zero = SortedSet.empty[T]

      def andNot(a: SortedSet[T], b: SortedSet[T]) = a diff b

      def or(a: SortedSet[T], b: SortedSet[T]) = a union b

      def and(a: SortedSet[T], b: SortedSet[T]) = a intersect b

      def xor(a: SortedSet[T], b: SortedSet[T]) = (a diff b) union (b diff a)

      def isZero(x: SortedSet[T]) = x.isEmpty

      def isOne(x: SortedSet[T]) = false
    }

    implicit def boolAndEqIsValue[T](implicit e: Eq[T], bool: Bool[T]): Value[T] = new Value[T] {

      def zero = Bool[T].zero

      def andNot(a: T, b: T) = a & ~b

      def or(a: T, b: T) = bool.or(a, b)

      def isOne(x: T) = e.eqv(x, bool.one)

      def isZero(x: T) = e.eqv(x, bool.zero)

      def and(a: T, b: T) = a & b

      def xor(a: T, b: T) = a ^ b

      def eqv(x: T, y: T) = e.eqv(x, y)
    }
  }

  private implicit def intervalsSetIsImpl[K, V](x: IntervalMap[K, V]): Impl[K, V] =
    x.asInstanceOf[Impl[K, V]]

  private final class Impl[K, V](
                                  val belowAll: V,
                                  val edges: Array[IntervalMap.Edge[K, V]]) extends IntervalMap[K, V] {
    lhs ⇒

    override def equals(rhs: Any) = rhs match {
      case rhs: Impl[K, V] =>
        lhs.belowAll == rhs.belowAll && lhs.edges === rhs.edges
      case _ ⇒
        false
    }

    def values(implicit v: Value[V]) = edges.foldLeft(belowAll) {
      case (a, c) ⇒
        v.or(a, v.or(c.at, c.above))
    }

    override def hashCode: Int =
      (belowAll.hashCode, edges.toIndexedSeq.hashCode).hashCode

    override def toString: String =
      s"IntervalsSet($belowAll, ${edges.toIndexedSeq})"

    def entries(implicit ev0: Order[K], ev1: Value[V]) = new AbstractTraversable[(Interval[K], V)] {
      override def foreach[U](f: ((Interval[K], V)) => U): Unit = foreachInterval(f)
    }

    private def foreachInterval[U](f: ((Interval[K], V)) => U)(implicit o: Order[K], v: Value[V]): Unit = {
      var prevBound: Bound[K] = Unbound[K]()
      var prevValue: V = belowAll
      for (c <- edges) {
        val below = prevValue
        val at = c.at
        val above = c.above
        prevBound = (!v.eqv(below, at), !v.eqv(at, above)) match {
          case (true, true) ⇒
            // change both below and above. We have to produce a point
            f((Interval.fromBounds(prevBound, Open(c.x)), below))
            f((Interval.point(c.x), at))
            Open(c.x)
          case (true, false) ⇒
            // change just below. The preceding interval must be open
            f((Interval.fromBounds(prevBound, Open(c.x)), below))
            Closed(c.x)
          case (false, true) ⇒
            // change just above. The preceding interval must be closed
            f((Interval.fromBounds(prevBound, Closed(c.x)), below))
            Open(c.x)
          case (false, false) ⇒
            // no change at all. we should not ever get here with a valid set
            prevBound
        }
        prevValue = above
      }
      f((Interval.fromBounds(prevBound, Unbound()), prevValue))
    }
  }

  def all[K: Order, V: Value: Bool]: IntervalMap[K, V] =
    IntervalMap(Bool[V].one, Array.empty[Edge[K, V]])

  def constant[K: Order, V: Value](value: V): IntervalMap[K, V] =
    IntervalMap(value, Array.empty[Edge[K, V]])

  def empty[K: Order, V: Value]: IntervalMap[K, V] =
    IntervalMap(Value[V].zero, Array.empty[Edge[K, V]])

  def point[K: Order, V: Value](x: K, value: V): IntervalMap[K, V] = {
    val empty = Value[V].zero
    IntervalMap(empty, Array(Edge(x, value, empty)))
  }

  def hole[K: Order, V: Value](x: K, value: V): IntervalMap[K, V] = {
    val empty = Value[V].zero
    IntervalMap(value, Array(Edge(x, empty, value)))
  }

  def atOrAbove[K: Order, V: Value](x: K, value: V): IntervalMap[K, V] = {
    val empty = Value[V].zero
    val delta = value
    IntervalMap(empty, Array(Edge(x, value, value)))
  }

  def above[K: Order, V: Value](x: K, value: V): IntervalMap[K, V] = {
    val empty = Value[V].zero
    IntervalMap[K, V](empty, Array(Edge(x, empty, value)))
  }

  def atOrBelow[K: Order, V: Value](x: K, value: V): IntervalMap[K, V] = {
    val empty = Value[V].zero
    IntervalMap(value, Array(Edge(x, value, empty)))
  }

  def below[K: Order, V: Value](x: K, value: V): IntervalMap[K, V] = {
    val empty = Value[V].zero
    IntervalMap(value, Array(Edge(x, empty, empty)))
  }

  def apply[K: Order, V: Value](iv: (Interval[K], V)*): IntervalMap[K,V] = {
    val singles = iv.map { case (i,v) ⇒ IntervalMap(i, v) }
    Reducer.reduce(singles)(_ | _).getOrElse(empty)
  }

  def apply[K: Order, V: Value](interval: Interval[K], value: V): IntervalMap[K, V] = {
    val vzero = Value[V].zero
    interval.fold {
      case (Closed(a), Closed(b)) if a == b => point(a, value)
      case (Unbound(), Open(x)) => below(x, value)
      case (Unbound(), Closed(x)) => atOrBelow(x, value)
      case (Open(x), Unbound()) => above(x, value)
      case (Closed(x), Unbound()) => atOrAbove(x, value)
      case (Closed(a), Closed(b)) => fromTo(Edge(a, value, value), Edge(b, value, vzero))
      case (Closed(a), Open(b)) => fromTo(Edge(a, value, value), Edge(b, vzero, vzero))
      case (Open(a), Closed(b)) => fromTo(Edge(a, vzero, value), Edge(b, value, vzero))
      case (Open(a), Open(b)) => fromTo(Edge(a, vzero, value), Edge(b, vzero, vzero))
      case (Unbound(), Unbound()) => constant[K, V](value)
      case (EmptyBound(), EmptyBound()) => empty[K, V]
    }
  }

  private def negate[K, V: Bool](x: Impl[K, V]): IntervalMap[K, V] = {
    val b = Bool[V]
    val belowAll1 = ~x.belowAll
    val edges1 = x.edges.map(e ⇒ Edge(e.x, ~e.at, ~e.above))
    new Impl(belowAll1, edges1)
  }

  private def fromTo[K, V: Value](a: Edge[K, V], b: Edge[K, V]): IntervalMap[K, V] =
    IntervalMap(Value[V].zero, Array(a, b))

  private def apply[K, V](belowAll: V, edges: Array[Edge[K, V]]) =
    new Impl[K, V](belowAll, edges)

  case class Edge[K, V](x: K, at: V, above: V)

  object Edge {

    implicit def eqv[K, V]: spire.algebra.Eq[Edge[K, V]] = spire.optional.genericEq.generic[Edge[K, V]]
  }

  private abstract class MergeOperation[K, V] {

    def same(a: V, b: V): Boolean = vValue.eqv(a, b)

    def isZero(x: V): Boolean = vValue.isZero(x)

    def isOne(x: V): Boolean = vValue.isOne(x)

    def and(a: V, b: V): V = vValue.and(a, b)

    def or(a: V, b: V): V = vValue.or(a, b)

    def xor(a: V, b: V): V = vValue.xor(a, b)

    def andNot(a: V, b: V) = vValue.andNot(a, b)

    def kOrder: Order[K]

    def vValue: Value[V]

    def lhs: Impl[K, V]

    def rhs: Impl[K, V]

    def r0: V

    protected[this] val as = lhs.edges

    protected[this] val bs = rhs.edges

    protected[this] val rs = Array.ofDim[Edge[K, V]](as.length + bs.length)

    protected[this] var ri = 0

    def collision(ai: Int, bi: Int): Unit

    def fromA(a0: Int, a1: Int, b: Int): Unit

    def fromB(a: Int, b0: Int, b1: Int): Unit

    def binarySearch(array: Array[Edge[K, V]], key: K, from: Int, until: Int): Int = {
      var low = from
      var high = until - 1
      while (low <= high) {
        val mid = (low + high) >>> 1
        val midVal = array(mid)
        val c = kOrder.compare(midVal.x, key)
        if (c < 0) {
          low = mid + 1
        }
        else if (c > 0) {
          high = mid - 1
        }
        else {
          // scalastyle:off return
          return mid
          // scalastyle:on return
        }
      }
      -(low + 1)
    }

    def merge0(a0: Int, a1: Int, b0: Int, b1: Int): Unit = {
      if (a0 == a1) {
        if(b0 != b1)
          fromB(a0, b0, b1)
      } else if (b0 == b1) {
        fromA(a0, a1, b0)
      } else {
        val am = (a0 + a1) / 2
        val res = binarySearch(bs, as(am).x, b0, b1)
        if (res >= 0) {
          // same elements
          val bm = res
          // merge everything below a(am) with everything below the found element
          merge0(a0, am, b0, bm)
          // add the elements a(am) and b(bm)
          collision(am, bm)
          // merge everything above a(am) with everything above the found element
          merge0(am + 1, a1, bm + 1, b1)
        } else {
          val bm = -res - 1
          // merge everything below a(am) with everything below the found insertion point
          merge0(a0, am, b0, bm)
          // add a(am)
          fromA(am, am + 1, bm)
          // everything above a(am) with everything above the found insertion point
          merge0(am + 1, a1, bm, b1)
        }
      }
    }

    def add(c: Edge[K, V]): Unit = {
      rs(ri) = c
      ri += 1
    }

    def result: IntervalMap[K, V] = {
      merge0(0, as.length, 0, bs.length)
      IntervalMap(r0, rs.take(ri))
    }
  }

  private abstract class BinaryOp[K, V] extends MergeOperation[K, V] {

    val r0 = op(lhs.belowAll, rhs.belowAll)
    var aCurr = lhs.belowAll
    var bCurr = rhs.belowAll
    def rCurr = if(ri == 0) r0 else rs(ri - 1).above

    def op(a: V, b: V): V

    def edge(x: K, below: V, at: V, above: V): Unit = {
      if (!same(below, at) || !same(at, above)) {
        add(Edge(x, at, above))
      }
    }

    def collision(ai: Int, bi: Int) = {
      val a = as(ai)
      val b = bs(bi)
      edge(a.x, rCurr, op(a.at, b.at), op(a.above, b.above))
      aCurr = a.above
      bCurr = b.above
    }

    def fromA(a0: Int, a1: Int, b: Int) = {
      var i = a0
      while (i < a1) {
        val a = as(i)
        edge(a.x, rCurr, op(a.at, bCurr), op(a.above, bCurr))
        i += 1
      }
      aCurr = as(a1 - 1).above
    }

    def fromB(a: Int, b0: Int, b1: Int) = {
      var i = b0
      while (i < b1) {
        val b = bs(i)
        edge(b.x, rCurr, op(aCurr, b.at), op(aCurr, b.above))
        i += 1
      }
      bCurr = bs(b1 - 1).above
    }

    def copyA(a0: Int, a1: Int): Unit = {
      System.arraycopy(as, a0, rs, ri, a1 - a0)
      ri += a1 - a0
      aCurr = as(a1 - 1).above
    }

    def copyB(b0: Int, b1: Int): Unit = {
      System.arraycopy(bs, b0, rs, ri, b1 - b0)
      ri += b1 - b0
      bCurr = bs(b1 - 1).above
    }

  }

  private final class And[K, V](val lhs: Impl[K, V], val rhs: Impl[K, V])(implicit val kOrder: Order[K], val vValue: Value[V]) extends BinaryOp[K, V] {
    def op(a: V, b: V) =
      and(a, b)

    override def fromA(a0: Int, a1: Int, b: Int) = {
      if(isOne(bCurr))
        super.copyA(a0, a1)
      else if(!isZero(bCurr))
        super.fromA(a0, a1, b)
      else
        aCurr = as(a1 - 1).above
    }

    override def fromB(a: Int, b0: Int, b1: Int) = {
      if(isOne(aCurr))
        super.copyB(b0, b1)
      else if(!isZero(aCurr))
        super.fromB(a, b0, b1)
      else
        bCurr = bs(b1 - 1).above
    }
  }

  private final class Or[K, V](val lhs: Impl[K, V], val rhs: Impl[K, V])(implicit val kOrder: Order[K], val vValue: Value[V]) extends BinaryOp[K, V] {
    def op(a: V, b: V) =
      or(a, b)

    override def fromA(a0: Int, a1: Int, b: Int) = {
      if(isZero(bCurr))
        super.copyA(a0, a1)
      else if(!isOne(bCurr))
        super.fromA(a0, a1, b)
      else
        aCurr = as(a1 - 1).above
    }

    override def fromB(a: Int, b0: Int, b1: Int) = {
      if(isZero(aCurr))
        super.copyB(b0, b1)
      else if(!isOne(aCurr))
        super.fromB(a, b0, b1)
      else
        bCurr = bs(b1 - 1).above
    }
  }

  private final class Xor[K, V](val lhs: Impl[K, V], val rhs: Impl[K, V])(implicit val kOrder: Order[K], val vValue: Value[V]) extends BinaryOp[K, V] {
    def op(a: V, b: V) =
      xor(a, b)

    override def fromA(a0: Int, a1: Int, b: Int) = {
      if(isZero(bCurr))
        super.copyA(a0, a1)
      else
        super.fromA(a0, a1, b)
    }

    override def fromB(a: Int, b0: Int, b1: Int) = {
      if(isZero(aCurr))
        super.copyB(b0, b1)
      else
        super.fromB(a, b0, b1)
    }
  }
}