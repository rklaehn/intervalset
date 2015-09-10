package com.rklaehn.interval

import language.implicitConversions
import spire.algebra.{Eq, Bool, Order}
import spire.math.Interval
import spire.math.interval._
import spire.algebra.Order.ordering
import spire.implicits._

import scala.collection.AbstractTraversable
import scala.collection.immutable.SortedSet

sealed abstract class IntervalsSet[K: Order, V: IntervalsSet.Value] { lhs ⇒
  import IntervalsSet._

  def belowAll: V

  def ^(rhs: IntervalsSet[K, V]): IntervalsSet[K, V] =
    new Xor[K, V](lhs, rhs).result

  def &(rhs: IntervalsSet[K, V]): IntervalsSet[K, V] =
    new And[K, V](lhs, rhs).result

  def |(rhs: IntervalsSet[K, V]): IntervalsSet[K, V] =
    new Or[K, V](lhs, rhs).result

  def intervals: Traversable[(Interval[K], V)]

  def all: V
}

object IntervalsSet {

  trait Value[@specialized(Boolean) V] {
    def zero: V
    def isZero(x: V): Boolean
    def xor(a: V, b: V): V
    def and(a: V, b: V): V
    def or(a: V, b: V): V
    def andNot(a: V, b: V): V
  }

  object Value {

    implicit def apply[V: Value]: Value[V] = implicitly[Value[V]]

    implicit object booleanIsValue extends Value[Boolean] {

      def zero = false

      def andNot(a: Boolean, b: Boolean) = a & (!b)

      def or(a: Boolean, b: Boolean) = a | b

      def and(a: Boolean, b: Boolean) = a & b

      def xor(a: Boolean, b: Boolean) = a ^ b

      def isZero(x: Boolean) = !x
    }

    implicit def sortedSetIsValue[T: Order]: Value[SortedSet[T]] = new Value[SortedSet[T]] {
      def zero = SortedSet.empty[T]

      def andNot(a: SortedSet[T], b: SortedSet[T]) = a diff b

      def or(a: SortedSet[T], b: SortedSet[T]) = a union b

      def and(a: SortedSet[T], b: SortedSet[T]) = a intersect b

      def xor(a: SortedSet[T], b: SortedSet[T]) = (a diff b) union (b diff a)

      def isZero(x: SortedSet[T]) = x.isEmpty
    }
  }

  private implicit def intervalsSetIsImpl[K, V](x: IntervalsSet[K, V]): Impl[K, V] =
    x.asInstanceOf[Impl[K, V]]

  private final class Impl[K, V](
      val belowAll: V,
      val changes: Array[IntervalsSet.Change[K, V]])(implicit order: Order[K], v: Value[V]) extends IntervalsSet[K, V] { lhs ⇒

    override def equals(rhs: Any)= rhs match {
      case rhs: Impl[K, V] =>
        lhs.belowAll == rhs.belowAll && lhs.changes === rhs.changes
      case _ ⇒
        false
    }

    def all = changes.foldLeft(belowAll) {
      case (a, c) ⇒
        v.or(a, v.or(c.below, c.above))
    }

    override def hashCode: Int =
      (belowAll.hashCode, changes.toIndexedSeq.hashCode).hashCode

    override def toString: String =
      s"IntervalsSet($belowAll, ${changes.toIndexedSeq})"

    def intervals = new AbstractTraversable[(Interval[K], V)] {
      override def foreach[U](f: ((Interval[K], V)) => U): Unit = foreachInterval(f)
    }

    private def foreachInterval[U](f:((Interval[K], V)) => U) : Unit = {
      var prevBound: Bound[K] = Unbound[K]()
      var prevValue: V = belowAll
      for(c <- changes) {
        val below = prevValue
        val at = v.xor(below, c.below)
        val above = v.xor(at, c.above)
        prevBound = (!v.isZero(c.below), !v.isZero(c.above)) match {
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

  def constant[K: Order, V: Value](value: V): IntervalsSet[K, V] =
    IntervalsSet(value, Array.empty[Change[K, V]])

  def empty[K: Order, V: Value]: IntervalsSet[K, V] =
    IntervalsSet(Value[V].zero, Array.empty[Change[K, V]])

  def point[K: Order, V: Value](x: K, value: V): IntervalsSet[K, V] = {
    val empty = Value[V].zero
    val delta = value
    IntervalsSet(empty, Array(Change(x, delta, delta)))
  }

  def hole[K: Order, V: Value](x: K, value: V): IntervalsSet[K, V] = {
    val empty = Value[V].zero
    val delta = value
    IntervalsSet(delta, Array(Change(x, delta, delta)))
  }

  def atOrAbove[K: Order, V: Value](x: K, value: V): IntervalsSet[K, V] =  {
    val empty = Value[V].zero
    val delta = value
    IntervalsSet(empty, Array(Change(x, delta, empty)))
  }

  def above[K: Order, V: Value](x: K, value: V): IntervalsSet[K, V] =  {
    val empty = Value[V].zero
    val delta = value
    IntervalsSet[K, V](empty, Array(Change(x, empty, delta)))
  }

  def atOrBelow[K: Order, V: Value](x: K, value: V): IntervalsSet[K, V] =  {
    val empty = Value[V].zero
    val delta = value
    IntervalsSet(delta, Array(Change(x, empty, delta)))
  }

  def below[K: Order, V: Value](x: K, value: V): IntervalsSet[K, V] =  {
    val empty = Value[V].zero
    val delta = value
    IntervalsSet(delta, Array(Change(x, delta, empty)))
  }

  def apply[K: Order, V: Value](interval: Interval[K], value: V): IntervalsSet[K, V] = interval.fold {
    case (Closed(a),    Closed(b)) if a == b => point(a, value)
    case (Unbound(),    Open(x))      => below(x, value)
    case (Unbound(),    Closed(x))    => atOrBelow(x, value)
    case (Open(x),      Unbound())    => above(x, value)
    case (Closed(x),    Unbound())    => atOrAbove(x, value)
    case (Closed(a),    Closed(b))    => fromTo(Change.below(a, value), Change.above(b, value))
    case (Closed(a),    Open(b))      => fromTo(Change.below(a, value), Change.below(b, value))
    case (Open(a),      Closed(b))    => fromTo(Change.above(a, value), Change.above(b, value))
    case (Open(a),      Open(b))      => fromTo(Change.above(a, value), Change.below(b, value))
    case (Unbound(),    Unbound())    => constant[K, V](value)
    case (EmptyBound(), EmptyBound()) => empty[K, V]
  }

  private def fromTo[K: Order, V: Value](a: Change[K, V], b: Change[K, V]): IntervalsSet[K, V] =
    IntervalsSet(Value[V].zero, Array(a, b))

  private def apply[K: Order, V: Value](belowAll: V, changes: Array[Change[K, V]]) = {
    val changes1 = changes.filterNot(_.isZero)
    new Impl[K, V](belowAll, changes1)
  }

  case class Change[K, V](x: K, below: V, above: V) {

    def isZero(implicit v: Value[V]): Boolean = v.isZero(below) && v.isZero(above)
  }

  object Change {

    implicit def eqv[K, V]: spire.algebra.Eq[Change[K, V]] = spire.optional.genericEq.generic[Change[K, V]]

    def below[K: Order, V: Value](x: K, value: V): Change[K, V] = {
      val empty = Value[V].zero
      val delta = value
      Change(x, delta, empty)
    }

    def above[K: Order, V: Value](x: K, value: V): Change[K, V] = {
      val empty = Value[V].zero
      val delta = value
      Change(x, empty, delta)
    }

  }

  private abstract class MergeOperation[K, V] {

    def and(a: V, b: V): V = vValue.and(a, b)

    def or(a: V, b: V): V = vValue.or(a, b)

    def xor(a: V, b: V): V = vValue.xor(a, b)

    def andNot(a: V, b: V) = vValue.andNot(a, b)

    def kOrder: Order[K]

    def vValue: Value[V]

    def lhs:Impl[K, V]

    def rhs:Impl[K, V]

    def r0:V

    protected[this] val as = lhs.changes

    protected[this] val bs = rhs.changes

    private[this] val rs = Array.ofDim[Change[K,V]](as.length + bs.length)

    private[this] var ri = 0

    def copyA(a0: Int, a1: Int): Unit = {
      System.arraycopy(as, a0, rs, ri, a1 - a0)
      ri += a1 - a0
    }

    def copyB(b0: Int, b1: Int): Unit = {
      System.arraycopy(bs, b0, rs, ri, b1 - b0)
      ri += b1 - b0
    }

    def collision(ai: Int, bi: Int): Unit

    def fromA(a0:Int, a1: Int, b:Int) : Unit

    def fromB(a:Int, b0:Int, b1: Int) : Unit

    def binarySearch(array: Array[Change[K, V]], key: K, from: Int, until: Int): Int = {
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

    def add(c: Change[K, V]): Unit = {
      if(!c.isZero(vValue)) {
        rs(ri) = c
        ri += 1
      }
    }

    def result : IntervalsSet[K, V] = {
      merge0(0, as.length, 0, bs.length)
      IntervalsSet(r0, rs.take(ri))(kOrder, vValue)
    }
  }

  private final class Xor[K, V](val lhs: Impl[K, V], val rhs: Impl[K, V])(implicit val kOrder: Order[K], val vValue: Value[V]) extends MergeOperation[K, V] {

    def r0 = xor(lhs.belowAll, rhs.belowAll)

    def collision(ai: Int, bi: Int) = {
      val ae = as(ai)
      val be = bs(bi)
      add(Change(ae.x, vValue.xor(ae.below, be.below), vValue.xor(ae.above, be.above)))
    }

    def fromB(a: Int, b0: Int, b1: Int) = copyB(b0, b1)

    def fromA(a0: Int, a1: Int, b: Int) = copyA(a0, a1)
  }

  private final class And[K, V](val lhs: Impl[K, V], val rhs: Impl[K, V])(implicit val kOrder: Order[K], val vValue: Value[V]) extends MergeOperation[K, V] {

    def r0 = and(lhs.belowAll, rhs.belowAll)
    var aCurr = lhs.belowAll
    var bCurr = rhs.belowAll

    def collision(ai: Int, bi: Int) = {
      val a = as(ai)
      val b = bs(bi)
      val r0 = and(aCurr, bCurr)
      aCurr = xor(aCurr, a.below)
      bCurr = xor(bCurr, b.below)
      val r1 = and(aCurr, bCurr)
      aCurr = xor(aCurr, a.above)
      bCurr = xor(bCurr, b.above)
      val r2 = and(aCurr, bCurr)
      val rBelow = xor(r0, r1)
      val rAbove = xor(r1, r2)
      add(Change(a.x, rBelow, rAbove))
    }

    def fromB(a: Int, b0: Int, b1: Int) = {
      var i = b0
      while(i < b1) {
        val b = bs(i)
        // add result
        add(Change(b.x, and(aCurr, b.below), and(aCurr, b.above)))
        // update bCurr
        bCurr = xor(bCurr, b.below)
        bCurr = xor(bCurr, b.above)
        i += 1
      }
    }

    def fromA(a0: Int, a1: Int, b: Int) = {
      var i = a0
      while(i < a1) {
        val a = as(i)
        // add result
        add(Change(a.x, and(a.below, bCurr), and(a.above, bCurr)))
        // update aCurr
        aCurr = xor(aCurr, a.below)
        aCurr = xor(aCurr, a.above)
        i += 1
      }
    }
  }

  private final class Or[K, V](val lhs: Impl[K, V], val rhs: Impl[K, V])(implicit val kOrder: Order[K], val vValue: Value[V]) extends MergeOperation[K, V] {

    def r0 = or(lhs.belowAll, rhs.belowAll)
    var aCurr = lhs.belowAll
    var bCurr = rhs.belowAll

    def collision(ai: Int, bi: Int) = {
      val a = as(ai)
      val b = bs(bi)
      val r0 = or(aCurr, bCurr)
      aCurr = xor(aCurr, a.below)
      bCurr = xor(bCurr, b.below)
      val r1 = or(aCurr, bCurr)
      aCurr = xor(aCurr, a.above)
      bCurr = xor(bCurr, b.above)
      val r2 = or(aCurr, bCurr)
      val rBelow = xor(r0, r1)
      val rAbove = xor(r1, r2)
      add(Change(a.x, rBelow, rAbove))
    }

    def fromB(a: Int, b0: Int, b1: Int) = {
      var i = b0
      while(i < b1) {
        val b = bs(i)
        // add result
        add(Change(b.x, andNot(b.below, aCurr), andNot(b.above, aCurr)))
        // update bCurr
        bCurr = xor(bCurr, b.below)
        bCurr = xor(bCurr, b.above)
        i += 1
      }
    }

    def fromA(a0: Int, a1: Int, b: Int) = {
      var i = a0
      while(i < a1) {
        val a = as(i)
        // add result
        add(Change(a.x, andNot(a.below, bCurr), andNot(a.above, bCurr)))
        // update aCurr
        aCurr = xor(aCurr, a.below)
        aCurr = xor(aCurr, a.above)
        i += 1
      }
    }
  }
}