package com.rklaehn.interval

import java.util.{Comparator, Arrays}

import spire.algebra.Order
import spire.math.{Below, Interval}
import spire.math.interval.{Bound, Open, Closed, Unbound}

import scala.collection.AbstractTraversable
import scala.reflect.ClassTag

abstract class AbstractIntervalSet[T, S <: AbstractIntervalSet[T,_]] extends (T => Boolean) {

  def isEmpty: Boolean

  def at(value: T): Boolean

  def above(value: T): Boolean

  def below(value: T): Boolean

  def belowAll: Boolean

  def aboveAll: Boolean

  def |(rhs:S) : S

  def &(rhs:S) : S

  def ^(rhs:S) : S

  def unary_~ : S

  def intervals: Traversable[Interval[T]]

  private[interval] def edges:Traversable[T]
}

object IntervalSeq {

  private def singleton[T: Order](belowAll: Boolean, value: T, kind: Byte): IntervalSeq[T] = IntervalSeq(belowAll, Array(value)(classTag), Array(kind), implicitly[Order[T]])

  def atOrAbove[T: Order](value: T) = singleton(false, value, K11)

  def above[T: Order](value: T) = singleton(false, value, K01)

  def atOrBelow[T: Order](value: T) = singleton(true, value, K10)

  def below[T: Order](value: T) = singleton(false, value, K00)

  def point[T: Order](value: T) = singleton(false, value, K10)

  def hole[T: Order](value: T) = singleton(true, value, K01)

  def zero[T: Order]: IntervalSeq[T] = IntervalSeq[T](false, Array()(classTag), Array(), implicitly[Order[T]])

  def one[T: Order]: IntervalSeq[T] = IntervalSeq[T](true, Array()(classTag), Array(), implicitly[Order[T]])

  def constant[T: Order](value: Boolean) : IntervalSeq[T] = IntervalSeq[T](value, Array()(classTag), Array(), implicitly[Order[T]])
  
  val K00 = 0.toByte
  val K10 = 1.toByte
  val K01 = 2.toByte
  val K11 = 3.toByte

  def classTag[T] = ClassTag.AnyRef.asInstanceOf[ClassTag[T]]

  def negateKind(kind: Byte) = ((~kind) & 3).toByte

  private def kindToString(kind:Byte) = ("0" + kind.toBinaryString).takeRight(2).reverse

  private def valueAt(kind: Byte): Boolean = (kind & 1) != 0

  private def valueAbove(kind: Byte): Boolean = (kind & 2) != 0
}

case class IntervalSeq[T](belowAll: Boolean, values: Array[T], kinds: Array[Byte], order: Order[T]) extends AbstractIntervalSet[T, IntervalSeq[T]] with Comparator[T] { lhs =>
  implicit def implicitOrder = order
  
  import IntervalSeq._

  private def binarySearch[T](elements: Array[T], element: T): Int = {
    Arrays.binarySearch(
      elements.asInstanceOf[Array[AnyRef]],
      element.asInstanceOf[AnyRef],
      this.asInstanceOf[Comparator[AnyRef]])
  }

  override def compare(x: T, y: T): Int = order.compare(x, y)

  def below(index: Int): Boolean = {
    if (index == 0)
      belowAll
    else
      valueAbove(kinds(index - 1))
  }

  def at(value: T): Boolean = {
    val index = binarySearch(values, value)
    if (index >= 0)
      valueAt(kinds(index))
    else
      below(-index - 1)
  }

  def above(value: T): Boolean = {
    val index = binarySearch(values, value)
    if (index >= 0)
      valueAbove(kinds(index))
    else
      below(-index - 1)
  }

  def below(value: T): Boolean = {
    val index = binarySearch(values, value)
    if (index > 0)
      valueAbove(kinds(index - 1))
    else if (index == 0)
      belowAll
    else
      below(-index - 1)
  }

  def apply(value:T) = at(value)

  def aboveAll = if (values.isEmpty) belowAll else valueAbove(kinds.last)

  def unary_~ :IntervalSeq[T] = copy(belowAll = !belowAll, kinds = negateKinds(kinds))

  def |(rhs:IntervalSeq[T]) = new Or[T](lhs, rhs).result

  def &(rhs:IntervalSeq[T]) = new And[T](lhs, rhs).result

  def ^(rhs:IntervalSeq[T]) = new Xor[T](lhs, rhs).result

  override def toString = {
//    val vs = values.mkString("[",",","]")
//    val ks = kinds.map(kindToString).mkString("[",",","]")
//    s"SeqBasedIntervalSet($belowAll, $vs, $ks)"
    if(isEmpty)
      Interval.empty[T].toString()
    else
      intervals.mkString(";")
  }

  override def hashCode = {
    belowAll.## * 41 + Arrays.hashCode(kinds) * 23 + Arrays.hashCode(values.asInstanceOf[Array[AnyRef]])
  }

  override def equals(rhs:Any) = rhs match {
    case rhs:IntervalSeq[_] =>
      lhs.belowAll == rhs.belowAll &&
        Arrays.equals(lhs.kinds, rhs.kinds) &&
        Arrays.equals(values.asInstanceOf[Array[AnyRef]], rhs.values.asInstanceOf[Array[AnyRef]])
    case _ => false
  }

  def edges = values

  def isEmpty = !belowAll && values.isEmpty

  def intervals = new AbstractTraversable[Interval[T]] {
    override def foreach[U](f: (Interval[T]) => U): Unit = foreachInterval(f)
  }

  private def foreachInterval[U](f:Interval[T] => U) : Unit = {
    var prev: Option[Bound[T]] = if(belowAll) Some(Unbound()) else None
    for(i<-values.indices) {
      val vi = values(i)
      val ki = kinds(i)
      prev = (ki, prev) match {
        case (K00, Some(prev)) =>
          f(Interval.fromBounds(prev, Open(vi)))
          None
        case (K01, Some(prev)) =>
          f(Interval.fromBounds(prev, Open(vi)))
          Some(Open(vi))
        case (K11, Some(prev)) =>
          Some(prev)
        case (K10, Some(prev)) =>
          f(Interval.fromBounds(prev, Closed(vi)))
          None
        case (K00, None) =>
          // WTF!
          None
        case (K10, None) =>
          f(Interval.fromBounds(Closed(vi),Closed(vi)))
          None
        case (K11, None) =>
          Some(Closed(vi))
        case (K01, None) =>
          Some(Open(vi))
      }
    }
    for(prev <- prev)
      f(Interval.fromBounds(prev, Unbound()))
  }

  private def negateKinds(kinds:Array[Byte]): Array[Byte] = {
    var i = 0
    val result = new Array[Byte](kinds.length)
    while(i < kinds.length) {
      result(i) = negateKind(kinds(i))
      i += 1
    }
    result
  }
}

private abstract class MergeOperation[T] {
  import IntervalSeq._
  implicit val ct = classTag[T]
  def lhs:IntervalSeq[T]
  def rhs:IntervalSeq[T]
  val a0 = lhs.belowAll
  val b0 = rhs.belowAll
  val a = lhs.values
  val b = rhs.values
  val ak = lhs.kinds
  val bk = rhs.kinds
  def r0 = op(a0, b0)
  val order = lhs.order
  val r = new Array[T](a.length + b.length)
  val rk = new Array[Byte](a.length + b.length)
  var ri = 0

  def copyA(a0: Int, a1: Int): Unit = {
    System.arraycopy(a, a0, r, ri, a1 - a0)
    System.arraycopy(ak, a0, rk, ri, a1 - a0)
    ri += a1 - a0
  }

  def flipA(a0: Int, a1: Int): Unit = {
    System.arraycopy(a, a0, r, ri, a1 - a0)
    var ai = a0
    while(ai < a1) {
      rk(ri) = negateKind(ak(ai))
      ri += 1
      ai += 1
    }
  }

  def copyB(b0: Int, b1: Int): Unit = {
    System.arraycopy(b, b0, r, ri, b1 - b0)
    System.arraycopy(bk, b0, rk, ri, b1 - b0)
    ri += b1 - b0
  }

  def flipB(b0: Int, b1: Int): Unit = {
    System.arraycopy(b, b0, r, ri, b1 - b0)
    var bi = b0
    while(bi < b1) {
      rk(ri) = negateKind(bk(bi))
      ri += 1
      bi += 1
    }
  }

  def op(a:Boolean, b:Boolean) : Boolean

  def op(a:Byte, b:Byte) : Int

  def collision(ai: Int, bi: Int): Unit = {
    val kind = op(ak(ai), bk(bi)).toByte
    val below = rBelow
    if((below && kind != K11) || (!below && kind != K00)) {
      rk(ri) = kind
      r(ri) = a(ai)
      ri += 1
    }
  }

  def fromA(a0:Int, a1: Int, b:Boolean) : Unit

  def fromB(a:Boolean, b0:Int, b1: Int) : Unit

  @inline def valueAbove(kind: Byte): Boolean = (kind & 2) != 0

  @inline def aBelow(i:Int) = if(i>0) valueAbove(ak(i-1)) else a0

  @inline def bBelow(i:Int) = if(i>0) valueAbove(bk(i-1)) else b0

  @inline def rBelow = if(ri > 0) valueAbove(rk(ri-1)) else r0

  def binarySearch(array: Array[T], key: T, from: Int, until: Int): Int = {
    var low = from
    var high = until - 1
    while (low <= high) {
      val mid = (low + high) >>> 1
      val midVal = array(mid)
      val c = order.compare(midVal, key)
      if (c < 0) {
        low = mid + 1
      }
      else if (c > 0) {
        high = mid - 1
      }
      else {
        return mid
      }
    }
    -(low + 1)
  }

  def merge0(a0: Int, a1: Int, b0: Int, b1: Int): Unit = {
    if (a0 == a1) {
      fromB(aBelow(a0), b0, b1)
    } else if (b0 == b1) {
      fromA(a0, a1, bBelow(b0))
    } else {
      val am = (a0 + a1) / 2
      val res = binarySearch(b, a(am), b0, b1)
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
        fromA(am, am + 1, bBelow(bm))
        // everything above a(am) with everything above the found insertion point
        merge0(am + 1, a1, bm, b1)
      }
    }
  }

  merge0(0, a.length, 0, b.length)

  def result : IntervalSeq[T] = {
    if(ri == r.length)
      IntervalSeq(r0, r, rk, order)
    else
      IntervalSeq(r0, r.take(ri), rk.take(ri), order)
  }
}

private class And[T](val lhs:IntervalSeq[T], val rhs:IntervalSeq[T]) extends MergeOperation[T] {
  import IntervalSeq._

  override def op(a: Boolean, b: Boolean): Boolean = a & b

  override def op(a: Byte, b: Byte): Int = (a & b)

  override def fromA(a0: Int, a1: Int, b: Boolean): Unit =
    if(b)
      copyA(a0,a1)

  override def fromB(a: Boolean, b0: Int, b1: Int): Unit =
    if(a)
      copyB(b0,b1)
}

private class Or[T](val lhs:IntervalSeq[T], val rhs:IntervalSeq[T]) extends MergeOperation[T] {

  override def op(a: Boolean, b: Boolean): Boolean = a | b

  override def op(a: Byte, b: Byte): Int = (a | b)

  override def fromA(a0: Int, a1: Int, b: Boolean): Unit =
    if(!b)
      copyA(a0,a1)

  override def fromB(a: Boolean, b0: Int, b1: Int): Unit =
    if(!a)
      copyB(b0,b1)
}

private class Xor[T](val lhs:IntervalSeq[T], val rhs:IntervalSeq[T]) extends MergeOperation[T] {

  override def op(a: Boolean, b: Boolean): Boolean = a ^ b

  override def op(a: Byte, b: Byte): Int = (a ^ b)

  override def fromA(a0: Int, a1: Int, b: Boolean): Unit =
    if(!b)
      copyA(a0,a1)
    else
      flipA(a0,a1)

  override def fromB(a: Boolean, b0: Int, b1: Int): Unit =
    if(!a)
      copyB(b0,b1)
    else
      flipB(b0,b1)
}