package com.rklaehn.interval

import language.implicitConversions
import spire.algebra.{AdditiveMonoid, Order}
import spire.math.Interval.{Bound, Closed, Open, Unbound}
import spire.math._

import scala.annotation.tailrec
import scala.collection.AbstractTraversable

sealed abstract class IntervalSet[T] extends (T => Boolean) {

  def isEmpty: Boolean

  def isContiguous: Boolean

  def hull: Interval[T]

  def below(value:T) : Boolean

  def at(value:T) : Boolean

  def above(value:T) : Boolean

  def apply(value:T) : Boolean

  def &(rhs:IntervalSet[T]): IntervalSet[T]

  def |(rhs:IntervalSet[T]): IntervalSet[T]

  def ^(rhs:IntervalSet[T]): IntervalSet[T]

  def unary_~ : IntervalSet[T]

  def intervals: Traversable[Interval[T]]

  def isSupersetOf(rhs:IntervalSet[T]):Boolean

  def isProperSupersetOf(rhs:IntervalSet[T]): Boolean

  private[interval] def edges:Traversable[T]
}

object IntervalSet {

  trait IntervalSetElement[@specialized(Float, Int, Long, Double) T] {

    implicit def ops:Order[T] with AdditiveMonoid[T]

    def toKey(value:T) : Long

    def fromKey(key:Long) : T
  }

  implicit object ByteIntervalSetElement extends IntervalSetElement[Byte] {

    def ops = spire.std.byte.ByteAlgebra

    def toKey(value:Byte) = value

    def fromKey(key:Long): Byte = key.toByte
  }

  implicit object ShortIntervalSetElement extends IntervalSetElement[Short] {

    def ops = spire.std.short.ShortAlgebra

    def toKey(value:Short) = value

    def fromKey(key:Long): Short = key.toShort
  }

  implicit object IntIntervalSetElement extends IntervalSetElement[Int] {

    def ops = spire.std.int.IntAlgebra

    def toKey(value:Int) = value

    def fromKey(key:Long) : Int = key.toInt
  }

  implicit object LongIntervalSetElement extends IntervalSetElement[Long] {

    def ops = spire.std.long.LongAlgebra

    def toKey(value:Long) = value

    def fromKey(key:Long) : Long = key
  }

  implicit object FloatIntervalSetElement extends IntervalSetElement[Float] {

    def ops = spire.std.float.FloatAlgebra

    def toKey(value:Float): Long = {
      if(value.isNaN)
        throw new IllegalArgumentException("NaN")
      // sign and magnitude signed integer
      val signAndMagnitude = java.lang.Float.floatToIntBits(value)
      // two's complement signed integer: if the sign bit is set, negate everything except the sign bit
      val twosComplement = if(signAndMagnitude>=0) signAndMagnitude else (-signAndMagnitude | (1L<<63))
      twosComplement
    }

    def fromKey(twosComplement:Long): Float = {
      // sign and magnitude signed integer: if the sign bit is set, negate everything except the sign bit
      val signAndMagnitude = if(twosComplement>=0) twosComplement else (-twosComplement | (1L<<63))
      // double from sign and magnitude signed integer
      java.lang.Float.intBitsToFloat(signAndMagnitude.toInt)
    }
  }

  implicit object DoubleIntervalSetElement extends IntervalSetElement[Double] {

    def ops = spire.std.double.DoubleAlgebra

    def toKey(value:Double): Long = {
      if(value.isNaN)
        throw new IllegalArgumentException("NaN")
      // sign and magnitude signed integer
      val signAndMagnitude = java.lang.Double.doubleToLongBits(value)
      // two's complement signed integer: if the sign bit is set, negate everything except the sign bit
      val twosComplement = if(signAndMagnitude>=0) signAndMagnitude else (-signAndMagnitude | (1L<<63))
      twosComplement
    }

    def fromKey(twosComplement:Long): Double = {
      // sign and magnitude signed integer: if the sign bit is set, negate everything except the sign bit
      val signAndMagnitude = if(twosComplement>=0) twosComplement else (-twosComplement | (1L<<63))
      // double from sign and magnitude signed integer
      java.lang.Double.longBitsToDouble(signAndMagnitude)
    }
  }

  implicit object UByteIntervalSetElement extends IntervalSetElement[UByte] {

    def ops = spire.math.UByte.UByteAlgebra

    def toKey(value:UByte) = value.toLong

    def fromKey(key:Long) : UByte = UByte(key.toByte)
  }

  implicit object UShortIntervalSetElement extends IntervalSetElement[UShort] {

    def ops = spire.math.UShort.UShortAlgebra

    def toKey(value:UShort) = value.toLong

    def fromKey(key:Long) : UShort = UShort(key.toShort)
  }

  implicit object UIntIntervalSetElement extends IntervalSetElement[UInt] {

    def ops = spire.math.UInt.UIntAlgebra

    def toKey(value:UInt) = value.toLong

    def fromKey(key:Long) : UInt = UInt(key.toInt)
  }

  implicit object ULongIntervalSetElement extends IntervalSetElement[ULong] {

    def ops = spire.math.ULong.ULongAlgebra

    def toKey(value:ULong) = value.toLong + Long.MinValue

    def fromKey(key:Long) : ULong = ULong(key - Long.MinValue)
  }

  import IntervalTrie._

  private implicit def tIsLong[T](value:T)(implicit tl:IntervalSetElement[T]) = tl.toKey(value)

  private[interval] def fromKind[T:IntervalSetElement](value:T, kind:Int) = {
    val bound = kind match {
      case 0 => Below(value)
      case 1 => Above(value)
      case 2 => Both(value)
    }
    IntervalSet[T](false, bound)
  }

  def constant[T:IntervalSetElement](value:Boolean) = IntervalSet[T](value, null)

  def zero[T:IntervalSetElement] = constant[T](false)

  def point[T:IntervalSetElement](value:T) = IntervalSet[T](false, IntervalTrie.Leaf(toPrefix(value), true, false))

  def atOrAbove[T:IntervalSetElement](value:T) = IntervalSet[T](false, IntervalTrie.Leaf(toPrefix(value), true, true))

  def above[T:IntervalSetElement](value:T) = IntervalSet[T](false, IntervalTrie.Leaf(toPrefix(value), false, true))

  def one[T:IntervalSetElement] = constant[T](true)

  def hole[T:IntervalSetElement](value:T) = IntervalSet[T](true, IntervalTrie.Leaf(toPrefix(value), true, false))

  def below[T:IntervalSetElement](value:T) = IntervalSet[T](true, IntervalTrie.Leaf(toPrefix(value), true, true))

  def atOrBelow[T:IntervalSetElement](value:T) = IntervalSet[T](true, IntervalTrie.Leaf(toPrefix(value), false, true))

  def apply[T:IntervalSetElement](interval:Interval[T]) : IntervalSet[T] = interval.fold {
    case (Unbound(), Unbound()) => one[T]
    case (Open(a),   Open(b))   if a == b => zero[T]
    case (Closed(a), Closed(b)) if a == b => point(a)
    case (Unbound(), Open(x))   => below(x)
    case (Unbound(), Closed(x)) => atOrBelow(x)
    case (Open(x),   Unbound()) => above(x)
    case (Closed(x), Unbound()) => atOrAbove(x)
    case (Closed(a), Closed(b)) => fromTo(Below(a), Above(b))
    case (Closed(a), Open(b))   => fromTo(Below(a), Below(b))
    case (Open(a),   Closed(b)) => fromTo(Above(a), Above(b))
    case (Open(a),   Open(b))   => fromTo(Above(a), Below(b))
  }

  private object Below {

    def apply[T: IntervalSetElement](value:T) = Leaf(toPrefix(value), true, true)

    def unapply(l:Leaf) = if(l.at && l.sign) Some(l.key) else None
  }

  private object Above {

    def apply[T: IntervalSetElement](value:T) = Leaf(toPrefix(value), false, true)

    def unapply(l:Leaf) = if(!l.at && l.sign) Some(l.key) else None
  }

  private[interval] object Both {

    def apply[T: IntervalSetElement](value:T) = Leaf(toPrefix(value), true, false)

    def unapply(l:Leaf) = if(l.at && !l.sign) Some(l.key) else None
  }

  private def fromTo[T:IntervalSetElement](a:Leaf, b:Leaf) : IntervalSet[T] = {
    IntervalSet[T](false, concat(a, b))
  }

  def apply(text:String) : IntervalSet[Long] = {
    val la = spire.std.long.LongAlgebra
    def rationalToLong(r:Rational) : Long = {
      if(r>Long.MaxValue || r<Long.MinValue)
        throw new NumberFormatException("Integer number too large")
      else
        r.toLong
    }
    def intervalToIntervalSet(i:Interval[Long]) : IntervalSet[Long] = apply(i)
    val intervals = text.split(';').map(IntervalParser.apply).map(_.mapBounds(rationalToLong)(la,la))
    val simpleSets = intervals.map(intervalToIntervalSet)
    (zero[Long] /: simpleSets)(_ | _)
  }

  final def foreachInterval[T:IntervalSetElement, U](a0:Boolean, a:IntervalTrie)(f:Interval[T] => U): Unit = {
    val x = implicitly[IntervalSetElement[T]]
    import x._
    def op(b0:Bound[T], a0:Boolean, a:IntervalTrie): Bound[T] = a match {
      case Below(a) =>
        if(a0)
          f(Interval.fromBounds(b0, Open(fromKey(a))))
        Closed(fromKey(a))
      case Above(a) =>
        if(a0)
          f(Interval.fromBounds(b0, Closed(fromKey(a))))
        Open(fromKey(a))
      case Both(a) =>
        if(a0)
          f(Interval.fromBounds(b0, Open(fromKey(a))))
        else
          f(Interval.point(fromKey(a)))
        Open(fromKey(a))
      case a:Branch =>
        val am = a0 ^ a.left.sign
        val bm = op(b0, a0, a.left)
        val b1 = op(bm, am, a.right)
        b1
      case _ =>
        Unbound()
    }
    val last = op(Unbound(), a0, a)
    if(a0 ^ ((a ne null) && a.sign))
      f(Interval.fromBounds(last, Unbound()))
  }


  private def apply[T:IntervalSetElement](below:Boolean, tree:IntervalTrie): IntervalSet[T] =
    TreeBasedIntervalSet(below, tree, implicitly[IntervalSetElement[T]])

  private final case class TreeBasedIntervalSet[T](belowAll:Boolean, tree:IntervalTrie, implicit val ise:IntervalSetElement[T]) extends IntervalSet[T] { lhs =>

    import IntervalTrie._

    def aboveAll: Boolean = if(tree eq null) belowAll else belowAll ^ tree.sign

    def isEmpty = !belowAll && (tree eq null)

    def isContiguous = if(belowAll) {
      tree match {
        case a:Leaf => a.sign
        case null => true
        case _ => false
      }
    } else {
      tree match {
        case _:Leaf => true
        case Branch(_,_,a:Leaf, b:Leaf) => a.sign & b.sign
        case null => true
        case _ => false
      }
    }

    def hull: Interval[T] = {
      implicit val ops = ise.ops
      @tailrec
      def lowerBound(a:IntervalTrie) : Bound[T] = a match {
        case a:Branch => lowerBound(a.left)
        case Above(x) => Open(ise.fromKey(x))
        case Below(x) => Closed(ise.fromKey(x))
        case Both(x) => Closed(ise.fromKey(x))
      }
      @tailrec
      def upperBound(a:IntervalTrie) : Bound[T] = a match {
        case a:Branch => upperBound(a.right)
        case Above(x) => Closed(ise.fromKey(x))
        case Below(x) => Open(ise.fromKey(x))
        case Both(x) => Closed(ise.fromKey(x))
      }
      if(isEmpty) {
        Interval.empty[T]
      } else {
        val lower = if(belowAll) Unbound[T] else lowerBound(tree)
        val upper = if(aboveAll) Unbound[T] else upperBound(tree)
        Interval.fromBounds(lower, upper)
      }
    }

    def below(value:T) : Boolean = SampleBelow(belowAll, tree, toPrefix(ise.toKey(value)))

    def at(value:T) : Boolean = SampleAt(belowAll, tree, toPrefix(ise.toKey(value)))

    def above(value:T) : Boolean = SampleAbove(belowAll, tree, toPrefix(ise.toKey(value)))

    def apply(value:T) : Boolean = at(value)

    def & (rhs:IntervalSet[T]) = rhs match {
      case rhs:TreeBasedIntervalSet[T] =>
        IntervalSet[T](lhs.belowAll & rhs.belowAll, AndCalculator(lhs.belowAll, lhs.tree, rhs.belowAll, rhs.tree))
    }

    def | (rhs:IntervalSet[T]) = rhs match {
      case rhs: TreeBasedIntervalSet[T] =>
        IntervalSet[T](lhs.belowAll | rhs.belowAll, OrCalculator(lhs.belowAll, lhs.tree, rhs.belowAll, rhs.tree))
    }

    def ^ (rhs:IntervalSet[T]) = rhs match {
      case rhs: TreeBasedIntervalSet[T] => IntervalSet[T](lhs.belowAll ^ rhs.belowAll, XorCalculator(lhs.belowAll, lhs.tree, rhs.belowAll, rhs.tree))
    }

    def unary_~ = IntervalSet[T](!belowAll, tree)

    def isSupersetOf(rhs:IntervalSet[T]) = (lhs | rhs) == lhs

    def isProperSupersetOf(rhs:IntervalSet[T]) = isSupersetOf(rhs) && (rhs != lhs)

    def intervals = new AbstractTraversable[Interval[T]] {
      override def foreach[U](f: Interval[T] => U): Unit = foreachInterval(belowAll, tree)(f)
    }

    def edges = new AbstractTraversable[T] {
      override def foreach[U](f: T => U): Unit = IntervalTrie.foreachEdge(tree)(key => f(ise.fromKey(key)))
    }

    override def toString = {
      import ise.ops
      if ((tree eq null) && !belowAll)
        Interval.empty[T].toString
      else
        intervals.map(_.toString).mkString(";")
    }
  }

}
