package scala.collection.immutable

import spire.math.Interval.{Closed, Open, Unbound}
import spire.math.{Rational, Interval}
import spire.std.long._

import scala.collection.AbstractTraversable
import scala.collection.immutable.IntervalSet.IntervalSetElement

final case class IntervalSet[T] private[immutable] (below:Boolean, tree:IntervalTrie)(implicit t:IntervalSetElement[T]) extends (T => Boolean) { lhs =>

  import IntervalTrie._

  private[immutable] def ise = implicitly[IntervalSetElement[T]]

  def isConstant : Boolean = tree eq null

  def below(value:T) : Boolean = IntervalTrie.SampleBelow(below, tree, toPrefix(t.toKey(value)))

  def at(value:T) : Boolean = IntervalTrie.SampleAt(below, tree, toPrefix(t.toKey(value)))

  def above(value:T) : Boolean = IntervalTrie.SampleAbove(below, tree, toPrefix(t.toKey(value)))

  def apply(value:T) : Boolean = at(value)

  def & (rhs:IntervalSet[T]) = IntervalSet(lhs.below & rhs.below,
    AndCalculator(lhs.below, lhs.tree, rhs.below, rhs.tree))

  def | (rhs:IntervalSet[T]) = IntervalSet(lhs.below | rhs.below,
    OrCalculator(lhs.below, lhs.tree, rhs.below, rhs.tree))

  def ^ (rhs:IntervalSet[T]) = IntervalSet(lhs.below ^ rhs.below,
    XorCalculator(lhs.below, lhs.tree, rhs.below, rhs.tree))

  def unary_~ = IntervalSet(!below, tree)

  def isSupersetOf(rhs:IntervalSet[T]) = (lhs | rhs) == lhs

  def isProperSupersetOf(rhs:IntervalSet[T]) = isSupersetOf(rhs) && (rhs != lhs)

  def intervals = new AbstractTraversable[Interval[Long]] {
    override def foreach[U](f: Interval[Long] => U): Unit = IntervalTrie.foreachInterval(below, tree)(f)
  }

  def edges = new AbstractTraversable[T] {
    override def foreach[U](f: T => U): Unit = IntervalTrie.foreachEdge(tree)(key => f(t.fromKey(key)))
  }

  override def toString =
    if(isConstant && !below)
      Interval.empty[Long].toString
    else
      intervals.map(_.toString).mkString(";")
}

object IntervalSet {

  trait IntervalSetElement[@specialized(Float, Int, Long, Double) T] extends Any {

    def toKey(value:T) : Long

    def fromKey(key:Long) : T
  }

  implicit object IntIntervalSetElement extends IntervalSetElement[Int] {

    def toKey(value:Int) = value

    def fromKey(key:Long) : Int = key.toInt
  }

  implicit object LongIntervalSetElement extends IntervalSetElement[Long] {

    def toKey(value:Long) = value

    def fromKey(key:Long) : Long = key
  }

  implicit object FloatIntervalSetElement extends IntervalSetElement[Float] {

    def toKey(value:Float): Long = {
      if(value.isNaN)
        throw new IllegalArgumentException("NaN")
      // sign and magnitude signed integer
      val signAndMagnitude = java.lang.Double.doubleToLongBits(value)
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

  import IntervalTrie._

  private implicit def tIsLong[T](value:T)(implicit tl:IntervalSetElement[T]) = tl.toKey(value)

  def fromKind[T:IntervalSetElement](value:T, kind:Int) = IntervalSet[T](false, IntervalTrie.Leaf(toPrefix(value), kind))

  def constant[T:IntervalSetElement](value:Boolean) = IntervalSet[T](value, null)

  def zero[T:IntervalSetElement] = constant[T](false)

  def point[T:IntervalSetElement](value:T) = IntervalSet[T](false, IntervalTrie.Leaf(toPrefix(value), Both))

  def atOrAbove[T:IntervalSetElement](value:T) = IntervalSet[T](false, IntervalTrie.Leaf(toPrefix(value), Below))

  def above[T:IntervalSetElement](value:T) = IntervalSet[T](false, IntervalTrie.Leaf(toPrefix(value), Above))

  def one[T:IntervalSetElement] = constant[T](true)

  def hole[T:IntervalSetElement](value:T) = IntervalSet[T](true, IntervalTrie.Leaf(toPrefix(value), Both))

  def below[T:IntervalSetElement](value:T) = IntervalSet[T](true, IntervalTrie.Leaf(toPrefix(value), Below))

  def atOrBelow[T:IntervalSetElement](value:T) = IntervalSet[T](true, IntervalTrie.Leaf(toPrefix(value), Above))

  def apply[T:IntervalSetElement](interval:Interval[T]) : IntervalSet[T] = interval.fold {
    case (Unbound(), Unbound()) => one[T]
    case (Open(a),   Open(b))   if a == b => zero[T]
    case (Unbound(), Open(x))   => below(x)
    case (Unbound(), Closed(x)) => atOrBelow(x)
    case (Open(x),   Unbound()) => above(x)
    case (Closed(x), Unbound()) => atOrAbove(x)
    case (Closed(a), Closed(b)) => fromTo(a, Below, b, Above)
    case (Closed(a), Open(b))   => fromTo(a, Below, b, Below)
    case (Open(a),   Closed(b)) => fromTo(a, Above, b, Above)
    case (Open(a),   Open(b))   => fromTo(a, Above, b, Below)
  }

  private def fromTo[T:IntervalSetElement](a:T, ak:Int, b:T, bk:Int) : IntervalSet[T] = {
    new IntervalSet[T](false, join(Leaf(toPrefix(a), ak), Leaf(toPrefix(b),bk)))
  }

  def parse(text:String) : IntervalSet[Long] = {
    import spire.std.long._
    def rationalToLong(r:Rational) : Long = {
      if(r>Long.MaxValue || r<Long.MinValue)
        throw new NumberFormatException("Integer number too large")
      else
        r.toLong
    }
    def intervalToIntervalSet(i:Interval[Long]) : IntervalSet[Long] = apply(i)
    val intervals = text.split(';').map(Interval.apply).map(_.mapBounds(rationalToLong)).map(intervalToIntervalSet)
    (zero[Long] /: intervals)(_ | _)
  }
}
