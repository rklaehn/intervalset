package scala.collection.immutable

import scala.collection.immutable.IntervalSet.IntervalSetElement

final case class IntervalSet[T] private (below:Boolean, tree:IntervalTrie)(implicit t:IntervalSetElement[T]) extends (T => Boolean) { lhs =>

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

  override def toString = s"IntervalSet($below,$tree)"
}

object IntervalSet {

  trait IntervalSetElement[@specialized(Long, Double) T] extends Any {

    def toKey(value:T) : Long

    def fromKey(key:Long) : T
  }

  implicit object intIsIntervalSetElement extends IntervalSetElement[Int] {

    def toKey(value:Int) = value - Long.MinValue

    def fromKey(key:Long) : Int = {
      val value = (key + Long.MinValue)
      if(value < Int.MinValue)
        Int.MinValue
      else if(value > Int.MaxValue)
        Int.MaxValue
      else
        value.toInt
    }
  }

  implicit object longIsIntervalSetElement extends IntervalSetElement[Long] {

    def toKey(value:Long) = value

    def fromKey(key:Long) : Long = key
  }

  implicit object doubleIsIntervalSetElement extends IntervalSetElement[Double] {

    private val minKey = toKey(Double.NegativeInfinity)

    private val maxKey = toKey(Double.PositiveInfinity)

    @inline private final def unsigned_<(i: Long, j: Long) = (i < j) ^ (i < 0L) ^ (j < 0L)

    def toKey(value:Double) = {
      if(value.isNaN)
        throw new IllegalArgumentException("NaN")
      // sign and magnitude signed integer
      val signAndMagnitude = java.lang.Double.doubleToLongBits(value)
      // two's complement signed integer: if the sign bit is set, negate everything except the sign bit
      val twosComplement = if(signAndMagnitude>=0) signAndMagnitude else (-signAndMagnitude | (1L<<63))
      // rotate because the radix tree uses unsigned integers
      twosComplement
    }

    def fromKey(key:Long) = {
      val twosComplement = if(key < minKey) minKey else if(maxKey < key) maxKey else key
      // sign and magnitude signed integer: if the sign bit is set, negate everything except the sign bit
      val signAndMagnitude = if(twosComplement>=0) twosComplement else (-twosComplement | (1L<<63))
      // double from sign and magnitude signed integer
      val value = java.lang.Double.longBitsToDouble(signAndMagnitude)
      value
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
}
