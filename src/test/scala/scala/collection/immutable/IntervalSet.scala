package scala.collection.immutable

import scala.collection.immutable.IntervalSet.IntervalSetElement

final case class IntervalSet[T] private (tree:IntervalTrie)(implicit t:IntervalSetElement[T]) extends (T => Boolean) {

  def apply(value:T) = tree.at(t.toKey(value))

  def & (that:IntervalSet[T]) = IntervalSet(IntervalTrie.AndCalculator(tree, that.tree))

  def | (that:IntervalSet[T]) = IntervalSet(IntervalTrie.OrCalculator(tree, that.tree))

  def unary_~ = IntervalSet(tree flip true)

  def ^ (that:IntervalSet[T]) = IntervalSet(IntervalTrie.XorCalculator(tree, that.tree))

  def isSupersetOf(that:IntervalSet[T]) = (this | that) == this

  def isProperSupersetOf(that:IntervalSet[T]) = isSupersetOf(that) && (that != this)

  override def toString = IntervalTrie.format(tree, x => t.fromKey(x).toString)
}

object IntervalSet {

  trait IntervalSetElement[@specialized T] extends Any {

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

    def toKey(value:Long) = value - Long.MinValue

    def fromKey(key:Long) : Long = key + Long.MinValue
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
      val unsigned = twosComplement - Long.MinValue
      unsigned
    }

    def fromKey(key:Long) = {
      val unsigned = if(unsigned_<(key,minKey)) minKey else if(unsigned_<(maxKey, key)) maxKey else key
      // rotate because the radix tree uses unsigned integers
      val twosComplement = unsigned + Long.MinValue
      // sign and magnitude signed integer: if the sign bit is set, negate everything except the sign bit
      val signAndMagnitude = if(twosComplement>=0) twosComplement else (-twosComplement | (1L<<63))
      // double from sign and magnitude signed integer
      val value = java.lang.Double.longBitsToDouble(signAndMagnitude)
      value
    }
  }

  private implicit def tIsLong[T](value:T)(implicit tl:IntervalSetElement[T]) = tl.toKey(value)

  def zero[T:IntervalSetElement] = IntervalSet[T](IntervalTrie.zero)

  def one[T:IntervalSetElement] = IntervalSet[T](IntervalTrie.one)

  def point[T:IntervalSetElement](value:T) = IntervalSet[T](IntervalTrie.point(value))

  def hole[T:IntervalSetElement](value:T) = IntervalSet[T](IntervalTrie.hole(value))

  def from[T:IntervalSetElement](value:T) = IntervalSet[T](IntervalTrie.startAt(value))

  def above[T:IntervalSetElement](value:T) = IntervalSet[T](IntervalTrie.startAfter(value))

  def below[T:IntervalSetElement](value:T) = IntervalSet[T](IntervalTrie.endBefore(value))

  def to[T:IntervalSetElement](value:T) = IntervalSet[T](IntervalTrie.endAt(value))
}
