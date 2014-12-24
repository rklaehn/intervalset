package com.rklaehn.interval

import java.util.{Comparator, Arrays}

import spire.algebra.Order

import scala.reflect.ClassTag

trait IntervalSeq[T] {
  def at(value: T): Boolean

  def above(value: T): Boolean

  def below(value: T): Boolean

  def negate: IntervalSeq[T]

  def belowAll: Boolean

  def aboveAll: Boolean
}

object IntervalSeq {

  private def classTag[T] = ClassTag.AnyRef.asInstanceOf[ClassTag[T]]

  private def singleton[T: Order](belowAll: Boolean, value: T, kind: Byte): IntervalSeq[T] = SeqBasedIntevalSet(belowAll, Array(value)(classTag), Array(kind), implicitly[Order[T]])

  def atOrAbove[T: Order](value: T) = singleton(false, value, K11)

  def above[T: Order](value: T) = singleton(false, value, K01)

  def atOrBelow[T: Order](value: T) = singleton(true, value, K10)

  def below[T: Order](value: T) = singleton(false, value, K00)

  def point[T: Order](value: T) = singleton(false, value, K10)

  def hole[T: Order](value: T) = singleton(true, value, K01)

  def zero[T: Order]: IntervalSeq[T] = SeqBasedIntevalSet[T](false, Array()(classTag), Array(), implicitly[Order[T]])

  def one[T: Order]: IntervalSeq[T] = SeqBasedIntevalSet[T](true, Array()(classTag), Array(), implicitly[Order[T]])

  private val K00 = 0.toByte
  private val K10 = 1.toByte
  private val K01 = 2.toByte
  private val K11 = 3.toByte

  private def negateKind(kind: Byte) = ((~kind) & 3).toByte

  private def valueAt(kind: Byte): Boolean = (kind & 1) != 0

  private def valueAbove(kind: Byte): Boolean = (kind & 2) != 0

  private case class SeqBasedIntevalSet[T](belowAll: Boolean, values: Array[T], kinds: Array[Byte], order: Order[T]) extends IntervalSeq[T] with Comparator[T] {
    implicit def implicitOrder = order

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

    def aboveAll = if (values.isEmpty) belowAll else valueAbove(kinds.last)

    override def negate: IntervalSeq[T] = copy(belowAll = !belowAll, kinds = kinds.map(negateKind))
  }

}
