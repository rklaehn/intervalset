package com.rklaehn.rope

import spire.algebra.Order

import scala.annotation.tailrec
import scala.reflect.ClassTag

object Slice {
  def apply[T](rope: Rope[T]): Slice[T] =
    Slice(rope, SearchResult.before(0), SearchResult.before(rope.length))
}

case class Slice[T](var rope: Rope[T], var from: SearchResult, var until: SearchResult) {

  def init(source: Slice[T]): Unit = {
    from = source.from
    until = source.until
    rope = source.rope
  }

  def search(key: T)(implicit order: Order[T]): SearchResult =
    rope.search(from.toIndex, until.toIndex, key)

  def from(value: SearchResult): Unit = {
    from = value
    @tailrec
    def descend(): Unit = rope match {
      case tree: Rope.Branch[T] if from.toIndex >= tree.weight =>
        from += tree.weight
        until += tree.weight
        rope = tree.right
        descend()
      case _ =>
    }
    descend()
  }

  def until(value: SearchResult): Unit = {
    until = value
    @tailrec
    def descend(): Unit = rope match {
      case tree: Rope.Branch[T] if until.toIndex <= tree.weight =>
        rope = tree.left
        descend()
      case _ =>
    }
    descend()
  }
}

sealed abstract class Rope[T: ClassTag] {

  def length: Int

  def apply(x: Int): T

  def ++(that: Rope[T]): Rope[T]

  def copyToArray(from: Int, until: Int, target: Array[T], offset: Int): Unit

  def search(from: Int, until: Int, key: T)(implicit order: Order[T]): SearchResult
}

object Rope {

  def apply[T: ClassTag](array: Array[T]): Rope[T] = Leaf(array.clone)

  sealed abstract class Tree[T: ClassTag] extends Rope[T] {

    def weight: Int

    def head: T

    def ++(that: Rope[T]): Rope[T] = Branch(this, that.asInstanceOf[Tree[T]])
  }

  case class Leaf[T: ClassTag](elements: Array[T]) extends Tree[T] {

    def head: T = elements.head

    def apply(index: Int): T = elements(index)

    def length: Int = elements.length

    def weight: Int = elements.length

    private def binarySearch(from: Int, until: Int, key: T)(implicit order: Order[T]): SearchResult = {

      @tailrec
      def binarySearch0(low: Int, high: Int): SearchResult =
        if (low <= high) {
          val mid = (low + high) >>> 1
          val midVal = elements(mid)
          val c = order.compare(midVal, key)
          if (c < 0)
            binarySearch0(mid + 1, high)
          else if (c > 0)
            binarySearch0(low, mid - 1)
          else
            SearchResult(mid)
        } else SearchResult.before(low)
      binarySearch0(from, until - 1)
    }

    def search(from: Int, until: Int, key: T)(implicit order: Order[T]): SearchResult =
      binarySearch(from, until, key)

    def copyToArray(from: Int, until: Int, target: Array[T], offset: Int): Unit =
      System.arraycopy(elements, from, target, offset, until - from)
  }

  case class Branch[T: ClassTag](left: Tree[T], right: Tree[T]) extends Tree[T] {

    def head = left.head

    val pivot = right.head

    val length: Int = left.length + right.length

    def weight: Int = left.length

    def apply(index: Int) = if (index < weight) left(index) else right(index - weight)

    override def search(from: Int, until: Int, key: T)(implicit order: Order[T]): SearchResult =
      if (from >= until)
        SearchResult.before(from)
      else if (until <= weight)
        left.search(from, until, key)
      else if (from >= weight)
        right.search(from - weight, until - weight, key) + weight
      else {
        val c = order.compare(key, pivot)
        if (c < 0)
          left.search(from, weight, key)
        else if (c > 0)
          right.search(1, until - weight, key) + weight
        else
          SearchResult(weight)
      }

    def copy(left: Tree[T], right: Tree[T]): Branch[T] =
      if ((left eq this.left) && (right == this.right))
        this
      else
        Branch(left, right)

    override def copyToArray(from: Int, until: Int, target: Array[T], offset: Int): Unit = {
      val l = (weight - from) max 0
      val r = (until - weight) max 0
      if (l > 0)
        left.copyToArray(from, from + l, target, offset)
      if (r > 0)
        right.copyToArray(until - r - weight, until - weight, target, offset + r)
    }
  }
}