//package com.rklaehn.interval
//
//import spire.algebra.Order
//
//import scala.annotation.tailrec
//import spire.implicits._
//
//import scala.reflect.ClassTag
//
//sealed abstract class PivotableTree[T] {
//
//  def size: Int
//
//  def head: T
//
//  def last: T
//}
//
//object PivotableTree {
//
//  case class Branch[T](left: PivotableTree[T], right: PivotableTree[T]) extends PivotableTree[T] {
//    val size = left.size + right.size
//
//    val head = left.head
//
//    val last = right.last
//  }
//
//  case class Leaf[T](elements: Array[T], li: Boolean, ui: Boolean) extends PivotableTree[T] {
//    def size = elements.length
//
//    def head = elements.head
//
//    def last = elements.last
//  }
//
//  trait Element[T, U] {
//
//    def maxLeaf: Int
//
//    def elem(a: T): U
//
//    def pivot(a: T, b: T): U
//
//    def compare(a: T, b: U): Int
//
//    def compare(a: T, b: T): Int
//
//    def tOrder: Order[T]
//
//    def tClassTag: ClassTag[T]
//  }
//
//  def concat[T](a: PivotableTree[T], b: PivotableTree[T]): PivotableTree[T] =
//    if(a eq null) b else if(b eq null) a else Branch(a, b)
//
//  def splitAt[T, U](tree: PivotableTree[T], key: U, keyIsPivot: Boolean)(implicit e: Element[T, U]): (PivotableTree[T], PivotableTree[T]) = {
//    if(e.compare(tree.last, key) < 0)
//      (tree, null)
//    else if(e.compare(tree.head, key) > 0)
//      (null, tree)
//    else tree match {
//      case leaf@Leaf(elements, li, ui) =>
//        // split
//        ???
//      case branch@Branch(l, r) =>
//        val (l0, l1) = splitAt(l, key, keyIsPivot)
//        val (r0, r1) = splitAt(r, key, keyIsPivot)
//        (concat(l0, r0), concat(l1, r1))
//    }
//  }
//
//  def slice[T](tree: PivotableTree[T], from: Int, until: Int): PivotableTree[T] = tree match {
//    case Leaf(content, li, ui) =>
//      val content1 = content.slice(from, until)
//      val li1 = li && from == 0
//      val ui1 = ui && until == content.length
//      Leaf(content1, li1, ui1)
//    case branch@Branch(left, right) =>
//      if(from == 0 && until == branch.size)
//        branch
//      else if(until <= left.size)
//        slice(left, from, until)
//      else if(from > left.size)
//        slice(right, from - left.size, until - left.size)
//      else
//        Branch(
//          slice(left, from, left.size),
//          slice(right, 0, until - left.size)
//        )
//  }
//
//  def create[T, U](elements: Array[T])(implicit e: Element[T, U]): PivotableTree[T] =
//    new Creator[T, U](elements, 0, elements.length).result
//
//  def merge[T, U](a: PivotableTree[T], b: PivotableTree[T])(implicit e: Element[T, U]): PivotableTree[T] = {
//    if(a eq null) b
//    else if(b eq null) a
//    else new Merger(a, b).result
//  }
//
//  private class Merger[T, U](a: PivotableTree[T], b: PivotableTree[T])(implicit e: Element[T, U]) {
//
//    def merge(a: PivotableTree[T], b: PivotableTree[T]): PivotableTree[T] = {
//      if(e.compare(a.last, b.head) < 0)
//        Branch(a, b)
//      (a, b) match {
//        case (a@Leaf(ae, ali, aui), b@Leaf(be, bli, bui)) =>
//          if(e.compare(ae.last, be.head) < 0) // a is before b
//            Branch(a, b)
//          else if(e.compare(ae.head, be.last) > 0) // b is before a
//            Branch(b, a)
//          else {
//            // sort them both
//            val e1 = (ae ++ be).toArray(e.tClassTag)
//            e1.qsort(e.tOrder, e.tClassTag)
//            create(e1)
//          }
//      }
//    }
//
//    def result: PivotableTree[T] = merge(a, b)
//  }
//
//  private class Creator[T, U](a: Array[T], from: Int, until: Int)(implicit e: Element[T, U]) {
//
//    def binarySearch(key: U, from: Int, until: Int): Int = {
//      @tailrec
//      def binarySearch0(low: Int, high: Int): Int =
//        if (low <= high) {
//          val mid = (low + high) >>> 1
//          val midVal = a(mid)
//          val c = e.compare(midVal, key)
//          if (c < 0)
//            binarySearch0(mid + 1, high)
//          else if (c > 0)
//            binarySearch0(low, mid - 1)
//          else
//            mid
//        } else -(low + 1)
//      binarySearch0(from, until - 1)
//    }
//
//    def tree(from: Int, until: Int): PivotableTree[T] = {
//      if(until - from <= e.maxLeaf) {
//        Leaf(
//          a.slice(from, until),
//          from != this.from,
//          until != this.until)
//      } else {
//        val pivot = e.pivot(a(from), a(until - 1))
//        val mid = binarySearch(pivot, from, until)
//        Branch(
//          tree(from, mid),
//          tree(mid, until)
//        )
//      }
//    }
//
//    def result = tree(from, until)
//  }
//}