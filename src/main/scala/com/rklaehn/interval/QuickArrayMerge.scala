package com.rklaehn.interval

import spire.algebra.Order

import scala.reflect.ClassTag

object CountingOrder {
  def apply[T:Order, U](f: Order[T] => U) = {
    val countingOrder = new CountingOrder[T]
    val result = f(countingOrder)
    println("Order was called " + countingOrder.count + " times")
    result
  }
}

class CountingOrder[T:Order] extends Order[T] {
  val wrapped = implicitly[Order[T]]
  var count = 0

  override def compare(x: T, y: T): Int = {
    count += 1
    wrapped.compare(x, y)
  }
}

object QuickArrayMerge {

  def binarySearch[T: Order](array: Array[T], key: T, from: Int, until: Int): Int = {
    val order = implicitly[Order[T]]
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

  def merge[T: Order : ClassTag](a: Array[T], b: Array[T]): Array[T] = {
    val builder = Array.newBuilder[T]
    builder.sizeHint(a.length + b.length)

    def merge0(a0: Int, a1: Int, b0: Int, b1: Int): Unit = {
      if (a0 == a1) {
        for (bi <- b0 until b1)
          builder += b(bi)
      } else if (b0 == b1) {
        for (ai <- a0 until a1)
          builder += a(ai)
      } else {
        val am = (a0 + a1) / 2
        val res = binarySearch(b, a(am), b0, b1)
        if (res >= 0) {
          // same elements
          val bm = res
          // merge everything below a(am) with everything below the found element
          merge0(a0, am, b0, bm)
          // add the elements a(am) and b(bm)
          builder += a(am)
          builder += b(bm)
          // merge everything above a(am) with everything above the found element
          merge0(am + 1, a1, bm + 1, b1)
        } else {
          val bm = -res - 1
          // merge everything below a(am) with everything below the found insertion point
          merge0(a0, am, b0, bm)
          // add a(am)
          builder += a(am)
          // everything above a(am) with everything above the found insertion point
          merge0(am + 1, a1, bm, b1)
        }
      }
    }

    merge0(0, a.length, 0, b.length)
    builder.result()
  }
}
