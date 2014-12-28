package com.rklaehn.interval

import spire.algebra.Order

import scala.reflect.ClassTag

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
        // scalastyle:off return
        return mid
        // scalastyle:on return
      }
    }
    -(low + 1)
  }

  def merge[T: Order : ClassTag](a: Array[T], b: Array[T]): Array[T] = /* if(a.length > b.length) merge(b, a) else */ {
    val r = new Array[T](a.length + b.length)

    def merge0(a0: Int, a1: Int, b0: Int, b1: Int, i: Int): Int = {
      if (a0 == a1) {
        System.arraycopy(b, b0, r, i, b1 - b0)
        i + b1 - b0
      } else if (b0 == b1) {
        System.arraycopy(a, a0, r, i, a1 - a0)
        i + a1 - a0
      } else {
        val am = (a0 + a1) / 2
        val res = binarySearch(b, a(am), b0, b1)
        if (res >= 0) {
          // same elements
          val bm = res
          // merge everything below a(am) with everything below the found element
          val i1 = merge0(a0, am, b0, bm, i)
          // add the elements a(am) and b(bm)
          r(i1) = a(am)
          r(i1 + 1) = b(bm)
          // merge everything above a(am) with everything above the found element
          merge0(am + 1, a1, bm + 1, b1, i1 + 2)
        } else {
          val bm = -res - 1
          // merge everything below a(am) with everything below the found insertion point
          val i1 = merge0(a0, am, b0, bm, i)
          // add a(am)
          r(i1) = a(am)
          // everything above a(am) with everything above the found insertion point
          merge0(am + 1, a1, bm, b1, i1 + 1)
        }
      }
    }

    merge0(0, a.length, 0, b.length, 0)
    r
  }
}
