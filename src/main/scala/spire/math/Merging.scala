package spire.math
import spire.algebra.Order

import scala.annotation.tailrec
import scala.reflect.ClassTag

abstract class BinaryMerge {

  private[this] final def binarySearchB(ai: Int, b0: Int, b1: Int): Int = {

    @tailrec
    def binarySearch0(low: Int, high: Int): Int =
      if (low <= high) {
        val mid = (low + high) >>> 1
        val c = compare(ai, mid)
        if (c > 0)
          binarySearch0(mid + 1, high)
        else if (c < 0)
          binarySearch0(low, mid - 1)
        else
          mid
      } else -(low + 1)
    binarySearch0(b0, b1 - 1)
  }

  def compare(ai: Int, bi: Int): Int

  def collision(ai: Int, bi: Int): Unit

  def fromA(a0: Int, a1: Int, bi: Int): Unit

  def fromB(ai: Int, b0: Int, b1: Int): Unit

  def merge0(a0: Int, a1: Int, b0: Int, b1: Int): Unit = {
    if (a0 == a1) {
      if (b0 != b1)
        fromB(a0, b0, b1)
    } else if (b0 == b1) {
      fromA(a0, a1, b0)
    } else {
      val am = (a0 + a1) / 2
      val res = binarySearchB(am, b0, b1)
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
        fromA(am, am + 1, bm)
        // everything above a(am) with everything above the found insertion point
        merge0(am + 1, a1, bm, b1)
      }
    }
  }
}

object BinaryMerge {

  def merge[@specialized T: Order: ClassTag](a: Array[T], b: Array[T]): Array[T] = {
    new ArrayBinaryMerge(a,b).result
  }

  def linearMerge[@specialized T: Order : ClassTag](a: Array[T], b: Array[T]): Array[T] = {
    val o = implicitly[Order[T]]
    val r = Array.ofDim[T](a.length + b.length)
    var ri = 0
    var ai = 0
    var bi = 0
    while (ai < a.length && bi < b.length) {
      val c = o.compare(a(ai), b(bi))
      if (c < 0) {
        r(ri) = a(ai)
        ri += 1
        ai += 1
      } else if (c > 0) {
        r(ri) = b(bi)
        ri += 1
        bi += 1
      } else {
        r(ri) = b(bi)
        ri += 1
        ai += 1
        bi += 1
      }
    }
    while (ai < a.length) {
      r(ri) = a(ai)
      ri += 1
      ai += 1
    }
    while (bi < b.length) {
      r(ri) = b(bi)
      ri += 1
      bi += 1
    }
    resize(r, ri)
  }

  private[this] def resize[T:ClassTag](x:Array[T], n: Int): Array[T] = {
    if (n == x.length)
      x
    else {
      val t = Array.ofDim[T](n)
      System.arraycopy(x, 0, t, 0, n)
      t
    }
  }

  private class ArrayBinaryMerge[@specialized T](a: Array[T], b: Array[T])(implicit o: Order[T], c: ClassTag[T]) extends BinaryMerge {

    def compare(ai: Int, bi: Int) = o.compare(a(ai), b(bi))

    def fromA(a0: Int, a1: Int, bi: Int) = {
      System.arraycopy(a, a0, r, ri, a1 - a0)
      ri += a1 - a0
    }

    def fromB(ai: Int, b0: Int, b1: Int) = {
      System.arraycopy(b, b0, r, ri, b1 - b0)
      ri += b1 - b0
    }

    def collision(ai: Int, bi: Int) = {
      r(ri) = a(ai)
      ri += 1
    }

    val r = Array.ofDim[T](a.length + b.length)
    var ri = 0
    merge0(0, a.length, 0, b.length)

    def result =
      if (ri == r.length)
        r
      else {
        val t = Array.ofDim[T](ri)
        System.arraycopy(r, 0, t, 0, ri)
        t
      }
  }
}