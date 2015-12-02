package com.rklaehn.interval

import spire.algebra.Order

/**
 * A wrapper for the spire order typeclass that counts invocations of compare
 */
class CountingOrder[T: Order] extends Order[T] {
  val wrapped = implicitly[Order[T]]
  var count = 0

  override def compare(x: T, y: T): Int = {
    count += 1
    wrapped.compare(x, y)
  }
}
