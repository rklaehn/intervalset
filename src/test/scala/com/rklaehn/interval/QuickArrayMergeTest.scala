package com.rklaehn.interval

import org.scalatest.FunSuite
import spire.implicits._

import scala.reflect.ClassTag

class QuickArrayMergeTest extends FunSuite {

  test("worstCase") {
    val a = Array.range(0, 100).map(_ * 2)
    val b = Array.range(1, 100).map(_ * 2)
    val o = new CountingOrder[Int]
    val r = QuickArrayMerge.merge(a, b)(o, ClassTag.Int)
    //    println(r.sorted.corresponds(r)(_ == _))
    //    println(r.size)
    //    println(o.count)
  }
}
