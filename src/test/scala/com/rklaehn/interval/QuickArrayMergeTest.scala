package com.rklaehn.interval

import spire.implicits._
import org.junit.Assert._
import org.junit.Test

import scala.reflect.ClassTag


class QuickArrayMergeTest {

  @Test
  def testWorstCase(): Unit = {
    val a = Array.range(0,100).map(_ * 2)
    val b = Array.range(1,100).map(_ * 2)
    val o = new CountingOrder[Int]
    val r = QuickArrayMerge.merge(a,b)(o, ClassTag.Int)
//    println(r.sorted.corresponds(r)(_ == _))
//    println(r.size)
//    println(o.count)
  }
}
