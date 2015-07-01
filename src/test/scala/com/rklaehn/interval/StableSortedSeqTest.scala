package com.rklaehn.interval

import org.junit.Assert._
import org.junit.Test
import spire.implicits._

class StableSortedSeqTest {

  @Test
  def testBasic(): Unit = {
    val a = StableSortedSeq.single(0L, ())
    val b = StableSortedSeq.single(100L, ())
    val c = StableSortedSeq.single(200L, ())
    val d = StableSortedSeq.merge(StableSortedSeq.merge(a, b), c)
    println(d)
  }

  @Test
  def testFull(): Unit = {
    val (x, w) = StableSortedSeq.LongPartitioner.partition(1, 3)
    println(s"$x $w")
    val elems = (0L until 1000L).map(x => x * 10).map(x => StableSortedSeq.single(x, ()))
    val r1 = elems.reduce((a,b) => StableSortedSeq.merge(a,b))
    val r2 = elems.qshuffled.reduce((a,b) => {
      val ac = StableSortedSeq.keys(a).size
      val bc = StableSortedSeq.keys(b).size
      val r = StableSortedSeq.merge(a,b)
      val rc = StableSortedSeq.keys(r).size
      if(ac + bc != rc)
        require(ac + bc == rc)
      r
    })
    val r3 = elems.reverse.reduce((a,b) => StableSortedSeq.merge(a,b))
    println(StableSortedSeq.keys(r1).mkString(","))
    println(StableSortedSeq.keys(r2).mkString(","))
    println(StableSortedSeq.keys(r3).mkString(","))
  }
}
