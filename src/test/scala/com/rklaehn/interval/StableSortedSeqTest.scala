package com.rklaehn.interval

import org.junit.Assert._
import org.junit.Test
import spire.algebra.Eq
import spire.implicits._

class StableSortedSeqTest {

  implicit val UnitEq: Eq[Unit] = new Eq[Unit] {

    def eqv(x: Unit, y: Unit) = true
  }

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
    val r2 = elems.reverse.reduce((a,b) => StableSortedSeq.merge(a,b))
    val ref = StableSortedSeq.keys(r1).mkString(",")
    assertEquals(ref, StableSortedSeq.keys(r2).mkString(","))
    for(i <- 0 until 10) {
      val rn1 = elems.qshuffled.reduce((a,b) => StableSortedSeq.merge(a,b))
      val rn2 = elems.qshuffled.reduceRight((a,b) => StableSortedSeq.merge(a,b))
      val (s0, s1) = elems.qshuffled.splitAt(500)
      val rn3 = StableSortedSeq.merge(
        s0.reduce((a,b) => StableSortedSeq.merge(a,b)),
        s1.reduce((a,b) => StableSortedSeq.merge(a,b)))
      assertEquals(ref, StableSortedSeq.keys(rn1).mkString(","))
      assertEquals(ref, StableSortedSeq.keys(rn2).mkString(","))
      assertEquals(ref, StableSortedSeq.keys(rn3).mkString(","))
      assertTrue(StableSortedSeq.structuralEquals[Long, Unit](r1, rn1))
      assertTrue(StableSortedSeq.structuralEquals[Long, Unit](r1, rn2))
      assertTrue(StableSortedSeq.structuralEquals[Long, Unit](r1, rn3))
    }
  }
}
