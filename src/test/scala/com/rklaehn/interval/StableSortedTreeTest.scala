package com.rklaehn.interval

import org.junit.Assert._
import org.junit.Test
import spire.algebra.{AdditiveMonoid, Monoid, Eq}
import spire.implicits._

class StableSortedTreeTest {

  @Test
  def testIntervalMap0(): Unit = {
    val m = IntervalMap.fromTo(0L, 10L, 1)
    val n = IntervalMap.fromTo(0L, 20L, 2)
    val o = m merge n
    println(o.elements.mkString(","))
    // println(o.value)
    println(o.at(-5L))
    println(o.at(0L))
    println(o.at(5L))
    println(o.at(10L))
    println(o.at(15L))
    println(o.at(20L))
    println(o.at(25L))
  }

  @Test
  def testAggregation(): Unit = {
    val res = (0L until 1000L).foldLeft(SortedSet.empty[Long, Int]) {
      case (m,x) => m merge SortedSet.single(x, 1)
    }
    assertEquals(1000, res.value)
  }

  implicit val intMonoid = implicitly[AdditiveMonoid[Int]].additive

  implicit val UnitEq: Eq[Unit] = new Eq[Unit] {

    def eqv(x: Unit, y: Unit) = true
  }

  implicit val UnitMonoid: Monoid[Unit] = new Monoid[Unit] {
    def id = ()

    def op(x: Unit, y: Unit) = ()
  }

  @Test
  def testBasic(): Unit = {
    val a = StableSortedTree.single[Long, Int](0L, 1)
    val b = StableSortedTree.single[Long, Int](100L, 1)
    val c = StableSortedTree.single[Long, Int](200L, 1)
    val d = StableSortedTree.merge[Long, Int](StableSortedTree.merge[Long, Int](a, b), c)
    println(d)
  }

  @Test
  def testFull(): Unit = {
    val elems = (0L until 1000L).map(x => x * 10).map(x => StableSortedTree.single(x, ()))
    val r1 = elems.reduce((a,b) => StableSortedTree.merge[Long, Unit](a,b))
    val r2 = elems.reverse.reduce((a,b) => StableSortedTree.merge[Long, Unit](a,b))
    val ref = StableSortedTree.keys(r1).mkString(",")
    assertEquals(ref, StableSortedTree.keys(r2).mkString(","))
    for(i <- 0 until 10) {
      val rn1 = elems.qshuffled.reduce((a,b) => StableSortedTree.merge[Long, Unit](a,b))
      val rn2 = elems.qshuffled.reduceRight((a,b) => StableSortedTree.merge[Long, Unit](a,b))
      val (s0, s1) = elems.qshuffled.splitAt(500)
      val rn3 = StableSortedTree.merge[Long, Unit](
        s0.reduce((a,b) => StableSortedTree.merge[Long, Unit](a,b)),
        s1.reduce((a,b) => StableSortedTree.merge[Long, Unit](a,b)))
      assertEquals(ref, StableSortedTree.keys(rn1).mkString(","))
      assertEquals(ref, StableSortedTree.keys(rn2).mkString(","))
      assertEquals(ref, StableSortedTree.keys(rn3).mkString(","))
      assertTrue(StableSortedTree.structuralEquals[Long, Unit](r1, rn1))
      assertTrue(StableSortedTree.structuralEquals[Long, Unit](r1, rn2))
      assertTrue(StableSortedTree.structuralEquals[Long, Unit](r1, rn3))
    }
  }
}
