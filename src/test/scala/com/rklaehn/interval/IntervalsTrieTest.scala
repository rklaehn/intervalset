package com.rklaehn.interval

import org.junit.Assert._
import org.junit.Test
import spire.implicits._
import spire.math.Interval

object IntervalsTrieBench extends App {
  val th = ichi.bench.Thyme.warmed(warmth = ichi.bench.Thyme.HowWarm.Bench, verbose = println)
  val leafs = th.pbenchWarm(th.Warm((0L until 1000000L).map(i => IntervalsTrie.single(Interval.openUpper(i * 10, i* 10 +5), i))))
  // todo: use reducer
  val tree = th.pbenchWarm(th.Warm(leafs.reduce(_ xor _)))
  val truncated = th.pbenchWarm(th.Warm(tree.truncate(10003, 100000000L)))
  val access = th.pbenchWarm(th.Warm(tree.at(1000000)))
  println(access)
  println(truncated.entries.take(10).mkString(","))
}

class IntervalsTrieTest {

  @Test
  def testIntervalMap0(): Unit = {
    val is = Array(
      Interval.closed(0L, 10L) -> 1,
      Interval.open(0L, 20L) -> 2,
      Interval.all[Long] -> 3,
      Interval.point(0L) -> 4
    )
    val o = IntervalsTrie(is: _*)
    println(o.entries.mkString(","))
    println(is.map { case (i,v) => s"$i: $v" }.mkString("\n"))
    println(o.intervals(1))
    println(o.intervals(2))
    println(o.intervals(3))
    println(o.intervals(4))
    println(o.belowAll)
    println(o.elements.mkString(","))
    println("Truncated")
    val o1 = o.truncate(0, 10)
    println(o1.entries.mkString(","))
    println(o1.intervals(1))
    println(o1.intervals(2))
    println(o1.intervals(3))
    println(o1.intervals(4))
    println(o1.belowAll)
    println(o1.entries.mkString(","))
    // println(o.value)
    for(t <- Seq(-5L, 0L, 5L, 10L, 15L, 20L, 25L)) {
      val x = o.at(t)
      println(s"$t $x")
    }

    println(o.values)

    val o3 = o.filterValues(_ > 2)
    println(o3)
    println(o3.entries.mkString(","))
    println(o3.intervals(1))
    println(o3.intervals(2))
    println(o3.intervals(3))
    println(o3.intervals(4))
  }

}
