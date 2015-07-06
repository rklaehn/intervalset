package com.rklaehn.interval

import org.junit.Assert._
import org.junit.Test
import spire.implicits._
import spire.math.Interval

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
    println(is.map { case (i,v) => s"$i: $v" }.mkString("\n"))
    println(o.intervals(1))
    println(o.intervals(2))
    println(o.intervals(3))
    println(o.intervals(4))
    println(o.belowAll)
    println(o.elements.mkString(","))
    println("Truncated")
    val o1 = o.truncate(0, 10)
    println(o1.intervals(1))
    println(o1.intervals(2))
    println(o1.intervals(3))
    println(o1.intervals(4))
    println(o1.belowAll)
    println(o1.elements.mkString(","))
    // println(o.value)
    for(t <- Seq(-5L, 0L, 5L, 10L, 15L, 20L, 25L)) {
      val x = o.at(t)
      println(s"$t $x")
    }

    println(o.values)
  }

}
