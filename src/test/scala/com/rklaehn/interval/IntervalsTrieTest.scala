package com.rklaehn.interval

import org.junit.Assert._
import org.junit.Test
import spire.implicits._
import spire.math.Interval

class IntervalsTrieTest {

  @Test
  def testIntervalMap0(): Unit = {
    val o = IntervalsTrie(
      Interval.closed(0L, 10L) -> 1,
      Interval.open(0L, 20L) -> 2,
      Interval.all[Long] -> 3,
      Interval.point(0L) -> 4
    )
    println(o.elements.mkString(","))
    // println(o.value)
    for(t <- Seq(-5L, 0L, 5L, 10L, 15L, 20L, 25L)) {
      val x = o.at(t)
      println(s"$t $x")
    }

    println(o.values)
  }

}
