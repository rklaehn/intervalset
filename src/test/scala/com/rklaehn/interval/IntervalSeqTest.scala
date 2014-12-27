package com.rklaehn.interval

import org.junit.Assert._
import org.junit.Test
import spire.implicits._

class IntervalSeqTest {

  import com.rklaehn.interval.IntervalSeq._

  @Test
  def leafOperationTest(): Unit = {
    val r = point(1) ^ point(1)
    println(r)
  }
}
