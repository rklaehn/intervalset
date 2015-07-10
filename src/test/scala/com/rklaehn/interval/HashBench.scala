package com.rklaehn.interval

import ichi.bench.Thyme

object HashBench extends App {
  val a = Array.tabulate(1000000)(_.toString)
  val th = ichi.bench.Thyme.warmed(warmth = Thyme.HowWarm.Bench)
  th.pbenchWarm(th.Warm(java.util.Arrays.hashCode(a.asInstanceOf[Array[AnyRef]])))
}
