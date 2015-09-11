package com.rklaehn.interval

import spire.implicits._
import ichi.bench.Thyme
import spire.math.Interval

import scala.collection.immutable.{HashSet, SortedSet}

object IntervalsSetBench extends App {
  val th = ichi.bench.Thyme.warmed(warmth = Thyme.HowWarm.Bench, verbose = println)

  def setBench[T: IntervalsSet.Value](title: String, f: Int ⇒ T): Unit = {
    val elements = Array.tabulate(1000000)(i ⇒ Interval(i, i + 2) → f(i))
    def create(): IntervalsSet[Int, T] = IntervalsSet(elements: _*)
    val timeline = th.pbenchWarm(th.Warm(create()), title = title + " create")
    val all = timeline.all
    def slice() = timeline & IntervalsSet(Interval(100000, 200000) → all)
    val l = th.pbenchWarm(th.Warm(slice()), title = title + " slice")
  }

  setBench("sortedset", x ⇒ SortedSet(x.toString))
  setBench("set", x ⇒ Set(x.toString))
  setBench("hashset", x ⇒ HashSet(x.toString): Set[String])
}
