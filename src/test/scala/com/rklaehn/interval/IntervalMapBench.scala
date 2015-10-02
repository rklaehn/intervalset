package com.rklaehn.interval

import com.rklaehn.abc.NegatableArraySet
import spire.implicits._
import ichi.bench.Thyme
import spire.math.Interval

import scala.collection.immutable.{HashSet, SortedSet}

object IntervalMapBench extends App {
  val th = ichi.bench.Thyme.warmed(warmth = Thyme.HowWarm.Bench, verbose = println)

  def mapBench[T: IntervalMap.Value](title: String, f: Int ⇒ T): Unit = {
    val elements = Array.tabulate(1000000)(i ⇒ Interval(i, i + 2) → f(i))
    def create(): IntervalMap[Int, T] = IntervalMap(elements: _*)
    val timeline = th.pbenchWarm(th.Warm(create()), title = title + " create")
    val all = timeline.values
    def slice() = timeline & IntervalMap(Interval.above(500000) → all)
    val l = th.pbenchWarm(th.Warm(slice()), title = title + " slice")
    println(l.entries.size)
  }

  def invSetMapBench(title: String, f: Int ⇒ NegatableArraySet[String]): Unit = {
    val elements = Array.tabulate(1000000)(i ⇒ Interval(i, i + 2) → f(i))
    def create(): IntervalMap[Int, NegatableArraySet[String]] = IntervalMap(elements: _*)
    val timeline = th.pbenchWarm(th.Warm(create()), title = title + " create")
    def slice() = timeline & IntervalMap(Interval.above(500000) → NegatableArraySet.all[String])
    val l = th.pbenchWarm(th.Warm(slice()), title = title + " slice")
    println(l.entries.size)
  }

  invSetMapBench("invset", x ⇒ com.rklaehn.abc.NegatableArraySet(x.toString))
  mapBench("sortedset", x ⇒ SortedSet(x.toString))
  mapBench("set", x ⇒ Set(x.toString))
  mapBench("hashset", x ⇒ HashSet(x.toString): Set[String])
}
