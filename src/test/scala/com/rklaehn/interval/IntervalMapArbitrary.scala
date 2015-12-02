package com.rklaehn.interval

import org.scalacheck.{ Gen, Arbitrary }
import spire.implicits._

import scala.collection.immutable.SortedSet

object IntervalMapArbitrary {

  private def simpleGen[V: Arbitrary: IntervalMap.Value]: Gen[IntervalMap[Int, V]] = for {
    value <- implicitly[Arbitrary[V]].arbitrary
    is <- IntervalSeqArbitrary.arbitrary.arbitrary
  } yield is.intervals.map(i => IntervalMap(i, value)).reduceOption(_ ^ _).getOrElse(IntervalMap.empty[Int, V])

  private def combinedGen[V: Arbitrary: IntervalMap.Value]: Gen[IntervalMap[Int, V]] =
    for {
      levels <- Gen.containerOf[Array, IntervalMap[Int, V]](simpleGen[V])
    } yield
      levels.reduceOption(_ ^ _).getOrElse(IntervalMap.empty[Int, V])

  val intSetArbitrary = Arbitrary(combinedGen[Set[Int]])

  val intSortedSetArbitrary = Arbitrary(combinedGen[SortedSet[Int]])

  val boolArbitrary = Arbitrary(combinedGen[Boolean])
}
