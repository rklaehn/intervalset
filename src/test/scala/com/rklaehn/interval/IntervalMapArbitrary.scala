package com.rklaehn.interval

import org.scalacheck.{ Gen, Arbitrary }
import Arbitrary.arbitrary
import spire.algebra.{AdditiveGroup, Eq, Monoid}
import spire.implicits._

import scala.collection.immutable.SortedSet

object IntervalMapArbitrary {

  private def genFromBool[V: Arbitrary: IntervalMap.Value: Eq]: Gen[IntervalMap[Int, V]] = {
    import IntervalMap.CreateFromBool._

    def simpleGen: Gen[IntervalMap[Int, V]] = for {
      value <- arbitrary[V]
      is <- IntervalSeqArbitrary.arbIntervalSeq.arbitrary
    } yield is.intervals.map(i => IntervalMap(i, value)).reduceOption(_ ^ _).getOrElse(IntervalMap.zero[Int, V])

    for {
      levels <- Gen.containerOf[Array, IntervalMap[Int, V]](simpleGen)
    } yield
      levels.reduceOption(_ ^ _).getOrElse(IntervalMap.zero[Int, V])
  }

  private def genFromMonoid[V: Arbitrary: Monoid: Eq]: Gen[IntervalMap[Int, V]] = {
    import IntervalMap.CreateFromMonoid._

    val m = IntervalMap.monoid[Int, V]

    for {
      value <- arbitrary[V]
      is <- IntervalSeqArbitrary.arbIntervalSeq.arbitrary
    } yield is.intervals.map(i => IntervalMap(i, value)).reduceOption(m.op).getOrElse(m.id)
  }

  implicit def setEq[T] = spire.optional.genericEq.generic[Set[T]]

  implicit def sortedSet[T] = spire.optional.genericEq.generic[SortedSet[T]]

  implicit val intSetArbitrary = Arbitrary(genFromBool[Set[Int]])

  implicit val intSortedSetArbitrary = Arbitrary(genFromBool[SortedSet[Int]])

  implicit val boolArbitrary = Arbitrary(genFromBool[Boolean])

  implicit def g[T](implicit g:AdditiveGroup[T]) = g.additive
  implicit val intArbitrary = Arbitrary(genFromMonoid[Int])
}
