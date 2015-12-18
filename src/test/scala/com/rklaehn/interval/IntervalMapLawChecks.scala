package com.rklaehn.interval

import spire.algebra.AdditiveGroup
import spire.implicits._
import org.scalacheck.Properties
import spire.laws.{GroupLaws, LogicLaws}
import IntervalMapArbitrary._

object IntervalMapLawChecks extends Properties("IntervalMap") with AddProperties {

  val boolAlgebra = IntervalMapAlgebra.booleanAlgebra[Int, Boolean]
//  val sortedSetAlgebra = IntervalMapAlgebra.booleanAlgebra[Int, SortedSet[Int]]

  addProperties("LogicLaws(Boolean)", LogicLaws(boolAlgebra, boolArbitrary).bool(boolAlgebra))
//  addProperties("LogicLaws(SortedSet[Int])", LogicLaws(sortedSetAlgebra, intSortedSetArbitrary).bool(sortedSetAlgebra))

  implicit def g[T](implicit g:AdditiveGroup[T]) = g.additive
  addProperties("MonoidLaws(Int)", GroupLaws[IntervalMap[Int, Int]].group)
}
