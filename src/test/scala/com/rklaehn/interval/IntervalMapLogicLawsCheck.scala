package com.rklaehn.interval

import spire.implicits._
import org.scalacheck.Properties
import spire.laws.LogicLaws
import IntervalMapArbitrary._

import scala.collection.immutable.SortedSet

object IntervalMapLogicLawsCheck extends Properties("IntervalMap") with AddProperties {

  val boolAlgebra = IntervalMapAlgebra.booleanAlgebra[Int, Boolean]
//  val sortedSetAlgebra = IntervalMapAlgebra.booleanAlgebra[Int, SortedSet[Int]]

  addProperties("LogicLaws(Boolean)", LogicLaws(boolAlgebra, boolArbitrary).bool(boolAlgebra))
//  addProperties("LogicLaws(SortedSet[Int])", LogicLaws(sortedSetAlgebra, intSortedSetArbitrary).bool(sortedSetAlgebra))
}
