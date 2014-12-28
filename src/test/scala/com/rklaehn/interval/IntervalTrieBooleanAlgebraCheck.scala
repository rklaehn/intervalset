package com.rklaehn.interval

object IntervalTrieBooleanAlgebraCheck
  extends BooleanAlgebraSpecification[IntervalTrie[Long]]("IntervalSet.BooleanAlgebra") {

  def algebra = IntervalTrieAlgebra.booleanAlgebra

  def eq = IntervalTrieAlgebra.booleanAlgebra

  def arb = IntervalTrieArbitrary.arbitrary
}
