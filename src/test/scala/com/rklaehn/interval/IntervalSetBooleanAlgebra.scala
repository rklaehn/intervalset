package com.rklaehn.interval

import spire.algebra.{Eq, BooleanAlgebra}

object IntervalSetBooleanAlgebra {

  implicit def algebra[T:IntervalSet.IntervalSetElement] = new BooleanAlgebra[IntervalSet[T]] with Eq[IntervalSet[T]] {

    def eqv(x: IntervalSet[T], y: IntervalSet[T]) = x == y

    def zero = IntervalSet.zero[T]

    def one = IntervalSet.one[T]

    def complement(a: IntervalSet[T]) = ~a

    def or(a: IntervalSet[T], b: IntervalSet[T]) = a | b

    def and(a: IntervalSet[T], b: IntervalSet[T]) = a & b

    override def xor(a: IntervalSet[T], b:IntervalSet[T]) = a ^ b
  }
}
