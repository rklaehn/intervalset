package com.rklaehn.interval

import spire.algebra.{Order, Bool, Eq}

object IntervalSeqAlgebra {

  implicit def booleanAlgebra[T:Order] = new Bool[IntervalSeq[T]] with Eq[IntervalSeq[T]] {

    def eqv(x: IntervalSeq[T], y: IntervalSeq[T]) = x == y

    def zero = IntervalSeq.zero[T]

    def one = IntervalSeq.one[T]

    def complement(a: IntervalSeq[T]) = ~a

    def or(a: IntervalSeq[T], b: IntervalSeq[T]) = a | b

    def and(a: IntervalSeq[T], b: IntervalSeq[T]) = a & b

    override def xor(a: IntervalSeq[T], b: IntervalSeq[T]) = a ^ b
  }
}
