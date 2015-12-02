package com.rklaehn.interval

import spire.algebra.{Order, Bool, Eq}

object IntervalMapAlgebra {

  implicit def booleanAlgebra[K: Order, V: IntervalMap.Value: Bool] = new Bool[IntervalMap[K, V]] with Eq[IntervalMap[K, V]] {

    def eqv(x: IntervalMap[K, V], y: IntervalMap[K, V]) = x == y

    def zero = IntervalMap.zero[K, V]

    def one = IntervalMap.one[K, V]

    def complement(a: IntervalMap[K, V]) = ~a

    def or(a: IntervalMap[K, V], b: IntervalMap[K, V]) = a | b

    def and(a: IntervalMap[K, V], b: IntervalMap[K, V]) = a & b

    override def xor(a: IntervalMap[K, V], b: IntervalMap[K, V]) = a ^ b
  }
}
