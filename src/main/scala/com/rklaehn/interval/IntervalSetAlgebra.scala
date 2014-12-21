package com.rklaehn.interval

import com.rklaehn.interval.IntervalSet.Element
import spire.algebra.{BooleanAlgebra, Eq}

object IntervalSetAlgebra {

  implicit def booleanAlgebra[T:Element] = new BooleanAlgebra[IntervalSet[T]] with Eq[IntervalSet[T]] {

    def eqv(x: IntervalSet[T], y: IntervalSet[T]) = x == y

    def zero = IntervalSet.zero[T]

    def one = IntervalSet.one[T]

    def complement(a: IntervalSet[T]) = ~a

    def or(a: IntervalSet[T], b: IntervalSet[T]) = a | b

    def and(a: IntervalSet[T], b: IntervalSet[T]) = a & b

    override def xor(a: IntervalSet[T], b: IntervalSet[T]) = a ^ b
  }

//  implicit def ordering[T:IntervalSetElement]: PartialOrdering[IntervalSet[T]] = new PartialOrdering[IntervalSet[T]] {
//
//    override def tryCompare(x: IntervalSet[T], y: IntervalSet[T]): Option[Int] = {
//      if(x == y)
//        Some(0)
//      else if(x.isSupersetOf(y))
//        Some(1)
//      else if(y.isSupersetOf(x))
//        Some(-1)
//      else
//        None
//    }
//
//    override def lteq(x: IntervalSet[T], y: IntervalSet[T]): Boolean = y.isProperSupersetOf(x)
//  }
}
