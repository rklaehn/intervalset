package com.rklaehn.interval

import com.rklaehn.interval.IntervalTrie.Element
import spire.algebra.{Bool, Eq}

object IntervalTrieAlgebra {

  implicit def booleanAlgebra[T:Element] = new Bool[IntervalTrie[T]] with Eq[IntervalTrie[T]] {

    def eqv(x: IntervalTrie[T], y: IntervalTrie[T]) = x == y

    def zero = IntervalTrie.empty[T]

    def one = IntervalTrie.all[T]

    def complement(a: IntervalTrie[T]) = ~a

    def or(a: IntervalTrie[T], b: IntervalTrie[T]) = a | b

    def and(a: IntervalTrie[T], b: IntervalTrie[T]) = a & b

    override def xor(a: IntervalTrie[T], b: IntervalTrie[T]) = a ^ b
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
