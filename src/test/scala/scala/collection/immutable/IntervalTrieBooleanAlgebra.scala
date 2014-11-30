package scala.collection.immutable

import spire.algebra.{Eq, BooleanAlgebra}

object IntervalTrieBooleanAlgebra {

  implicit object algebra extends BooleanAlgebra[IntervalTrie] with Eq[IntervalTrie] {
    import IntervalTrie._

    def eqv(x: IntervalTrie, y: IntervalTrie) =
      x == y

    def zero = IntervalTrie.zero

    def one = IntervalTrie.one

    def complement(a: IntervalTrie) = a flip true

    def or(a: IntervalTrie, b: IntervalTrie) = OrCalculator(a,b)

    def and(a: IntervalTrie, b: IntervalTrie) = AndCalculator(a, b)
  }
}
