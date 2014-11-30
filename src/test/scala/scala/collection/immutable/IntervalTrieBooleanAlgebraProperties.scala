package scala.collection.immutable

import org.scalacheck.Test.Parameters
import org.scalacheck.{Prop, Properties, Gen, Arbitrary}
import spire.algebra.{Eq, BooleanAlgebra}
import spire.std.any._
import org.scalacheck.Prop._
import spire.syntax.all._

object IntervalTrieBooleanAlgebraProperties
  extends BooleanAlgebraSpecification[IntervalTrie]("IntervalTrie.BooleanAlgebra") {

  def algebra = IntervalTrieBooleanAlgebra.algebra

  def eq = IntervalTrieBooleanAlgebra.algebra

  def arb = IntervalTrieArbitrary.arbitrary

}



