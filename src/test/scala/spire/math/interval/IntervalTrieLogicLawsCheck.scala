package spire.math.interval

import org.scalacheck.Properties
import spire.laws.LogicLaws

object IntervalTrieLogicLawsCheck extends Properties("IntervalTrie") with AddProperties {

  val algebra = IntervalTrieAlgebra.booleanAlgebra[Long]

  val arb = IntervalTrieArbitrary.arbitrary

  addProperties("LogicLaws", LogicLaws(algebra, arb).bool(algebra))
}
