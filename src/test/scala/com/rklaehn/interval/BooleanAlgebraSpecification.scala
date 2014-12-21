package com.rklaehn.interval

import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Properties}
import spire.algebra.{BooleanAlgebra, Eq}
import spire.syntax.booleanAlgebra._
import spire.syntax.eq._

/**
 * Abstract specification for a boolean algebra. Copied over from the spire test framework.
 * @param name Name for the concrete specification
 * @tparam A
 */
abstract class BooleanAlgebraSpecification[A](name:String) extends Properties(name) {

  implicit def algebra : BooleanAlgebra[A]

  implicit def eq : Eq[A]

  implicit def arb: Arbitrary[A]

  /**
   * This is just a workaround because we don't have the latest spire version
   */
  implicit class ImpPimp(val lhs:A) {

    def imp(rhs:A) = ~lhs | rhs
  }

  property("associative") = forAll { (x: A, y: A, z: A) =>
    ((x & y) & z) === (x & (y & z)) && ((x | y) | z) === (x | (y | z))
  }

  property("commutative") = forAll { (x: A, y: A) =>
    (x & y) === (y & x) && (x | y) === (y | x)
  }

  property("adsorption") = forAll { (x: A, y: A) =>
    (x & (x | y)) === x && (x | (x & y)) === x
  }

  property("distributive") = forAll { (x: A, y: A, z: A) =>
    (x & (y | z)) === ((x & y) | (x & z)) && (x | (y & z)) === ((x | y) & (x | z))
  }

  property("consistent") = forAll { (x: A) => (x & ~x) === algebra.zero }

  property("excluded middle") = forAll { (x: A) => (x | ~x) === algebra.one }

  property("¬x = (x → 0)") = forAll { (x: A) => ~x === (x imp algebra.zero) }

  property("x → x = 1") = forAll { (x: A) => (x imp x) === algebra.one }

  property("if x → y and y → x then x=y") = forAll { (x: A, y: A) =>
    ((x imp y) =!= algebra.one) || ((y imp x) =!= algebra.one) || x === y
  }

  property("if (1 → x)=1 then x=1") = forAll { (x: A) =>
    ((algebra.one imp x) =!= algebra.one) || (x === algebra.one)
  }

  property("x → (y → x) = 1") = forAll { (x: A, y: A) => (x imp (y imp x)) === algebra.one }

  property("(x→(y→z)) → ((x→y)→(x→z)) = 1") = forAll { (x: A, y: A, z: A) =>
    ((x imp (y imp z)) imp ((x imp y) imp (x imp z))) === algebra.one
  }

  property("x∧y → x = 1") = forAll { (x: A, y: A) => ((x & y) imp x) === algebra.one }

  property("x∧y → y = 1") = forAll { (x: A, y: A) => ((x & y) imp y) === algebra.one }

  property("x → y → (x∧y) = 1") = forAll { (x: A, y: A) => (x imp (y imp (x & y))) === algebra.one }

  property("x → x∨y") = forAll { (x: A, y: A) => (x imp (x | y)) === algebra.one }

  property("y → x∨y") = forAll { (x: A, y: A) => (y imp (x | y)) === algebra.one }

  property("(x → z) → ((y → z) → ((x | y) → z)) = 1") = forAll { (x: A, y: A, z: A) =>
    ((x imp z) imp ((y imp z) imp ((x | y) imp z))) === algebra.one
  }

  property("(0 → x) = 1") = forAll { (x: A) => (algebra.zero imp x) === algebra.one }
}
