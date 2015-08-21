//package com.rklaehn.interval
//
//import org.scalacheck.Prop._
//import org.scalacheck.Properties
//import spire.implicits._
//import org.scalacheck.Prop._
//
//object IntervalTrieSampleCheck extends Properties("IntervalsTrie.Sample") {
//
//  // this will resolve to the Arbitrary instance for Boolean from scalacheck
//  implicit def arb = IntervalSeqArbitrary.arbitrary
//
//  private def toIntervalsTrie(a: IntervalSeq[Long], v: String): IntervalsTrie[Long, String] = {
//    a.intervals
//      .map(x => IntervalsTrie.single(x, v))
//      .foldLeft(IntervalsTrie.empty[Long, String])(_ xor _)
//  }
//
//  property("sample_xor") = forAll { (a: IntervalSeq[Long], b: IntervalSeq[Long]) =>
//    val reference = a ^ b
//    val a1 = toIntervalsTrie(a, "x")
//    val b1 = toIntervalsTrie(b, "x")
//    val r1 = a1 xor b1
//    r1.intervals("x") == reference
//  }
//
//}
