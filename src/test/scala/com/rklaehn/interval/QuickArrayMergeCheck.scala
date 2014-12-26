package com.rklaehn.interval

import java.util.Arrays

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop._
import spire.implicits._

object QuickArrayMergeCheck extends Properties("QuickArrayMerge") {

  implicit val arbitraryArray = implicitly[Arbitrary[Array[Int]]]

  property("merge") = forAll { (a:Array[Int], b:Array[Int]) =>
    val r = (a ++ b)
    Arrays.sort(a)
    Arrays.sort(b)
    Arrays.sort(r)
    val r1 = QuickArrayMerge.merge(a,b)
//    val sa = a.mkString(",")
//    val sb = b.mkString(",")
//    println(s"$sa\n$sb\n")
//    true
    r1.corresponds(r)(_ == _)
  }
}
