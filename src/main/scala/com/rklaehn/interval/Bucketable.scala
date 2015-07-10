//package com.rklaehn.interval
//
//trait Bucket[S <: Bucket[S]] {
//
//  def merge(that: S): S
//
//  def compareHeight(that: S): Int
//
//  def compare(that: S): Int
//
//  def contains(that: S): Boolean
//}
//
//trait Bucketable[T, S <: Bucket[S]] {
//
//  def bucket(a: T, b: T): S
//}
//
//object Bucketable {
//
//  case class LongBucket(p: Long, hw: Long) extends Bucket[LongBucket] {
//
//    def merge(that: LongBucket) = {
//
//    }
//
//    def compareHeight(that: LongBucket) = java.lang.Long.compare(this.hw, that.hw)
//
//    def compare(that: LongBucket) =
//      if(this.p + this.hw <= that.p - that.hw)
//        -1
//      else if(this.p - this.hw >= that.p + that.hw)
//        +1
//      else
//        0
//
//    def contains(that: LongBucket) = {
//      ???
//    }
//  }
//
//  implicit object LongIsBucketable extends Bucketable[Long, LongBucket] {
//
//    def bucket(a: Long, b: Long): Bucket[LongBucket] = {
//      if(a == b) LongBucket(a, 0L)
//      else if((a.signum < 0) != (b.signum < 0)) LongBucket(0L, Long.MaxValue)
//      else {
//        val bit = java.lang.Long.highestOneBit(a ^ b)
//        val mask = ~(bit - 1)
//        LongBucket(math.max(a, b) & mask, bit)
//      }
//    }
//
//  }
//}