package com.rklaehn.interval

import java.lang.Long.numberOfLeadingZeros

object DisassemblyTestApp {

  @noinline final def toPrefix(key: Long): Long = key - Long.MinValue

  @noinline final def fromPrefix(key: Long): Long = key + Long.MinValue

  @noinline final def unsigned_<(i: Long, j: Long) = (i < j) ^ (i < 0L) ^ (j < 0L)

  @noinline def levelAbove(a: Long, b: Long): Byte =
    (63 - numberOfLeadingZeros(a ^ b)).toByte

  @noinline final def maskAbove(prefix: Long, bit: Byte) = {
    // this is not the same as (-1L << (bit + 1)) due to the somewhat strange behavior of the java shift operator
    // -1L << 64 gives -1L, whereas (-1L << 63) << 1 gives 0L like we need
    prefix & ((-1L << bit) << 1)
  }

  @noinline final def zeroAt(value: Long, bit: Byte) =
    (value & (1L << bit)) == 0L

  @noinline final def hasMatchAt(key: Long, prefix: Long, level: Byte) =
    maskAbove(key, level) == prefix

  def main(args: Array[String]): Unit = {
    var i = 0
    var sum = 0L
    while(i < 100000) {
//      sum += levelAbove(i, i * 3 + 2)
//      sum += maskAbove(i, 4)
      sum += java.lang.Long.numberOfLeadingZeros(i)
      i += 1
    }
    println(sum)
  }
}
