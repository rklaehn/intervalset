package spire.math

import org.junit.Assert._
import org.junit.Test
import spire.math.interval.CountingOrder
import spire.implicits._

import scala.reflect.ClassTag

class BinaryMergeTest {

  @Test
  def testWorstCase(): Unit = {
    val a = Array.range(0,100).map(_ * 2)
    val b = Array.range(1,100).map(_ * 2)
    val o = new CountingOrder[Int]
    val r = BinaryMerge.merge(a,b)(o, ClassTag.Int)
    assertTrue(r.sorted.corresponds(r)(_ == _))
    assertTrue(o.count < 200)
  }

  @Test
  def testLinearMerge(): Unit = {
    val a = Array.range(0,100).map(_ * 2)
    val b = Array.range(1,100).map(_ * 3)
    val o = new CountingOrder[Int]
    val r1 = BinaryMerge.linearMerge(a,b)(o, ClassTag.Int)
    val r2 = BinaryMerge.linearMerge(b,a)(o, ClassTag.Int)
    assertTrue(r1.sorted.corresponds(r1)(_ == _))
    assertTrue(r2.sorted.corresponds(r2)(_ == _))
  }
}
