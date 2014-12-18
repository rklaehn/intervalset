package scala.collection.immutable

import org.junit.Assert._
import org.junit.Test

class IntervalSetManualTest {

  import IntervalTrie._
  import IntervalSet._

  @Test
  def leafOperationTest(): Unit = {
    val a = above(1L)
    val b = atOrAbove(1L)
    val c = point(1L)
    val d = hole(1L)
    assertEquals(atOrAbove(1L), a | b)
    assertEquals(above(1L), a & b)
    assertEquals(point(1L), a ^ b)
    assertEquals(atOrAbove(1L), a | c)
    assertEquals(zero[Long], a & c)
    assertEquals(atOrAbove(1L), a ^ c)
    assertEquals(hole(1L), a | d)
    assertEquals(above(1L), a & d)
    assertEquals(below(1L), a ^ d)
  }

  @Test
  def orManualTest(): Unit = {
    val a = IntervalSet[Long](false, Branch(-9223372036854775808L, 5, Branch(-9223372036854775808L, 3, Leaf(-9223372036854775807L, 0), Leaf(-9223372036854775800L, 2)), Leaf(-9223372036854775775L, 0)))
    val b = IntervalSet[Long](false, Branch(-9223372036854775808L, 5, Leaf(-9223372036854775795L, 0), Leaf(-9223372036854775775L, 0)))
    val r = a | b
    println(s"or\n$a\n$b\n$r")
    assertTrue(IntervalTrieSampleProperties.binarySampleTest(a, b, r, _ | _))
  }
}
