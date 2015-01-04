package com.rklaehn.geom

import org.junit.Assert._
import org.junit.Test
import spire.implicits._

class PointSetTest {

  @Test
  def simpleTest(): Unit = {
    val test =
      PointSet.point(0, 0, 1) +
      PointSet.point(2, 2, 1) +
      PointSet.edge(3, 3, 0, 0, 0, 1) +
      PointSet.edge(5, 5, 0, 0, 0, -1)
    assertEquals(test(0, 0), 1)
    assertEquals(test(2, 2), 1)
    assertEquals(test(3, 3), 0)
    assertEquals(test(3, 4), 0)
    assertEquals(test(4, 3), 0)
    assertEquals(test(4, 4), 1)
    assertEquals(test(5, 5), 1)
    assertEquals(test(6, 6), 0)
  }

  @Test
  def negateTest(): Unit = {
    val test = -(
      PointSet.point(0, 0, 1) +
        PointSet.point(2, 2, 1) +
        PointSet.edge(3, 3, 0, 0, 0, 1) +
        PointSet.edge(5, 5, 0, 0, 0, -1))
    assertEquals(test(0, 0), -1)
    assertEquals(test(2, 2), -1)
    assertEquals(test(3, 3), 0)
    assertEquals(test(3, 4), 0)
    assertEquals(test(4, 3), 0)
    assertEquals(test(4, 4), -1)
    assertEquals(test(5, 5), -1)
    assertEquals(test(6, 6), 0)
  }

  @Test
  def verticalEdgeTest1(): Unit = {
    val x = 5
    val ya = 0
    val yb = 10
    val level = 1
    val test = PointSet.verticalEdge(x = x, ya = ya, yb = yb, level = level)
    assertEquals(0, test(x, ya-1))
    assertEquals(level, test(x, ya))
    assertEquals(level, test(x, ya+1))
    assertEquals(level, test(x, yb-1))
    assertEquals(level, test(x, yb))
    assertEquals(0, test(x, yb+1))

    assertEquals(0, test(x-1, ya-1))
    assertEquals(0, test(x-1, ya))
    assertEquals(0, test(x-1, ya +1))
    assertEquals(0, test(x-1, yb -1))
    assertEquals(0, test(x-1, yb))
    assertEquals(0, test(x-1, yb + 1))

    assertEquals(0, test(x+1, ya-1))
    assertEquals(0, test(x+1, ya))
    assertEquals(0, test(x+1, ya +1))
    assertEquals(0, test(x+1, yb -1))
    assertEquals(0, test(x+1, yb))
    assertEquals(0, test(x+1, yb + 1))
  }

  @Test
  def horizontalEdgeTest1(): Unit = {
    val xa = 0
    val xb = 10
    val y = 5
    val level = 1
    val test = PointSet.horizontalEdge(xa = xa, xb = xb, y = y, level = level)
    assertEquals(0, test(xa-1, y))
    assertEquals(level, test(xa, y))
    assertEquals(level, test(xa+1, y))
    assertEquals(level, test(xb-1, y))
    assertEquals(level, test(xb, y))
    assertEquals(0, test(xb+1, y))

    assertEquals(0, test(xa-1, y-1))
    assertEquals(0, test(xa, y-1))
    assertEquals(0, test(xa+1, y-1))
    assertEquals(0, test(xb-1, y-1))
    assertEquals(0, test(xb, y-1))
    assertEquals(0, test(xb+1, y-1))

    assertEquals(0, test(xa-1, y+1))
    assertEquals(0, test(xa, y+1))
    assertEquals(0, test(xa+1, y+1))
    assertEquals(0, test(xb-1, y+1))
    assertEquals(0, test(xb, y+1))
    assertEquals(0, test(xb+1, y+1))
  }

  @Test
  def rectangleTest1(): Unit = {
    val xa = 3
    val ya = 5
    val xb = 13
    val yb = 17
    val outline = 2
    val inside = -1
    val test = PointSet.rectangle(xa = xa, ya = ya, xb = xb, yb = yb, outline = outline, inside = inside)
    assertEquals(0,       test(xa-1,ya-1))
    assertEquals(0,       test(xa-1,ya+0))
    assertEquals(0,       test(xa-1,ya+1))
    assertEquals(0,       test(xa+0,ya-1))
    assertEquals(outline, test(xa+0,ya+0))
    assertEquals(outline, test(xa+0,ya+1))
    assertEquals(0,       test(xa+1,ya-1))
    assertEquals(outline, test(xa+1,ya+0))
    assertEquals(inside,  test(xa+1,ya+1))

    assertEquals(0,       test(xb-1,ya-1))
    assertEquals(outline, test(xb-1,ya+0))
    assertEquals(inside,  test(xb-1,ya+1))
    assertEquals(0,       test(xb+0,ya-1))
    assertEquals(outline, test(xb+0,ya+0))
    assertEquals(outline, test(xb+0,ya+1))
    assertEquals(0,       test(xb+1,ya-1))
    assertEquals(0,       test(xb+1,ya+0))
    assertEquals(0,       test(xb+1,ya+1))

    assertEquals(0,       test(xa-1,yb-1))
    assertEquals(0,       test(xa-1,yb+0))
    assertEquals(0,       test(xa-1,yb+1))
    assertEquals(outline, test(xa+0,yb-1))
    assertEquals(outline, test(xa+0,yb+0))
    assertEquals(0,       test(xa+0,yb+1))
    assertEquals(inside,  test(xa+1,yb-1))
    assertEquals(outline, test(xa+1,yb+0))
    assertEquals(0,       test(xa+1,yb+1))

    assertEquals(inside,  test(xb-1,yb-1))
    assertEquals(outline, test(xb-1,yb+0))
    assertEquals(0,       test(xb-1,yb+1))
    assertEquals(outline, test(xb+0,yb-1))
    assertEquals(outline, test(xb+0,yb+0))
    assertEquals(0,       test(xb+0,yb+1))
    assertEquals(0,       test(xb+1,yb-1))
    assertEquals(0,       test(xb+1,yb+0))
    assertEquals(0,       test(xb+1,yb+1))
  }
//
//  @Test
//  def manualSumTest(): Unit = {
//    val a = PointSet.point(24L, -140L, -4)
//    val b = PointSet.point(-52L, 188L, -10)
//    val r = (a + b).asInstanceOf[PointSet.Impl[Long]]
//    val c = r.compare(r.vs(0), r.vs(1))
//    val result = r(-52, 188)
//    assertEquals(true, PointSetSampleCheck.sumTest(a, b))
//  }
}
