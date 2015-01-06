package com.rklaehn.geom
import org.scalacheck.Properties
import org.scalacheck.Prop._
import spire.implicits._

object PointSetSampleCheck extends Properties("PointSet.Sample") {

  val e = 1

  implicit val ordering = Ordering.fromLessThan(Vertex.order[Long].lteqv)

  implicit val arb = PointSetArbitrary.arbitrary

  // a test that works by sampling the result at all relevant places and checks consistency with the scalar operation
  def unarySampleTest(a:PointSet[Long], r:PointSet[Long], op:Int => Int) = {
    val support = a.vertices.toArray.sorted.distinct
    support.forall { case Vertex(x,y) =>
      def test(x:Long, y:Long) = op(a(x,y)) == r(x,y)
      val r1 = test(x-e,y-e)
      val r2 = test(x-e,y  )
      val r3 = test(x-e,y+e)
      val r4 = test(x  ,y-e)
      val r5 = test(x  ,y  )
      val r6 = test(x  ,y+e)
      val r7 = test(x+e,y-e)
      val r8 = test(x+e,y  )
      val r9 = test(x+e,y+e)
      r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9
    }
  }

  // a test that works by sampling the result at all relevant places and checks consistency with the scalar operation
  def binarySampleTest(a:PointSet[Long], b:PointSet[Long], r:PointSet[Long], op:(Int, Int) => Int) = {
    val support = (a.vertices ++ b.vertices).toArray.sorted.distinct
    support.forall { case Vertex(x, y) =>
      def test(x: Long, y: Long) = op(a(x, y), b(x, y)) == r(x, y)
      val r1 = test(x - e, y - e)
      val r2 = test(x - e, y)
      val r3 = test(x - e, y + e)
      val r4 = test(x, y - e)
      val r5 = test(x, y)
      val r6 = test(x, y + e)
      val r7 = test(x + e, y - e)
      val r8 = test(x + e, y)
      val r9 = test(x + e, y + e)
      r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9
    }
  }

  property("Negate") = forAll { a:PointSet[Long] =>
    unarySampleTest(a, -a, - _)
  }

  property("Sum") = forAll { (a:PointSet[Long], b:PointSet[Long]) =>
    binarySampleTest(a, b, a + b, _ + _)
  }

  property("Diff") = forAll { (a:PointSet[Long], b:PointSet[Long]) =>
    binarySampleTest(a, b, a - b, _ - _)
  }

  property("GT") = forAll { a:PointSet[Long] =>
    unarySampleTest(a, a > 0, { x => if(x>0) 1 else 0})
  }
}
