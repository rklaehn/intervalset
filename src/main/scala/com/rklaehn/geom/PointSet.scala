package com.rklaehn.geom

import java.util.Arrays

import spire.algebra.{Eq, Order}
import spire.implicits._

sealed abstract class AbstractPointSet[T, S <: AbstractPointSet[T, _]] extends ((T, T) => Int) {

  def isEmpty: Boolean

  def unary_- : S

  def +(rhs: S): S

  def -(rhs: S): S

  def >(value:Int): S

  def vertices: Iterable[Vertex[T]]
}

sealed abstract class Vertex[T] {

  def x:T

  def y:T
}

object Vertex {

  def unapply[T](v: Vertex[T]) = Some((v.x, v.y))

  implicit def order[T](implicit order:Order[T]): Order[Vertex[T]] = new Order[Vertex[T]] {

    override def compare(a: Vertex[T], b: Vertex[T]): Int = {
      val o1 = order.compare(a.x, b.x)
      if (o1 != 0)
        o1
      else
        order.compare(a.y, b.y)
    }
  }
}

sealed abstract class PointSet[T] extends AbstractPointSet[T, PointSet[T]]

object PointSet {

  def zero[T: Order]: PointSet[T] = const(0)

  def one[T: Order]: PointSet[T] = const(1)

  def const[T: Order](level: Int): PointSet[T] = Impl(level, Array.ofDim(0))

  def point[T: Order](x: T, y: T, level: Int = 1): PointSet[T] = Impl(0, Array(Vertex(x, y, level, 0, 0, 0)))

  def edge[T:Order](x: T, y: T, d00:Int, d01:Int, d10:Int, d11:Int): PointSet[T] = Impl(0, Array(Vertex(x,y,d00 = d00, d01 = d01, d10 = d10, d11 = d11)))

  def verticalEdge[T: Order](x: T, ya: T, yb: T, level: Int): PointSet[T] = {
    if (ya === yb)
      point(x, ya, level)
    else if (ya > yb)
      verticalEdge(x, yb, ya, level)
    else
      Impl(0, Array(Vertex(x, ya, level, level, 0, 0), Vertex(x, yb, 0, -level, 0, 0)))
  }

  def horizontalEdge[T: Order](xa: T, xb: T, y: T, level: Int): PointSet[T] = {
    if (xa === xb)
      point(xa, y, level)
    else if (xa > xb)
      horizontalEdge(xb, xa, y, level)
    else
      Impl(0, Array(Vertex(xa, y, level, 0, level, 0), Vertex(xb, y, 0, 0, -level, 0)))
  }

  def rectangle[T:Order](xa: T, ya:T, xb:T, yb:T, outline:Int = 1, inside:Int = 1) : PointSet[T] = {
    if(xa === xb) {
      verticalEdge(xa, ya, yb, outline)
    } else if(ya === yb) {
      horizontalEdge(xa, xb, ya, outline)
    } else if(xa > xb) {
      rectangle(xb, ya, xa, yb, outline, inside)
    } else if(ya > yb) {
      rectangle(xa, yb, xb, ya, outline, inside)
    } else {
      Impl(0, Array(
        Vertex(xa, ya, outline, outline, outline, inside),
        Vertex(xa, yb, 0, -outline, -inside + outline, -inside),
        Vertex(xb, ya, 0, -inside + outline, -outline, -inside),
        Vertex(xb, yb, inside - outline, -outline + inside, -outline + inside, inside)
      ))
    }
  }

  private[geom] case class Impl[T](ambient: Int, vs: Array[Vertex[T]])
                            (implicit val order: Order[T])
    extends PointSet[T] with Order[Vertex[T]] { lhs =>

    def vertices = vs

    def apply(x: T, y: T): Int = {
      var level = ambient
      var i = 0
      while (i < vs.length) {
        val v = vs(i)
        val vx_x = order.compare(v.x, x)
        val vy_y = order.compare(v.y, y)
        if (vx_x < 0) {
          if(vy_y < 0) {
            level += v.d11
          } else if(vy_y == 0) {
            level += v.d10
          }
        } else if (vx_x == 0) {
          if(vy_y < 0) {
            level += v.d01
          } else if(vy_y == 0) {
            level += v.d00
          }
        } else {
          return level
        }
        i += 1
      }
      level
    }

    def isEmpty = vs.isEmpty && ambient == 0

    def unary_- : PointSet[T] = copy(ambient = -ambient, vs = vs.map(~_))

    def +(rhs: PointSet[T]): PointSet[T] = rhs match {
      case rhs: Impl[T] =>
        new Sum(lhs, rhs).result
    }

    def -(rhs: PointSet[T]): PointSet[T] = rhs match {
      case rhs: Impl[T] =>
        new Diff(lhs, rhs).result
    }

    def >(rhs: Int) =
      new GT(lhs, rhs).result

    def compare(a: Vertex[T], b: Vertex[T]): Int = {
      val o1 = order.compare(a.x, b.x)
      if (o1 != 0)
        o1
      else
        order.compare(a.y, b.y)
    }

    def binarySearch(key: Vertex[T], from: Int, until: Int): Int = {
      var low = from
      var high = until - 1
      while (low <= high) {
        val mid = (low + high) >>> 1
        val midVal = vs(mid)
        val c = compare(midVal, key)
        if (c < 0) {
          low = mid + 1
        }
        else if (c > 0) {
          high = mid - 1
        }
        else {
          // scalastyle:off return
          return mid
          // scalastyle:on return
        }
      }
      -(low + 1)
    }

    override def toString = vertices.mkString(s"PointSet($ambient,", ",", ")")

    override def equals(rhs:Any) = rhs match {
      case rhs:Impl[T] => lhs.ambient == rhs.ambient && Arrays.equals(lhs.vs.asInstanceOf[Array[AnyRef]], rhs.vs.asInstanceOf[Array[AnyRef]])
      case _ => false
    }

    override def hashCode = lhs.ambient * 41 + Arrays.hashCode(lhs.vs.asInstanceOf[Array[AnyRef]])
  }

  private[geom] case class Vertex[T](x: T, y: T, d00: Int, d01: Int, d10: Int, d11: Int) extends com.rklaehn.geom.Vertex[T] {
    lhs =>

    def unary_~ = Vertex(x, y, -d00, -d01, -d10, -d11)
  }

  private abstract class MergeOperation[T] {

    def lhs: Impl[T]

    def rhs: Impl[T]

    private[this] val r0 = op(lhs.ambient, rhs.ambient)

    private[this] val a = lhs.vs

    private[this] val b = rhs.vs

    private[this] def order: Order[Vertex[T]] = lhs

    private[this] val r = Array.ofDim[Vertex[T]](a.length + b.length)

    private[this] var ri = 0

    def fromA(a0: Int, b0: Int): Unit

    def fromB(a0: Int, b0: Int): Unit

    def copyA(a0: Int, a1: Int): Unit = {
      System.arraycopy(a, a0, r, ri, a1 - a0)
      ri += a1 - a0
    }

    def flipA(a0: Int, a1: Int): Unit = {
      var ai = a0
      while (ai < a1) {
        r(ri) = ~a(ai)
        ri += 1
        ai += 1
      }
    }

    def copyB(b0: Int, b1: Int): Unit = {
      System.arraycopy(b, b0, r, ri, b1 - b0)
      ri += b1 - b0
    }

    def flipB(b0: Int, b1: Int): Unit = {
      var bi = b0
      while (bi < b1) {
        r(ri) = ~b(bi)
        ri += 1
        bi += 1
      }
    }

    def op(a: Int, b: Int): Int

    def collision(ai: Int, bi: Int): Unit = {
      val av = a(ai)
      val bv = b(bi)
      val d00 = op(av.d00, bv.d00)
      val d01 = op(av.d01, bv.d01)
      val d10 = op(av.d10, bv.d10)
      val d11 = op(av.d11, bv.d11)
      if (d00 != 0 || d01 != 0 || d10 != 0 || d11 != 0) {
        r(ri) = av.copy[T](d00 = d00, d01 = d01, d10 = d10, d11 = d11)
        ri += 1
      }
    }

    def binarySearch(array: Array[Vertex[T]], key: Vertex[T], from: Int, until: Int): Int = {
      var low = from
      var high = until - 1
      while (low <= high) {
        val mid = (low + high) >>> 1
        val midVal = array(mid)
        val c = order.compare(midVal, key)
        if (c < 0) {
          low = mid + 1
        }
        else if (c > 0) {
          high = mid - 1
        }
        else {
          // scalastyle:off return
          return mid
          // scalastyle:on return
        }
      }
      -(low + 1)
    }

    def merge0(a0: Int, a1: Int, b0: Int, b1: Int): Unit = {
      if (a0 == a1) {
        fromB(b0, b1)
      } else if (b0 == b1) {
        fromA(a0, a1)
      } else {
        val am = (a0 + a1) / 2
        val res = binarySearch(b, a(am), b0, b1)
        if (res >= 0) {
          // same elements
          val bm = res
          // merge everything below a(am) with everything below the found element
          merge0(a0, am, b0, bm)
          // add the elements a(am) and b(bm)
          collision(am, bm)
          // merge everything above a(am) with everything above the found element
          merge0(am + 1, a1, bm + 1, b1)
        } else {
          val bm = -res - 1
          // merge everything below a(am) with everything below the found insertion point
          merge0(a0, am, b0, bm)
          // add a(am)
          fromA(am, am + 1)
          // everything above a(am) with everything above the found insertion point
          merge0(am + 1, a1, bm, b1)
        }
      }
    }

    merge0(0, a.length, 0, b.length)

    def result: Impl[T] = {
      if (ri == r.length)
        Impl[T](r0, r)(lhs.order)
      else
        Impl[T](r0, r.take(ri))(lhs.order)
    }
  }

  private class Sum[T](val lhs: Impl[T], val rhs: Impl[T]) extends MergeOperation[T] {

    override def op(a: Int, b: Int): Int = a + b

    override def fromA(a0: Int, a1: Int): Unit = copyA(a0, a1)

    override def fromB(b0: Int, b1: Int): Unit = copyB(b0, b1)
  }

  private class Diff[T](val lhs: Impl[T], val rhs: Impl[T]) extends MergeOperation[T] {

    override def op(a: Int, b: Int): Int = a - b

    override def fromA(a0: Int, a1: Int): Unit = copyA(a0, a1)

    override def fromB(b0: Int, b1: Int): Unit = flipB(b0, b1)
  }

  private abstract class ScanOperation[T] {

    def lhs: Impl[T]

    def r0: Int

    /**
     * The vertex array of the source
     */
    protected[this] val a = lhs.vs

    /**
     * The result vertex array
     */
    protected[this] val r = Array.newBuilder[Vertex[T]]
    r.sizeHint(a.length)

    /**
     * The previous scan line vertices
     */
    protected[this] var b = Array.ofDim[Vertex[T]](a.length)

    /**
     * The size of the previous scan line
     */
    protected[this] var bn = 0

    /**
     * The target scan line vertices
     */
    protected[this] var s = Array.ofDim[Vertex[T]](a.length)

    /**
     * The size of the target scan line
     */
    protected[this] var si = 0

    /**
     * The x coordinate of the current scan line
     */
    protected[this] var x = null.asInstanceOf[T]

    /**
     * the current level below x (level_lt)
     */
    protected[this] var l_l = 0

    /**
     * The current level at x (level_eq)
     */
    protected[this] var l_e = 0

    /**
     * The current level above x (level_gt)
     */
    protected[this] var l_g = 0

    private[this] def compareY(a:Vertex[T], b:Vertex[T]) = lhs.order.compare(a.y, b.y)

    def println(text:Any) : Unit = {}

    private[this] def binarySearch(array: Array[Vertex[T]], key: Vertex[T], from: Int, until: Int): Int = {
      var low = from
      var high = until - 1
      while (low <= high) {
        val mid = (low + high) >>> 1
        val midVal = array(mid)
        val c = compareY(midVal, key)
        if (c < 0) {
          low = mid + 1
        }
        else if (c > 0) {
          high = mid - 1
        }
        else {
          // scalastyle:off return
          return mid
          // scalastyle:on return
        }
      }
      -(low + 1)
    }

    def fromA(a0:Int, a1:Int): Unit

    def fromB(b0:Int, b1:Int): Unit

    def collision(a0:Int, b0:Int): Unit

    def merge0(a0: Int, a1: Int, b0: Int, b1: Int): Unit = {
      if (a0 == a1) {
        fromB(b0, b1)
      } else if (b0 == b1) {
        fromA(a0, a1)
      } else {
        val am = (a0 + a1) / 2
        val res = binarySearch(b, a(am), b0, b1)
        if (res >= 0) {
          // same elements
          val bm = res
          // merge everything below a(am) with everything below the found element
          merge0(a0, am, b0, bm)
          // add the elements a(am) and b(bm)
          collision(am, bm)
          // merge everything above a(am) with everything above the found element
          merge0(am + 1, a1, bm + 1, b1)
        } else {
          val bm = -res - 1
          // merge everything below a(am) with everything below the found insertion point
          merge0(a0, am, b0, bm)
          // add a(am)
          fromA(am, am + 1)
          // everything above a(am) with everything above the found insertion point
          merge0(am + 1, a1, bm, b1)
        }
      }
    }

    def flip(): Unit = {
      // swap b and s
      val t = b; b = s; s = t
      // store si in bi and set si to 0
      bn = si; si = 0
    }

    def mergeScanLines(): Unit = {
      var a0 = 0
      while (a0 < a.length) {
        x = a(a0).x
        l_l = lhs.ambient
        l_e = lhs.ambient
        l_g = lhs.ambient
        si = 0
        var a1 = a0 + 1
        while (a1 < a.length && a(a1).x == x) {
          a1 += 1
        }
        println(s"Scanline $x $a0 $a1")
        merge0(a0, a1, 0, bn)
        flip()
        println(s"Scanline after $x is $bn. $l_e $l_g")
        a0 = a1
      }
    }
    mergeScanLines()

    def result: Impl[T] = {
      Impl[T](r0, r.result())(lhs.order)
    }
  }

  private class GT[T:Order](val lhs:Impl[T], val rhs:Int) extends ScanOperation[T] {

    def op(value:Int) : Int = if(value > rhs) 1 else 0

    def r0 = op(lhs.ambient)

    def vertex(y:T, l00:Int, l01:Int, l02:Int, l10:Int, l11:Int, l12:Int, l20:Int, l21:Int, l22: Int) : Unit = {
      println(s"vertex $x $y")
      println(s"before y $l00 $l10 $l20")
      println(s"at y     $l01 $l11 $l21")
      println(s"after y  $l02 $l12 $l22")

      val r00 = op(l00)
      val r01 = op(l01)
      val r02 = op(l02)
      val r10 = op(l10)
      val r11 = op(l11)
      val r12 = op(l12)
      val r20 = op(l20)
      val r21 = op(l21)
      val r22 = op(l22)

      val d00 = (r11 - r10) - (r01 - r00)
      val d01 = (r12 - r10) - (r02 - r00)
      val d10 = (r21 - r20) - (r01 - r00)
      val d11 = (r22 - r20) - (r02 - r00)
      if(d00 != 0 || d01 != 0 || d10 != 0 || d11 != 0) {
        println(s"result $x $y")
        println(s"$d00 $d10")
        println(s"$d01 $d11")
        r += Vertex(x, y, d00, d01, d10, d11)
      }
    }

    def vertex(y:T, d00:Int, d01:Int, d10:Int, d11:Int) : Unit = {
      println(s"vertex $x $y")
      val before0 = op(l_e)
      val at0 = op(l_e + d00)
      val after0 = op(l_e + d01)
      val before1 = op(l_g)
      val at1 = op(l_g + d10)
      val after1 = op(l_g + d11)
      val r00 = at0 - before0
      val r01 = after0 - before0
      val r10 = at1 - before1
      val r11 = after1 - before1
      println(s"$before0 $before1")
      println(s"$at0 $at1")
      println(s"$after0 $after1")
      if(r00 != 0 || r01 != 0 || r10 != 0 || r11 != 0) {
        println(s"result $x $y")
        println(s"$r00 $r10")
        println(s"$r01 $r11")
        r += Vertex(x, y, r00, r01, r10, r11)
      }
    }

    override def fromA(a0: Int, a1: Int): Unit = if(a0 < a1) {
      // add vertices from source, so we know already that v.x === x
      println(s"fromA($a0,$a1)")
      var i = a0
      while(i < a1) {
        val av = a(i)
        assert(av.x === x)
        // call the vertex cb
        vertex(av.y,
          l_l, l_l, l_l,
          l_e, l_e + av.d00, l_e + av.d01,
          l_g, l_g + av.d10, l_g + av.d11)
        // vertex(v.y, v.d00, v.d01, v.d10, v.d11)
        // only add vertices to the scanline if they have a delta of != 0 above x
        if(av.d10 != 0 || av.d11 != 0) {
          s(si) = av
          si += 1
        }
        // adjust the level at and above x. Use d01/d11 since v.x = x
        l_e += av.d01
        l_g += av.d11
        i += 1
      }
    }

    override def fromB(b0: Int, b1: Int): Unit = if(b0 < b1)  {
      // add vertices from previous scanline. we know that v.x < x
      println(s"fromB($b0,$b1)")
      var i = b0
      while(i < b1) {
        val bv = b(i)
        assert(bv.x < x)
        // call the vertex cb
        vertex(bv.y,
          l_l, l_l + bv.d10, l_l + bv.d11,
          l_e, l_e + bv.d10, l_e + bv.d11,
          l_g, l_g + bv.d10, l_g + bv.d11)
        // vertex(v.y, d00 = v.d10, d01 = v.d11, d10 = v.d10, d11 = v.d11)
        // vertices from the previous scanline are always copied into the next scanline unless there is a collision
        s(si) = bv
        si += 1
        // adjust the level below, at and above x using d11, since we know that v.x < x
        l_l += bv.d11
        l_e += bv.d11
        l_g += bv.d11
        i += 1
      }
    }

    override def collision(a0: Int, b0: Int): Unit = {
      // vertex from a replaces vertex from scanline (b)
      println(s"collision($a0,$b0)")
      // the vertex from the source
      val av = a(a0)
      // the vertex from the previous scanline
      val bv = b(b0)
      assert(av.x == x && bv.x < x && av.y === bv.y)
      // calculate the vertex that has the same effect as v+p for above x
      val r00 = av.d00 + bv.d10
      val r01 = av.d01 + bv.d11
      val r10 = av.d10 + bv.d10
      val r11 = av.d11 + bv.d11
      println(s"$r10 $r11")
      // call the vertex cb
      vertex(av.y,
        l_l, l_l + bv.d10, l_l + bv.d11,
        l_e, l_e + bv.d10 + av.d00, l_e + bv.d11 + av.d01,
        l_g, l_g + bv.d10 + av.d10, l_g + bv.d11 + av.d11)
      //vertex(av.y, r00, r01, r10, r11)
      // only add the vertex if we still need it in the scanline
      if(r10 != 0 || r11 != 0) {
        s(si) = av.copy[T](d00 = r00, d01 = r01, d10 = r10, d11 = r11)
        si += 1
      }
      l_l += bv.d11
      l_e += r01
      l_g += r11
    }
  }
}
