package com.rklaehn.interval

import language.implicitConversions
import reftree.render._
import reftree.diagram._
import java.nio.file.Paths

import scala.concurrent.duration._
import spire.implicits._
import spire.math._
import RefTreeImplicits._
import com.rklaehn.interval.RadixTreeVisualization.a

object RadixTreeVisualization extends App {

  val empty = IntervalTrie.empty[Long]
  val all = IntervalTrie.all[Long]
  val point = IntervalTrie.point(10L)
  val atOrAbove = IntervalTrie.atOrAbove(10L)
  val above = IntervalTrie.above(10L)
  val below = IntervalTrie.below(10L)
  val atOrBelow = IntervalTrie.atOrBelow(10L)
  val hole = IntervalTrie.hole(10L)
  val interval = IntervalTrie.atOrAbove(0L) & IntervalTrie.atOrBelow(1)

  val a = IntervalTrie(Interval.openUpper(0,1))
  val b = IntervalTrie(Interval.openUpper(4,5))
  val a_or_b = a | b
  val not_a = ~a

  val complex = IntervalTrie(Interval(0L, 1L)) | IntervalTrie(Interval(4L, 5L)) | IntervalTrie.above(10)

  val renderer = Renderer(
    renderingOptions = RenderingOptions(density = 200),
    animationOptions = AnimationOptions(keyFrameDuration = 2000.milliseconds, interpolationDuration = 500.milliseconds),
    format = "gif",
    directory = Paths.get("target")
  )

//  val queueAnimation: Animation = Animation.startWithSequence(Seq(test))
//    .build(Diagram(_).withAnchor("queue").withCaption("empty"))

//  renderer.render("empty", Diagram(empty))
//  renderer.render("all", Diagram(all))
  val named = Seq(all, empty, below, atOrBelow, point, hole, atOrAbove, above, complex).map(x => x.toString -> x)
  for((name, value) <- named) {
    renderer.render(name, Diagram(value))
    renderer.render(name + "_caption", Diagram(value).withCaption(name))
  }

  val animation = Animation.startWithSequence(named)
    .build { case (name, value) => Diagram(value).withCaption(name) }

  val or1 = Diagram(a).withCaption("a=" + a) + Diagram(b).withCaption("b=" + b)
  val or2 = or1 + Diagram(a_or_b).withCaption("a | b")
  renderer.render("or_anim", Animation(Seq(or1, or2)))

  val not1 = Diagram(a).withCaption("a=" + a)
  val not2 = not1 + Diagram(not_a).withCaption("~a")
  renderer.render("not_anim", Animation(Seq(not1, not2)))

  val c = IntervalTrie.atOrAbove(0)
  val and1 = Diagram(a).withCaption("a=" + a) + Diagram(c).withCaption("c=" + c)
  val and2 = and1 + Diagram(a & c).withCaption("a & c")
  renderer.render("and_anim", Animation(Seq(and1, and2)))

  val xor1 = Diagram(a).withCaption("a=" + a) + Diagram(b).withCaption("b=" + b)
  val xor2 = xor1 + Diagram(a ^ b).withCaption("a ^ b")
  renderer.render("xor_anim", Animation(Seq(xor1, xor2)))

  renderer.render("or", or2)

  def renderLarge(): Unit = {
    def mk(offset: Int, stride: Int, n: Int): IntervalTrie[Long] = {
      (0L until n)
        .map(i => IntervalTrie(Interval.openUpper(i * stride * 2 + offset, i * stride * 2 + stride + offset)))
        .reduce(_ | _)
    }
    val a = mk(0, 2, 3)
    val b = mk(1, 2, 3)
    val c = mk(1, 1000, 3)

    val and1 = Diagram(a).withCaption("a=" + a) + Diagram(c).withCaption("b=" + b)
    val and2 = and1 + Diagram(a & b).withCaption("a & b")
    renderer.render("and_anim_full", and2)

    val and3 = Diagram(a).withCaption("a=" + a) + Diagram(c).withCaption("c=" + c)
    val and4 = and1 + Diagram(a & c).withCaption("a & c")
    renderer.render("and_anim_cut", and4)
  }
  renderLarge()

  // renderer.render("anim", animation)
}
