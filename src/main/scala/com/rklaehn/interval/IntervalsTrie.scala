package com.rklaehn.interval

import spire.algebra.{Monoid, Order}

import spire.implicits._
import Order.ordering
import spire.math
import spire.math.Interval
import spire.math.interval._
import spire.util.Opt
import scala.collection.AbstractTraversable
import scala.collection.immutable.HashSet
import StableSortedTree2._

import scala.reflect.ClassTag

object IntervalsTrie {

  private implicit def valueXorMonoid[V: Order]: Monoid[HashSet[V]] = new Monoid[HashSet[V]] {
    def id = HashSet.empty[V]

    def op(x: HashSet[V], y: HashSet[V]): HashSet[V] = (x diff y) union (y diff x)
  }

  def apply[K: Partitioner, V: Order](elems: (Interval[K], V)*) = {
    if(elems.isEmpty) empty[K, V]
    else elems.map { case (k,v) => single(k, v) }.reduce(_ xor _)
  }

  def empty[K: Partitioner, V: Order]: IntervalsTrie[K, V] =
    new IntervalsTrie[K, V](HashSet.empty, null)

  def single[K: Partitioner, V: Order](i: Interval[K], v: V): IntervalsTrie[K, V] = {
    implicit val vm = valueXorMonoid[V]
    val sv = HashSet(v)
    val none = HashSet.empty[V]
    def point(k: K): IntervalsTrie[K, V] =
      new IntervalsTrie[K, V](none, Leaf(k, sv, none))
    def below(k: K): IntervalsTrie[K, V] =
      new IntervalsTrie[K, V](sv, Leaf(k, sv, sv))
    def atOrBelow(k: K): IntervalsTrie[K, V] =
      new IntervalsTrie[K, V](sv, Leaf(k, none, sv))
    def above(k: K): IntervalsTrie[K, V] =
      new IntervalsTrie[K, V](none, Leaf(k, none, sv))
    def atOrAbove(k: K): IntervalsTrie[K, V] =
      new IntervalsTrie[K, V](none, Leaf(k, sv, sv))
    def fromTo(a: K, ai:Boolean, b: K, bi: Boolean) =
      new IntervalsTrie[K, V](none,
        merge(
          Leaf(a, if(ai) sv else none, sv),
          Leaf(b, if(bi) none else sv, sv)
        )
      )
    def all = new IntervalsTrie[K, V](sv, null)
    i.fold {
      case (Closed(a), Closed(b)) if a == b => point(a)
      case (Unbound(), Open(x)) => below(x)
      case (Unbound(), Closed(x)) => atOrBelow(x)
      case (Open(x), Unbound()) => above(x)
      case (Closed(x), Unbound()) => atOrAbove(x)
      case (Closed(a), Closed(b)) => fromTo(a, true, b, true)
      case (Closed(a), Open(b)) => fromTo(a, true, b, false)
      case (Open(a), Closed(b)) => fromTo(a, false, b, true)
      case (Open(a), Open(b)) => fromTo(a, false, b, false)
      case (Unbound(), Unbound()) => all
      case (EmptyBound(), EmptyBound()) => empty[K, V]
    }
  }

  /*
  abstract class Merger[K, V](p: Partitioner[K]) {
    implicit def keyOrder = p.o

    def nodeAbove(l: Node[K, HashSet[V]], r: Node[K, HashSet[V]]): Branch[K, HashSet[V]] = {
      require(l.p < r.p)
      val (p1, hw1) = p.partition(l.p, r.p)
      Branch(p1, hw1, l, r)
    }

    def overlapA(a0: HashSet[V], a: Node[K, HashSet[V]], b0: HashSet[V]): Node[K, HashSet[V]]

    def overlapB(a0: HashSet[V], b0: HashSet[V], b: Node[K, HashSet[V]]): Node[K, HashSet[V]]

    def collision(a0: HashSet[V], a: Leaf[K, HashSet[V]], b0: HashSet[V], b: Leaf[K, HashSet[V]]): Node[K, HashSet[V]]

    def apply(a0: HashSet[V], a: Node[K, HashSet[V]], b0: HashSet[V], b: Node[K, HashSet[V]]): Node[K, HashSet[V]] = {
      (a, b) match {
        case (a: Branch[K, HashSet[V]], b: Branch[K, HashSet[V]]) =>
          val p_ab = a.p compare b.p
          if (p_ab == 0) {
            // nodes have exactly the same pivot
            // just merge them
            // a   |
            // b   |
            val p1 = a.p
            val l1 = apply(a0, a.l, b0, b.l)
            val r1 = apply(a0 ^ a.l.sign, a.r, b0 ^ b.l.sign, b.r)
            val hw1 = math.max(a.hw, b.hw)
            Branch(p1, hw1, l1, r1)
          } else if (p_ab < 0) {
            // a is below b
            // a |
            // b     |
            val hw_ab = a.hw compare b.hw
            if (hw_ab == 0) {
              // they have the same half width, so they are guaranteed not to overlap
              // we can just create a node above the two
              nodeAbove(a, b)
            } else if (hw_ab < 0) {
              // a is smaller than b and to the left of b
              if (a.p - a.hw >= b.p - b.hw)
                withL(b, apply(a, b.l))
              else
                nodeAbove(a, b)
            } else {
              // b is smaller than a and to the right of a
              if (a.p + a.hw >= b.p + b.hw)
                withR(a, apply(a.r, b))
              else
                nodeAbove(a, b)
            }
          } else {
            // a is above b
            // a     |
            // b |
            val hw_ab = a.hw compare b.hw
            if (hw_ab == 0) {
              // they have the same half width, so they are guaranteed not to overlap
              // we can just create a node above the two
              nodeAbove(b, a)
            } else if (hw_ab < 0) {
              // a is smaller than b and to the right of b
              if (a.p + a.hw <= b.p + b.hw)
                withR(b, apply(a, b.r))
              else
                nodeAbove(b, a)
            } else {
              // b is smaller than a and to the left of a
              if (a.p - a.hw <= b.p - b.hw)
                withL(a, apply(a.l, b))
              else
                nodeAbove(b, a)
            }
          }
        case (a: Branch[K, V], b: Leaf[K, V]) =>
          val p_ab = a.p compare b.p
          if (p_ab <= 0) {
            // b.p >= a.p
            if (a.p + a.hw > b.p)
              withR(a, apply(a.r, b))
            else
              nodeAbove(a, b)
          } else {
            // b.p < a.p
            if (a.p - a.hw <= b.p)
              withL(a, apply(a.l, b))
            else
              nodeAbove(b, a)
          }
        case (a: Leaf[K, V], b: Branch[K, V]) =>
          val p_ab = a.p compare b.p
          if (p_ab >= 0) {
            if(a.p < b.p + b.hw)
              withR(b, apply(a, b.r))
            else
              nodeAbove(b, a)
          } else {
            // a.p < b.p
            if (a.p >= b.p - b.hw)
              withL(b, apply(a, b.l))
            else
              nodeAbove(a, b)
          }
        case (a: Leaf[K, V], b: Leaf[K, V]) =>
          val p_ab = a.p compare b.p
          if(p_ab == 0)
            Leaf(a.p, combine(a.at, b.at), combine(a.sign, b.sign))
          else if(p_ab < 0)
            nodeAbove(a, b)
          else
            nodeAbove(b, a)
        case (a, null) => a
        case (null, b) => b
        case _ => null
      }
    }
  }
  */

  def orMerge[K, V](a0: HashSet[V], a: Node[K, HashSet[V]], b0: HashSet[V], b: Node[K, HashSet[V]]): Node[K, HashSet[V]] = {

    ???
    /*
    (a, b) match {
      case (a: Branch[K, V], b: Branch[K, V]) =>
        val p_ab = a.p compare b.p
        if (p_ab == 0) {
          // nodes have exactly the same pivot
          // just merge them
          // a   |
          // b   |
          val p1 = a.p
          val l1 = apply(a.l, b.l)
          val r1 = apply(a.r, b.r)
          val hw1 = math.max(a.hw, b.hw)
          Branch(p1, hw1, l1, r1)
        } else if (p_ab < 0) {
          // a is below b
          // a |
          // b     |
          val hw_ab = a.hw compare b.hw
          if (hw_ab == 0) {
            // they have the same half width, so they are guaranteed not to overlap
            // we can just create a node above the two
            nodeAbove(a, b)
          } else if (hw_ab < 0) {
            // a is smaller than b and to the left of b
            if (a.p - a.hw >= b.p - b.hw)
              withL(b, apply(a, b.l))
            else
              nodeAbove(a, b)
          } else {
            // b is smaller than a and to the right of a
            if (a.p + a.hw >= b.p + b.hw)
              withR(a, apply(a.r, b))
            else
              nodeAbove(a, b)
          }
        } else {
          // a is above b
          // a     |
          // b |
          val hw_ab = a.hw compare b.hw
          if (hw_ab == 0) {
            // they have the same half width, so they are guaranteed not to overlap
            // we can just create a node above the two
            nodeAbove(b, a)
          } else if (hw_ab < 0) {
            // a is smaller than b and to the right of b
            if (a.p + a.hw <= b.p + b.hw)
              withR(b, apply(a, b.r))
            else
              nodeAbove(b, a)
          } else {
            // b is smaller than a and to the left of a
            if (a.p - a.hw <= b.p - b.hw)
              withL(a, apply(a.l, b))
            else
              nodeAbove(b, a)
          }
        }
      case (a: Branch[K, V], b: Leaf[K, V]) =>
        val p_ab = a.p compare b.p
        if (p_ab <= 0) {
          // b.p >= a.p
          if (a.p + a.hw > b.p)
            withR(a, apply(a.r, b))
          else
            nodeAbove(a, b)
        } else {
          // b.p < a.p
          if (a.p - a.hw <= b.p)
            withL(a, apply(a.l, b))
          else
            nodeAbove(b, a)
        }
      case (a: Leaf[K, V], b: Branch[K, V]) =>
        val p_ab = a.p compare b.p
        if (p_ab >= 0) {
          if(a.p < b.p + b.hw)
            withR(b, apply(a, b.r))
          else
            nodeAbove(b, a)
        } else {
          // a.p < b.p
          if (a.p >= b.p - b.hw)
            withL(b, apply(a, b.l))
          else
            nodeAbove(a, b)
        }
      case (a: Leaf[K, V], b: Leaf[K, V]) =>
        val p_ab = a.p compare b.p
        if(p_ab == 0)
          Leaf(a.p, combine(a.at, b.at), combine(a.sign, b.sign))
        else if(p_ab < 0)
          nodeAbove(a, b)
        else
          nodeAbove(b, a)
      case (a, null) => a
      case (null, b) => b
      case _ => null
    }
    */
  }

  private def or0[K: Partitioner, V](a0: HashSet[V], a: Node[K, HashSet[V]], b0: HashSet[V], b: Node[K, HashSet[V]]): IntervalsTrie[K, V] = {
    val belowAll1 = a0 | b0
    val tree1 = orMerge(a0, a, b0, b)
    new IntervalsTrie[K, V](belowAll1, tree1)
  }

  implicit class HashSetOps[T](private val lhs: HashSet[T]) extends AnyVal {
    def ^(rhs: HashSet[T]) = (lhs diff rhs) union (rhs diff lhs)
  }

  private class EntriesTraversable[K: Order, V](t: IntervalsTrie[K, V]) extends AbstractTraversable[(Interval[K], V)] {
    import spire.math.interval._

    def foreach[U](f: ((Interval[K], V)) => U) = {
      val b0: Map[V, Bound[K]] = Map(t.belowAll.toSeq.map(v => v -> Unbound[K]()): _*)
      def op(b0:Map[V, Bound[K]], a0:HashSet[V], a:Node[K, HashSet[V]]): Map[V, Bound[K]] = a match {
        case a: Leaf[K, HashSet[V]] =>
          (a.at union a.sign).foldLeft(b0) { case (b1, v) =>
            val below = a0(v)
            val at = a.at(v)
            val above = at ^ a.sign(v)
            (at, above) match {
              case (true, false) => // below
                if(below)
                  f(Interval.fromBounds(b1(v), Open(a.p)) -> v)
                b1.updated(v, Closed(a.p))
              case (false, true) => // above
                if(below)
                  f(Interval.fromBounds(b1(v), Closed(a.p)) -> v)
                b1.updated(v, Open(a.p))
              case (true, true) => // both
                if(below)
                  f(Interval.fromBounds(b1(v), Open(a.p)) -> v)
                else
                  f(Interval.point(a.p) -> v)
                b1.updated(v, Open(a.p))
              case _ =>
                b1
            }
          }
        case a: Branch[K, HashSet[V]] =>
          val am = a0 ^ a.l.sign
          val bm = op(b0, a0, a.l)
          val b1 = op(bm, am, a.r)
          b1
        case _ =>
          b0
      }
      val last = op(b0, t.belowAll, t.root)
      for(v <- t.aboveAll)
        f(Interval.fromBounds(last(v), Unbound()) -> v)
    }
  }
}

class IntervalsTrie[K, V] private (val belowAll: HashSet[V], private val root: Node[K, HashSet[V]])(implicit p: Partitioner[K], m: Monoid[HashSet[V]]) {

  implicit def order = p.o

  def aboveAll: HashSet[V] = root match {
    case n: Node[K,HashSet[V]] => m.op(belowAll, n.sign)
    case _ => belowAll
  }

  def entries: Traversable[(Interval[K], V)] =
    new IntervalsTrie.EntriesTraversable(this)

  def intervals(v: V)(implicit c: ClassTag[K]): IntervalSeq[K] = {
    val belowAll = this.belowAll(v)
    val values = Array.newBuilder[K]
    val kinds = Array.newBuilder[Byte]
    var current = belowAll
    this.elements.foreach { case (k, below, delta) =>
      val changeBelow = below.contains(v)
      val changeDelta = delta.contains(v)
      if(changeBelow || changeDelta) {
        val kind = (changeBelow ^ current, changeDelta ^ current) match {
          case (false, false) => IntervalSeq.K00
          case (true, false) => IntervalSeq.K10
          case (false, true) => IntervalSeq.K01
          case (true, true) => IntervalSeq.K11
        }
        values += k
        kinds += kind
      }
      current ^= changeDelta
    }
    new IntervalSeq[K](belowAll, values.result(), kinds.result(), p.o)
  }

  def or(rhs: IntervalsTrie[K, V]): IntervalsTrie[K, V] =
    IntervalsTrie.or0[K, V](belowAll, root, rhs.belowAll, rhs.root)

  def filterValues(f: V => Boolean): IntervalsTrie[K, V] = {
    val belowAll1 = belowAll.filter(f)
    val fs : HashSet[V] => Opt[HashSet[V]] = { x =>
      val result = x.filter(f)
      if(result.isEmpty) Opt.empty
      else Opt(result)
    }
    val root1 = StableSortedTree2.modifyOrRemove(root, fs)
    new IntervalsTrie[K, V](belowAll1, root1)
  }

  def values: HashSet[V] =
    StableSortedTree2.elements(root).foldLeft(belowAll.to[HashSet]) { case (s, (_, before, delta)) =>
      s ++ before ++ delta
    }

  def at(k: K): HashSet[V] = {
    def delta(node: Node[K, HashSet[V]]): HashSet[V] = node match {
      case x:Branch[K, HashSet[V]] =>
        if(k < x.p) delta(x.l)
        else m.op(x.l.sign, delta(x.r))
      case x:Leaf[K, HashSet[V]] =>
        val kp = k.compare(x.p)
        if(kp < 0) m.id
        else if(kp == 0) x.at
        else x.sign
      case _ => m.id
    }
    m.op(belowAll, delta(root))
  }

  def truncate(min: K, max: K): IntervalsTrie[K, V] = {
    val tree1 = StableSortedTree2.truncate(root, IntervalSeq(Interval.open(min, max)))
    val atmin = StableSortedTree2.Leaf(min, at(min), at(min))
    val atmax = StableSortedTree2.Leaf(max, m.id, at(max))
    val tree2 = StableSortedTree2.merge(tree1, atmin)
    val tree3 = StableSortedTree2.merge(tree2, atmax)
    new IntervalsTrie(m.id, tree3)
  }
//
//  def filterValues(f: V => Boolean): IntervalsTrie[K, V] = {
//
//  }

  def xor(that: IntervalsTrie[K, V]) = {
    val root1 = StableSortedTree2.merge(this.root, that.root)
    val initial1 = m.op(this.belowAll, that.belowAll)
    new IntervalsTrie[K, V](initial1, root1)
  }

  def elements: Traversable[(K, HashSet[V], HashSet[V])] = StableSortedTree2.elements(root)
}