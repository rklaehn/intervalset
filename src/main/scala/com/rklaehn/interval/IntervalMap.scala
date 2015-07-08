//package com.rklaehn.interval
//
//import spire.algebra.{Monoid, Order}
//
//import spire.implicits._
//import Order.ordering
//import scala.collection.immutable.SortedMap
//
//object IntervalMap {
//
//  private implicit def valueMonoid[V: Order]: Monoid[SortedMap[V, Int]] = new Monoid[SortedMap[V, Int]] {
//    def id = SortedMap.empty[V, Int]
//
//    def op(x: SortedMap[V, Int], y: SortedMap[V, Int]): SortedMap[V, Int] = if(x.size < y.size) op(y, x) else {
//      y.foldLeft(x) { case (r, (k, count)) =>
//        val count1 = count + r.getOrElse(k, 0)
//        if(count1 != 0) r.updated(k, count1)
//        else r - k
//      }
//    }
//  }
//
//  def empty[K: StableSortedTree.Partitioner, V: Order]: IntervalMap[K, V] =
//    new IntervalMap[K, V](SortedMap.empty[V, Int], null)
//
//  def fromTo[K: StableSortedTree.Partitioner, V: Order](min: K, max: K, v: V): IntervalMap[K, V] = {
//    val d0 = StableSortedTree.single[K, SortedMap[V, Int]](min, SortedMap(v -> 1))
//    val d1 = StableSortedTree.single[K, SortedMap[V, Int]](max, SortedMap(v -> -1))
//    val tree = StableSortedTree.merge[K, SortedMap[V, Int]](d0, d1)
//    new IntervalMap[K, V](SortedMap.empty[V, Int], tree)
//  }
//}
//
//class IntervalMap[K, V] private (private val initial: SortedMap[V, Int], private val root: StableSortedTree.Node[K, SortedMap[V, Int]])(implicit p: StableSortedTree.Partitioner[K], m: Monoid[SortedMap[V, Int]]) {
//
//  implicit def order = p.o
//
//  def value: SortedMap[V, Int] = StableSortedTree.v[SortedMap[V, Int]](root)
//
//  def at(k: K): SortedMap[V, Int] = {
//    def delta(node: StableSortedTree.Node[K, SortedMap[V, Int]]): SortedMap[V, Int] = node match {
//      case x:StableSortedTree.Branch[K, SortedMap[V, Int]] =>
//        if(k < x.p) delta(x.l)
//        else m.op(x.l.v, delta(x.r))
//      case x:StableSortedTree.Leaf[K, SortedMap[V, Int]] =>
//        if(k < x.p) m.id
//        else x.v
//      case _ => m.id
//    }
//    m.op(initial, delta(root))
//  }
//
//  def merge(that: IntervalMap[K, V]) = {
//    val root1 = StableSortedTree.merge(this.root, that.root)
//    val initial1 = m.op(this.initial, that.initial)
//    new IntervalMap[K, V](initial1, root1)
//  }
//
//  def elements: Traversable[(K, SortedMap[V, Int])] = StableSortedTree.elements(root)
//}