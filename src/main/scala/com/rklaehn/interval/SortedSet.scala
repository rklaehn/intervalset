//package com.rklaehn.interval
//
//import spire.implicits._
//import spire.algebra._
//import spire.math
//
//import scala.collection.AbstractTraversable
//
//object SortedSet {
//
//  def empty[K: StableSortedTree.Partitioner, V: Monoid]: SortedSet[K, V] = new SortedSet[K, V](null)
//
//  def single[K: StableSortedTree.Partitioner, V: Monoid](k: K, v: V) = new SortedSet[K, V](StableSortedTree.single(k, v))
//}
//
//class SortedSet[K: StableSortedTree.Partitioner, V: Monoid] private (private val root: StableSortedTree.Node[K, V]) {
//
//  def value: V = StableSortedTree.v[V](root)
//
//  def merge(that: SortedSet[K, V]): SortedSet[K, V] = {
//    val root1 = StableSortedTree.merge(this.root, that.root)
//    new SortedSet[K, V](root1)
//  }
//
//  def isEmpty: Boolean = root eq null
//
//  def keys: Traversable[K] = StableSortedTree.keys(root)
//}