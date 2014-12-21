package com.rklaehn.interval

/**
 * Operations that are just useful for testing
 */
private[interval] object IntervalSetTestOps {
  import com.rklaehn.interval.IntervalTrie._

  private def foreachKey[T, U](tree:IntervalTrie, toKey:Long => T, f : T => U) : Unit = tree match {
    case branch:Branch =>
      foreachKey(branch.left, toKey, f)
      foreachKey(branch.right, toKey, f)
    case leaf:Leaf => f(toKey(leaf.key))
    case _ =>
  }

  implicit class MergeEnhancement(private val lhs:IntervalSet[Long]) extends AnyVal {

    def support:Traversable[Long] = new Traversable[Long] {
      override def foreach[U](f: (Long) => U): Unit = foreachKey(lhs.tree, lhs.ise.toKey, f)
    }
  }
}
