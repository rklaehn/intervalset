package scala.collection.immutable

private[immutable] object IntervalTrieTestOps {
  import IntervalTrie._

  /**
   * Negates a tree by negating the leaves and building a new tree. This is just used for testing.
   * @param a the tree
   * @return a tree with the same structure as a, but with all leaves negated
   */
  def negate(a:IntervalTrie) : IntervalTrie = a match {
    case a:Leaf => Leaf(a.key, !a.at, !a.after)
    case a:Branch => Branch(a.prefix, a.level, negate(a.left), negate(a.right), a.s)
  }

  /**
   * A binary operation that just merges leaves from the lhs and the rhs, without considering their value at all.
   * In the case of leaves with identical prefixes, the rhs will be returned.
   * Note that the result of this operation can be a non-canonical tree, so this is not to be used from outside.
   */
  object MergeCalculator extends OrderedBinaryOperator {

    protected def collision(a0:Boolean, a: Leaf, as: Boolean, b0: Boolean, b:Leaf, bs:Boolean) = b

    protected def overlapA(a: IntervalTrie, as:Boolean, b0: Boolean) = a

    protected def overlapB(a0: Boolean, b: IntervalTrie, bs:Boolean) = b
  }

  /**
   * An XOR operator that fully negates branches instead of just flipping the sign bit. This is inefficient, so it is
   * just used for testing.
   */
  object XorCalculator0 extends OrderedBinaryOperator {

    protected def collision(a0:Boolean, a: Leaf, as:Boolean, b0: Boolean, b:Leaf, bs:Boolean) = {
      val before1 = a0 ^ b0
      val at1 = (a.at ^ as) ^ (b.at ^ bs)
      val after1 = (a.after ^ as) ^ (b.after ^ bs)
      if(before1 == at1 && at1 == after1 && a.key != 0)
        null // noop leaf
      else if(a.at == at1 && a.after == after1)
        a // reuse a if possible
      else if(b.at == at1 && b.after == after1)
        b // reuse b if possible
      else
        Leaf(a.key, at1, after1)
    }

    protected def overlapA(a: IntervalTrie, as:Boolean, b0: Boolean) =
      if(b0)
        negate(a flip as)
      else
        a flip as

    protected def overlapB(a0: Boolean, b: IntervalTrie, bs: Boolean) =
      if(a0)
        negate(b flip bs)
      else
        b flip bs
  }

  private def isValid0(a0:Boolean, a:IntervalTrie, as:Boolean) : Boolean = a match {
    case a:Branch =>
      val as1 = as ^ a.s
      isValid0(a0, a.left, as1) && isValid0(a.after ^ as, a.right, as1)
    case a:Leaf =>
      val before = a0
      val at = (a.at ^ as)
      val after = (a.after ^ as)
      val same = before == at && at == after
      !same
    case null =>
      false
  }

  def isValid(a:IntervalTrie) : Boolean = {
    a!=null && (a.support.head == 0L) && isValid0(a0 = false, a, as = false)
  }
}