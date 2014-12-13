package scala.collection.immutable

/**
 * Operations that are just useful for testing
 */
private[immutable] object IntervalTrieTestOps {
  import scala.collection.immutable.IntervalTrie._

  /**
   * Negates a tree by negating the leaves and building a new tree. This is just used for testing.
   * @param a the tree
   * @return a tree with the same structure as a, but with all leaves negated
   */
  def negate(a:IntervalTrie) : IntervalTrie = a match {
    case a:Leaf => Leaf(a.key, !a.at, !a.above)
    case a:Branch => Branch(a.prefix, a.level, negate(a.left), negate(a.right), a.s)
  }

  /**
   * An XOR operator that fully negates branches instead of just flipping the sign bit. This is inefficient, so it is
   * just used for testing.
   */
  object XorCalculator0 extends OrderedBinaryOperator {

    protected def collision(a0:Boolean, a: Leaf, as:Boolean, b0: Boolean, b:Leaf, bs:Boolean) = {
      val before1 = a0 ^ b0
      val at1 = (a.at ^ as) ^ (b.at ^ bs)
      val after1 = (a.above ^ as) ^ (b.above ^ bs)
      if(before1 == at1 && at1 == after1 && a.key != 0)
        null // noop leaf
      else if(a.at == at1 && a.above == after1)
        a // reuse a if possible
      else if(b.at == at1 && b.above == after1)
        b // reuse b if possible
      else
        Leaf(a.key, at1, after1)
    }

    protected def overlapA(a: IntervalTrie, as:Boolean, b0: Boolean) =
      if(b0)
        negate(a xor as)
      else
        a xor as

    protected def overlapB(a0: Boolean, b: IntervalTrie, bs: Boolean) =
      if(a0)
        negate(b xor bs)
      else
        b xor bs
  }

  def head(a:IntervalTrie) : Leaf = a match {
    case a:Branch => head(a.left)
    case a:Leaf => a
  }

  private def isValid0(a0:Boolean, a:IntervalTrie, as:Boolean) : Boolean = a match {
    case a:Branch if a.level == levelAbove(a.left.prefix, a.right.prefix) =>
      val m = maskAbove(a.prefix, a.level)
      val as1 = as ^ a.s
      ((a.left.prefix & m) == (a.right.prefix & m)) &&
      isValid0(a0, a.left, as1) &&
      isValid0(a.above ^ as, a.right, as1)
    case a:Leaf =>
      val before = a0
      val at = a.at ^ as
      val after = a.above ^ as
      val same = before == at && at == after
      !same
    case _ =>
      // this includes null and broken branches
      false
  }

  def isValid(a:IntervalTrie) : Boolean = {
    a!=null && (head(a).key == 0L) && isValid0(a0 = false, a, as = false)
  }

  implicit class CreateEnhancement(private val value:IntervalTrie.type) extends AnyVal {
    /**
     * The interval trie that is true everywhere (except before 0, where it is false by convention
     */
    final def one: IntervalTrie = Leaf(0L, at = true, above = true)

    /**
     * The interval trie that is false everywhere
     */
    final def zero: IntervalTrie = Leaf(0L, at = false, above = false)

    def start(value: Long, included: Boolean) = zero merge Leaf(value, at = included, above = true)

    def startAt(value: Long) = start(value, included = true)

    def startAfter(value: Long) = start(value, included = false)

    def end(value: Long, included: Boolean) = one merge Leaf(value, at = included, above = false)

    def endAt(value: Long) = end(value, true)

    def endBefore(value: Long) = end(value, false)

    def hole(a: Long) = zero merge Leaf(a, false, true)

    def point(a: Long) = zero merge Leaf(a, true, false)

    def interval(a: Long, ai: Boolean, b: Long, bi: Boolean) = {
      if (!unsigned_<(a, b))
        throw new IllegalArgumentException("a must be less than b (unsigned)!")
      zero merge Leaf(a, ai, true) merge Leaf(b, bi, false)
    }

    def apply(elems: Leaf*): IntervalTrie =
      elems.foldLeft(zero)(merge0)
  }

  def foreachKey[U](tree:IntervalTrie, f : Long => U) : Unit = tree match {
    case Branch(_, _, left, right, _) =>
      foreachKey(left, f)
      foreachKey(right, f)
    case Leaf(key, _, _) => f(key)
    case _ =>
  }

  implicit class MergeEnhancement(private val lhs:IntervalTrie) extends AnyVal {

    def merge(rhs:Leaf) = merge0(lhs,rhs)

    def before(value:Long) = IntervalTrie.SampleBefore(false, lhs, value)

    def at(value:Long) = IntervalTrie.SampleAt(false, lhs, value)

    def after(value:Long) = IntervalTrie.SampleAfter(false, lhs, value)

    def support:Traversable[Long] = new Traversable[Long] {
      override def foreach[U](f: (Long) => U): Unit = foreachKey(lhs, f)
    }
  }
}