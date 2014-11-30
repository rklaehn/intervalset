package scala.collection.immutable

private[immutable] object IntervalTrie {

  import java.lang.Long.numberOfLeadingZeros

  @inline final def unsigned_<(i: Long, j: Long) = (i < j) ^ (i < 0L) ^ (j < 0L)

  @inline final def levelAbove(a:Long, b:Long) : Byte =
    (63 - numberOfLeadingZeros(a ^ b)).toByte

  @inline final def maskAbove(prefix:Long, bit:Byte) =
    prefix & ((-1L << bit) << 1)

  @inline final def zeroAt(value:Long, bit:Byte) =
    (value & (1L << bit)) == 0L

  @inline final def hasMatchAt(key: Long, prefix: Long, level: Byte) =
    maskAbove(key, level) == prefix

  def join(p1 : Long, t1 : IntervalTrie, p2 : Long, t2 : IntervalTrie) : IntervalTrie = {
    val l = levelAbove(p1, p2)
    val p = maskAbove(p1, l)
    if (zeroAt(p1, l))
      Branch(p, l, t1, t2)
    else
      Branch(p, l, t2, t1)
  }

  /**
   * Creates a branch from two possibly null children. In case one of the children is null, the other child will
   * be returned. So if both children are null, this operation can return null.
   * @param p the prefix to be used for a new branch
   * @param level the level to be used for a new branch
   * @param l the left child
   * @param r the right child
   * @return the result, can be null
   */
  @inline private final def branch(p:Long, level:Byte, l:IntervalTrie, r:IntervalTrie) : IntervalTrie =
    if(l eq null)
      r
    else if(r eq null)
      l
    else
      Branch(p,level,l,r)

  private def unreachable : Nothing = throw new NotImplementedError("You should never get here")

  /**
   * A binary calculator that preserves the order of operands. This is the most generic case. It is used even for
   * symmetric operations. There might be some performance benefit in doing an operator for symmetric operations, but
   * I doubt that it is worth it. And in any case I am too lazy right now.
   */
  abstract class OrderedBinaryOperator {

    /**
     * Joins two non-overlapping leaves
     * @param a0 the value before a
     * @param a a node (leaf or branch) from the lhs
     * @param as the sign of a. If this is true, a is negated. The sign applies just to a, not to a0
     * @param b0 the value before b
     * @param b a node (leaf or branch) from the rhs
     * @param bs the sign of b. If this is true, b is negated. The sign applies just to b, not to b0
     * @return the result, can be null
     */
    @inline private final def join(a0:Boolean, a: IntervalTrie, as:Boolean, b0:Boolean, b: IntervalTrie, bs:Boolean): IntervalTrie = {
      val a_p = a.prefix
      val b_p = b.prefix
      val level = levelAbove(a_p, b_p)
      val p = maskAbove(a_p, level)
      if (zeroAt(a_p, level)) {
        // a is before b, so it is overlapped by b0
        val a1 = overlapA(a, as, b0)
        // b is behind a, so it is overlapped by a.last
        val b1 = overlapB(a.after ^ as, b, bs)
        // make the branch (results can be empty)
        branch(p, level, a1, b1)
      } else {
        // b is before a, so it is overlapped by a0
        val b1 = overlapB(a0, b, bs)
        // a is behind b, so it is overlapped by b.last
        val a1 = overlapA(a, as, b.after ^ bs)
        // make the branch (results can be empty)
        branch(p, level, b1, a1)
      }
    }

    /**
     * This is called if two leaves collide (have the same prefix)
     * @param a0 the value before a
     * @param a a leaf from the lhs
     * @param as the sign of a. If this is true, a is negated. The sign applies just to a, not to a0
     * @param b0 the value before b
     * @param b a leaf from the rhs
     * @param bs the sign of b. If this is true, b is negated. The sign applies just to b, not to b0
     * @return the result. Can be a leaf or null
     */
    protected def collision(a0:Boolean, a:Leaf, as:Boolean, b0:Boolean,b:Leaf, bs:Boolean) : IntervalTrie

    /**
     * This will be called when a is completely covered by a contiguous interval of b
     * @param a a non-null tree (leaf or branch)
     * @param as the sign of a. If this is true, a is negated.
     * @param b0 the constant value of b in the complete interval of a
     * @return the result, can be null
     */
    protected def overlapA(a:IntervalTrie, as:Boolean, b0:Boolean) : IntervalTrie

    /**
     * This will be called when b is completely covered by a contiguous interval of a
     * @param a0 the constant value of a in the complete interval of b
     * @param b a non-null tree (leaf or branch)
     * @param bs the sign of b. If this is true, b is negated.
     * @return the result, can be null
     */
    protected def overlapB(a0:Boolean, b:IntervalTrie, bs:Boolean) : IntervalTrie

    /**
     * Performs the binary operation for two arbitrary trees
     * @param a0 the value before a
     * @param a a node (leaf or branch) from the lhs
     * @param as the sign of a. If this is true, a is negated. The sign applies just to a, not to a0
     * @param b0 the value before b
     * @param b a node (leaf or branch) from the rhs
     * @param bs the sign of b. If this is true, b is negated. The sign applies just to b, not to b0
     * @return the result, can be null
     */
    private final def op(a0:Boolean, a: IntervalTrie, as:Boolean, b0:Boolean, b: IntervalTrie, bs:Boolean): IntervalTrie = {
      val a_l = a.level
      val a_p = a.prefix
      val b_l = b.level
      val b_p = b.prefix

      if (a_l > b_l) {
        // a is larger => a must be a branch
        if (!hasMatchAt(b_p, a_p, a_l)) {
          // the prefix of a and b is different. We don't care if a is a branch or a leaf
          join(a0, a, as, b0, b, bs)
        } else a match {
          case a: Branch =>
            val as1 = as ^ a.s
            if (zeroAt(b_p, a_l)) {
              // b fits into the left child of a
              branch(a_p, a_l, op(a0, a.left, as1, b0, b, bs), overlapA(a.right, as1, b.after ^ bs))
            } else {
              // b fits into the right child of a
              branch(a_p, a_l, overlapA(a.left, as1, b0), op(a.mid ^ as, a.right, as1, b0, b, bs))
            }
          case _ =>
            unreachable
        }
      } else if (b_l > a_l) {
        // b is larger => b must be a branch
        if (!hasMatchAt(a_p, b_p, b_l)) {
          // the prefix of a and b is different. We don't care if b is a branch or a leaf
          join(a0, a, as, b0, b, bs)
        } else b match {
          case b: Branch =>
            val bs1 = bs ^ b.s
            if (zeroAt(a_p, b_l)) {
              // a fits into the left child of b
              branch(b_p, b_l, op(a0, a, as, b0, b.left, bs1), overlapB(a.after ^ as, b.right, bs1))
            } else {
              // a fits into the right child of b
              branch(b_p, b_l, overlapB(a0, b.left, bs1), op(a0, a, as, b.mid ^ bs, b.right, bs1))
            }
          case _ =>
            unreachable
        }
      } else {
        // a_m == b_m, trees are the same size
        if (a_p == b_p) {
          (a, b) match {
            case (a: Branch, b: Branch) =>
              val as1 = as ^ a.s
              val bs1 = bs ^ b.s
              // same prefix. leaves have to be merged
              branch(a_p, a_l, op(a0, a.left, as1, b0, b.left, bs1), op(a.mid ^ as, a.right, as1, b.mid ^ bs, b.right, bs1))
            case (a: Leaf, b: Leaf) =>
              collision(a0, a, as, b0, b, bs)
            case _ =>
              unreachable
          }
        } else {
          // same mask, different prefix
          join(a0, a, as, b0, b, bs)
        }
      }
    }

    /**
     * Internal version of the operation. Can return null
     * @param a the lhs
     * @param b the rhs
     * @return the result, can be null
     */
    protected def apply0(a:IntervalTrie, b:IntervalTrie) =
      op(a0 = false, a, as = false, b0 = false, b, bs = false)

    /**
     * External version of the operation. The result will never be null
     * @param a the lhs
     * @param b the rhs
     * @return the result, guaranteed to be non-null
     */
    final def apply(a: IntervalTrie, b: IntervalTrie) =
      nullToZero(apply0(a, b))
  }

  object OrCalculator extends OrderedBinaryOperator {

    protected def collision(a0:Boolean, a: Leaf, as:Boolean, b0: Boolean, b:Leaf, bs:Boolean) = {
      val before1 = a0 | b0
      val at1 = (a.at ^ as) | (b.at ^ bs)
      val after1 = (a.after ^ as) | (b.after ^ bs)
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
        null
      else
        a flip as

    protected def overlapB(a0: Boolean, b: IntervalTrie, bs: Boolean) =
      if(a0)
        null
      else
        b flip bs
  }

  object XorCalculator extends OrderedBinaryOperator {

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
      a flip (as ^ b0)

    protected def overlapB(a0: Boolean, b: IntervalTrie, bs: Boolean) =
      b flip (bs ^ a0)
  }

  object AndCalculator extends OrderedBinaryOperator {

    protected def collision(a0:Boolean, a: Leaf, as:Boolean, b0: Boolean, b:Leaf, bs:Boolean) = {
      val before1 = a0 & b0
      val at1 = (a.at ^ as) & (b.at ^ bs)
      val after1 = (a.after ^ as) & (b.after ^ bs)
      if(before1 == at1 && at1 == after1 && a.key != 0)
        null // noop leaf
      else if(a.at == at1 && a.after == after1)
        a // reuse a if possible
      else if(b.at == at1 && b.after == after1)
        b // reuse b if possible
      else
        Leaf(a.key, at1, after1)
    }

    protected def overlapA(a: IntervalTrie, as:Boolean, b0: Boolean) = {
      if(b0)
        a flip as
      else
        null
    }

    protected def overlapB(a: Boolean, b: IntervalTrie, bs:Boolean) = {
      if(a)
        b flip bs
      else
        null
    }
  }

  /**
   * An operation that calculates the value before, at or behind a position
   */
  sealed abstract class Sampler {

    def apply(a: IntervalTrie, value: Long) = op(false, a, false, value)

    /**
     * Method that is invoked when a leaf is found. This allows to customize whether we want at, before or after
     * @param a0 the value before the leaf
     * @param a the leaf
     * @param as the sign of the leaf
     */
    protected def onLeaf(a0: Boolean, a: Leaf, as: Boolean): Boolean

    private final def op(a0: Boolean, a: IntervalTrie, as: Boolean, value: Long): Boolean = a match {
      case a: Branch =>
        val prefix = a.prefix
        val level = a.level
        if (!hasMatchAt(value, prefix, level)) {
          // key is either before or after a
          val branchLevel = levelAbove(prefix, value)
          if (zeroAt(prefix, branchLevel))
            a.after ^ as // after
          else
            a0 // before
        } else {
          val as1 = as ^ a.s
          // key is within a
          if (zeroAt(value, level))
            op(a0, a.left, as1, value)
          else
            op(a.mid ^ as, a.right, as1, value)
        }
      case a: Leaf =>
        if (a.key == value)
          onLeaf(a0, a, as)
        else if (unsigned_<(a.key, value))
          a.after ^ as
        else
          a0
    }
  }

  object SampleBefore extends Sampler {

    protected def onLeaf(a0: Boolean, a: Leaf, as: Boolean): Boolean = a0
  }

  object SampleAt extends Sampler {

    protected def onLeaf(a0: Boolean, a: Leaf, as: Boolean): Boolean = a.at ^ as
  }

  object SampleAfter extends Sampler {

    protected def onLeaf(a0: Boolean, a: Leaf, as: Boolean): Boolean = a.after ^ as
  }

  /**
   * The interval trie that is true everywhere (except before 0, where it is false by convention
   */
  final val one : IntervalTrie = Leaf(0L, at = true, after = true)

  /**
   * The interval trie that is false everywhere
   */
  final val zero : IntervalTrie = Leaf(0L, at = false, after = false)

  def point(value:Long) = Leaf(value, at = true, after = false)

  def hole(value:Long) = Leaf(value, at = false, after = true)

  def start(value:Long, included:Boolean) = Leaf(value, at = included, after = true)

  def startAt(value:Long) = start(value, included = true)

  def startAfter(value:Long) = start(value, included = false)

  def end(value:Long, included:Boolean) = Leaf(value, at = included, after = false)

  def endAt(value:Long) = end(value, true)

  def endBefore(value:Long) = end(value, false)

  def apply(elems : Leaf*) : IntervalTrie =
    nullToZero(elems.foldLeft(zero)(merge0))

  /**
   * A leaf. This is going to be changed to 4 different leaf types for the 4 possible combinations of at and after
   * @param prefix the prefix, which in case of a leaf is identical to the key
   * @param at the value exactly at the key
   * @param after the value immediately after the key
   */
  final case class Leaf(prefix: Long, at:Boolean, after:Boolean) extends IntervalTrie {

    /**
     * For a leaf, the prefix is the key
     */
    def key = prefix

    /**
     * This is -1 for leaves, so that the smallest possible branch (level=0) has a bigger level than a leaf
     */
    def level = -1.toByte

    /**
     * xors the values of at and after with the value
     * @param value true if the leaf shall be flipped
     */
    def flip(value:Boolean) : Leaf = if(value) copy(at = !at, after = !after) else this

    /**
     * The hash code uses just the key, so that a tree with the root negated will have the same hash as one with the leaves negated
     */
    override def hashCode = prefix.##
  }

  /**
   * A branch
   * @param prefix the common prefix of both children
   * @param level the level of the node. 0..63 for branches. Higher means bigger
   * @param left the left child
   * @param right the right child
   * @param s the sign. If this is true, the tree is negated
   */
  final case class Branch(prefix : Long, level : Byte, left : IntervalTrie, right : IntervalTrie, s:Boolean = false) extends IntervalTrie {

    /**
     * The value between the left and right child
     */
    val mid = left.after ^ s

    /**
     * The value after the right child.
     */
    val after = right.after ^ s

    /**
     * negates the branch if value is true
     * @param value true if the tree shall be negated
     */
    def flip(value:Boolean) : Branch = if(value) copy(s = !s) else this

    /**
     * The hash code excludes the sign so that a tree with the root negated will have the same hash as one with the leaves negated
     */
    override def hashCode = left.## + 41 * right.##
  }

  /**
   * Equals method that considers the sign. E.g. equals0(a, a.negate, true) will return true
   * @param a the lhs
   * @param b the rhs
   * @param abs the product of the signs of a and b. False for a straight comparison
   * @return true if the trees are equal modulo the product abs
   */
  private final def equals0(a:IntervalTrie, b:IntervalTrie, abs:Boolean) : Boolean = {
    if(a.prefix != b.prefix)
      // this works for both branches and leafs
      false
    else if(a.level != b.level)
      // this ensures that the type is the same
      false
    else (a,b) match {
      case (a:Branch, b:Branch) =>
        // create the product of the sign from above and the signs of the operands
        val abs1 = abs ^ a.s ^ b.s
        equals0(a.left, b.left, abs1) && equals0(a.right, b.right, abs1)
      case (a:Leaf, b:Leaf) =>
        // we know that the prefixes are the same, so we don't have to compare the keys
        (a.at ^ abs == b.at) && (a.after ^ abs == b.after)
      case _ =>
        // we already know that the levels are the same, so both must be either leaf or branch
        unreachable
    }
  }

  /**
   * Helper method to transform the internal representation (null allowed) to the external representation
   * @param value the internal representation (can be null)
   */
  @inline private final def nullToZero(value:IntervalTrie) : IntervalTrie = if(value eq null) zero else value

  /**
   * Merges a leaf into a tree, using the rhs in case of a collision
   * @param lhs the original tree. Can be null (=empty)
   * @param rhs the leaf to be merged. Must not be null
   * @return a new tree with rhs merged in
   */
  def merge0(lhs: IntervalTrie, rhs: Leaf): IntervalTrie = lhs match {
    case lhs@Branch(prefix, level, left, right, _) =>
      if (!hasMatchAt(rhs.key, prefix, level))
        join(rhs.key, rhs, prefix, lhs)
      else if (zeroAt(rhs.key, level))
        Branch(prefix, level, merge0(left, rhs), right)
      else
        Branch(prefix, level, left, merge0(right, rhs))
    case lhs@Leaf(key2, _, _) =>
      if (rhs.key == key2) rhs
      else join(rhs.key, rhs, key2, lhs)
    case _ =>
      rhs
  }

  /**
   * Loops over the leafs of the trie in unsigned order of the keys.
   */
  final def foreachLeaf[U](a:IntervalTrie, as:Boolean,f : Leaf =>  U) : Unit = a match {
    case a:Branch =>
      val as1 = as ^ a.s
      foreachLeaf(a.left, as1, f)
      foreachLeaf(a.right, as1, f)
    case leaf:Leaf =>
      f(leaf flip as)
  }

  /**
   * formats the tree to a string, given a function to convert an element to a string
   * @param a the tree
   * @param elementToString the element toString method
   * @param zeroText the text to use for the zero tree, default is "zero"
   */
  def format(a:IntervalTrie, elementToString: Long => String = _.toString, zeroText:String = "zero") = {
    if (a == zero)
      zeroText
    else {
      val rb = new StringBuilder
      var before = false
      foreachLeaf(a, false, leaf => {
        def key = elementToString(leaf.key)
        for(text <- (before, leaf.at, leaf.after) match {
          case (false, true, false) => Some(s"[$key]")
          case (true, false, true) => Some(s"$key[, ]$key")
          case (false, false, true) => Some(s"]$key")
          case (false, true, true) => Some(s"[$key")
          case (true, true, false) => Some(s"$key]")
          case (true, false, false) => Some(s"$key[")
          case _ => None
        }) {
          val sep = if (!leaf.after) ", " else ".."
          rb.append(text)
          rb.append(sep)
        }
        before = leaf.after
      })
      if (a.after)
        rb.append(s"${elementToString(-1)}]")
      else
        rb.length -= 2
      rb.toString()
    }

  }
}

private[immutable] sealed abstract class IntervalTrie {

  import IntervalTrie._

  def prefix : Long

  def level : Byte

  def after : Boolean
  
  def flip(that:Boolean) : IntervalTrie

  def before(value:Long) : Boolean =
    SampleBefore(this, value)

  def at(value:Long) : Boolean =
    SampleAt(this, value)

  def after(value:Long) : Boolean =
    SampleAfter(this, value)

  final override def equals(that:Any) = that match {
    case that:IntervalTrie => equals0(this, that, false)
    case _ => false
  }

  def foreachKey[U](f : Long => U) : Unit = this match {
    case Branch(_, _, left, right, _) =>
      left.foreachKey(f)
      right.foreachKey(f)
    case Leaf(key, _, _) => f(key)
  }

  /**
   * The number of leaves in the tree. This is always >=1, since even the zero tree is represented as a leaf
   */
  final def size : Int = this match {
    case leaf:Leaf => 1
    case Branch(_, _, left, right, _) => left.size + right.size
  }

  def isEmpty = this match {
    case Leaf(key, at, after) => key==0 && !at && !after
    case _ => false
  }
}