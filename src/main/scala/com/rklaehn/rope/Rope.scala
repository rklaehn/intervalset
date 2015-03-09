package com.rklaehn.rope

import java.util

import spire.algebra.Order

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

object Slice {
  def apply[T](rope: Rope[T]): Slice[T] =
    Slice(rope, SearchResult.before(0), SearchResult.before(rope.length))
}

case class Slice[T](var rope: Rope[T], var from: SearchResult, var until: SearchResult) {

  def init(source: Slice[T]): Unit = {
    from = source.from
    until = source.until
    rope = source.rope
  }

  def search(key: T)(implicit order: Order[T]): SearchResult =
    rope.search(from.toIndex, until.toIndex, key)

  def from(value: SearchResult): Unit = {
    from = value
    @tailrec
    def descend(): Unit = rope match {
      case tree: Rope.Branch[T] if from.toIndex >= tree.weight =>
        from += tree.weight
        until += tree.weight
        rope = tree.right
        descend()
      case _ =>
    }
    descend()
  }

  def until(value: SearchResult): Unit = {
    until = value
    @tailrec
    def descend(): Unit = rope match {
      case tree: Rope.Branch[T] if until.toIndex <= tree.weight =>
        rope = tree.left
        descend()
      case _ =>
    }
    descend()
  }
}

sealed abstract class Rope[T: ClassTag] {

  def length: Int

  def depth: Int

  def apply(x: Int): T

  def ++(that: Rope[T]): Rope[T]

  def copyToArray(from: Int, until: Int, target: Array[T], offset: Int): Unit

  def search(from: Int, until: Int, key: T)(implicit order: Order[T]): SearchResult
}

object Rope {

  def apply[T: ClassTag](array: Array[T]): Rope[T] = Leaf(array.clone)

  sealed abstract class Tree[T: ClassTag] extends Rope[T] {

    def weight: Int

    def head: T

    def ++(that: Rope[T]): Rope[T] = concatenate(this, that.asInstanceOf[Tree[T]])

    def depth: Int

    def isBalanced: Boolean
  }

  case class Leaf[T: ClassTag](elements: Array[T]) extends Tree[T] {

    def depth = 0

    def isBalanced = true

    def head: T = elements(0)

    def apply(index: Int): T = elements(index)

    def length: Int = elements.length

    def weight: Int = elements.length

    private def binarySearch(from: Int, until: Int, key: T)(implicit order: Order[T]): SearchResult = {

      @tailrec
      def binarySearch0(low: Int, high: Int): SearchResult =
        if (low <= high) {
          val mid = (low + high) >>> 1
          val midVal = elements(mid)
          val c = order.compare(midVal, key)
          if (c < 0)
            binarySearch0(mid + 1, high)
          else if (c > 0)
            binarySearch0(low, mid - 1)
          else
            SearchResult(mid)
        } else SearchResult.before(low)
      binarySearch0(from, until - 1)
    }

    def search(from: Int, until: Int, key: T)(implicit order: Order[T]): SearchResult =
      binarySearch(from, until, key)

    def copyToArray(from: Int, until: Int, target: Array[T], offset: Int): Unit =
      System.arraycopy(elements, from, target, offset, until - from)
  }

  case class Branch[T: ClassTag](left: Tree[T], right: Tree[T]) extends Tree[T] {

    def depth: Int = (left.depth max right.depth) + 1

    def isBalanced: Boolean = length >= minLengthByDepth(depth)

    val head = left.head

    def pivot = right.head

    val length: Int = left.length + right.length

    def weight: Int = left.length

    def apply(index: Int) = if (index < weight) left(index) else right(index - weight)

    override def search(from: Int, until: Int, key: T)(implicit order: Order[T]): SearchResult =
      if (from >= until)
        SearchResult.before(from)
      else if (until <= weight)
        left.search(from, until, key)
      else if (from >= weight)
        right.search(from - weight, until - weight, key) + weight
      else {
        val c = order.compare(key, pivot)
        if (c < 0)
          left.search(from, weight, key)
        else if (c > 0)
          right.search(1, until - weight, key) + weight
        else
          SearchResult(weight)
      }

    def copy(left: Tree[T], right: Tree[T]): Branch[T] =
      if ((left eq this.left) && (right == this.right))
        this
      else
        Branch(left, right)

    override def copyToArray(from: Int, until: Int, target: Array[T], offset: Int): Unit = {
      val l = (weight - from) max 0
      val r = (until - weight) max 0
      if (l > 0)
        left.copyToArray(from, from + l, target, offset)
      if (r > 0)
        right.copyToArray(until - r - weight, until - weight, target, offset + r)
    }
  }

  /**
   * Concatenate the given strings while performing various optimizations to
   * slow the growth rate of tree depth and tree node count. The result is
   * either a {@link LiteralByteString} or a {@link RopeByteString}
   * depending on which optimizations, if any, were applied.
   *
   * <p>Small pieces of length less than {@link
   * ByteString#CONCATENATE_BY_COPY_SIZE} may be copied by value here, as in
   * BAP95.  Large pieces are referenced without copy.
   *
   * @param left  string on the left
   * @param right string on the right
   * @return concatenation representing the same sequence as the given strings
   */
  def concatenate[T: ClassTag](left: Tree[T], right: Tree[T]): Tree[T] = {
    val leftRope = left match {
      case left:Branch[T] => left
      case _ => null
    }
    if (right.length == 0) {
      left
    } else if (left.length == 0) {
      right
    } else {
      val newLength = left.length + right.length
      if (newLength < CONCATENATE_BY_COPY_SIZE) {
        // Optimization from BAP95: For short (leaves in paper, but just short
        // here) total length, do a copy of data to a new leaf.
        concatenateBytes(left, right)
      } else if (leftRope != null
        && leftRope.right.length + right.length < CONCATENATE_BY_COPY_SIZE) {
        // Optimization from BAP95: As an optimization of the case where the
        // ByteString is constructed by repeated concatenate, recognize the case
        // where a short string is concatenated to a left-hand node whose
        // right-hand branch is short.  In the paper this applies to leaves, but
        // we just look at the length here. This has the advantage of shedding
        // references to unneeded data when substrings have been taken.
        //
        // When we recognize this case, we do a copy of the data and create a
        // new parent node so that the depth of the result is the same as the
        // given left tree.
        val newRight = concatenateBytes(leftRope.right, right)
        Branch(leftRope.left, newRight)
      } else if (leftRope != null
        && leftRope.left.depth > leftRope.right.depth
        && leftRope.depth > right.depth) {
        // Typically for concatenate-built strings the left-side is deeper than
        // the right.  This is our final attempt to concatenate without
        // increasing the tree depth.  We'll redo the the node on the RHS.  This
        // is yet another optimization for building the string by repeatedly
        // concatenating on the right.
        val newRight = Branch(leftRope.right, right)
        Branch(leftRope.left, newRight)
      } else {
        // Fine, we'll add a node and increase the tree depth--unless we
        // rebalance ;^)
        val newDepth = (left.depth max right.depth) + 1
        if (newLength >= minLengthByDepth(newDepth)) {
          // The tree is shallow enough, so don't rebalance
          Branch(left, right)
        } else {
          new Balancer().balance(left, right)
        }
      }
    }
  }

  /**
   * Concatenates two strings by copying data values. This is called in a few
   * cases in order to reduce the growth of the number of tree nodes.
   *
   * @param left  string on the left
   * @param right string on the right
   * @return string formed by copying data bytes
   */
  def concatenateBytes[T: ClassTag](left: Tree[T],
    right: Tree[T]): Leaf[T] = {
    val leftSize = left.length
    val rightSize = right.length
    val target = Array.ofDim[T](leftSize + rightSize)
    left.copyToArray(0, leftSize, target, 0)
    right.copyToArray(0, rightSize, target, leftSize)
    Leaf(target)
  }

  /**
   * This class implements the balancing algorithm of BAP95. In the paper the
   * authors use an array to keep track of pieces, while here we use a stack.
   * The tree is balanced by traversing subtrees in left to right order, and the
   * stack always contains the part of the string we've traversed so far.
   *
   * <p>One surprising aspect of the algorithm is the result of balancing is not
   * necessarily balanced, though it is nearly balanced.  For details, see
   * BAP95.
   */
  private class Balancer[T: ClassTag] {

    // Stack containing the part of the string, starting from the left, that
    // we've already traversed.  The final string should be the equivalent of
    // concatenating the strings on the stack from bottom to top.
    private final val prefixesStack = new mutable.Stack[Tree[T]]()

    def balance(left: Tree[T], right: Tree[T]): Tree[T] = {
      doBalance(left)
      doBalance(right)

      // Sweep stack to gather the result
      var partialString = prefixesStack.pop()
      while (!prefixesStack.isEmpty) {
        val left1 = prefixesStack.pop()
        partialString = Branch(left1, partialString)
      }
      // We should end up with a RopeByteString since at a minimum we will
      // create one from concatenating left and right
      partialString
    }

    def doBalance(root: Tree[T]): Unit = {
      // BAP95: Insert balanced subtrees whole. This means the result might not
      // be balanced, leading to repeated rebalancings on concatenate. However,
      // these rebalancings are shallow due to ignoring balanced subtrees, and
      // relatively few calls to insert() result.
      root match {
        case root if root.isBalanced =>
          insert(root)
        case branch:Branch[T] =>
          doBalance(branch.left)
          doBalance(branch.right)
        case _ =>
          throw new IllegalArgumentException(
            "Has a new type of ByteString been created? Found " +
              root.getClass())
      }
    }

    /**
     * Push a string on the balance stack (BAP95).  BAP95 uses an array and
     * calls the elements in the array 'bins'.  We instead use a stack, so the
     * 'bins' of lengths are represented by differences between the elements of
     * minLengthByDepth.
     *
     * <p>If the length bin for our string, and all shorter length bins, are
     * empty, we just push it on the stack.  Otherwise, we need to start
     * concatenating, putting the given string in the "middle" and continuing
     * until we land in an empty length bin that matches the length of our
     * concatenation.
     *
     * @param tree string to place on the balance stack
     */
    private def insert(tree: Tree[T]): Unit = {
      var depthBin = getDepthBinForLength(tree.length)
      var binEnd = minLengthByDepth(depthBin + 1)

      // BAP95: Concatenate all trees occupying bins representing the length of
      // our new piece or of shorter pieces, to the extent that is possible.
      // The goal is to clear the bin which our piece belongs in, but that may
      // not be entirely possible if there aren't enough longer bins occupied.
      if (prefixesStack.isEmpty || prefixesStack.top.length >= binEnd) {
        prefixesStack.push(tree)
      } else {
        val binStart = minLengthByDepth(depthBin)

        // Concatenate the subtrees of shorter length
        var newTree = prefixesStack.pop()
        while (!prefixesStack.isEmpty
          && prefixesStack.top.length < binStart) {
          val left = prefixesStack.pop()
          newTree = Branch(left, newTree)
        }

        // Concatenate the given string
        newTree = Branch(newTree, tree)

        // Continue concatenating until we land in an empty bin
        while (!prefixesStack.isEmpty) {
          depthBin = getDepthBinForLength(newTree.length)
          binEnd = minLengthByDepth(depthBin + 1)
          if (prefixesStack.top.length < binEnd) {
            val left = prefixesStack.pop()
            newTree = Branch(left, newTree)
          } else {
            prefixesStack.push(newTree)
            return
          }
        }
        prefixesStack.push(newTree)
      }
    }

    private def getDepthBinForLength(length: Int): Int = {
      var depth = util.Arrays.binarySearch(minLengthByDepth, length)
      if (depth < 0) {
        // It wasn't an exact match, so convert to the index of the containing
        // fragment, which is one less even than the insertion point.
        val insertionPoint = -(depth + 1)
        depth = insertionPoint - 1
      }

      depth
    }
  }

  private[this] val CONCATENATE_BY_COPY_SIZE = 64

  private[this] val minLengthByDepth: Array[Int] = {
    val builder = Array.newBuilder[Int]
    @tailrec
    def fibs(f1: Int, f2: Int): Unit =
      if (f2 > 0) {
        builder += f2
        fibs(f2, f1 + f2)
      }
    fibs(1, 1)
    builder.result()
  }
}