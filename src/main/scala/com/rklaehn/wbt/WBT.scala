package com.rklaehn.wbt

sealed trait Balance {

  def isBalanced(a:Int, b:Int): Boolean

  def isSingle(a:Int, b:Int): Boolean
}

/**
 * The integer Nievergelt balance
 */
object NievergeltBalance1 extends Balance {

  // Nievergelt
  val delta = 3

  val ratio = 2

  def isBalanced(a: Int, b:Int): Boolean =
    delta * (a + 1) >= (b + 1)

  def isSingle(a:Int, b:Int): Boolean =
    (a + 1) < ratio * (b + 1)

}

/**
 * The stronger Nievergelt balance
 */
object NievergeltBalance2 extends Balance {

  def isBalanced(a: Int, b:Int): Boolean = {
    val x = a + 1L
    val y = b + 1L
    val z = x + y
    2 * y * y <= z * z
  }

  def isSingle(a:Int, b:Int): Boolean = {
    val z = a + 1L
    val w = b + 1L
    z * z < 2 * w * w
  }
}

abstract class WBT[T] {

  final case class Bin(left: Bin, value: T, right: Bin)

  def compare(a: T, b: T): Int

  val balance = NievergeltBalance2

  val empty: Bin = null

  def depth(t: Bin): Int = if (t eq null) 0 else 1 + (depth(t.left) max depth(t.right))

  def merge(a: T, b: T): T = b

  def singleton(k: T) = Bin(null, k, null)

  def size(tree: Bin): Int = tree match {
    case Bin(l, _, r) => size(l) + 1 + size(r)
    case _ => 0
  }

  def insert(tree: Bin, value: T): Bin = tree match {
    case Bin(l, a, r) =>
      compare(value, a) match {
        case -1 => balanceR(Bin(insert(l, value), a, r))
        case 0  => Bin(l, merge(tree.value, value), r)
        case 1  => balanceL(Bin(l, a, insert(r, value)))
      }
    case _ => singleton(value)
  }

  def isBalanced(a: Bin, b: Bin): Boolean =
    balance.isBalanced(size(a), size(b))

  def isSingle(a: Bin, b: Bin): Boolean =
    balance.isSingle(size(a), size(b))

  def balanced(tree: Bin): Boolean = tree match {
    case Bin(l, _, r) =>
      isBalanced(l, r) && isBalanced(r, l) &&
        balanced(l) && balanced(r)
    case _ => true
  }

  def balanceL(tree: Bin): Bin = {
    tree match {
      case Bin(l, _, r) =>
        if (isBalanced(l, r)) tree
        else rotateL(tree)
    }
  } ensuring(balanced _)

  def balanceR(tree: Bin): Bin = {
    tree match {
      case Bin(l, _, r) =>
        if (isBalanced(r, l)) tree
        else rotateR(tree)
    }
  } ensuring(balanced _)

  def rotateL(tree: Bin): Bin = { tree match {
    case Bin(_, _, Bin(rl, _, rr)) =>
      if (isSingle(rl, rr)) singleL(tree)
      else doubleL(tree)
  }
  } ensuring(balanced _)

  def rotateR(tree: Bin): Bin = { tree match {
    case Bin(Bin(ll, _, lr), _, _) =>
      if (isSingle(lr, ll)) singleR(tree)
      else doubleR(tree)
    }
  } ensuring(balanced _)

  def singleL(tree: Bin): Bin = {
    tree match {
      case Bin(l, a, Bin(rl, b, rr)) => Bin(Bin(l, a, rl), b, rr)
    }
  } ensuring(balanced _)

  def singleR(tree: Bin): Bin = {
    tree match {
      case Bin(Bin(ll, a, lr), b, r) => Bin(ll, a, Bin(lr, b, r))
    }
  } ensuring(balanced _)

  def doubleL(tree: Bin): Bin = {
    tree match {
      case Bin(l, a, Bin(Bin(rll, b, rlr), c, rr)) => Bin(Bin(l, a, rll), b, Bin(rlr, c, rr))
    }
  } ensuring(balanced _)

  def doubleR(tree: Bin): Bin = {
    tree match {
      case Bin(Bin(ll, a, Bin(lrl, b, lrr)), c, r) => Bin(Bin(ll, a, lrl), b, Bin(lrr, c, r))
    }
  } ensuring(balanced _)
}

object IntWBT extends WBT[Int] {
  override def compare(a: Int, b: Int): Int = Integer.compare(a, b)
}

object IntWBTTest extends App {

  import IntWBT._

  val ints = (0 until 1000).toIndexedSeq
  val r = new scala.util.Random
  val s1 = (empty /: ints) { case (a,b) => insert(a,b) }
  val s2 = (empty /: ints.reverse) { case (a,b) => insert(a,b) }
  for (i <- 0 until 10) {
    val t = (empty /: r.shuffle(ints)) { case (a,b) => insert(a,b) }
    println(depth(t))
    println(t)
    println(balanced(t))
  }
}