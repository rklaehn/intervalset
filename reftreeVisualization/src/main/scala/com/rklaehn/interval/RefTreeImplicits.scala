package com.rklaehn.interval
import reftree.core.RefTree.Ref
import reftree.core._

trait RefTreeImplicits {

  implicit def intervalTrieToRefTree[A]: ToRefTree[IntervalTrie[A]] = ToRefTree(intervalTrieConvert)

  private def getId(x: Any): String = System.identityHashCode(x).toString

  private def intervalTrieConvert(is: IntervalTrie[_]): RefTree = {
    val treeF = is.getClass.getDeclaredField("tree")
    val belowAllF = is.getClass.getDeclaredField("belowAll")
    treeF.setAccessible(true)
    belowAllF.setAccessible(true)
    val tree = treeF.get(is).asInstanceOf[Tree]
    val belowAll = belowAllF.get(is).asInstanceOf[Boolean]
    RefTree.Ref(
      "IntervalTrie",
      getId(is),
      Seq(
        belowAll.refTree.toField.withName("belowAll"),
        intervalTrieTreeConvert(tree).toField.withName("tree")
      ),
      highlight = false)
  }

  private def prefixToField(prefix: Long, name: String = "prefix"): Ref.Field = {
    val prefix1 = ("1" + prefix.toBinaryString.takeRight(7)).toLong
    prefix1.refTree.toField.withName(name)
  }

  private def intervalTrieTreeConvert(tree: Tree): RefTree = tree match {
    case branch@Tree.Branch(prefix, level, left, right) =>
      RefTree.Ref(
        "Branch", getId(branch), Seq(
          intervalTrieTreeConvert(left).toField.withName("left"),
          intervalTrieTreeConvert(right).toField.withName("right"),
          prefixToField(prefix),
          level.refTree.toField.withName("level"),
          branch.sign.refTree.toField.withName("sign")
        ), false)
    case leaf@Tree.Leaf(prefix, at, sign) =>

      RefTree.Ref("Leaf", getId(leaf), Seq(
        prefixToField(prefix, "value"),
        at.refTree.toField.withName("at"),
        sign.refTree.toField.withName("sign")
      ), false)
    case null =>
      RefTree.Null()
  }
}

object RefTreeImplicits extends RefTreeImplicits

