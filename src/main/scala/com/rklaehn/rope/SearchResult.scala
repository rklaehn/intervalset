package com.rklaehn.rope

object SearchResult {

  def apply(index: Int) = new SearchResult(index)

  def before(index: Int) = apply(-(index + 1))
}

class SearchResult(val value: Int) extends AnyVal {

  def isDirect: Boolean = value >= 0

  def toIndex: Int = if (value >= 0) value else -value - 1

  def +(offset: Int): SearchResult =
    if (value >= 0)
      SearchResult(value + offset)
    else
      SearchResult(value - offset)

  override def toString = if (isDirect) s"At($value)" else s"Before(${-value - 1})"
}