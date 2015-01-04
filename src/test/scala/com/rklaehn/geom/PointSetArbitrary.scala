package com.rklaehn.geom

import org.scalacheck.{Gen, Arbitrary}
import spire.implicits._

object PointSetArbitrary {

  def genX = Gen.choose(-100L, 100L).map(_ * 2)

  def genY = Gen.choose(-100L, 100L).map(_ * 2)

  def genLevel = Gen.choose(-10, 10)

  def genPoint:Gen[PointSet[Long]] = for {
    x <- genX
    y <- genY
    l <- genLevel
  } yield PointSet.point(x,y,l)

  def genVerticalLine:Gen[PointSet[Long]] = for {
    x <- genX
    ya <- genY
    yb <- genY
    l <- genLevel
  } yield PointSet.verticalEdge(x,ya,yb,l)

  def genHorizontalLine:Gen[PointSet[Long]] = for {
    xa <- genX
    xb <- genX
    y <- genY
    l <- genLevel
  } yield PointSet.verticalEdge(xa,xb,y,l)

  def genRectangle:Gen[PointSet[Long]] = for {
    xa <- genX
    xb <- genX
    ya <- genY
    yb <- genY
    o <- genLevel
    f <- genLevel
  } yield PointSet.rectangle(xa,ya,xb,yb,o,f)

  def genConstant:Gen[PointSet[Long]] = for {
    l <- genLevel
  } yield PointSet.const(l)

  def genSimplePointSet = Gen.frequency(
    1 -> Gen.const(PointSet.zero[Long]),
    1 -> genConstant,
    10 -> genPoint,
    10 -> genVerticalLine,
    10 -> genHorizontalLine,
    10 -> genRectangle
  )

  def genPointSet = for {
    elements <- Gen.containerOfN[Array, PointSet[Long]](100, genSimplePointSet)
  } yield
    (PointSet.zero[Long] /: elements)(_ + _)

  val arbitrary = Arbitrary(genPointSet)
}
