package scala.collection.immutable

import org.scalacheck.{Gen, Arbitrary}

object IntervalSetArbitrary {

  def makeProfileXor(initial:Boolean, support:Array[Long], kind:Array[Int]) : IntervalSet[Long] = {
    require(support.length == kind.length)
    require(kind.forall(x => x >= 0 && x <= 2))
    val r = IntervalSet.constant[Long](initial)
    (r /: (support zip kind)) {
      case (current, (x,k)) => current ^ IntervalSet.fromKind(x,k)
    }
  }

  private def randomProfileXor(min: Long, max: Long, count: Int): Gen[IntervalSet[Long]] = {
    for {
      initial <- Gen.oneOf(true, false)
      edges <- Gen.resize(count, Gen.containerOf[Array, Long](Gen.choose(min, max)))
      support = edges.sorted.distinct
      kind <- Gen.containerOfN[Array, Int](support.length, Gen.oneOf(0, 1, 2))
    } yield
      makeProfileXor(initial, support, kind)
  }

  private def randomProfileGen(size:Int) = Gen.frequency[IntervalSet[Long]](
    1 -> IntervalSet.zero[Long],
    1 -> IntervalSet.one[Long],
    30 -> randomProfileXor(0, 100, size)
  )

  val arbitrary = Arbitrary[IntervalSet[Long]](randomProfileGen(10))
}
