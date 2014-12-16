package scala.collection.immutable

import ichi.bench._
import spire.syntax.all._

object IntervalTrieBench extends App {

  def makeOnOffProfile(n:Int, offset:Long = 0, stride:Long = 2) = {
    val support = (0 until n).map(_ * stride + offset).toArray
    IntervalSetArbitrary.makeProfileXor(false, support, Array.fill(support.length)(0))
  }

  val thyme = new Thyme() // Thyme.warmed(verbose = println)
  println("starting benchmark")
  // in this test, we have to go all the way down to the leaves in all cases
  def fullTraversalTest(n:Int) : Unit = {
    println(s"Full traversal benchmark (n=$n)")
    val a = makeOnOffProfile(n, offset = 0, stride = 2)
    val b = makeOnOffProfile(n, offset = 1, stride = 2)

    thyme.pbenchWarm(thyme.Warm(a | b), title = "a | b")
    thyme.pbenchWarm(thyme.Warm(a & b), title = "a & b")
    thyme.pbenchWarm(thyme.Warm(a ^ b), title = "a ^ b")
    thyme.pbenchWarm(thyme.Warm(~a), title = "~a")
  }

  // in this test, most operations should not have to traverse the entire tree
  def cutoffTest(n:Int) : Unit = {
    println(s"Cutoff benchmark (n=$n)")
    val a = makeOnOffProfile(n, offset = 0, stride = 2)
    val b = makeOnOffProfile(n, offset = 1, stride = 1000)

    thyme.pbenchWarm(thyme.Warm(a | b), title = "a | b")
    thyme.pbenchWarm(thyme.Warm(a & b), title = "a & b")
    thyme.pbenchWarm(thyme.Warm(a ^ b), title = "a ^ b")
  }

  fullTraversalTest(100000)
  cutoffTest(100000)
}
