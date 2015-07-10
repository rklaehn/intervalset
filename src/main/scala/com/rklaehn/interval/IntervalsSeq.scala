package com.rklaehn.interval

import scala.collection.immutable.HashSet
import IntervalsSeq._

class IntervalsSeq[T](belowAll: HashSet[T], elements: Array[Element[T]]) {

}

object IntervalsSeq {

  case class Element[T](at: HashSet[T], sign: HashSet[T])
}