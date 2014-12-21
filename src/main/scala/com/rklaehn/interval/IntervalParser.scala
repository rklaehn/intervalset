package com.rklaehn.interval
import spire.math.{Rational, Interval}
import spire.math.Interval._

private object IntervalParser {

  private val NullRe = "^ *\\( *Ø *\\) *$".r
  private val SingleRe = "^ *\\[ *([^,]+) *\\] *$".r
  private val PairRe = "^ *(\\[|\\() *(.+?) *, *(.+?) *(\\]|\\)) *$".r

  def apply(s: String): Interval[Rational] =
    s match {
      case NullRe() => Interval.empty[Rational]
      case SingleRe(x) => Interval.point(Rational(x))
      case PairRe(left, x, y, right) =>
        (left, x, y, right) match {
          case ("(", "-∞", "∞", ")") => Interval.all[Rational]
          case ("(", "-∞", y, ")") => Interval.below(Rational(y))
          case ("(", "-∞", y, "]") => Interval.atOrBelow(Rational(y))
          case ("(", x, "∞", ")") => Interval.above(Rational(x))
          case ("[", x, "∞", ")") => Interval.atOrAbove(Rational(x))
          case ("[", x, y, "]") => Interval.closed(Rational(x), Rational(y))
          case ("(", x, y, ")") => Interval.open(Rational(x), Rational(y))
          case ("[", x, y, ")") => Interval.fromBounds(Closed(Rational(x)), Open(Rational(y)))
          case ("(", x, y, "]") => Interval.fromBounds(Open(Rational(x)), Closed(Rational(y)))
          case _ => throw new NumberFormatException("Impossible: " + s)
        }
      case _ => throw new NumberFormatException("For input string: " + s)
    }

}
