# IntervalMap

IntervalMap[K, V] is a data structure similar to [IntervalSet](IntervalSet.md), but with a value attached to each interval.

## Example

### Boolean operations

When using a value type such as Set that supports [Bool] or [GenBool] operations, an IntervalSet can be used to represent a schedule of overlapping activities.

Take the following day schedule (Int used to represent times for simplicity, even though it would be easy enough to
use e.g. java.time.Instance)

```
          012345678901234567890123
awake            |---------------|
breakfast         ||
work                |-------|
dinner                       ||
sleep     |-----|              |-|
```

This can be encoded as an IntervalMap[Int, Set[String]] like this:

```scala
import com.rklaehn.interval._
import spire.math.Interval
import spire.implicits._
import IntervalMap.CreateFromBool._ // this is to choose which set of factory methods to use

implicit def setEq[T] = spire.optional.genericEq.generic[Set[T]] // not sure why spire does not provide an instance by default...

val a = IntervalMap(Interval(8, 9) -> Set("breakfast")) // outside the given interval, the zero element Set.empty will be used
val b = IntervalMap(Interval(10, 18) -> Set("work"))
val c = IntervalMap(Interval(19,20) -> Set("dinner"))
val d = IntervalMap(Interval(7, 23) -> Set("awake"))
val e = IntervalMap(Interval(0, 6) -> Set("sleep"), Interval(23, 24) -> Set("sleep"))
val schedule = a | b | c | d | e
```

Now you can do simple things like checking activities by time

```scala
scala> schedule(0)
res2: scala.collection.immutable.Set[String] = Set(sleep)

scala> schedule(10)
res3: scala.collection.immutable.Set[String] = Set(work, awake)
```

And more complex things like getting a subset of all activities

```
scala> val eating = IntervalMap(Interval.all[Int] -> Set("breakfast", "dinner"))
scala> (schedule & eating).entries.mkString
res8: String = ((-∞, 8),Set())([8, 9],Set(breakfast))((9, 19),Set())([19, 20],Set(dinner))((20, ∞),Set())
```

### Group operations

If you have a Monoid or Group for your value, usage is very similar. Imagine you have a number of activities and want to aggregate a scalar value such as power consumption for each of them:
```
          012345678901234567890123
light           |------------|
washing           |--|
drying               |--|
```
This can be encoded as an IntervalMap[Int, Double] like this:

```scala
import com.rklaehn.interval._
import spire.math.Interval
import spire.implicits._
import IntervalMap.CreateFromMonoid._ // this is to choose which set of factory methods to use

implicit def m: Monoid[Double] = implicitly[AdditiveMonoid[Double]].additive // use the additive monoid instance

val light    = IntervalMap(Interval(6, 19) -> 40.0) // outside the given interval, the zero element Set.empty will be used
val washing  = IntervalMap(Interval(8, 10) -> 1000.0)
val drying   = IntervalMap(Interval(10,14) -> 3000.0)
val total = light |+| washing |+| drying
```

Now you can check for the total power consumption at a point in time

```scala
scala> total(7)
res17: Double = 40.0

scala> total(11)
res19: Double = 3040.0
```

### Transformations

An IntervalMap does not capture any typeclass instances. So you can build an IntervalMap using boolean operations, and then transform it:

```scala
val activityCount = schedule.mapValues(_.size)
scala> activityCount(12)
res22: Int = 3
```

## Internal representation

The internal representation is similar to IntervalSet, except that for each boundary there are two values stored: one for
the value exactly *at* the boundary, and one for *above* the boundary (as defined by the order of the key)

Redundant boundaries are never stored. To determine when a boundary is redundant, an Eq instance for the value type is
required for most operations. (I can imagine some cases where this is not possible (e.g. functions as values), but in 
that case you can always use a dummy Eq)

[Bool]: https://github.com/non/algebra/blob/master/lattice/src/main/scala/algebra/lattice/Bool.scala
[GenBool]: https://github.com/non/algebra/blob/master/lattice/src/main/scala/algebra/lattice/GenBool.scala
