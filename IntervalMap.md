# IntervalMap

IntervalMap[K, V] is a data structure similar to [IntervalSet][IntervalSet.md], but with a value attached to each interval.

## Example

### Boolean operations

When using a value type such as Set that supports [Bool] or [GenBool] operations, an IntervalSet can be used to represent
a schedule of overlapping activities.

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

val a = IntervalMap(Interval(8, 9) -> Set("breakfast"))
val b = IntervalMap(Interval(10, 18) -> Set("work"))
val c = IntervalMap(Interval(19,20) -> Set("dinner"))
val d = IntervalMap(Interval(7, 23) -> Set("awake"))
val e = IntervalMap(Interval(0, 6) -> Set("sleep"), Interval(11, 24) -> Set("sleep"))
val schedule = a | b | c | d | e
```

Now you can do simple things like checking activities by time
```

```

## Internal representation

The internal representation is similar to IntervalSet, except that for each boundary there are two values stored: one for
the value exactly *at* the boundary, and one for *above* the boundary (as defined by the order of the key)

Redundant boundaries are never stored. To determine when a boundary is redundant, an Eq instance for the value type is
required for most operations. (I can imagine some cases where this is not possible (e.g. functions as values), but in 
that case you can always use a dummy Eq)
