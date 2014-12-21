# intervalset

Efficient immutable interval sets

[![Build Status](https://travis-ci.org/rklaehn/intervalset.png)](https://travis-ci.org/rklaehn/intervalset)

This is a data structure for Long interval sets based on binary TRIEs. See [Fast Mergeable Integer Maps](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.37.5452) for details on the basic data structure. [scala.collection.immutable.LongMap](https://github.com/scala/scala/blob/d34388c1e8fad289a6198b127c6ae92c296d9246/src/library/scala/collection/immutable/LongMap.scala) was used as a starting point, but there are now significant differences. Most notably, the level (0..63) of a branch node is stored instead of the mask (64bit) to make room for some other summary information in branches.

Boundaries are either inclusive or exclusive, so (0..2] is different to [1..2]. That might seem like a strange choice for long sets, but it is useful for my use case and does not have significant overhead.

Supported fundamental operations are union, intersection, negation. There is a zero element (the empty interval set) and a one element (the interval set containing all long integers). So it is possible to define a [boolean algebra](https://github.com/non/spire/blob/a0211697d993cade7c3618076ae997f84a6b5f3c/core/src/main/scala/spire/algebra/Bool.scala) for intervals. This is used in property-based testing using scalacheck.

There are typeclass instances to make the data structure seamlessly usable with all primitive types except for char and boolean, as well as the unsigned value classes UByte, UShort, UInt and ULong from Spire. You can easily provide your own instance for any key type that can be converted to a signed long while preserving order.

## Usage

```scala
scala> import com.rklaehn.interval.IntervalSet._
import IntervalSet._

scala> above(math.E) & below(math.Pi)
res1: com.rklaehn.interval.IntervalSet[Double] = (2.718281828459045, 3.141592653589793)

scala> val x = above(math.E) & below(math.Pi)
x: com.rklaehn.interval.IntervalSet[Double] = (2.718281828459045, 3.141592653589793)

scala> ~x
res2: com.rklaehn.interval.IntervalSet[Double] = (-∞, 2.718281828459045];[3.141592653589793, ∞)

scala> point(1L) ^ hole(2L)
res3: com.rklaehn.interval.IntervalSet[Long] = (-∞, 1);(1, 2);(2, ∞)
```

## Time Complexity

### Set/Set operations

n = lhs.size + rhs.size

|Operation|Worst&nbsp;Case|Best&nbsp;Case|Remark|
|---|---|---|---|
|union|O(n)|O(1)||
|intersection|O(n)|O(1)||
|negate|O(1)|O(1)|Negation is always just a simple bit flip of the root node|
|xor|O(n)|O(1)|xor is not a fundamental operation, but is implemented as a separate operation for efficiency|

## Set/Element operations

|Operation|Time&nbsp;Complexity|Remark|
|---|---|---|---|
|membership&nbsp;test|O(log(n))|   |
|insertion|O(log(n))|Insertion is not public. Complex sets are constructed by using union or intersection of simple sets|

## Memory usage

Leaves have just 10 bytes of user data, so they should take as much space as a boxed long. I got 24 bytes when measuring with jamm. Branches have two pointers, one 8 byte long and 2 bytes of byte/boolean values, giving a total of 10 bytes of direct user data and two pointers. I got 32 bytes with jamm. So in total, expect about 56 bytes per boundary on a 64bit JVM with CompressedOOPS enabled. YMMV.

## Structural sharing

All operations will use as much structural sharing as possible. E.g. the union of two non-intersecing sets will usually reuse the entire sets, same for intersection where one of the operands is true for the relevant interval.

```scala
a union zero eq a
a intersection one eq a
```
    
## Benchmarks

Benchmarks are done using [Thyme](https://github.com/Ichoran/thyme). Two cases are tested:

### Full traversal

In this test, the two sets have the following form:

```
a = [0, 2); [4, 6); [8, 10); [12, 14); [16, 18) ...
b = [1, 3); [5, 7); [9, 11); [13, 15); [17, 19) ...
```

For sets with such a structure, the operation has to traverse to the leafs of both trees, and new branch nodes have to be constructed.

```
Full traversal benchmark (n=100000)
Benchmark for a | b (300 calls in 4.259 s)
  Time:    3.375 ms   95% CI 3.265 ms - 3.485 ms   (n=20)
  Garbage: 26.56 us   (n=5 sweeps measured)
Benchmark for a & b (140 calls in 1.834 s)
  Time:    3.241 ms   95% CI 3.098 ms - 3.383 ms   (n=20)
  Garbage: 34.37 us   (n=3 sweeps measured)
Benchmark for a ^ b (60 calls in 1.772 s)
  Time:    3.577 ms   95% CI 3.435 ms - 3.720 ms   (n=20)
  Garbage: 84.38 us   (n=4 sweeps measured)
Benchmark for ~a (140 calls in 2.119 s)
  Time:    7.148 ns   95% CI 6.823 ns - 7.473 ns   (n=20)
  Garbage: 0.09537 ns   (n=10 sweeps measured)
```

### Cutoff

```
a = [0, 2); [4, 6); [8, 10); [12, 14); [16, 18) ...
b = [0, 200); [400, 600); [800, 1000); [1200, 1400); [1600, 1800) ...
```

For this case, a contiguous interval in b will overlap many intervals in a, so traversal does not have to go all the way into the leaves. **This allows keeping entire subtrees of a in the case of all fundamental operations (and, or, xor)**.

```
Cutoff benchmark (n=100000)
Benchmark for a | b (140 calls in 2.061 s)
  Time:    28.56 us   95% CI 27.39 us - 29.73 us   (n=20)
  Garbage: 97.66 ns   (n=3 sweeps measured)
Benchmark for a & b (140 calls in 2.345 s)
  Time:    32.55 us   95% CI 31.11 us - 34.00 us   (n=20)
  Garbage: 146.5 ns   (n=5 sweeps measured)
Benchmark for a ^ b (300 calls in 6.302 s)
  Time:    38.94 us   95% CI 37.38 us - 40.51 us   (n=20)
  Garbage: 366.2 ns   (n=24 sweeps measured)
```

### Analysis

As expected, negate is almost unmeasurably fast. It just creates a copy of the root node with a flag flipped. 
Xor is slightly slower than and/or, which is to be expected since the resulting profile is more complex
There is a significant (~factor 100) benefit in the case where subtrees can be reused, even in the case of xor.

Obviously, the benchmark data is specific to my computer. To run the benchmark yourself, use
```
sbt test:run
```
and choose the benchmark.
