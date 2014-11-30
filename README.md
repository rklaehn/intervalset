# intervalset

Efficient immutable interval sets

[![Build Status](https://travis-ci.org/rklaehn/intervalset.png)](https://travis-ci.org/rklaehn/intervalset)

This is a data structure for Long interval sets based on binary TRIEs. See [Fast Mergeable Integer Maps](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.37.5452) for details on the basic data structure. [scala.collection.immutable.LongMap](https://github.com/scala/scala/blob/d34388c1e8fad289a6198b127c6ae92c296d9246/src/library/scala/collection/immutable/LongMap.scala) was used as a starting point, but there are now significant differences. Most notably, the level (0..63) of a branch node is stored instead of the mask (64bit) to make room for some other summary information in branches.

Boundaries are either inclusive or exclusive, so ]0..2] is different to [1..2]. That might seem like a strange choice for long sets, but it is useful for my use case and does not have significant overhead.

Supported fundamental operations are union, intersection, negation. There is a zero element (the empty interval set) and a one element (the interval set containing all long integers). So it is possible to define a [boolean algebra](https://github.com/non/spire/blob/a0211697d993cade7c3618076ae997f84a6b5f3c/core/src/main/scala/spire/algebra/Bool.scala) for intervals. This is used in property-based testing using scalacheck.

## Time Complexity

### Set/Set operations

n = lhs.size + rhs.size

|Operation|Worst&nbsp;Case|Best&nbsp;Case|Remark|
|---|---|---|---|
|union|O(n&nbsp;log(n))|O(1)||
|intersection|O(n&nbsp;log(n))|O(1)||
|negate|O(1)|O(1)|Negation is always just a simple bit flip of the root node|
|xor|O(n&nbsp;log(n))|O(1)|xor is not a fundamental operation, but is implemented as a separate operation for efficiency|

## Set/Element operations

|Operation|Time&nbsp;Complexity|Remark|
|---|---|---|---|
|membership&nbsp;test|O(log(n))|   |
|insertion|O(log(n))|Insertion is not public. Complex sets are constructed by using union or intersection of simple sets|

## Memory usage

Leaves have just 8 bytes of user data, so they should take as much space as a boxed long. Branches have two pointers, one 8 byte long and less than 8 bytes of byte/boolean values, giving a total of 16 bytes of direct user data and two pointers. So in total, expect about 40 bytes per boundary on a 64bit JVM with CompressedOOPS enabled. YMMV.

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
a = [0..2[, [4..6[, [8..10[, [12..14[, [16..18[ ...
b = [1..3[, [5..7[, [9..11[, [13..15[, [17..19[ ...
```

For sets with such a structure, the operation has to traverse to the leafs of both trees, and new branch nodes have to be constructed.

```
Full traversal benchmark (n=100000)
Benchmark for a | b (140 calls in 2.713 s)
  Time:    2.275 ms   95% CI 2.153 ms - 2.398 ms   (n=19)
  Garbage: 23.44 us   (n=3 sweeps measured)
Benchmark for a & b (60 calls in 1.244 s)
  Time:    2.586 ms   95% CI 2.437 ms - 2.736 ms   (n=20)
  Garbage: 15.63 us   (n=1 sweeps measured)
Benchmark for a ^ b (140 calls in 2.058 s)
  Time:    3.492 ms   95% CI 3.344 ms - 3.640 ms   (n=20)
  Garbage: 71.88 us   (n=2 sweeps measured)
Benchmark for ~a (140 calls in 2.721 s)
  Time:    9.124 ns   95% CI 8.824 ns - 9.424 ns   (n=19)
  Garbage: 0.03576 ns   (n=5 sweeps measured)
```

### Cutoff

```
a = [0..2[, [4..6[, [8..10[, [12..14[, [16..18[ ...
b = [0..200[, [400..600[, [800..1000[, [1200..1400[, [1600..1800[ ...
```

For this case, a contiguous interval in b will overlap many intervals in a, so traversal does not have to go all the way into the leaves. This allows keeping entire subtrees of a in the case of union and intersection, or _negating and keeping entire subtrees_ of a in case of xor.


```
Cutoff benchmark (n=100000)
Benchmark for a | b (140 calls in 2.500 s)
  Time:    33.89 us   95% CI 32.79 us - 34.99 us   (n=19)
  Garbage: 219.7 ns   (n=2 sweeps measured)
Benchmark for a & b (140 calls in 2.329 s)
  Time:    32.64 us   95% CI 31.40 us - 33.88 us   (n=20)
  Garbage: 146.5 ns   (n=3 sweeps measured)
Benchmark for a ^ b (140 calls in 1.827 s)
  Time:    50.80 us   95% CI 48.61 us - 53.00 us   (n=20)
  Garbage: 195.3 ns   (n=4 sweeps measured)
```

### Analysis

As expected, negate is almost unmeasurably fast. It just creates a copy of the root node with a flag flipped. 
Xor is slower than and/or, which is to be expected since the resulting profile is more complex
There is a significant (~factor 100) benefit in the case where subtrees can be reused, even in the case of xor.
