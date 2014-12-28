# IntervalSeq

This is a data structure for sets of non-overlapping intervals of based on ordered sequences of boundaries. It requires just an Order instance for the element type. 

## Internal representation

An interval seq consists of a Boolean value *belowAll* giving the value at negative infinity, and a (potentially empty) strictly ordered sequene of boundaries. A boundary is a pair consisting of a value *x* of type T and a kind of type Byte. Kind can have one of four values: 

|kind|value at x|value after x|
|----|----------|-------------|
| K00|     false|        false|
| K01|     false|         true|
| K10|      true|        false|
| K11|      true|         true|

Redundant boundaries are never stored, so a sequence [(-1.0, K01), (0.0, K11), (1.0, K10)] would be illegal because the middle boundary is redundant.

*Note that for reasons of efficiency, kinds and values are stored in two separate arrays in the actual implementation.*

## Usage

```scala
scala> import IntervalSeq._
import IntervalSeq._

scala> (above(Rational(1,3)) | below(Rational(-4,7))) ^ point(Rational(17,5))
res0: com.rklaehn.interval.IntervalSeq[spire.math.Rational] = (-∞, -4/7);(1/3, 17/5);(17/5, ∞)

scala> ~res0
res1: com.rklaehn.interval.IntervalSeq[spire.math.Rational] = [-4/7, 1/3];[17/5]
```

## Time Complexity

### Set/Set operations

#### Number of comparisons

n = lhs.size + rhs.size

Best case performance will be reached e.g. if one sequence of boundaries is much smaller than the other, or if the sequences do not overlap.

|Operation|Worst&nbsp;Case|Best&nbsp;Case|Remark|
|---|---|---|---|
|union|O(n)|O(log(n))||
|intersection|O(n)|O(log(n))||
|negate|O(n)|O(n)|Negation consists of flipping the kind of all boundaries while leaving the values unchanged|
|xor|O(n)|O(log(n))|xor is not a fundamental operation, but is implemented as a separate operation for efficiency|

#### Element copying

Since the internal representation is a sequence, all boundaries always have to be copied to the result. So given two sequences of boundaries of size m and n, the number of boundary copy operations will always be m+n in the worst case for all operations.

However, copying elements using System.arraycopy is extremely fast, so you will only be affected by this for very large bounardy sequences or in case the comparison operation is extremely cheap. However, for primitive types with a very cheap comparison operation it is usually possible to use [IntervalTrie](IntervalTrie.md).


## Set/Element operations

Membership test is using a binary search, so it is O(log(N)) with a very small constant factor.

## Memory usage

The IntervalSeq data structure uses one byte for the kind of each boundary, one pointer (4 bytes in case of CompressedOOPS) for each value, and a constant overhead for the class itself and the kinds and values arrays. The value array is always an Array[AnyRef] internally, since this avoids having to provide a ClassTag for the element type. So if you use IntervalSeq with primitive types, they will be boxed. However, in this case it would be more efficient to use [IntervalTrie](IntervalTrie.md) in most cases. It is anticipated that IntervalSeq will usually be used with reference types such as Rational, where using an Array[AnyRef] does not have an overhead.

## Benchmarks

Benchmarks are done using [Thyme](https://github.com/Ichoran/thyme). Two cases are tested:
### Full traversal

In this test, the two sets have the following form:

```
a = [0, 2); [4, 6); [8, 10); [12, 14); [16, 18) ...
b = [1, 3); [5, 7); [9, 11); [13, 15); [17, 19) ...
```

For seqs with such a structure, the number of comparisons is almost as high as with a simple linear merge. 

```
Full traversal benchmark (n=100000)
Benchmark for a | b (300 calls in 3.960 s)
  Time:    6.680 ms   95% CI 6.376 ms - 6.984 ms   (n=20)
  Garbage: 9.375 us   (n=1 sweeps measured)
Benchmark for a & b (300 calls in 3.917 s)
  Time:    3.388 ms   95% CI 3.234 ms - 3.542 ms   (n=20)
  Garbage: 7.813 us   (n=2 sweeps measured)
Benchmark for a ^ b (300 calls in 3.960 s)
  Time:    6.510 ms   95% CI 6.259 ms - 6.762 ms   (n=20)
  Garbage: 12.50 us   (n=1 sweeps measured)
Benchmark for ~a (300 calls in 9.300 s)
  Time:    13.96 us   95% CI 13.48 us - 14.45 us   (n=19)
  Garbage: 418.1 ns   (n=61 sweeps measured)
```

### Cutoff

```
a = [0, 2); [4, 6); [8, 10); [12, 14); [16, 18) ...
b = [0, 200); [400, 600); [800, 1000); [1200, 1400); [1600, 1800) ...
```

For this case, a contiguous interval in b will overlap many intervals in a, so the number of comparisons is much lower than for a simple linear merge.

```
Cutoff benchmark (n=100000)
Benchmark for a | b (140 calls in 2.538 s)
  Time:    279.3 us   95% CI 266.6 us - 292.0 us   (n=20)
  Garbage: 5.664 us   (n=10 sweeps measured)
Benchmark for a & b (1260 calls in 30.28 s)
  Time:    176.2 us   95% CI 172.5 us - 179.9 us   (n=19)
  Garbage: 5.957 us   (n=153 sweeps measured)
Benchmark for a ^ b (140 calls in 2.689 s)
  Time:    299.2 us   95% CI 284.5 us - 313.9 us   (n=20)
  Garbage: 6.055 us   (n=10 sweeps measured)
```

### Analysis

Negate is slower than for [IntervalTrie](IntervalTrie.md), since the kind array has to be flipped. It is still pretty fast though. 

Even though the complexity of the output profiles is similar in the full traversal and cutoff case, the cutoff case is significantly faster (~factor 10) because the number of comparisons is lower and large parts of the source seqs can be copied/flipped to the target seq using System.arraycopy. Note that is for pretty large arrays and a very cheap comparison operation. For a type with a more expensive comparison operation (e.g. Rational, SafeLong) the difference would be even larger.

Obviously, the benchmark data is specific to my computer. To run the benchmark yourself, use
```
sbt test:run
```
and choose IntervalSeqBenchmark.
