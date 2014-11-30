# intervalset

Efficient immutable interval sets

This is a data structure for Long interval sets based on binary TRIEs. See [Fast Mergeable Integer Maps](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.37.5452) for details on the basic data structure. [scala.collection.immutable.LongMap](https://github.com/scala/scala/blob/d34388c1e8fad289a6198b127c6ae92c296d9246/src/library/scala/collection/immutable/LongMap.scala) was used as a starting point, but there are now significant differences. For example, the level (0..63) of a branch node is stored instead of the mask (64bit) to make room for some other summary information in branches.

Boundaries are either inclusive or exclusive, so ]0..2] is different to [1..2]. 

Supported fundamental operations are union, intersection, negation. There is a zero element (the empty interval set) and a one element (the interval set containing all long integers). So it is possible to define a [boolean algebra](https://github.com/non/spire/blob/a0211697d993cade7c3618076ae997f84a6b5f3c/core/src/main/scala/spire/algebra/Bool.scala) for intervals. This is used in property-based testing using scalacheck.

## Time Complexity

### Set/Set operations

n = lhs.size + rhs.size

|Operation|Worst Case|Best Case|Remark|
|---|---|---|---|
|union|O(n log(n))|O(1)||
|intersection|O(n log(n))|O(1)||
|negate|O(1)|O(1)|Negation is always just a simple bit flip of the root node|

## Set/Element operations

|Operation|Time Complexity|Remark|
|---|---|---|---|
|lookup|O(log(n))|   |

## Memory usage

Leaves have just 8 bytes of user data, so they should take as much space as a boxed long. Branches have two pointers, one 8 byte long and less than 8 bytes of byte/boolean values, giving a total of 16 bytes of direct user data and two pointers. So in total, expect about 40 bytes per boundary on a 64bit JVM with CompressedOOPS enabled. YMMV.

## Structural sharing

All operations will use as much structural sharing as possible. E.g. the union of two non-intersecing sets will usually reuse the entire sets, same for intersection where one of the operands is true for the relevant interval.

    a union zero eq a
    a intersection one eq a
