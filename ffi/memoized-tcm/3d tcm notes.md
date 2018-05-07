# Edits/additions to memoized cost matrix to add 3d lookup/storage

3D lookups involve three 2D lookups, then standard Sankoff computations:

• make a `struct` that contains both 2d and 3d
• 2d stores same information as before: cost & median
• 3d stores same for 3 elements
• for each 3d lookup, do Sankoff:

  1. Receive three ambiguous inputs, `A`, `B`, `C`
  1. For each input, i<sub>x</sub>:

     1. for each element, `e`, in alphabet,

        1. two accumulators, `curMinCost`, `curMedian`
        1. cost = lookup_2d<i<sub>x</sub>,e> (note that this is ordered)
        1.


Necessary changes to 2d matrix:

• Add a matrix for 3d lookups
• Make a 3d lookup key
• Ordering of keys in 3d doesn't matter
• Make a 3d equals operator



```
export LD_LIBRARY_PATH=/usr/local/c++-ffi-example/lib:$LD_LIBRARY_PATH
./dist/build/tst-prog/tst-prog
```
