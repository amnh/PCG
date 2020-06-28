# Transition Cost Matrix
---

### Related packages:
 * `tcm`

### Related modules:

 - **`Data.MetricRepresentation`**
 - **`Data.TCM`**
 - **`Data.TCM.Dense`**
 -  *`Data.TCM.Dense.FFI`*
 -  *`Data.TCM.Internal`*
 - **`Data.TCM.Memoized`**
 -  *`Data.TCM.Memoized.FFI`*

A transition cost matrix (TCM) is a lookup table related to a character's alphabet of symbols. The TCM provides a transition cost and a median state from two ambiguity groups of symbols from the character alphabet.

The `TCM` data-type constitutes a temporary, slightly inefficient representation of a TCM, used between file parsing and unification of the code. One can call `diagnoseTCM` on a `TCM` value to learn the most efficient way to represent a the TCM in the character metadata. This diagnosis will report the TCMs underlying mathematical structure, along with factoring out any possible real-valued weight. There are a handful of "efficient" represnetations we will now explore.

 1. **Generating functions:** If the TCM has a special structure, such as the discrete metric ("non-additive" character) or th L1 norm ("additive" character) as the specified metric, then there is no reason to represent the TCM in memory with an array-backed data  structure. Instead, we can use a generating function to compute the transition cost and resulting median state. This always has the best performance.

 2. **Dense matrix:** If the TCM does not have a special structure, but the TCM's dimensions are small (ie. less than or equal to 8), then we will pre-compute the entirety of the TCM's transition costs and median states. This is backed by unboxed arrays, allowing constant time lookups of costs and medians along with being transferable across the FFI in constant time.
 
 3. **Sparse matrix:** If the TCM does not have a special structure and the TCM's dimensions are not small (ie. greater than 8), then a sparse TCM lookup structure is used. This representation is backed by a memoized hashtable. Each transition cost and median state is computed only once upon the first time it is requested from the sparse TCM and then stored for subsequent lookups. Unrequested costs and medians are not computed. For even moderately large alphabets (ie greater than 32), the memory required to store the whole TCM vastly exceeds the memory capacity of even theoretical machines.

The `Data.MetricRepresentation` module controls the represnetation and dispatch while also defining a consistent interface for handling the three efficient representation cases above.

The `Data.TCM.Dense` module exports the `DenseTransitionCostMatrix` data-type. This is represented by a fully evaluated, unboxed array. It has bindings to the C FFI which are used for string alignment. It allows for constant time lookup of pairwise and threeway transition costs and medians.

The `Data.TCM.Memoized` module exports the `MemoizedCostMatrix` data-type. This is represented by a lazily evaluated, memoizing hashtable. It has bindings to the C++ FFI. It allows for amortized time, constant lookup of pairwise and threeway transition costs and medians.
