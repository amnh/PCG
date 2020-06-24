
# Graph Scoring
---

### Related packages:
 * `analysis`

### Related modules:

 - **`Analysis.Parsimony.Additive`**
 -  *`Analysis.Parsimony.Additive.Internal`*
 - **`Analysis.Parsimony.Fitch`**
 -  *`Analysis.Parsimony.Fitch.Internal`*
 - **`Analysis.Parsimony.Dynamic.DirectOptimization`**
 -  *`Analysis.Parsimony.Dynamic.DirectOptimization.Internal`*
 -  *`Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise`*
 -  *`Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Internal`*
 -  *`Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Ukkonen.Internal`*
 -  *`Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.FFI`*
 -  *`Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.NeedlemanWunsch`*
 -  *`Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Ukkonen`*
 -  *`Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Ukkonen.Ribbon`*
 -  *`Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.UnboxedFullMatrix`*
 -  *`Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.UnboxedSwapping`*
 -  *`Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.UnboxedUkkonenFullSpace`*
 -  *`Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.UnboxedUkkonenSwapping`*
 - **`Analysis.Parsimony.Sankoff`**
 -  *`Analysis.Parsimony.Sankoff.Internal`*

This sub-system determines how to score a character in the graph. The scoring for each character is broken up into two parts. The first is the post-order traversal logic, where character decoration information from the children is consumed to produce a new decoration for the parent. Second there is the pre-order traversal logic, where the parent character decoration information is consumed to update the child decorations. Here when we use the phrase "traversal logic," we refer to the logic applied between nodes in the graph in order to update the information on the nodes, not the process of how to move about the graph performing the updates. For more information on the graph traversals, see the [Graph Structure sub-system architecture documentation](https://github.com/amnh/PCG/blob/master/doc/architecture/Graph-Structure.md).

Characters regarded as "additive," that is having the L1 norm as their specified metric, can be scored in a specialized manner using a series of algorithms. The specialized post-order algorithm is [described by Farris](https://www.jstor.org/stable/pdf/2412028.pdf) [1]. The specialized pre-order algorithm is  [described by Goloboff](https://onlinelibrary.wiley.com/doi/pdf/10.1111/j.1096-0031.1993.tb00236.x) [2]. The algorithms form a specialization of the general Sankoff algorithm which is more efficient for the L1 norm metric. The `Analysis.Parsimony.Additive` module exports functions for performing the post-order and pre-order logic of the Farris-Goloboff algorithm.

Characters regarded as "fitch," that is having the discrete metric as their specified metric, can be scored in a specialized using an algorithm [described by Fitch](https://www.jstor.org/stable/2412116?seq=1#metadata_info_tab_contents) [3]. This algorithm is a specialization of the general Sankoff algorithm that is more efficient for the discrete metric. The `Analysis.Parsimony.Fitch` module exports functions for performing the post-order and pre-order logic of the Fitch algorithm.

Characters regarded as "static," that is not a dynamic character and also not regarded as "additive" or "fitch" characters,  can be scored using a general algorithm [described by Sankoff](http://albuquerque.bioinformatics.uottawa.ca/Papers/JournalPublication/1975_Sankoff_Rousseau.pdf
) [4], which works with any specified metric. The `Analysis.Parsimony.Sankoff` module exports functions for performing the post-order and pre-order logic of the Sankoff algorithm.

Characters regarded as "dynamic," if their correspondences are not known *a priori* and hence are topologically dependant. The `Analysis.Parsimony.Dynamic.DirectOptimization` and `Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise` modules present a group of algorithms implementing the character optimization of unaligned sequences. 

The post-order logic for dynamic characters is, broadly, to perform a "string" alignment of the children, and assign the alignment to the parent. This process was first described by [Wheeler in 1996](https://onlinelibrary.wiley.com/doi/pdf/10.1111/j.1096-0031.1996.tb00189.x
) [5] and was elaborated on in much greater detail in the book [Dynamic Homology and Systematics](https://wardwheeler.files.wordpress.com/2016/12/wheeler_etal2006b.pdf) [6]. It is best to read through the works before modifying the string alignment subsystem.

The pairwise string alignment required for the post-order logic of dynamic character scoring has two main requirements:
 - *commutativity*, the string alignment should be commutative. This ensures "label invariance" of the scoring results over isomorphic trees.
 - *efficiency*, the string alignment is often the performance bottleneck of phylogentic search involving dynamic characters.

To address the requirements, a complicated but necessary collection of steps is taken to ensure that the most efficient string alignment is performed.

Gaps in an input string do not effect the resulting alignment, but they increase the dimensionality of the alignment matrix, and hence are removed before building the alignment matrix and added back in after processing the alignment matrix.

Input strings are measured and conditionally swapped to ensure that the shortest string is always on the left side of the alignment matrix. If strings were swapped before the alignemnt matrix, the alignment result is swapped back after the alignment matrix has been processed. This ensures commutativity of the string alignment operation.

Various alignment matrices can be constructed and each have their performance benefits. The C FFI binding offers unparalleled speed, but only works with alphabets of size 8 or less. The Ukkonen banded matrix promises substantially less computational work by constructing only part of the alignment matrix, but it cannot be used if the string lengths differ greatly. The full matrix is the simplest and requires no additional bookkeeping or marshaling, but it is also the most computationally expensive. All alignment matrices can be configured to use gap open costs for an "affine" alignment, as [described by Varón](https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-13-293) [7].



*The following is a flow chart outlining the steps required for efficient and correct commutative string alignment:*
```
+--------------------------------------+
| Remove gaps from input string        |
+-----------------+--------------------+
                  |
                  |
                  v
+-----------------+--------------------+
| Measure ungapped string              |
| Conditionally swap for commutativity |
+-----------------+--------------------+
                  |
                  |
                  v
+-----------------+--------------------+  Both not missing  +------------------------+
| Handle missing (empty) strings       +------------------->+ Check size of alphabet |
+-----------------+--------------------+                    +----+--------------+----+
                  |                                              |              |
                  | Either string missing                 small  |              |  not small
                  v                                              v              v
+-----------------+-------------------+                +---------+-----+   +----+---------------------------+
| Other string is the alignment       |                | C FFI Binding |   | Check string length difference |
+-----------------+-------------------+                +---------+-----+   +----+----------------------+----+
                  |                                              |              |                      |
                  |                                              |       small  |                      |  not small
                  |                                              |              v                      v
                  |                                              |         +----+------------------+ +-+-----------+
                  |                                              |         | Ukkonen banded matrix | | Full matrix |
                  |                                              |         +----+------------------+ +-+-----------+
                  v                                              |              |                      |
+-----------------+-------------------+                          |              |                      |
| Reinsert the removed gaps           +<-------------------------+--------------+----------------------+
+-----------------+-------------------+
                  |
                  |
                  v
+-----------------+-------------------+
| Swap string back if they were       |
+ conditionally swapped before        +
+-------------------------------------+

```

The pre-order logic of a dynamic character consists of taking the preliminary alignments from the post-order and generating an implied alignment as [described by Wheeler](https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1096-0031.2003.tb00369.x) [8] and later elaborated on by Washburn [9]. An implied alignment requires taking the global scope of all strings at the root node and "threading down" the information towards the leaves, adding in gaps where appropriate, resulting in a multiple sequence alignment. Final median states for each node in the graph can be derived by removing gaps from the implied alignment annotation on each node.

Dynamic character scores are not exact. To improve the heuristic bounds, all possible "rootings" are considered for a character. Consequently, a dynamic character has traversal foci, which represents a list of all edges on where the post-order and pre-order traversals can originate, which result in a minimal cost for the dynamic character.

### Citations:
---

 1. Farris, J. S. 1970. A method for computing Wagner trees. Syst. Zool. 19:83–92. 
 2. Goloboff, P. A. 1993a. Character optimization and calculation of tree lengths. Cladistics 9:433–436.
 3. Fitch, W. M. 1971. Toward defining the course of evolution: minimum change for a specific tree topology. Syst. Zool. 20:406–416.
 4. Sankoff, D. M. and Rousseau, P. 1975. Locating the vertices of a Steiner tree in arbitrary space. Math. Program. 9:240–246.
 5. Wheeler, W. C. 1996. Optimization alignment: The end of multiple sequence alignment in phylogenetics? Cladistics 12:1–9.
 6. Wheeler, W. C., Aagesen, L., Arango, C. P., Faivovich, J., Grant, T., D’Haese, C., Janies, D., Smith, W. L., Varo ́n, A., and Giribet, G. 2006a. Dynamic Ho- mology and Systematics: A Unified Approach. American Museum of Natural History, New York.
 7. Varón, A, and W. C. Wheeler.  2012. The Tree Alignment problem.  BMC Bioinformatics, 13:293.
 8. Wheeler, W. C. 2003.  Implied Alignment:  A Synapomorphy-Based Multiple Sequence Alignment Method. Cladistics, 19:261-268.
 9. Washburn, A. J. and W. C. Wheeler. 2020. Efficient Implied Alignment. BMC Bioinformatics, (in press)
