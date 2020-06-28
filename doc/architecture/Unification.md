# Unification
---

### Related packages:
 * `data-unification`

### Related modules:

 - **`Data.Unification`**
 -  *`Data.Unification.Error`*
 -  *`Data.Unification.InputData`*

PCG marshals all normalized input data through a unification procedure which ensured that the input data holistically is consistent. If the data is consistent, the unification process will then construct an initial graph state for PCG. Character observations, metadata specifications, and topological structures are the three types of input data which must be unified.

There are a few types of unification errors which can occur:

  - There are duplicate taxa listed within a graph. Taxa must be unique node labels of a graph, graphs with duplicates cannot be operated on.
  - The graph can have more taxa labels than are present in the data set. Each taxa in the graph must have some character observations present in the data set. Graphs with "extra" taxa, taxa that have no observations, cannot be operated on.
  - The graph has fewer taxa than are present in the data set. Each taxa in the data set must have a labeled location in the graph. Data set with unused taxa cannot be operated on.
  - There were no character observations and no graph information. Vacuous input cannot be operated on.
 
Once the collection input data is deemed consistent, a initial graph state is produced. If no character observations were supplied, but a graph structure was defined, then a topological initial state is produced without any character data. If no graph structure was supplied, but character observations were defined, then a "trivial" graph is produced, decorated with the character observation. The trivial graph, consists of one root and one leaf for each taxon, creating a disjoint forest of taxa. If both graph and character observations were supplied, the character observations and the taxa labels of the graphs are matched together resulting in an initial graph decorated with character sequences.

When decorating the initial graph, full sequences of characters must be produced for each taxon in the graph. Input files may have specified only partial observations of characters across the files. A stateful fold and repeated map merging is used to build up a complete character sequence for each taxon, of either character observations or missing data, for all characters across all files. Characters are converted from the normalized, verbose form to the encoded, compact form during the graph decoration. Additionally, a metadata sequence is created which is isomorphic to each of the character sequences in the graph.