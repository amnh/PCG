# Graph Search
---

### Related packages:
 * `pcg-graph`

### Related modules:

 - **`Data.Graph.Moves`**
 - **`PCG.Command.Build.Evaluate`**

### Design Ideas

Many of the graph moves can be best thought of as operating on a type of "graph of graphs":
```haskell
Graph f c e n (Graph f c e n t)
```

We then have an operation:
```haskell
join :: Graph f c e n (Graph f c e n t) -> Graph f c e n t
```
which re-interprets a "graph of graphs" as a graph. One useful aspect is that for re-scoring and similar operations one doens't need to use this operation.


 - Recombination: In particular for recombination we can take two graph solutions and simply exchange two leaf nodes and then re-cost to see if this solution 
   is superior.
 - SPR: During a Wagner build with clustering we build a graph for each cluster and then treat each cluster as a node and get back a "graph of graphs" as above.
We then perform a Wagner build treating each of these graphs as nodes. This same functionality can be coped to allow for SPR moves wherein we split off a subgraph
not containing a root and then treat this graph as a "leaf graph" and each of the previous leaf nodes as singleton graphs.
 - TBR: This is a combination of re-rooting on a subgraph and the above SPR move.
