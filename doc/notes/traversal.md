# Graph Traversal

We will use the terminology from terminology.md.

### Postorder

##### Resolution:

__input__:
  network
    { leaves: character sequence
    , internal nodes: empty
    }

Initially we begin with a (not-necessarily connected) rooted phylogenetic network with character sequences
assigned to each leaf node. The postorder pass traverses from leaves to the root nodes. As we traverse
we assign to each node a resolution cache corresponding to a choice of optimized character sequences
for each possible choice of display tree (oriented with respect to the given roots). Suppose we have the
following nodes:

\begin{ascii}

                            P ●
                              │
                              │
                              │
                        ┌────┘─└────┐
                     C1 ●           ● C2

\end{ascii}

During a postorder traversal we must decide which of the resolutions to take from C1 and C2 and in doing so
we use the following logic:
  - [Paired resolutions]: 
        If both C1 and C2 are not network nodes then we take all resolutions
    from C1 and all resolutions from C2 but we exclude those resolutions where C1 and C2 share
    descendant leaf nodes. In order for two nodes to share a descendent leaf node they must have
    a common descendent network node which would correspond to two incompatible display trees.
    We then optimize those resolutions specialized for the given character type and gather the
    relevant subtree resolution metadata.
  - [Left network]:
        If C1 is a network node then we include all paired resolutions as well as those resolutions
    taken only from C2 (with resolution metadata updated appropriately). This corresponds to the
    display tree where we do not include the (P,C1) edge.
    the resolutions 
  - [Right network]
        If C2 is a network node then we include all paired resolutions as well as those resolutions
    taken only from C1 (with resolution metadata updated appropriately). This corresponds to the
    display tree where we do not include the (P,C2) edge.
  - [Both network]
        If both C1 and C2 are network nodes then we include the paired resolutions, the resolutions
    from C1 and the resolutions from C2 (with metadata updated appropriately). This corresponds to
    either including the (P, C1) edge, the (P, C2) edge or both edges (capturing the case where each of
    the complementary network edges are not included in the display tree).
    
This logic is captured in the function `applySoftwireResolutions` in the module 
`Data.Graph.Bio.Postorder.Resolution`. The pairing logic is encoded in the `liftF2` method from the
`Apply` instance for `ResolutionCacheM`.

__output__:
  network
    { leaves: character sequence
    , internal nodes: resolution cache
    }

### Rerooting

After we have performed this first pass across the graph. We have optimized sequences for our given choice
of rooting. For non-exact characters we now need to decide on which edge we root for that particular choice of
character.


### Network costing


### Preorder
