# Graph Structure
---

### Related packages:
 * `data-structures`
 * `graph`

### Related modules:

 - **`Bio.Graph.PhylogeneticDAG`**
 -  *`Bio.Graph.PhylogeneticDAG.Internal`*
 - **`Bio.Graph.ReferenceDAG`**
 -  *`Bio.Graph.ReferenceDAG.Internal`*
 
 
---
Old Graph Design
---

### Graph

A phylogenetic directed acyclic graph (DAG) consists of a vector-backed graph structure `ReferenceDAG` of nodes and associated graph metadata sequence. A phylogenetic DAG represents a graph. The graph need not be connected, but each component must be a DAG. All nodes must have at *most* degree 3, either in-degree 2 and out-degree 1 or in-degree 1 and out-degree 2. The graph represents a phylogenetic network, which places additional "temporal" constraints on "network edges" present in the graph.

The type `PhylogeneticDAG` type has three type parameters, `d`, `e`, and `n`.  
- The `d` parameter stores the global metadata of the graph.
- The `e` parameter stores the edge data of the graph.
- The `n` parameter stores the node data of the graph.


### Nodes

Node data is defined by a type `IndexData`. This has type parameters `d` and `e` with the following fields:
- **`nodeDecoration`**  has type `n` and stores character data at that node.
- **`parentRefs`** has type `IntSet` and stores `Int`s which point at the indices of the parents in 
- the `references` vector.
- **`childRefs`** has type `IntMap e` which stores ints which point at the indices of the child nodes in the `references` vector along with edge data `e` asociated with each child node.

In a `PhylogeneticDAG` these types are speicalised to:
- `d = (PostorderContextualData (CharacterSequence u v w x y z))`
- `n = (PhylogeneticNode (CharacterSequence u v w x y z) n)`

A `PhylogeneticNode s n` consists of two fields:
- **`resolutions`** of type `(ResolutionCache s)` which store all of the different possible post-order-resolutions
at that particular node.
- **`nodeDecorationDatum2`** of type `n` is used to store the the node label. This is either that found within a input file
or is generated internally. Internal names are not output to the user.

A `PostorderContextualData (CharacterSequence u v w x y z))` stores data related to dynamic character re-rooting. 
It consists of the following fields:

- **`virtualNodeMapping`** has type `HashMap EdgeReference (ResolutionCache t)`. This is a mapping from each edge to the collection of best resolutions if the graph were rooted along this particular edge.
- **`contextualNodeDatum`** has type `Vector (HashMap EdgeReference (ResolutionCache t))`. This gives, for each node in the `references` vector, a map for the surrounding nodes which looks as follows:
```ascii
            (i)
             |
            (n)
           /   \
         (j)   (k)
```
For the edge `(i, n)` the mapping gives the valid resolutions if the graph were rooted along `(i,n)` and similarly for `(j,n)` and `(k,n)`. Note that this can lead to a re-orientation of the graph in the case of `(j,n)` and `(k,n)` in the sense that the previous child nodes are now considered parent nodes.
- **`minimalNetworkContext`** has type `Maybe (NonEmpty (TraversalTopology, Double, Double, Double, Vector (NonEmpty TraversalFocusEdge)))`. The `Non-Empty` type corresponds to each block and the elements of the tuple correspond to:
  - **`TraversalTopology`**: The chosen optimal topology for this block.
  - **`Double`**: rooting cost - the cost from placing roots.
  - **`Double`**: network cost - the network cost.
  - **`Double`**: character cost - the total character cost.
  - **`Vector (NonEmpty TraversalFocusEdge)`** For each dynamic character in this block, all the edges from which a minimal cost for the dynamic character can be derived.

### Resolution Cache

A `ResolutionCache` is a type synonym for a `NonEmpty (ResolutionInformation s)`. The type `ResolutionInformation` is defined as follows:

```haskell
data ResolutionInformation s
    = ResInfo
    { resolutionMetadata :: ResolutionMetadata
    , characterSequence  :: !s
    }
```

Here `ResolutionMetadata` is defined as:
```haskell
data ResolutionMetadata
    = ResolutionMetadata
    { totalSubtreeCost       :: {-# UNPACK #-} !Double
    , localSequenceCost      :: {-# UNPACK #-} !Double
    , leafSetRepresentation  :: {-# UNPACK #-} !UnionSet
    , subtreeRepresentation  ::                !NewickSerialization
    , subtreeEdgeSet         ::                !(EdgeSet (Int, Int))
    , topologyRepresentation :: {-# UNPACK #-} !(TopologyRepresentation (Int, Int))
    }
```

A piece of `ResolutionInformation` data represents a choice of optimal character sequence at a given node along with cached metadata. This metadata consists of which network edges this resolution uses, the cost of the given character sequence and the descendant leafset coverage of this choice of topology. This cached information is important when we ensure the following:
- Resolutions must not overlap during the postorder. This means that the leafset coverage of two child resolutions must be disjoint. Another way of saying this is that from the current node there is a unique path to any leaf node in its subtree.
- At the root node(s) valid resolutions must cover all leaf nodes. In other words from the collection of root nodes there must be a unique path to each leaf node.


### Graph Metadata

The graph Metadata consists of the following type:
```haskell
data  GraphData d
    = GraphData
    { dagCost         :: {-# UNPACK #-} !ExtendedReal
    , networkEdgeCost :: {-# UNPACK #-} !ExtendedReal
    , rootingCost     :: {-# UNPACK #-} !Double
    , totalBlockCost  :: {-# UNPACK #-} !Double
    , graphMetadata   :: d
    }
```

Here the `graphMetadata` in a `PhylogeneticDAG` consists of the `PostorderContextualData (CharacterSequence u v w x y z))` field described above.


### Sequences

A sequence represents a structured collection of characters or metadata. A sequence consists of one or more blocks, each block relates to one or more characters. The collection of characters is partitioned into block for reasons related to optimization constraints within a phylogenetic network. In short, characters within a block must share the same soft-wired resolution of the phylogenetic network, but each block in the sequence can choose a potentially different soft-wire resolution for their block of characters.

Sequences take two forms, *character sequences* and *metadata sequences*. All sequences in a run of PCG will be of the same length, since the number of characters is defined at the start of the program and remains the same throughout the course of PCG's computation. Individual characters will, in the future, be able to be "activated" or "deactivated," which remove them from scoring but does not remove them from the character sequence.

The blocks of a sequence contain "bins" of characters of the same "type." Characters with the same metric will be binned together within the same block, and all dynamic characters will be placed in the same bin within a block. This is done for convenience and efficiency of operating on similar characters. When scoring a graph, scoring functions are mapped over sequences, over their blocks, and applied to the appropriate character bins.




```
-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

888b    888                             .d8888b.                           888     
8888b   888                            d88P  Y88b                          888     
88888b  888                            888    888                          888     
888Y88b 888  .d88b.  888  888  888     888        888d888 8888b.  88888b.  88888b. 
888 Y88b888 d8P  Y8b 888  888  888     888  88888 888P"      "88b 888 "88b 888 "88b
888  Y88888 88888888 888  888  888     888    888 888    .d888888 888  888 888  888
888   Y8888 Y8b.     Y88b 888 d88P     Y88b  d88P 888    888  888 888 d88P 888  888
888    Y888  "Y8888   "Y8888888P"       "Y8888P88 888    "Y888888 88888P"  888  888
                                                                   888             
                                                                   888             
                                                                   888             
8888888b.                    d8b                                                   
888  "Y88b                   Y8P                                                   
888    888                                                                         
888    888  .d88b.  .d8888b  888  .d88b.  88888b.                                  
888    888 d8P  Y8b 88K      888 d88P"88b 888 "88b                                 
888    888 88888888 "Y8888b. 888 888  888 888  888                                 
888  .d88P Y8b.          X88 888 Y88b 888 888  888                                 
8888888P"   "Y8888   88888P' 888  "Y88888 888  888                                 
                                      888                                          
                                 Y8b d88P                                          
                                  "Y88P"

-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
```
  
  
   
### New Graph Design


### Branch:
 * `new-graph-representation`

### Related packages:
 * `pcg-graph`

### Related modules:

 - **`Data.Graph.Type`**
 - **`Data.Graph.Postorder.Resolution`**
 - **`Data.Graph.Sequence.Class `**`

### Graph 

The new graph has the following type:
```haskell
data  Graph
        (f :: Type -> Type)
        (c :: Type)
        (e :: Type)
        (n :: Type)
        (t :: Type)
   = Graph
   { leafReferences    :: {-# UNPACK #-} !(Vector (LeafIndexData       t   ))
   , treeReferences    :: {-# UNPACK #-} !(Vector (TreeIndexData    (f n) e))
   , networkReferences :: {-# UNPACK #-} !(Vector (NetworkIndexData (f n) e))
   , rootReferences    :: {-# UNPACK #-} !(Vector (RootIndexData    (f n) e))
   , cachedData        :: c
   }
```

This has the following important changes from the previous type:
 - The reference vectors for different types of nodes are now stored separately.
 - Internal nodes i.e. tree, network and root nodes now come with a "pattern" argument `f :: Type -> Type`. Different instantiations of `f` correspond to different phases of graph decoration:
    * Parse phase: Here `f` is intended to be instantiated with `Const ()` as after passing the graph internal nodes do not yet contain any information.
    * Postorder phase: Here `f` corresponds to a `ResolutionCache`, in particular each node contains a non-empty collection of resolution information.
    * Preorder phase: Here `f` corresponds to `Identity` as we make the final assignment of character sequence.

### Nodes

The core node type is as follows:

```haskell
data IndexData nodeContext nodeData  = IndexData
  { nodeData    :: nodeData
  , nodeContext :: !nodeContext
  }
```

This is then specialized for each given `nodeContext`:

```haskell
newtype RootContext e = RootContext
  { childInfoR :: Either (ChildInfo e) (ChildInfo e :!: ChildInfo e)
  }

type RootIndexData d e = IndexData (RootContext e) d

newtype LeafContext = LeafContext
  { parentIndsL :: TaggedIndex
  }

type LeafIndexData d = IndexData LeafContext d


data NetworkContext e = NetworkContext
  { parentIndsN :: {-# UNPACK #-} !(TaggedIndex :!: TaggedIndex)
  , childInfoN  :: {-# UNPACK #-} !(ChildInfo e)
  }

type NetworkIndexData d e = IndexData (NetworkContext e) d

data TreeContext e = TreeContext
  { parentIndsT :: {-# UNPACK #-} !TaggedIndex
  , childInfoT  :: {-# UNPACK #-} !(ChildInfo e :!: ChildInfo e)
  }

type TreeIndexData d e = IndexData (TreeContext e) d
```

We use a strict pair type with constructor `:!:`. The above also makes use of the following index types:
```haskell
data TaggedIndex  = TaggedIndex
  { untaggedIndex :: {-# UNPACK #-} !Int
  , tag           :: {-# UNPACK #-} !IndexType
  }

data IndexType = LeafTag | TreeTag | NetworkTag | RootTag
```

In particular `TaggedIndex` is essentially a "tagged struct" indicating the type of index we are looking at.
One planned optimization is to make this whole data type "flat":

```haskell
data NewTaggedIndex = LeafInd Int | TreeInd Int | NetworkInd Int | RootInd Int
```

This change has not yet been made.


Important differences with the old representation:
  - Each node type is now more well-typed in the sense that each node stores data corresponding to exactly its type.
    This unfortunately means that functions dealing directly with this representation need to traverse over four reference vector types.

  - Previously we had different node types for each phase but this is now separated by the "pattern functor" `f`. This makes many of the different phases easier to work with.


  - Currently the re-rooting metadata is not stored in the graph metadata and instead is intended to be passed as arguments to the functions which make use of it. This can be changed if it becomes burdensome.


### Resolutions

Resolutions make use of the following types:


```haskell
data Resolution cs = Resolution
  { characterSequence  :: !cs
  , resolutionMetadata :: !ResolutionMetadata
  }

data  ResolutionMetadata
    = ResolutionMetadata
    { totalSubtreeCost       :: {-# UNPACK #-} !Double
    , leafSetRepresentation  :: {-# UNPACK #-} !UnionSet
    , topologyRepresentation :: {-# UNPACK #-} !NetworkTopology
    , subTreeEdgeSet         :: {-# UNPACK #-} !(Set (EdgeIndex))
    , subTreeHash            :: {-# UNPACK #-} !Int
    }

newtype ResolutionCacheM m cs
  = ResolutionCacheM {runResolutionCacheM :: m (NonEmpty (Resolution cs))}

type ResolutionCache cs = ResolutionCacheM Identity cs
```

This is essentially the same as the old design with the following changes:
- `ResolutionInformation` is renamed to `Resolution`.
- Some of the `ResolutionMetadata` has changed. For example this now has a `subTreeHash` field rather than storing the Newick representation. The details of how the hashing works are not yet worked out or implemented.
- Currently we define a monad transformer type for the `ResolutionCache` via `ResolutionCacheM`. This was because we wanted to interleave effects with the postorder pass but we cannot do what was intended and so it is probably better to re-write this to use a newtype around `NonEmpty`. It is essential that this be a `newtype` as we write a custom `Apply` instance which handles filtering those resolutions which overlap between the two arguments.


### Sequences

The module `Data.Graph.Sequence.Class` offers a new interface to sequences. The idea behind this design is so that
we can add new sequence types (or split old sequence types) in a modular way. This starts by introducing the following type:
```haskell
 class BlockBin bin where
-- For any given character we have an associated type for how that character
-- is intialised at a leaf.
  type LeafBin           bin
-- Associated with each character type we have specific character metadata.
-- For example here we might store something like the character metric.
  type CharacterMetadata bin

-- This function takes the associated metadata and the data initially on a leaf
-- and `decorates` that data so that it can be used in a full postorder traversal
  leafInitialise :: CharacterMetadata bin -> LeafBin bin -> bin
-- The binary postPostorder function takes the metadata and the data at two internal
-- nodes and gives the character we get back at the parent node.
  binaryPostorder  :: CharacterMetadata bin -> bin -> bin -> bin
```

This gives an abstract description of a block type but also allows us to put multiple block types together via instances of the form:
```haskell
instance (Blockbin static, BlockBin dynamic) where
  type LeafBin (static, dynamic) = (LeafBin static, Leafbin dynamic)
   -- etc.
```
One can use `Generics` in order to write instances which work for all product types and allowing us to modularly extend the number of characters. Such instances using Generics are not yet written. Alternatively one can simply write the boilerplate by hand for the specific number of character blocks required.

We then add the following types:
```haskell
newtype CharacterSequence block = CharacterSequence {getCharacterSequence :: Vector block}

data MetadataBlock block meta = MetadataBlock
    { blockMetadata :: meta
    , binMetadata   :: CharacterMetadata block
    }

newtype MetadataSequence block meta = MetadataSequence
  { getMetadataSequence :: Vector (MetadataBlock block meta)
  }
```

These mimic the old types only we now allow for arbitrary block types.

As the block type is now abstract we often wish to contain a subblock to be a particular character type. In particular for dynamic character re-rooting we need to isolate the dynamic character block. For this we make use of the following constraint:
```haskell
type DynCharacterSubBlock subBlock dynChar =
  ( subBlock ~ Vector dynChar
  , CharacterMetadata subBlock ~ Vector (CharacterMetadata dynChar)
  )
```
This says that a given subBlock (which we typically have a lens into) is isomorphic to a `Vector` of dynamic characters and that the character metadata is a vector of dynamic character metadata.

We also describe an abstract class for `BlockCost` as follows:
```haskell
class (BlockBin block) => HasBlockCost block where
  staticCost  :: MetadataBlock block m -> block -> Double
  dynamicCost :: MetadataBlock block m -> block -> Double
  rootcost    :: MetadataBlock block m -> block -> Double
  blockCost   :: MetadataBlock block m -> block -> Double
```

All of the above means for instance that the postorder now has functions with types of the form:
```haskell
generateSubBlockLocalResolutions
  :: forall block subBlock meta .
     ( BlockBin block
     , BlockBin subBlock
     , HasSequenceCost block
     )
  => Lens' block subBlock
  -> Lens' (CharacterMetadata block) (CharacterMetadata subBlock)
  -> MetadataSequence block meta
  -> Resolution
       (PostorderContext
         (CharacterSequence (LeafBin block))
         (CharacterSequence block)
       )
  -> Resolution (CharacterSequence block)
```
The above function corresponds to resolving only those characters within the `subBlock` type.


### Graph Scoring Traversal Phases

**Post-order:** Combine the information from child nodes to decorate the parent node. This can result in multiple decorations on a node if the subtree involves one or more network nodes. The multiple decorations are stored ina  "resolution cache" to efficiently work with and compare possible subtrees in a consistent manner.

**Rerooting:** There are two general types of charters, *exact* and *non-exact*. The exact character will report an optimal score for the tree after the post-order. The non-exact characters will report a heuristic score that is not necessarily optimal. In order to improve this heuristic score, is to consider all possible edges of the graph as a potential root for each character.

**Network Edge Quantification:** When scoring the graph, one combines all the character cost and adds to this cumulative score a punitive network penalty based on how many network edges were added to the graph. This penalty prevents generating trivial graphs with an exorbitant number of network edges. This is also where the optimal soft-wire resolutions of the graph is selected for each block of characters

**Pre-order:** Once the optimal soft-wire resolutions are determined, the final states are derived by performing a pre-order pass over the correct resolution for each character. Pre-order traversals start that the appropriate traversal foci of each character and traverses the optional soft-wire resolutions tree.
