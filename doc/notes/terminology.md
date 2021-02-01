# Terminology

### Character Type

A character can have numerous different types. A character could take on discrete values,
continuous values, have a particular metric etc. The characters currnetly supported in PCG are:
[TO DO: Have a separate document explaining what each of these types represents]
  - continuous    [defined ???]
  - non-additive  [defined ???]
  - additive      [defined ???]
  - metric        [defined ???]
  - non-metric    [defined ???]
  - dynamic       [defined ???]

### Character Block [defined ???]
A character block consists of a collection of characters which are to be treated as
a single group which is optimized atomically across a phylogenetic network i.e. this group is to be
assigned a single topology from the collection of network display trees.


### Character Sequence [defined ???]
A character sequence is a collection of character blocks each of which may be
treated non-atomically during optimization meaning that we can assign different display trees of a
phylogenetic network to each block in the character sequence.


### Graph Traversal [define ???]
A graph traversal aims to take a phylogenetic network with character sequences assigned to each of the
leaves and to assign character sequences to each 


### Resolution [defined Data.Graph.Postorder.Resolution]
During a postorder pass we we wish to assign to each node the lowest cost character sequence from its
one or two children along with information that we propagate up the network including for instance the cost of the descendent subtrees, those leaf nodes in this resolution etc. Importantly a resolution makes sense only in the
context of a display tree. We do not propagate information from a network node to its two parents
(see Resolution Cache).

### Resolution Cache [defined Data.Graph.Postorder.Resolution]
A resolution cache is a non-empty collection of resolutions. Such a collection represents each of the possible
resolutions along each of the possible display trees that are descendent from the node.


### Non-exact character
[to do]
