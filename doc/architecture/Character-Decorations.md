# Character Decorations
---

### Related packages:
 * `data-structures`

### Related modules:

 - **`Bio.Character.Decoration.Additive`**
 -  *`Bio.Character.Decoration.Additive.Class`*
 -  *`Bio.Character.Decoration.Additive.Internal`*
 - **`Bio.Character.Decoration.Continuous`**
 -  *`Bio.Character.Decoration.Continuous.Class`*
 -  *`Bio.Character.Decoration.Continuous.Internal`*
 - **`Bio.Character.Decoration.Discrete`**
 -  *`Bio.Character.Decoration.Discrete.Class`*
 - **`Bio.Character.Decoration.Dynamic`**
 -  *`Bio.Character.Decoration.Dynamic.Class`*
 -  *`Bio.Character.Decoration.Dynamic.Internal`*
 - **`Bio.Character.Decoration.Fitch`**
 -  *`Bio.Character.Decoration.Fitch.Class`*
 -  *`Bio.Character.Decoration.Fitch.Internal`*
 - **`Bio.Character.Decoration.Metric`**
 -  *`Bio.Character.Decoration.Metric.Class`*
 -  *`Bio.Character.Decoration.Metric.Internal`*
 - **`Bio.Character.Decoration.NonMetric`**
 -  *`Bio.Character.Decoration.NonMetric.Class`*
 -  *`Bio.Character.Decoration.NonMetric.Internal`*
 -  *`Bio.Character.Decoration.Shared`*
 
This sub-system determines how characters and their associated data, referred to here as "decorations," are stored during the scoring process. Defines type-classes for extending decorations and more information is generated. Shared type-classes for multiple decorations or discrete character decorations are defined in the `Shared` and `Discrete` modules, respectively.

Additive and continuous characters use the `RangedPostorderDecoration` type-class to construct a new decoration and use the `RangedExtensionPreorder` type-class to extend an existing additive or continuous character decoration to a new final context after a pre-order traversal. Additive and continuous characters decorations have the following fields:
 - **Cost:** The cost of the entire subtree
 - **Final Interval:** Final state derived after the pre-order traversal
 - **Preliminary Interval:** Preliminary state derived after the post-order traversal, refined by the pre-order traversal
 - **Child Preliminary Intervals:** The preliminary intervals of the left and right children, used to keep the information in scope for the pre-order traversal
 - **Is Leaf:** Indicates if this decoration on a leaf node
 - **Character Field:** Representation of the character, *(possibly unused)*


Fitch characters use the `DiscreteExtensionFitchDecoration` type-class to construct a new decoration and extend an existing decoration after a pre-order traversal. Fitch character decorations have the following fields:
 - **Cost:** The cost of the entire subtree
 - **Final median:** Final state derived after the pre-order traversal
 - **Preliminary median:** Preliminary state derived after the post-order traversal, refined by the pre-order traversal
 - **Child medians:** The preliminary median of the left and right children, used to keep the information in scope for the pre-order traversal
 - **Is Leaf:** Indicates if this decoration on a leaf node
 - **Character Field:** Representation of the character, *(possibly unused)*


Metric characters use the `DiscreteExtensionSankoffDecoration` type-class to construct a new decoration and extend an existing decoration after a pre-order traversal. Non-metric characters are currently treated as metric characters. Metric character decorations have the following fields:
 - **Cost:** The cost of the entire subtree
 - **Minimum State Tuple:** Tuple of (a,a) where a is a per-parent-state list of lists of child states that contributed to the minimum cost of that state
 - **Minimum Cost Vector:** Minimum total cost per state (left + right)
 - **Preliminary Extra Costs:** List of preliminary per-character-state extra costs for the node; these store only the costs for assigning this state to *this* node, rather than the accumulated extra costs down through the tree when this assignment is chosen
 - **Final Extra Costs:** list of final extra costs for the node, so the sum of *all* of the extra costs, on the whole tree, for this assignment 
 - **Beta:** this is Goloboff's beta, where beta_(s,n) = min[t_(s,x) + prelimExtraCost_(x,n)] where t_(s,x) is the transition cost from state s to x
 - **Is Leaf:** Indicates if this decoration on a leaf node
 - **Character Field:** Representation of the character, *(possibly unused)*


Dynamic characters use the `DynamicCharacterDecoration`, `SimpleDynamicExtensionPostorderDecoration`, and `PostorderExtensionDirectOptimizationDecoration` type-classes to construct a new decorations and extend an existing decorations. Dynamic character decorations have the following fields:
 - **Subtree Cost:** The cost of the entire subtree
 - **Local Cost:** The cost of the string alignment between the two children
 - **Average Length:** The average length of the characters on the leaf nodes in the subtree
 - **Alignment Context:** The string alignment of the left and right children, stored in a compact and efficient reference format
 - **Implied Alignment:** The alignment of the character as implied by the tree. Final medians can be derived by taking the medians of the implied alignment and dropping any gaps
 - **Single Disambiguation:** A copy of the implied alignment in which all character elements are non-ambiguous