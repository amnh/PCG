# Character Metadata
---

### Related packages:
 * `character-name`
 * `data-structures`

### Related modules:

 - **`Bio.Metadata`**
 - **`Bio.Metadata.Continuous`**
 - **`Bio.Metadata.Discrete`**
 -  *`Bio.Metadata.Discrete.Class`*
 -  *`Bio.Metadata.Discrete.Internal`*
 - **`Bio.Metadata.DiscreteWithTCM`**
 -  *`Bio.Metadata.DiscreteWithTCM.Class`*
 -  *`Bio.Metadata.DiscreteWithTCM.Internal`*
 - **`Bio.Metadata.Dynamic`**
 -  *`Bio.Metadata.Dynamic.Class`*
 -  *`Bio.Metadata.Dynamic.Internal`*
 - **`Bio.Metadata.General`**
 -  *`Bio.Metadata.General.Class`*
 -  *`Bio.Metadata.General.Internal`*
 - **`Bio.Metadata.Metric`**
 -  *`Bio.Metadata.Metric.Class`*
 -  *`Bio.Metadata.Metric.Internal`*
 - **`Data.CharacterName`**

This sub-system determines how metadata for characters is efficiently stored and accessed. Different characters will have different metadata fields. Character metadata can be thought of as "growning" in the available fields acording to this diagram:

```
+-----------------< Dynamic Character >-------+
| +-----< Discrete with TCM Character >-----+ |
| | +------------< Discrete Character >---+ | |
| | | +--------< Continuous Character >-+ | | |
| | | | Character Name                  | | | |
| | | | Character Weight                | | | |
| | | | Pairwise Transition Cost Matrix | | | |
| | | +---------------------------------+ | | |
| | |   Character Alphabet                | | |
| | |   TCM Source File                   | | |
| | +-------------------------------------+ | |
| |     Sparse Transition Cost Matrix       | |
| |     Symbol Change Matrix                | |
| +-----------------------------------------+ |
|       Dense Transition Cost Matrix          |
|       Threeway Transition Cost Matrix       |
|       Traversal Foci                        |
+---------------------------------------------+
```

**Character Name:** 
A `CharacterName` represents an *unique* name identifier for a character that can be used as a textual reference. A `CharacterName` contains within it a reference to the file from which it was originally parsed, allowing identically named characters from different input files to be uniquely referenced. The file from which a 'CharacterName' was originally parsed can be queried by the function `sourceFile`. A `CharcterName` can be a either a user specified textual identifier or a default generated value. Whether a given character name is user specified or defaulted may be queried by the function `isUserDefined`.

**Character Weight:**
Represents the "real-valued" weight for a character. Currently the character weight is represented as a IEEE `Double`. This is fast, but has precision issues. All the cost computations (except for continuous characters) involve integral types. These computations have no loss in precision. After all the computationally expensive work is performed, PCg multiplies the character cost by the character weight. When the weight is represented as a `Double`, we introduce a vector for precision errors into PCG. We should strongly consider changing the weight type to something that does not have a loss of precision, even if it is slightly less efficient. Consider changing the representation to a `Rational` from the `Data.Ratio` module (if you want negative weights) or a `Cost` from the `Numeric.Cost` module (if you want strictly non-negative weights).
   
**Character Alphabet:**
The alphabet of symbols for the given character. The length of the alphabet determines the dimension of the charcter's TCM. For more information regarding character alphabets, see the [Character Alphabet sub-system documentation](https://github.com/amnh/PCG/blob/master/doc/architecture/Character-Alphabets.md).
   
**Symbol Change Matrix:**
Given two symbols from the character's alphabet, returns the transition cost between the two symbols. Not to be confused with a TCM, which given two ambiguity groups of symbols from the character alphabet will return the transition cost and median state.

**TCM Source File:**
The file from which the information of the TCM was derived. Can reference a default file if no TCM was specified and the metric was defaulted.

**Pairwise Transition Cost Matrix:**
Given two ambiguity groups of symbols from the character alphabet, will return the transition cost and median state. The function returned can be "backed" by a number of efficient representations. For more information, see the [Transition Cost Matrix sub-system documentation](https://github.com/amnh/PCG/blob/master/doc/architecture/Transition-Cost-Matrix.md).

**Threeway Transition Cost Matrix:**
Given three ambiguity groups of symbols from the character alphabet, will return the transition cost and median state.

**Dense Transition Cost Matrix:**
Conditionally returns a `Maybe DenseTransitionCostMatrix`, which represents a fully evaluated, array-based lookup table of costs and medians for charcters with small alphabets. The getter will return a `Nothing` value if the alphabet is larger than 8 (ie. ambuguity groups won't fit in a byte). For more information, see the [Transition Cost Matrix sub-system documentation](https://github.com/amnh/PCG/blob/master/doc/architecture/Transition-Cost-Matrix.md).

**Sparse Transition Cost Matrix:**
Conditionally returns a `Maybe MemoizedCostMatrix`, which represents a fully lazily evaluated, memoizing, hashtable backed lookup table of costs and medians for charcters with large alphabets. The getter will return a `Nothing` value if the alphabet is smaller than or equal to 8. For more information, see the [Transition Cost Matrix sub-system documentation](https://github.com/amnh/PCG/blob/master/doc/architecture/Transition-Cost-Matrix.md).

**Traversal Foci:**
The non-empty list of edges from which a traversal of the tree can be initiated (treated as a rooted), which will result in a minimal cost for the dynamic character.