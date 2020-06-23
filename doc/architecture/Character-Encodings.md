# Character Encoding
---

### Related packages:
 * `data-structures`
 * `exportable`

### Related modules:

 - **`Bio.Character.Encodable.Continuous`**
 -  *`Bio.Character.Encodable.Continuous.Class`*
 -  *`Bio.Character.Encodable.Continuous.Internal`*
 - **`Bio.Character.Encodable.Dynamic`**
 -  *`Bio.Character.Encodable.Dynamic.AmbiguityGroup`*
 -  *`Bio.Character.Encodable.Dynamic.Element`*
 -  *`Bio.Character.Encodable.Dynamic.Class`*
 -  *`Bio.Character.Encodable.Dynamic.Internal`*
 -  *`Bio.Character.Encodable.Internal`*
 - **`Bio.Character.Encodable.Static`**
 -  *`Bio.Character.Encodable.Static.Class`*
 -  *`Bio.Character.Encodable.Static.Internal`*
 -  *`Bio.Character.Encodable.Stream`*
 - **`Bio.Character.Exportable`**
 -  *`Bio.Character.Exportable.Class`*

This sub-system determines how characters are encoded at run time and their associated type-classes for manipulating the encoded characters in an ergonomic way.

Continuous characters are represented as a bounded range of two values [x, y) such that x & y ∈ ℝ≥0 ⋃ ∞. The range [0,∞) is considered the "missing" range, as it represents any possible continuous value. Continuous characters can be operated on by the `Bounded` and `Ranged` typeclasses.

The `EncodableStream` and `EncodableStreamElement` type-classes are used for encoding and decoding static characters and dynamic characters. They allow for querying gap elements of characters and indexing elements of a dynamic character. The `Stream` module also provides "smart rendering" functionality, treating characters as DNA/RNA/amino acids when applicable.

Static characters are represented as bitvectors. The bit vectors are encoded and decoded from ambiguity groups of "string-like" symbols using an alphabet. The width of the bitvector representing the static character is the length of the alphabet used to encode it, which each bit mapping to a symbol of the alphabet and the set/unset status of each bit representing the presence or absence of the symbol from the character observation. Static characters have a "missing" representation of *all* bits set. Static characters can be treated as a range and operated on using the `Ranged` type-class in the case of the L1-norm ("additive") metric. Static characters can also be operated on via the `Bits` and `FiniteBits` type-classes.

Dynamic characters are represented as a triple of vectors of bitvectors. The three vectors represent ambiguity group elements from the left child, right child, and the derived medians in an aligned context. There are four states that a "column" of the three vectors can be in:
 - **Alignment event**, taking information from *both* children and deriving the median state from their overlap
 - **Deletion event**, taking information from *only* the left child and deriving the median state from it's overlap with the gap element
 - **Insertion event**, taking information from *only* the right child and deriving the median state from it's overlap with the gap element
 - **Gapping event**, taking information from the *neither* the left or right child, (information came from an ancestral location in the tree) and the median state is treated as the gap element
 
Visually, you can picture a dynamic character like so:
```
α : Alignment event
δ : Deletion event
ι : Insertion event
- : Gapping event
              +---------------------------------------+
align context |α|ι|ι|α|α|δ|δ|α|α|α|α|α|α|α|-|-|α|α|α|α| <------- Context reference
              +---------------------------------------+
median states |A|a|a|C|C|g|g|T|T|A|R|Y|G|C|-|-|G|M|R|C| <--\
left   states |A|A|A|C|C| | |T|T|A|A|C|G|C| | |G|C|G|C| <---+--- Three vectors
ri1ght states |A| | |C|C|G|G|T|T|A|G|T|G|C| | |G|A|A|C| <--/
              +---------------------------------------+
```

Dynamic and static characters can be converted in ***O(n)*** time to one of two "exportable" representations for FFI bindings. 

The first representation, convertible via the `ExportableBuffer` type-class, is a "packed" representation, with a vector 64 bit elements representing the 64 bit chunks of the bitvector in ascending order of significance. Elements of the dynamic characters are not cleanly separated at word boundaries, they often spanning across boundaries. This is a direct translation from the bitvector representation to a C-like buffer. All bits contain some information from the original character.

The second representation, convertible via the `ExportableElements` type-class, is a "flat" representation, with a vector 64 bit elements. However, each element of the vector represents a single character element as a 64 word (assumed alphabet size is small). Character elements will never span multiple word boundaries. Only the least significant bits of each 64-bit word will contain information from the character element, the most significant bits will not contain information.