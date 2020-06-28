# Character Alphabets
---

### Related packages:
 * `alphabet`

### Related modules:

 - **`Data.Alphabet`**
 - **`Data.Alphabet.IUPAC`**
 - *`Data.Alphabet.Internal`*

Character alphabets can be interacted with via the `Data.Alphabet` module. They are defined with `Data.Alphabet.Internal`, and re-exported by `Data.Alphabet`. Character alphabets contain a ordered set of "string-like" symbols which represent discrete "states" of a character. Character alphabets can be created by the functions listed in the **Construction** section of the `Data.Alphabet` modules haddock documentation. The `Data.Alphabet.IUPAC` module facilitates conversion between single symbols and expanded ambiguity groups via bidirectional maps for DNA, RNA, and amino acid [IUPAC codes](https://www.bioinformatics.org/sms/iupac.html).

Character alphabets have a number of invariant that must hold.
 - All alphabets must have a gap character. If the gap character is not provided as part of the input symbols, the gap character is added.
 - The gap character is the *last* character of the alphabet. If the gap character is provided in a different position as part of the input symbols, it is repositioned to be the last symbol of the alphabet.
 - Alphabets are order dependent. The order in which symbols appear in an alphabet effects the encoding of their associated characters and the cost structure of their associated Transition Cost Matrices (TCMs).

The primary use of character alphabets to to encode and decode the characters to and from bitvectors. Because of this, the ordering of the alphabet must remain consistent throughout the course of the program. However, when initially constructing the alphabet, there is an opportunity to re-arrange the symbols. 

If, and only if, the provided TCM does not have an implicitly ordered structure (such as the L1 Norm/ "Additive" metric), the ordering of the symbols of the alphabet can be permuted before characters are encoded. If the opportunity is taken to permute the symbols of the alphabet to a sorted order, encoding and decoding of characters can be asymptotically improved from ***O(n)*** to ***O(log n)*** via binary search. Because of this, the `Alphabet` constructor has a `isSorted` flag which indicates if the alphabet is in a sorted order and the encoding and encoding functions will conditionally use the asymptotically faster algorithm is possible. While this does not have significant performance impacts on small alphabets (say DNA/RNA/Protein), it has enormous impact on large alphabets (cultural/linguistic).