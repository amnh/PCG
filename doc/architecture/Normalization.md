# Normalization
---

### Related packages:
 * `data-normalization`

### Related modules:

 - **`Data.Normalization.Character`**
 -  *`Data.Normalization.Character.Class`*
 -  *`Data.Normalization.Character.Internal`*
 - **`Data.Normalization.Metadata`**
 -  *`Data.Normalization.Metadata.Class`*
 -  *`Data.Normalization.Metadata.Internal`*
 - **`Data.Normalization.Topology`**

PCG accepts input from a variety of file formats. The inputs from these disparate formats need to be normalized into a single, comparable representation. The `data-normalization` library provides this functionality. It exposes the normalized data-types and type-classes for normalizing input data. There are three type-classes in total for normalizing data; `HasNormalizedCharacters`, `HasNormalizedMetadata`, and `HasNormalizedTopology`. Each input file format is an instance of all three type-classes and produces any information that the parse format possesses in the normalized data-type associated with the type-class.

Once the data has been normalized, the collection of input data can be holistically checked for consistency. For more information on this process, see the [Unification documentation](https://github.com/amnh/PCG/blob/master/doc/architecture/Unification.md).