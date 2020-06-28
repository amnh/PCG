# Input File Parsers
---

### Related packages:
 * `file-parsers`

### Related modules:

 - **`File.Format.Dot`**
 - **`File.Format.Fasta`**
 -  *`File.Format.Fasta.Converter`*
 -  *`File.Format.Fasta.Internal`*
 -  *`File.Format.Fasta.Parser`*
 - **`File.Format.Fastc`**
 -  *`File.Format.Fastc.Parser`*
 - **`File.Format.Newick`**
 -  *`File.Format.Newick.Internal`*
 -  *`File.Format.Newick.Parser`*
 - **`File.Format.Nexus`**
 -  *`File.Format.Nexus.Data`*
 -  *`File.Format.Nexus.Parser`*
 -  *`File.Format.Nexus.Partition`*
 -  *`File.Format.Nexus.Validate`*
 - **`File.Format.TNT`**
 -  *`File.Format.TNT.Command.CCode`*
 -  *`File.Format.TNT.Command.CNames`*
 -  *`File.Format.TNT.Command.Cost`*
 -  *`File.Format.TNT.Command.NStates`*
 -  *`File.Format.TNT.Command.Procedure`*
 -  *`File.Format.TNT.Command.TRead`*
 -  *`File.Format.TNT.Command.XRead`*
 -  *`File.Format.TNT.Parser`*
 -  *`File.Format.TNT.Partitioning`*
 -  *`File.Format.TNT.Internal`*
 - **`File.Format.TransitionCostMatrix`**
 -  *`File.Format.TransitionCostMatrix.Parser`*
 - **`File.Format.VertexEdgeRoot`**
 -  *`File.Format.VertexEdgeRoot.Parser`*
 -  *`Text.Megaparsec.Custom`*

The file parsers for all input data file formats are defined by the `file-parsers` library. They genrally parse three types of data; character observations, graphs, and metrics. Some file formats provide multiple types data. Some care has been taken to ensure meaningful error messages are generated on a parse error. While the error messages are servicable, there is still notable room for improvement.

| Format        | Parser coverage
|---------------|------------------
| **`Dot`**     | Standard is well defined, complete parser coverage
| **`Fasta`**   | Format is reasonably well defined, confident that the parser has sufficient coverage
| **`Fastc`**   | Format is well defined, complete parser coverage
| **`eNewick`** | Format is reasonably well defined, confident that the parser has sufficient coverage
| **`Nexus`**   | Format is reasonably well defined, but context sensetive or turing complete, parses a productive subset of the format
| **`TNT`**     | Format is not defined, and also context sensetive or turing complete, parses a productive subset of the format
| **`TCM`**     | Format is well defined, complete parser coverage
| **`VER`**     | Format is well defined, complete parser coverage