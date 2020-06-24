# Architectural Overview

```
+--------------------+
| PCG Script Parser  +---------------------------------------------------+
+--------------------+                                                   |
                                                                         |
+--------------------+    +---------------+    +-------------+    +------v------+    +---------------+
| Input File Parsers +--->+ Normalization +--->+ Unification +--->+ Interpreter +--->+ Serialization |
+--------------------+    +-+-------------+    +-------------+    +-+-----------+    +---------------+
                            |                                       |
                            |  +-----------------------+            |  +---------------+
                            +->+ Character Alphabets   |            +->+ Graph Scoring |
                            |  +-----------------------+            |  +---------------+
                            |                                       |
                            |  +-----------------------+            |  +---------------+
                            +->+ Character Decorations |            +->+ Graph Search  |
                            |  +-----------------------+               +---------------+
                            |
                            |  +-----------------------+
                            +->+ Character Encodings   |
                            |  +-----------------------+
                            |
                            |  +-----------------------+    +------------------------+
                            +->+ Character Metadata    +--->+ Transition Cost Matrix |
                            |  +-----------------------+    +------------------------+
                            |
                            |  +-----------------------+
                            +->+ Graph Structure       |
                               +-----------------------+
```

The design of PCG can be seperated into many discrete components which are connected as shown in the above informational flow chart.

Each section of the above flow chart has it's own sys-system archetecture described in the following files:

 * [Character Alphabets   ](https://github.com/amnh/PCG/blob/master/doc/architecture/Character-Alphabets.md)
 * [Character Decorations ](https://github.com/amnh/PCG/blob/master/doc/architecture/Character-Decorations.md)
 * [Character Encodings   ](https://github.com/amnh/PCG/blob/master/doc/architecture/Character-Encodings.md)
 * [Character Metadata    ](https://github.com/amnh/PCG/blob/master/doc/architecture/Character-Metadata.md)
 * [Graph Scoring         ](https://github.com/amnh/PCG/blob/master/doc/architecture/Graph-Scoring.md)
 * [Graph Search          ](https://github.com/amnh/PCG/blob/master/doc/architecture/Graph-Search.md)
 * [Graph Structure       ](https://github.com/amnh/PCG/blob/master/doc/architecture/Graph-Structure.md)
 * [Input File Parsers    ](https://github.com/amnh/PCG/blob/master/doc/architecture/Input-File-Parsers.md)
 * [Interpreter           ](https://github.com/amnh/PCG/blob/master/doc/architecture/Interpreter.md)
 * [Normalization         ](https://github.com/amnh/PCG/blob/master/doc/architecture/Normalization.md)
 * [PCG Script Parser     ](https://github.com/amnh/PCG/blob/master/doc/architecture/PCG-Script-Parser.md)
 * [Unification           ](https://github.com/amnh/PCG/blob/master/doc/architecture/Unification.md)
 * [Serialization         ](https://github.com/amnh/PCG/blob/master/doc/architecture/Serialization.md)
 * [Transition Cost Matrix](https://github.com/amnh/PCG/blob/master/doc/architecture/Transition-Cost-Matrix.md)
