Changelog
==========

PCG uses [PVP Versioning][1].
Releases are available [on GitHub][2].


## _0.2.1_ - Unreleased Changes

* Updated to GHC-8.8.2
* Updated to Cabal-3.0
* Updated to Cabal's multiple libraries functionality for sub-libraries
* Updated Travis CI build script
* Added greedy network search to the BUILD command
* Added Wagner distance to the BUILD command option
* Added distance CSV output to the REPORT command
* Added sting alignment benchmarking suite
* Enhanced alphabet data-type to support sorted alphabets whenever possible
* Improved efficiency of character encoding
* Improved efficiency of median state calculations
* Corrected defect regarding syntany prefixes in FASTC parser
* Reduced memory usage and pressure when normalizing and unifying inputs

## [0.2.0][5] - 2019-07-09

* Using semantic versioning
* Modularized sub-libraries
* Reorganized build process
* Streamlined build experience on MacOS
* Updated to GHC-8.6.5
* Updated to Cabal-2.4
* Documentation improvements
* Added Travis CI build script
* Added integration test suite
* Added benchmarking suite for FASTA, FASTC, Newick, TCM, & VER file parsers
* Added test suites for Data.List.Utility, Data.Alphabet, and Data.TCM
* Added support for compact regions of phylogenetic solutions
* Added SAVE and LOAD commands by serialising compact regions
* Added ECHO command for printing progress information
* Added lenses for many graph and node accessors
* Added makefile commands for outputting core, building with llvm backend, various test options and building a local hoogle database
* Added module for stricter fold functions
* Added module for memoized vector computation abstracting the method used in pre/postorder
* Added Validation monad transformer (for error handling)
* Added Cost data-type for perfect precision, non-negative rational number accumulation
* Added specific exit codes to indicate in which phase of the runtime failure(s) occurred
* Added option to perform hierarchical clustering in wagner build options
* Corrected defect in handling of prealigned input data
* Corrected defect in FFI resulting in a space leak
* Corrected defect in Data.Range value construction
* Corrected defect in Data.Range.Ranged type-class definitions
* Corrected defect in Data.Vector.NonEmpty.unfoldr
* Corrected defect in headEx & lastEx of several MonoFoldable instances
* Corrected defect in omap over missing dynamic characters
* Corrected defect in candidateNetworkEdges to not return inconsistent edges
* Corrected defect in decorating single node graphs
* Enhanced command line option parsing and error reporting
* Enhanced file parser selection to intelligently look at the file extension
* Enhanced BUILD command to add network edges to input graphs rather than wagner build a new tree first
* Enhanced DOT file parser to accept DOT labels on leaf nodes as identifiers
* Enhanced FASTA file parser to better interpret accept amino acid input files
* Enhanced REPORT command to move files by default if new output would overwrite an old file
* Enhanced READ command grammar to allow for more specific TCM specification
* Enhanced Evaluation monad to disallow NoOp state
* Enhanced Evaluation monad to have a read-only, "global settings" type variable
* Enhanced I/O stream input to consistently use "file-globs" when locating an input file source
* Enhanced I/O stream input and output to consistently indicate failures in a well-type manner
* Enhanced I/O stream parsing to consistently report parse errors and deserialization errors
* Enhanced I/O stream output to consistently write data in constant memory (theoretically)
* Improved efficiency and consistency of character metadata representation
* Improved efficiency of Wagner build from _O(n^3)_ to _O(n^2)_
* Improved efficiency of Data.Vector.NonEmpty.fromNonEmpty, unfoldr, and traverse
* Improved efficiency of three-way median and cost calculation (discrete metric, L1 norm, and small alphabet specialization)
* Improved efficiency of time & memory usage for FASTA, FASTC, Newick, & TCM file parsers
* Improved readability of postorder and preorder functions
* Improved readability of phylogeny pretty-printing
* Improved representation of character & metadata sequences
* Improved representation of Symbol Change Matrix (SCM) and Transition Cost Matrix (TCM)
* Improved representation of character alphabet & character names
* Improved representation of the types of postorder and preorder functions
* Improved representation of FASTA, FASTC, Newick, & TCM file parser outputs (less memory)
* Improved safety of data normalization and unification


## [0.1.1][4] - 2018-03-23

* Removed space leaks in the C & C++ FFI bindings
* Expanded memoized TCM to accept both pairs and triples of elements
* Corrected defect in return results of the memoized TCM regarding unambiguous elements
* Improved efficiency of memoized TCM implementation
* Improved efficiency file parsers
* Corrected binding issue which caused work to be repeated rather than shared
* Documentation improvements


## [0.1.0][3] - 2018-03-01

* Supports naive phylogenetic network construction
* Added file parsers for DOT, Fasta, Fastc, Newick, Nexus, and TNT inputs
* Added DOT and XML output formats
* Added custom rendering output format
* Correctly calculated parsimony score
* Computes "total edge cost"
* Computes punitive network edge cost
* Computes rooting cost


## 0.0.1 - 2014-11-17

* Initially created


[1]: https://pvp.haskell.org
[2]: https://github.com/amnh/PCG/releases
[3]: https://github.com/amnh/PCG/releases/tag/0.1.0
[4]: https://github.com/amnh/PCG/releases/tag/0.1.1
[5]: https://github.com/amnh/PCG/releases/tag/0.2.0

