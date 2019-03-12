Changelog
==========

PCG uses [PVP Versioning][1].
Releases are available [on GitHub][2].


## _v0.2.0.0_ - Unreleased Changes

* Using semantic versioning
* Updated to GHC-8.6.4
* Updated to Cabal-2.4
* Updated build command to add network edges to input graphs rather than wagner build a new tree first.
* Updated read command grammar to allow for more specific TCM specification
* Improved efficiency and consistency of character metadata representation
* Improved efficiency of Wagner build from _O(n^3)_ to _O(n^2)_
* Improved efficiency of Data.Vector.NonEmpty.fromNonEmpty, unfoldr, and traverse
* Improved efficiency of three-way median and cost calculation (discrete metric, L1 norm, and small alphabet specialization)
* Improved representation of character & metadata sequences
* Improved representation of Symbol Change Matrix (SCM) and Transition Cost Matrix (TCM)
* Improved representation of character alphabet & character names
* Improved representation of the types of postorder and preorder functions
* Improved master postorder and preorder functions for readability
* Improved safety of data normalization and unification
* Enhanced file parser selection to intelligently look at the file extension
* Enhanced DOT file parser to accept DOT labels on leaf nodes as identifiers
* Enhanced FASTA file parser to better interpret accept amino acid input files
* Enhanced REPORT command to move files by default if new output would overwrite an old file
* Enhanced Evaluation monad to disallow NoOp state
* Added Travis CI build script
* Added integration test suite
* Added test suites for Data.List.Utility, Data.Alphabet, and Data.TCM
* Added support for compact regions of phylogenetic solutions
* Added save and load commands by serialising compact regions
* Added echo command for printing progress information
* Added module for stricter fold functions
* Added lenses for many graph and node accessors
* Added makefile commands for outputting core, building with llvm backend, various test options and building a local hoogle database
* Added module for memoized vector computation abstracting the method used in pre/postorder
* Streamlined build experience on MacOS
* Corrected defect in handling of prealigned input data
* Corrected defect in FFI resulting in a space leak
* Corrected defect in Data.Range value construction
* Corrected defect in Data.Range.Ranged type-class definitions
* Corrected defect in Data.Vector.NonEmpty.unfoldr
* Corrected defect in headEx & lastEx of several MonoFoldable instances
* Corrected defect in omap over missing dynamic characters
* Corrected defect in candidateNetworkEdges to not return inconsistent edges
* Streamlined build experience on MacOS
* Polished command line option parsing and error reporting
* Improved phylogeny pretty-printing
* Documentation improvements


## [v0.1.0.1][4] - 2018-03-23

* Removed space leaks in the C & C++ FFI bindings
* Expanded memoized TCM to accept both pairs and triples of elements
* Corrected defect in return results of the memoized TCM regarding unambiguous elements
* Improved efficiency of memoized TCM implementation
* Improved efficiency file parsers
* Corrected binding issue which caused work to be repeated rather than shared
* Documentation improvements


## [v0.1.0.0][3] - 2018-03-01

* Supports naive phylogenetic network construction
* Added file parsers for DOT, Fasta, Fastc, Newick, Nexus, and TNT inputs
* Added DOT and XML output formats
* Added custom rendering output format
* Correctly calculated parsimony score
* Computes "total edge cost"
* Computes punitive network edge cost
* Computes rooting cost


## v0.0.1.0 - 2014-11-17

* Initially created


[1]: https://pvp.haskell.org
[2]: https://github.com/amnh/PCG/releases
[3]: https://github.com/amnh/PCG/releases/tag/v0.1.0.0
[4]: https://github.com/amnh/PCG/releases/tag/v0.1.0.1
