Changelog
==========

PCG uses [PVP Versioning][1].
Releases are available [on GitHub][2].


## _v0.1.1.0_ - Unreleased Changes

* Updated to GHC-8.4.2
* Updated to Cabal-2.2
* Added Travis CI build script
* Streamlined build experiance on MacOS
* Corrected defect in Data.Vector.NonEmpty.unfoldr
* Polished command line option parsing and error reporting
* Improved phylogeny pretty-printing
* Documentation improvements


## [v0.1.0.1][4] - 2018-03-23

* Removed space leaks in the C & C++ FFI bindings
* Expanded memoized TCM to accept both pairs and triples of elements
* Corrected defect in return results of the memoized TCM regarding unambiguous elments
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
