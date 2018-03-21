Change log
==========

PCG uses [PVP Versioning][1].
The change log is available [on GitHub][2].

[1]: https://pvp.haskell.org
[2]: https://github.com/amnh/PCG/releases

# Unreleased changes
* Removed space leaks in the C & C++ FFI bindings.
* Expanded memoized TCM to accept both pairs and triples of elements.
* Corrected defect in return results of the memoized TCM regarding unambiguous elments.
* Improved efficiency of memoized TCM implementation.

# 0.1.0.0 (Alpha Release)
* Supports naive phylogenetic network construction
* Added file parsers for DOT, Fasta, Fastc, Newick, Nexus, and TNT inputs
* Added DOT, XML output formats
* Added custom rendering output format
* Correctly calculated parsimony score
* Computes "total edge cost"
* Computes punitive network edge cost
* Computes rooting cost

# 0.0.1.0
* Initially created.
