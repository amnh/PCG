all: package

package: phylocomgraph.cabal
	cabal install --dependencies-only && cabal configure --enable-tests --enable-profiling && cabal build && cabal test
clean:
	find . -type f -name '*.o' -delete && find . -type f -name '*.hi' -delete
