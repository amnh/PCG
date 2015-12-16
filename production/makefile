all: stack-build

cabal-build: phylocomgraph.cabal
	cabal install --dependencies-only && cabal configure --enable-tests --enable-profiling && cabal build && cabal haddock --executables --html --hyperlink-source && cabal test

stack-build: phylocomgraph.cabal stack.yaml
	stack setup && stack build --haddock --test
clean:
	find . -type f -name '*.o' -delete && find . -type f -name '*.hi' -delete
