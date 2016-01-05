linking       = --make
warnings      = -w -W -Wall -Werror
sanity        = -fwarn-duplicate-exports -fwarn-incomplete-patterns -fwarn-missing-signatures -fwarn-overlapping-patterns -fwarn-tabs -fwarn-unused-binds -fwarn-unused-imports -fwarn-unused-matches -fwarn-unused-do-bind
stack         = -prof -auto-all -caf-all

all: stack-build

cabal-build: phylocomgraph.cabal
	cabal install --dependencies-only && cabal configure --enable-tests --enable-profiling && cabal build && cabal haddock --executables --html --hyperlink-source && cabal test

stack-build: setup rebuild

setup: phylocomgraph.cabal stack.yaml
	cabal update && cabal install stack && stack setup

rebuild: phylocomgraph.cabal stack.yaml
	stack build --haddock --test

clean:
	find . -type f -name '*.o' -delete && find . -type f -name '*.hi' -delete

test: lib/Analysis/DirectOptimization/Test.hs
	ghc $< -o $@ -main-is Analysis.DirectOptimization.Test $(linking) $(sanity) -ilib
