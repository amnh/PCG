linking       = --make
warnings      = -w -W -Wall -Werror
sanity        = -fwarn-duplicate-exports -fwarn-incomplete-patterns -fwarn-missing-signatures -fwarn-overlapping-patterns -fwarn-tabs -fwarn-unused-binds -fwarn-unused-imports -fwarn-unused-matches -fwarn-unused-do-bind
stack         = -prof -auto-all -caf-all

all: stack-build

cabal-build: phylocomgraph.cabal
	cabal install --dependencies-only && cabal configure --enable-tests --enable-profiling && cabal build && cabal haddock --executables --html --hyperlink-source && cabal test

stack-build: install-stack setup rebuild

install-stack:
	stack upgrade || (cabal update && cabal install stack)

setup: phylocomgraph.cabal stack.yaml
	stack setup

rebuild: phylocomgraph.cabal stack.yaml
	stack build --haddock --test

ffi-submakes: ffi/Analysis/Parsimony/Binary/SequentialAlign/makefile
	$(MAKE) -C ffi/Analysis/Parsimony/Binary/SequentialAlign

clean:
	find . -type f -name '*.o'  -delete
	find . -type f -name '*.hi' -delete
	find . -type f -name '*.*~' -delete
	find . -type f -name '#*.*' -delete

quick: phylocomgraph.cabal stack.yaml
	stack build --test
