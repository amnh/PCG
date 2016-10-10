#Flags for GHC
linking       = --make
warnings      = -w -W -Wall -Werror
sanity        = -fwarn-duplicate-exports -fwarn-incomplete-patterns -fwarn-missing-signatures -fwarn-overlapping-patterns -fwarn-tabs -fwarn-unused-binds -fwarn-unused-imports -fwarn-unused-matches -fwarn-unused-do-bind
stacktracing  = -prof -auto-all -caf-all

#Flags for `stack`
haddock       = --haddock --haddock-deps
profiling     = --executable-profiling --library-profiling


# Target aliases for easy CLI use
################################################################################


# Default build target
all: standard-build copy-docs

# Rebuilds with profiling
prof: stack-build-profiling

# Builds fast as possible
quick: stack-build-quick

# Rebuilds as fast as possible
rebuild: quick

# Clean then rebuild
rebuild-full: clean rebuild

# Rebuilds with optimizations and runs tests
test: stack-build-test

# Target Definitions
################################################################################


# Builds with useful features for a standard user of the package
standard-build: install-stack stack-setup
	stack install $(haddock)

# Builds with all useful options for package power users
full-build: install-stack clean stack-setup stack-build-prof

# Upgrade stack if installed or install stack if not installed
install-stack:
	stack upgrade || (cabal update && cabal install stack)

stack-setup: phylocomgraph.cabal stack.yaml
	stack setup

# Builds with no extra generated features and no optimizations
stack-build-quick: phylocomgraph.cabal stack.yaml
	stack build --fast

# Builds with profiling enabled
stack-build-profiling: phylocomgraph.cabal stack.yaml
	stack install $(profiling) --ghc-options="-fprof-cafs"

# Builds with profiling enabled
stack-build-test: phylocomgraph.cabal stack.yaml
	stack build --test


# Copies documentation director to local scope
copy-docs:
	rm -rf docs
	rm -rf docs.html
	cp -r .stack-work/dist/x86_64-linux/Cabal-1.24.0.0/doc/html/phylocomgraph docs
	ln -s docs/index.html docs.html

# Cleans up artefact files after a build
clean: phylocomgraph.cabal stack.yaml
	stack clean
	mv .stack-work ../
	find . -type f -name '*.o'  -delete
	find . -type f -name '*.hi' -delete
	find . -type f -name '*.*~' -delete
	find . -type f -name '#*.*' -delete
	mv ../.stack-work .

# Calls other make files to pre-process FFI files
ffi-code-cleaning: ffi/Analysis/Parsimony/Binary/SequentialAlign/makefile
	$(MAKE) -C ffi/Analysis/Parsimony/Binary/SequentialAlign

# Legacy cabal build option
cabal-build: phylocomgraph.cabal
	cabal install --dependencies-only && cabal configure --enable-tests --enable-profiling && cabal build && cabal haddock --executables --html --hyperlink-source && cabal test


# Legacy cabal build option
cabal-sandbox: phylocomgraph.cabal
	cabal update && cabal sandbox delete && cabal sandbox init && cabal install --dependencies-only

