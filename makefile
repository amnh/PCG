#Flags for GHC
linking       = --make
warnings      = -w -W -Wall -Werror
sanity        = -fwarn-duplicate-exports -fwarn-incomplete-patterns -fwarn-missing-signatures -fwarn-overlapping-patterns -fwarn-tabs -fwarn-unused-binds -fwarn-unused-imports -fwarn-unused-matches -fwarn-unused-do-bind
stacktracing  = -prof -fprof-auto -fprof-cafs

#Flags for `stack`
haddock       = --haddock --haddock-deps
              # For later:
              # --haddock-arguments --mathjax=https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML
profiling     = --executable-profiling --library-profiling

code-dirs     = app ffi lib src test utils

sub-libs      = pcg-file-parsers pcg-language pcg-utility


# Target aliases for easy CLI use
################################################################################


# Default build target
all: standard-build copy-haddock
#all: stack-build-profiling

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

test-failures: stack-build-test-failures

test-new: stack-build-test-new

# Runs linter

lint: run-linter


# Target Definitions
################################################################################


# Builds with useful features for a standard user of the package
standard-build: install-stack stack-setup
	stack install $(haddock)

# Builds with all useful options for package power users
full-build: install-stack clean stack-setup stack-build-prof

# Upgrade stack if installed or install stack if not installed
install-stack:
	which stack || (cabal update && cabal install stack)

stack-setup: phylocomgraph.cabal stack.yaml
	stack setup

# Builds with no extra generated features and no optimizations
stack-build-quick: phylocomgraph.cabal stack.yaml
	stack build --fast

# Builds with profiling enabled
stack-build-profiling: phylocomgraph.cabal stack.yaml
#	stack install $(profiling) --flag phylocomgraph:build-cpp-files
	stack install $(profiling) --fast --ghc-options="-fprof-cafs -rtsopts=all -O0"

# Builds with profiling enabled
stack-build-test: phylocomgraph.cabal stack.yaml
	stack build --test --ta "--rerun-update"

# Builds with profiling enabled
stack-build-test-failures: phylocomgraph.cabal stack.yaml
	stack build --test --ta "--rerun-filter=failures"

# Builds with profiling enabled
stack-build-test-new: phylocomgraph.cabal stack.yaml
	stack build --test --ta "--rerun-filter=new"

# install stylish haskell if not installed
install-stylish-haskell:
	which stylish-haskell || (stack install stylish-haskell)

format-code: install-stylish-haskell
	(./stylish.sh)

# install hlint if not installed
install-hlint:
	which hlint || (stack install hlint)

run-linter: install-hlint format-code
	hlint lib src test app

# Copies documentation director to local scope
copy-haddock: set-dir-variables
	rm -rf doc/haddock/*
	rm -f  doc/haddock.html
	mkdir doc/haddock/phylocomgraph && cp -r .stack-work/dist/$(DIR_ONE)/$(DIR_TWO)/doc/html/phylocomgraph doc/haddock/phylocomgraph
	for lib in $(sub-libs); do \
	  mkdir doc/haddock/$$lib && cp -r lib/$$lib/.stack-work/dist/$(DIR_ONE)/$(DIR_TWO)/doc/html/$$lib doc/haddock/$$lib; \
	done
	ln -s haddock/phylocomgraph/phylocomgraph/index.html doc/haddock.html

# Sets up variables of path names that are subject to change.
# Finds the most recently modified file in the directory
set-dir-variables:
	$(eval DIR_ONE = $(shell ls -t .stack-work/dist/            | head -n 1))
	$(eval DIR_TWO = $(shell ls -t .stack-work/dist/$(DIR_ONE)/ | head -n 1))
	@true

# Cleans up artefact files after a build
clean: phylocomgraph.cabal stack.yaml
	stack clean
	for dir in $(code-dirs); do \
	  find $$dir -type f -name '*.o'  -delete; \
	  find $$dir -type f -name '*.hi' -delete; \
	  find $$dir -type f -name '*.*~' -delete; \
	  find $$dir -type f -name '#*.*' -delete; \
	done

# Calls other make files to pre-process FFI files
ffi-code-cleaning: ffi/Analysis/Parsimony/Binary/SequentialAlign/makefile
	$(MAKE) -C ffi/Analysis/Parsimony/Binary/SequentialAlign

# Legacy cabal build option
cabal-build: phylocomgraph.cabal
	cabal install --dependencies-only && cabal configure --enable-tests --enable-profiling && cabal build && cabal haddock --executables --html --hyperlink-source && cabal test

# Legacy cabal build option
cabal-sandbox: phylocomgraph.cabal
	cabal update && cabal sandbox delete && cabal sandbox init && cabal install --dependencies-only

