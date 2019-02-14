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

# file paths

cabal-pcg-path = dist-newstyle/build/x86_64-linux/ghc-8.6.3/phylocomgraph-0.1.0.1/x/pcg/build/pcg/pcg


# Target aliases for easy CLI use
################################################################################

# Default build target
all: cabal-standard-build
#all: stack-build-profiling

# Rebuilds with profiling
prof: stack-build-profiling

# Builds fast as possible
quick: stack-build-quick

# Rebuilds as fast as possible
rebuild: quick

# Clean then rebuild
rebuild-full: clean rebuild

# Clean then rebuild outputting core
core: clean stack-build-core

# Clean then build with the llvm backend
llvm: clean stack-build-llvm

# Rebuilds with optimizations and runs tests
test: stack-build-test

# Re-builds project then runs only integration tests
test-integration: stack-build-test-integration

# Re-builds project then runs only unit tests
test-unit: stack-build-test-unit

test-failures: stack-build-test-failures

test-new: stack-build-test-new

test-golden-new: stack-build-test-golden-new

# Runs linter

lint: run-linter

# Makes hoogle server

hoogle: stack-hoogle-server


# Target Definitions
################################################################################


################################################################################
##############################  Stack Definitions ##############################
################################################################################

# Builds with useful features for a standard user of the package
stack-standard-build: install-stack stack-setup
	stack install $(haddock)

# Builds with all useful options for package power users
stack-full-build: install-stack clean stack-setup stack-build-prof

# Upgrade stack if installed or install stack if not installed
install-stack:
	which stack || (cabal update && cabal install stack)

stack-setup: phylocomgraph.cabal stack.yaml
	stack setup

# Builds with no extra generated features and no optimizations
stack-build-quick: phylocomgraph.cabal stack.yaml
	stack build --fast

# Builds with profiling enabled using a different work directory to cache
# built dependencies.
stack-build-profiling: phylocomgraph.cabal stack.yaml
#	stack install $(profiling) --flag phylocomgraph:build-cpp-files
	stack install $(profiling) --work-dir=".stack-work-proifle" --fast --ghc-options="-fprof-cafs -rtsopts=all -O0"

# Builds outputting simplified core files (without newtype coercions)
stack-build-core: phylocomgraph.cabal stack.yaml
	stack build --ghc-options="-ddump-simpl -dsupress-coercions"

# Builds with the llvm backend
stack-build-llvm: phylocomgraph.cabal stack.yaml
	stack build --ghc-options="-fllvm"

# Builds tests and updates log of tests that have been run
stack-build-test: phylocomgraph.cabal stack.yaml
	stack build --test --ta "--rerun-update"

# Builds and runs integration tests after a standard build.
stack-build-test-integration: phylocomgraph.cabal stack.yaml standard-build
	stack build phylocomgraph:test:integration-tests

# Builds and runs unit tests after a standard build.
stack-build-test-unit: phylocomgraph.cabal stack.yaml standard-build
	stack build phylocomgraph:test:unit-tests

# Builds tests and re-runs those that failed
stack-build-test-failures: phylocomgraph.cabal stack.yaml
	stack build --test --ta "--rerun-filter=failures"

# Builds tests and runs those that are not in the log
stack-build-test-new: phylocomgraph.cabal stack.yaml
	stack build --test --ta "--rerun-filter=new"

# Builds only integration tests and generates new golden files
stack-build-test-golden-new: phylocomgraph.cabal stack.yaml
	stack build phylocomgraph:test:integration-tests --ta "--accept"


# Builds haddock documentation searchable by locally hosted hoogle
stack-hoogle-server:  phylocomgraph.cabal stack.yaml
	stack hoogle --server


################################################################################
##############################  Cabal Definitions ##############################
################################################################################

# Builds with useful features for a standard user of the package
cabal-standard-build: cabal-setup
	cabal new-build
	$(copy-executable)

# Builds with all useful options for package power users
cabal-full-build: clean cabal-setup cabal-build-prof

# TO DO: fix this command to upgrade to at least cabal 2.4 if the user has not
install-cabal:
	(cabal new-update && cabal new-install cabal-install)

cabal-setup: phylocomgraph.cabal cabal.project
	cabal new-configure --project-file=cabal.project

# Builds with no extra generated features and no optimizations
cabal-build-quick: phylocomgraph.cabal cabal.project
	cabal new-build --ghc-options="-O0"

# Builds with profiling enabled
cabal-build-profiling: phylocomgraph.cabal cabal.project
	cabal new-build --enable-profiling --profiling-detail=all-functions --ghc-options="-O0 -fprof-cafs -rtsopts=all"
	$(copy-executable)

# Builds outputting simplified core files (without newtype coercions)
cabal-build-core: phylocomgraph.cabal cabal.project
	cabal new-build --ghc-options="-ddump-simpl -dsupress-coercions"

# Builds with the llvm backend
cabal-build-llvm: phylocomgraph.cabal cabal.project
	cabal new-build --ghc-options="-fllvm"

# Note: --test-option will be reimplemented in cabal new in cabal 3.0
# and so we can replace these solutions for how to pass test-flags
# when we upgrade to the next cabal.

# Builds tests and updates log of tests that have been run
cabal-test: phylocomgraph.cabal cabal.project cabal-test-unit cabal-test-integration
	cabal new-run test:unit-tests -- "--rerun-update"

# Builds and runs integration tests after a standard build.
cabal-test-integration: phylocomgraph.cabal cabal.project
	cabal new-run test:integration-tests -- "--rerun-update"

# Builds and runs unit tests after a standard build.
cabal-test-unit: phylocomgraph.cabal cabal.project
	cabal new-run test:unit-tests -- "--rerun-update"

# Builds tests and re-runs those that failed
cabal-test-failures: phylocomgraph.cabal cabal.project
	cabal new-run test:unit-tests -- "--rerun-filter=failures"
	cabal new-run test:integration-tests -- "--rerun-filter=failures"


# Builds tests and runs those that are not in the log
cabal-test-new: phylocomgraph.cabal cabal.project
	cabal new-run test:unit-tests -- "--rerun-filter=new"
	cabal new-run test:integration-tests -- "--rerun-filter=new"


# Builds only integration tests and generates new golden files
cabal-test-golden-new: phylocomgraph.cabal cabal.project
	cabal new-run test:integration-tests -- "--accept"


# Builds haddock documentation searchable by locally hosted hoogle
cabal-hoogle-server:  phylocomgraph.cabal cabal.project
	hoogle server --local

cabal-build-hoogle: phylocomgraph.cabal cabal.project install-hoogle
	cabal new-haddock --haddock-hoogle
	hoogle generate --local="."
	hoogle server --local

install-hoogle:
	which hoogle || (cabal v2-install hoogle)


### The code cleanliness section
### Installs hlint, stylish-haskell, and weeder
### Formats code, then reports and cleanliness issues
### NOTE: Should be run before merging into master!!!

# install hlint if not installed
install-hlint:
	which hlint           || (stack install hlint           --resolver=lts)

# install stylish haskell if not installed
install-stylish-haskell:
	which stylish-haskell || (stack install stylish-haskell --resolver=lts)

# install weeder if not installed
install-weeder:
	which weeder          || (stack install weeder          --resolver=lts)

format-code: install-stylish-haskell
	(./stylish.sh)

run-linter: install-hlint install-weeder format-code
	hlint app ffi lib src test utils
	weeder . --build

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
	cabal new-clean
	for dir in $(code-dirs); do \
	  find $$dir -type f -name '*.o'           -delete; \
	  find $$dir -type f -name '*.hi'          -delete; \
	  find $$dir -type f -name '*.*~'          -delete; \
	  find $$dir -type f -name '#*.*'          -delete; \
	  find $$dir -type f -name 'log.err'       -delete; \
	  find $$dir -type f -name 'log.out'       -delete; \
	  find $$dir -type f -name '*dump\-hi*'    -delete; \
	  find $$dir -type f -name '*dump\-simpl*' -delete; \
	done

# Calls other make files to pre-process FFI files
ffi-code-cleaning: ffi/Analysis/Parsimony/Binary/SequentialAlign/makefile
	$(MAKE) -C ffi/Analysis/Parsimony/Binary/SequentialAlign


# command to copy executable created by cabal-new
copy-executable = mkdir -p "bin" && cp  $(cabal-pcg-path) ./bin
