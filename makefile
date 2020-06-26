SHELL=/bin/bash

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

code-dirs     = app bench test $(shell find . -maxdepth 2 -type d -name "pcg-*") 

sub-libs      = pcg-file-parsers pcg-language pcg-utility

# file paths

cabal-pcg-path = dist-newstyle/build/x86_64-linux/ghc-8.10.1/phylogenetic-component-graph-0.1.0.1/x/pcg/build/pcg/pcg


# Target aliases for easy CLI use
################################################################################

# Default build target
all: cabal-standard-build
#all: stack-build-profiling

# Rebuilds with profiling
prof: cabal-build-profiling

# Builds fast as possible
quick: cabal-build-quick

# Rebuilds as fast as possible
rebuild: quick

# Clean then rebuild
rebuild-full: clean rebuild

# Clean then rebuild outputting core
core: clean cabal-build-core

# Clean then build with the llvm backend
llvm: clean cabal-build-llvm

# Rebuilds with optimizations and runs tests
test: cabal-test

# Re-builds project then runs only integration tests
test-integration: cabal-build-test-integration

# Re-builds project then runs only unit tests
test-unit: cabal-build-test-unit

test-failures: cabal-build-test-failures

test-new: cabal-build-test-new

test-golden-new: cabal-build-test-golden-new

# Runs linter

lint: run-linter

# Makes hoogle server

hoogle: cabal-hoogle-server


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

stack-setup: phylogenetic-component-graph.cabal stack.yaml
	stack setup

# Builds with no extra generated features and no optimizations
stack-build-quick: phylogenetic-component-graph.cabal stack.yaml
	stack build --fast

# Builds with profiling enabled using a different work directory to cache
# built dependencies.
stack-build-profiling: phylogenetic-component-graph.cabal stack.yaml
#	stack install $(profiling) --flag phylogenetic-component-graph:build-cpp-files
	stack install $(profiling) --work-dir=".stack-work-profile" --fast --ghc-options="-fprof-cafs -rtsopts=all -O0"


# Builds outputting simplified core files (without newtype coercions)
stack-build-core: phylogenetic-component-graph.cabal stack.yaml
	stack build --ghc-options="-ddump-simpl -dsupress-coercions"

# Builds with the llvm backend
stack-build-llvm: phylogenetic-component-graph.cabal stack.yaml
	stack build --ghc-options="-fllvm"

# Builds tests and updates log of tests that have been run
stack-build-test: phylogenetic-component-graph.cabal stack.yaml
	stack build --test --ta "--rerun-update"

# Builds and runs integration tests after a standard build.
stack-build-test-integration: phylogenetic-component-graph.cabal stack.yaml stack-standard-build
	stack build phylogenetic-component-graph:test:integration-tests

# Builds and runs unit tests after a standard build.
stack-build-test-unit: phylogenetic-component-graph.cabal stack.yaml stack-standard-build
	stack build phylogenetic-component-graph:test:unit-tests

# Builds tests and re-runs those that failed
stack-build-test-failures: phylogenetic-component-graph.cabal stack.yaml
	stack build --test --ta "--rerun-filter=failures"

# Builds tests and runs those that are not in the log
stack-build-test-new: phylogenetic-component-graph.cabal stack.yaml
	stack build --test --ta "--rerun-filter=new"

# Builds only integration tests and generates new golden files
stack-build-test-golden-new: phylogenetic-component-graph.cabal stack.yaml
	stack build phylogenetic-component-graph:test:integration-tests --ta "--accept"


# Builds haddock documentation searchable by locally hosted hoogle
stack-hoogle-server:  phylogenetic-component-graph.cabal stack.yaml
	stack hoogle --server


################################################################################
##############################  Cabal Definitions ##############################
################################################################################

# Builds with useful features for a standard user of the package
cabal-standard-build: cabal-setup
	cabal new-build
	$(copy-executable)

cabal-refresh:
	   rm -rf dist-newstyle && cabal new-clean  && cabal new-configur && cabal new-build

# Builds with all useful options for package power users
cabal-full-build: clean cabal-setup cabal-build-prof

# TO DO: fix this command to upgrade to at least cabal 2.4 if the user has not
install-cabal:
	(cabal new-update && cabal new-install cabal-install)


cabal-setup: phylogenetic-component-graph.cabal cabal.project
	cabal new-configure --project-file=cabal.project --enable-library-profiling --enable-executable-profiling --enable-tests --with-compiler=ghc-8.10.1 --allow-newer

# Builds with no extra generated features and no optimizations
cabal-build-quick: phylogenetic-component-graph.cabal cabal.project
	cabal new-build --ghc-options="-O0"

# Builds with profiling enabled
cabal-build-profiling: phylogenetic-component-graph.cabal cabal.project
	cabal new-build --enable-profiling --profiling-detail=all-functions --ghc-options="-O0 -fprof-cafs -rtsopts=all"
	$(copy-executable)

# Builds outputting simplified core files (without newtype coercions)
cabal-build-core: phylogenetic-component-graph.cabal cabal.project
	cabal new-build --ghc-options="-ddump-simpl -dsupress-coercions"

# Builds with the llvm backend
cabal-build-llvm: phylogenetic-component-graph.cabal cabal.project
	cabal new-build --ghc-options="-fllvm"

# Note: --test-option will be reimplemented in cabal new in cabal 3.0
# and so we can replace these solutions for how to pass test-flags
# when we upgrade to the next cabal.

# Builds tests and updates log of tests that have been run
cabal-test: phylogenetic-component-graph.cabal cabal.project cabal-test-unit cabal-test-integration
	cabal new-run test:unit-tests                  --enable-tests -- "--rerun-update"
	cabal new-run test:pcg-utility-test-suite      --enable-tests -- "--rerun-update"
	cabal new-run test:pcg-file-parsers-unit-tests --enable-tests -- "--rerun-update"
	cabal new-run test:alphabet-test-suite         --enable-tests -- "--rerun-update"
	cabal new-run test:evaluation-test-suite       --enable-tests -- "--rerun-update"
	cabal new-run test:tcm-test-suite              --enable-tests -- "--rerun-update"
	cabal new-run test:integration-tests           --enable-tests -- "--rerun-update"

# Builds and runs integration tests after a standard build.
cabal-test-integration: phylogenetic-component-graph.cabal cabal.project
	cabal new-run test:integration-tests -- "--rerun-update"

# Builds and runs unit tests after a standard build.
cabal-test-unit: phylogenetic-component-graph.cabal cabal.project
	cabal new-run test:unit-tests -- "--rerun-update"
#	cabal new-run test:unit-tests --enable-profiling --enable-executable-profiling --ghc-options="-O0 -fprof-cafs -rtsopts=all" "+RTS -xc -RTS"

# Builds tests and re-runs those that failed
cabal-test-failures: phylogenetic-component-graph.cabal cabal.project
	cabal new-run test:unit-tests -- "--rerun-filter=failures"
	cabal new-run test:integration-tests -- "--rerun-filter=failures"


# Builds tests and runs those that are not in the log
cabal-test-new: phylogenetic-component-graph.cabal cabal.project
	cabal new-run test:unit-tests -- "--rerun-filter=new"
	cabal new-run test:integration-tests -- "--rerun-filter=new"


# Builds only integration tests and generates new golden files
cabal-test-golden-new: phylogenetic-component-graph cabal.project
	cabal new-run test:integration-tests -- "--accept"


# Builds haddock documentation searchable by locally hosted hoogle
cabal-hoogle-server:  phylogenetic-component-graph.cabal cabal.project
	hoogle server --local

cabal-build-hoogle: phylogenetic-component-graph.cabal cabal.project install-hoogle
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
	@echo -n "[?] Formatting code..."
	@find $(code-dirs) -type f -name "*.hs" | while read fname; do \
	  stylish-haskell -i "$$fname"; \
	done
	@echo -n -e "\33[2K\r"
	@echo "[✓] Formatting complete!"

run-linter: install-hlint install-weeder format-code run-hlint
#	weeder . --build

run-hlint: 
	hlint --no-exit-code $(code-dirs)

# Copies documentation director to local scope
copy-haddock: set-dir-variables
	rm -rf doc/haddock/*
	rm -f  doc/haddock.html
	mkdir doc/haddock/phylogenetic-component-graph && cp -r .stack-work/dist/$(DIR_ONE)/$(DIR_TWO)/doc/html/phylogenetic-component-graph doc/haddock/phylogenetic-component-graph
	for lib in $(sub-libs); do \
	  mkdir doc/haddock/$$lib && cp -r lib/$$lib/.stack-work/dist/$(DIR_ONE)/$(DIR_TWO)/doc/html/$$lib doc/haddock/$$lib; \
	done
	ln -s haddock/phylogenetic-component-graph/phylogenetic-component-graph/index.html doc/haddock.html

# Sets up variables of path names that are subject to change.
# Finds the most recently modified file in the directory
set-dir-variables:
	$(eval DIR_ONE = $(shell ls -t .stack-work/dist/            | head -n 1))
	$(eval DIR_TWO = $(shell ls -t .stack-work/dist/$(DIR_ONE)/ | head -n 1))
	@true

# Cleans up artefact files after a build
clean: phylogenetic-component-graph.cabal stack.yaml
	stack clean
	cabal new-clean
	@echo -n "[X] Cleaning directories of junk files..."
	@for dir in $(code-dirs); do \
	  find $$dir -type f -name '*.o'           -delete; \
	  find $$dir -type f -name '*.hi'          -delete; \
	  find $$dir -type f -name '*.*~'          -delete; \
	  find $$dir -type f -name '#*.*'          -delete; \
	  find $$dir -type f -name 'log.err'       -delete; \
	  find $$dir -type f -name 'log.out'       -delete; \
	  find $$dir -type f -name '*dump\-hi*'    -delete; \
	  find $$dir -type f -name '*dump\-simpl*' -delete; \
	  find $$dir -type d -name '.stack-work*'  -print0 | xargs -0 rm -rf; \
	done
	@echo -n -e "\33[2K\r"
	@echo "[✓] Cleaning complete!"

code-lines:
	loc --exclude 'css|html|js|makefile|md|tex|sh|yaml' --sort lines

# Legacy cabal build option
cabal-build: phylogenetic-component-graph.cabal
	cabal install --dependencies-only && cabal configure --enable-tests --enable-profiling && cabal build && cabal haddock --executables --html --hyperlink-source && cabal test

# Legacy cabal build option
cabal-sandbox: phylogenetic-component-graph.cabal
	cabal update && cabal sandbox delete && cabal sandbox init && cabal install --dependencies-only

# command to copy executable created by cabal-new
copy-executable = mkdir -p "bin" && cp  $(cabal-pcg-path) ./bin
