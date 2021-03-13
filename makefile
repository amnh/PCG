SHELL=/bin/bash
CHECKMARK    := "\033[0;32m\xE2\x9C\x94\xE2\x83\x9E\033[0m"

bin-dir      := ./bin
hie-dir      := ./.hie
formatter    := $(bin-dir)/stylish-haskell

code-dirs  = app bench test $(shell find lib -maxdepth 1 -type d -name "*" | tail -n +2 | tr '\n' ' ')
code-files = $(shell find $(code-dirs) -type f -name "*.hs" | tr '\n' ' ')         


################################################################################
###   Target aliases for easy CLI use
################################################################################

# Clean up and build everything; default build target
all: cabal-clean cabal-setup cabal-deploy

build: prof

clean: cabal-clean clean-up-latex cleanup-log-files cleanup-junk-file

# Rebuilds with profiling
prof: cabal-quick-prof

lint: format-code spelling-check hlint-check documentation-check static-analysis-check dead-code-check


################################################################################
###   Reusable cabal build definitions
################################################################################

with-compiler-flags =
ifdef ghc
	with-compiler-flags := --with-compiler=ghc-$(ghc) --with-hc-pkg=ghc-pkg-$(ghc)
else
	with-compiler-flags := --with-compiler=ghc
endif

cabal-fast-prof     = --ghc-options="-O0" --enable-profiling --profiling-detail=all-functions --ghc-options="-O0 -fprof-cafs -rtsopts=all"
cabal-haddock-flags = --enable-documentation --haddock-all --haddock-hyperlink-source --haddock-internal
cabal-install-flags = --installdir=$(bin-dir) --overwrite-policy=always
cabal-total-targets = --enable-benchmarks --enable-tests
cabal-suffix        = 2>&1 | grep -v -f .ignored-warnings

# Actions to perform before building dependencies and project
# Seperating the "pre-deploy" from "deploy" allows for CI caching the results
# of the pre-deployment configuration.
cabal-setup:
	cabal update
	cabal clean
	cabal configure $(with-compiler-flags) $(cabal-haddock-flags) $(cabal-total-targets) $(cabal-suffix)
	cabal freeze    $(with-compiler-flags) $(cabal-haddock-flags) $(cabal-total-targets) $(cabal-suffix)

# Actions for building all binaries, benchmarks, and test-suites
cabal-build:
	cabal build     $(with-compiler-flags) $(cabal-haddock-flags) $(cabal-total-targets) --only-dependencies $(cabal-suffix)
	cabal build     $(with-compiler-flags) $(cabal-haddock-flags) $(cabal-total-targets) $(cabal-suffix)

# Actions for building all binaries, benchmarks, and test-suites
cabal-deploy: cabal-build
	cabal install   $(with-compiler-flags) $(cabal-haddock-flags) $(cabal-install-flags) $(cabal-suffix)

cabal-clean: cleanup-cabal-default cleanup-cabal-build-artifacts cleanup-HIE-files

# Builds with no extra generated features and no optimizations
cabal-quick-prof: phylogenetic-component-graph.cabal cabal.project
	cabal build   $(cabal-fast-prof) $(cabal-total-targets) --only-dependencies $(cabal-suffix)
	cabal build   $(cabal-fast-prof) $(cabal-total-targets) $(cabal-suffix)
	cabal install $(cabal-fast-prof) $(cabal-install-flags) $(cabal-suffix)


################################################################################
###   CI definitions
################################################################################

# Check that the project builds with the specified lower bounds
lower-bounds-check: lower-bounds.project lower-bounds-check-setuplower-bounds-check-deploy

lower-bounds-check-setup:  cabal-haddock-flags += --project-file=lower-bounds.project
lower-bounds-check-setup:  lower-bounds.project cabal-setup

lower-bounds-check-deploy: cabal-haddock-flags += --project-file=lower-bounds.project
lower-bounds-check-deploy: lower-bounds.project cabal-deploy


# Check that all unit tests pass
run-unit-tests: unit-tests-setup unit-tests-verification

unit-tests-setup: cabal.project cabal-setup

unit-tests-verification: cabal.project
	cabal build --enable-tests $(with-compiler-flags)
	cabal test


# Check that all example input files parse
run-file-tests: file-tests-setup file-tests-verification

file-tests-setup: cabal.project cabal-setup

file-tests-verification:
	cabal run file-tests


# Check that all example input files parse
run-integration-tests: integration-tests-setup integration-tests-verification

integration-tests-setup: cabal.project cabal-setup

integration-tests-verification:
	cabal install exe:pcg --overwrite-policy=always --installdir=./bin
	cabal run integration-tests


# Check that the project builds with the specified lower bounds
compilation-check: compilation-check-setup compilation-check-deploy

compilation-check-setup:  cabal-haddock-flags += $(with-compiler-flags)
compilation-check-setup:  cabal.project cabal-setup

compilation-check-deploy: cabal-haddock-flags += $(with-compiler-flags)
compilation-check-deploy: cabal.project cabal-deploy


################################################################################
###   Code hygeine
################################################################################

dead-code-check: dead-code-check-setup dead-code-check-deploy

dead-code-check-setup:
	@echo "Downloading weeder..."
	cabal update
	cabal install weeder $(with-compiler-flags) $(cabal-install-flags)
	$(bin-dir)/weeder --version
	@$(MAKE) --no-print-directory hie-setup

dead-code-check-deploy: hie-deploy
	cat weeder.dhall
	$(bin-dir)/weeder --config ./weeder.dhall --hie-directory $(hie-dir)

hlint-check:
	@curl -sSL https://raw.github.com/ndmitchell/hlint/master/misc/run.sh | sh -s $(code-dirs) -j

format-code: install-stylish-haskell
	@echo -n " ☐  Formatting code..."
	@find $(code-dirs) -type f -name "*.hs" | while read fname; do \
	  $(bin-dir)/stylish-haskell -i "$$fname"; \
	done
	@echo -e "\r\033[K" $(CHECKMARK) " Formatting code complete!"

formatting-check: install-stylish-haskell
	@echo "Checking code for required formatting"
	@$(eval diff-log := .diffs.log)
	@rm -f $(diff-log)
	@touch $(diff-log)
	@find $(code-dirs) -type f -name "*.hs" | while read fname; do \
	  $(bin-dir)/stylish-haskell "$$fname" > styled.temp; \
	  diff "$$fname" styled.temp >> $(diff-log); \
	done
	@rm -f styled.temp
	@find . -empty -name $(diff-log) | grep "^" &> /dev/null || ( \
	  echo -e "Formatting required!\nFix by running the following:\n\n> make format-code\n" && \
	  rm -f $(diff-log) && \
	  sleep 1 && \
	  exit 1 )

spelling-check: install-codespell
	@$(eval spell-check-flags := -I .ignored-words -x .excluded-lines)
ifdef fix-spelling
	@$(eval spell-check-flags += --write-changes)
endif
	@echo "Checking code for misspellings"
	@codespell $(spell-check-flags) $(code-files)

static-analysis-check: static-analysis-check-setup static-analysis-check-deploy

static-analysis-check-setup:
	@echo "Downloading stan..."
	cabal update
	cabal install stan $(with-compiler-flags) $(cabal-install-flags)
	$(bin-dir)/stan --version
	@$(MAKE) --no-print-directory hie-setup

static-analysis-check-deploy: hie-deploy
	@$(eval stan-log := .stan.log)
	@$(eval observations := .stan.observations.log)
	$(bin-dir)/stan | tee $(stan-log)
	@grep ' ID:            ' $(stan-log) > $(observations)
	@rm -f $(stan-log)
	@$(eval count := $(shell wc -l $(observations) | cut -d " " -f1))
	@find . -empty -name $(observations) | grep "^" &> /dev/null || ( \
	  echo -e "\nAnti-pattern observations found!\nCorrect the following" $(count) "observations:\n" && \
	  echo -n " ┏" && \
	  printf '━%.0s' {1..52} && \
	  echo "━" && \
	  cat $(observations) && \
	  rm -f $(observations) && \
	  sleep 1 && \
	  exit 1 \
	)

documentation-check: documentation-check-setup documentation-check-deploy

documentation-check-setup: cabal-setup

documentation-check-deploy:
	@$(eval docs-log := .haddock.log)
	@$(eval missing-docs := .missing.docs.log)
	@rm -f $(docs-log)
	@rm -f $(missing-docs)
	cabal build   $(with-compiler-flags) $(cabal-haddock-flags) $(cabal-total-targets) --only-dependencies $(cabal-suffix)
	cabal haddock $(with-compiler-flags) $(cabal-haddock-flags) $(cabal-total-targets) $(cabal-suffix) | tee $(docs-log)
	@grep '^  [[:digit:]][[:digit:]]% (' $(docs-log) > $(missing-docs)
	@rm -f $(docs-log)
	@$(eval count := $(shell wc -l $(missing-docs) | cut -d " " -f1))
	@find . -empty -name $(missing-docs) | grep "^" &> /dev/null || ( \
	  echo -e "\nMissing documetation!\nThe following" $(count) "files have incomplete documetation coverage:\n" && \
	  cat $(missing-docs) && \
	  rm -f $(missing-docs) && \
	  sleep 1 && \
	  exit 1 \
	)

hie-setup: compilation-check-setup cleanup-HIE-files

hie-deploy: compilation-check-deploy
	ls -ahlr $(hie-dir)

################################################################################
###   Cleaning
################################################################################

cleanup-cabal-default:
	@echo -n " ☐  Cleaning Cabal..."
	@cabal clean
	@echo -e "\r\033[K" $(CHECKMARK) " Cleaned Cabal defaults"

cleanup-cabal-build-artifacts:
	@echo -n " ☐  Cleaning extra Cabal build artifacts..."
	@rm -f lower-bounds.project?*
	@rm -f cabal.project?*
	@echo -e "\r\033[K" $(CHECKMARK) " Cleaned Cabal extras"

cleanup-HIE-files:
	@echo -n " ☐ Cleaning HIE files..."
	@rm -fR $(hie-dir)
	@echo -e "\r\033[K" $(CHECKMARK) " Cleaned HIE files"

cleanup-junk-file:
	@echo -n " ☐  Cleaning code directories of temporary files..."
	@for dir in $(code-dirs); do \
	  find $$dir -type f -name '*.o'           -delete; \
	  find $$dir -type f -name '*.hi'          -delete; \
	  find $$dir -type f -name '*.*~'          -delete; \
	  find $$dir -type f -name '#*.*'          -delete; \
	  find $$dir -type f -name '*#.*#'         -delete; \
	  find $$dir -type f -name 'log.err'       -delete; \
	  find $$dir -type f -name 'log.out'       -delete; \
	  find $$dir -type f -name '*dump\-hi*'    -delete; \
	  find $$dir -type f -name '*dump\-simpl*' -delete; \
	  find $$dir -type f -name '*.data.?*'     -delete; \
	  find $$dir -type f -name '*.dot.?*'      -delete; \
	  find $$dir -type f -name '*.xml.?*'      -delete; \
	done
	@echo -e "\r\033[K" $(CHECKMARK) " Cleaned temporary files"

cleanup-log-files:
	@echo -n " ☐ Cleaning log files..."
	@rm -fR .*.log*
	@echo -e "\r\033[K" $(CHECKMARK) " Cleaned log files"

clean-up-latex:
	@echo -n " ☐  Cleaning documentation directory of LaTeX artifacts..."
	@rm -f *.bbl
	@rm -f *.blg
	@rm -f *.synctex.gz
	@rm -f *.toc
	@echo -e "\r\033[K" $(CHECKMARK) " Cleaned LaTeX artifacts"

################################################################################
###   Miscellaneous
################################################################################

code-lines:
	loc --exclude 'css|html|js|makefile|md|tex|sh|yaml' --sort lines $(code-dirs)

install-codespell:
	@which codespell &> /dev/null || (\
	  echo "Downloading codespell" && \
	  pip3 install codespell \
	)

install-stylish-haskell:
	@ls $(formatter) &> /dev/null || (\
	  echo "Downloading stylish-haskell" && \
	  cabal update && \
	  cabal install stylish-haskell $(with-compiler-flags) $(cabal-install-flags) \
	)

.PHONY: all build clean lint prof test
