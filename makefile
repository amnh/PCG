SHELL=/bin/bash
CHECKMARK        := "\033[0;32m\xE2\x9C\x94\xE2\x83\x9E\033[0m"
package-name     := phylogenetic-component-graph
cabal-depends    := $(package-name).cabal cabal.project

bin-dir          := ./bin
cfg-dir          := ./config
hie-dir          := ./.hie
tmp-dir          := ./.temp

diff-log         := $(tmp-dir)/diffs.log
formatter        := $(bin-dir)/stylish-haskell
formatter-flags  := --config $(cfg-dir)/.stylish-haskell.yaml
linter-flags     := --hint=$(cfg-dir)/.hlint.yaml
lower-bound-file := $(cfg-dir)/lower-bounds.project
stan-exe         := $(bin-dir)/stan
stan-flags       := --config-file=$(cfg-dir)/.stan.toml --hiedir=$(hie-dir)
weeder-exe       := $(bin-dir)/weeder
weeder-flags     := --config $(cfg-dir)/weeder.dhall --hie-directory $(hie-dir)

code-dirs  := app bench test $(shell find lib -maxdepth 1 -type d -name "*" | tail -n +2 | tr '\n' ' ')
code-files := $(shell find $(code-dirs) -type f -name "*.hs" | tr '\n' ' ')


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
cabal-suffix        = 2>&1 | grep -v -f $(cfg-dir)/.ignored-warnings

# Actions to perform before building dependencies and project
# Seperating the "pre-deploy" from "deploy" allows for CI caching the results
# of the pre-deployment configuration.
cabal-setup: $(cabal-depends)
	cabal update
	cabal clean
	cabal configure $(with-compiler-flags) $(cabal-haddock-flags) $(cabal-total-targets) $(cabal-suffix)
	cabal freeze    $(with-compiler-flags) $(cabal-haddock-flags) $(cabal-total-targets) $(cabal-suffix)

# Actions for building all binaries, benchmarks, and test-suites
cabal-build: $(cabal-depends)
	cabal build     $(with-compiler-flags) $(cabal-haddock-flags) $(cabal-total-targets) --only-dependencies $(cabal-suffix)
	cabal build     $(with-compiler-flags) $(cabal-haddock-flags) $(cabal-total-targets) $(cabal-suffix)

# Actions for building all binaries, benchmarks, and test-suites
cabal-deploy: cabal-build make-bin-dir
	cabal install   $(with-compiler-flags) $(cabal-haddock-flags) $(cabal-install-flags) $(cabal-suffix)

cabal-clean: cleanup-cabal-default cleanup-cabal-build-artifacts cleanup-HIE-files

# Builds with no extra generated features and no optimizations
cabal-quick-prof: make-bin-dir $(cabal-depends)
	cabal build   $(cabal-fast-prof) $(cabal-total-targets) --only-dependencies $(cabal-suffix)
	cabal build   $(cabal-fast-prof) $(cabal-total-targets) $(cabal-suffix)
	cabal install $(cabal-fast-prof) $(cabal-install-flags) $(cabal-suffix)


################################################################################
###   CI definitions
################################################################################

# Check that the project builds with the specified lower bounds
lower-bounds-check: $(lower-bound-file) lower-bounds-check-setup lower-bounds-check-deploy

lower-bounds-check-setup:  cabal-haddock-flags += --project-file=$(lower-bound-file)
lower-bounds-check-setup:  $(lower-bound-file) cabal-setup

lower-bounds-check-deploy: cabal-haddock-flags += --project-file=$(lower-bound-file)
lower-bounds-check-deploy: $(lower-bound-file) cabal-deploy


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

dead-code-check-setup: install-weeder
	$(weeder-exe) --version
	@$(MAKE) --no-print-directory hie-setup

dead-code-check-deploy: hie-deploy
	$(weeder-exe) $(weeder-flags)

hlint-check:
	@curl -sSL https://raw.github.com/ndmitchell/hlint/master/misc/run.sh | sh -s $(code-dirs) -j $(linter-flags)

format-code:
	@echo -n " ☐  Formatting code..."
	@$(MAKE) --no-print-directory run-stylish-haskell in-place=true
	@echo -e "\r\033[K" $(CHECKMARK) " Formatting code complete!"

formatting-check: 
	@echo -n "Checking code for required formatting"
	@$(MAKE) --no-print-directory run-stylish-haskell
	@if find . -empty -wholename $(diff-log) | grep "^" &> /dev/null; \
	then \
	  echo -e "\r\033[KCode is properly formatted!"; \
	else \
	  echo -e "\nFormatting required!\nFix by running the following:\n\n> make format-code\n" \
	  rm -f $(diff-log) \
	  sleep 1 \
	  exit 1; \
	fi

run-stylish-haskell: install-stylish-haskell make-tmp-dir
ifdef in-place
	@$(eval formatter-flags += -i)
endif
	@$(eval temp-file := $(tmp-dir)/styled.temp)
	@rm -f $(diff-log)
	@touch $(diff-log)
	@echo $(code-files) | tr ' ' '\n' | while read fname; do \
	  $(formatter) $(formatter-flags) "$$fname" > $(temp-file); \
	  diff "$$fname" $(temp-file) >> $(diff-log) || true; \
	done
	@rm -f $(temp-file) || true

spelling-check: install-codespell
	@$(eval spell-check-flags := -I $(cfg-dir)/.ignored-words -x $(cfg-dir)/.excluded-lines)
ifdef fix-spelling
	@$(eval spell-check-flags += --write-changes)
endif
	@echo "Checking code for misspellings"
	@codespell $(spell-check-flags) $(code-files)

static-analysis-check: static-analysis-check-setup static-analysis-check-deploy

static-analysis-check-setup: install-stan
	$(stan-exe) --version
	@$(MAKE) --no-print-directory hie-setup

static-analysis-check-deploy: hie-deploy make-tmp-dir
	@$(eval stan-log := $(tmp-dir)/.stan.log)
	@$(eval observations := $(tmp-dir)/.stan.observations.log)
	@rm -f $(stan-log)
	@rm -f $(observations)
	@touch $(stan-log)
	$(stan-exe) $(stan-flags) | tee $(stan-log)
	@grep ' ID:            ' $(stan-log) > $(observations) || true
	@rm -f $(stan-log)
	@find . -empty -wholename $(observations) | grep "^" &> /dev/null || ( \
	  echo -e "\nAnti-pattern observations found!\nCorrect the following $$(wc -l $(observations) | cut -d " " -f1) observations:\n" && \
	  echo -n " ┏" && printf '━%.0s' {1..52} && echo "━" && \
	  cat $(observations) && \
	  rm -f $(observations) && \
	  echo -en " ┗" && printf '━%.0s' {1..52} && echo "━\n" && \
	  sleep 1 && \
	  exit 1 \
	)

documentation-check: documentation-check-setup documentation-check-deploy

documentation-check-setup: cabal-setup

documentation-check-deploy: make-tmp-dir
	@$(eval docs-log := $(tmp-dir)/.haddock.log)
	@$(eval missing-docs := $(tmp-dir)/.missing.docs.log)
	@rm -f $(docs-log)
	@rm -f $(missing-docs)
	cabal build   $(with-compiler-flags) $(cabal-haddock-flags) $(cabal-total-targets) --only-dependencies $(cabal-suffix)
	cabal haddock $(with-compiler-flags) $(cabal-haddock-flags) $(cabal-total-targets) $(cabal-suffix) | tee $(docs-log)
	@grep '^[[:space:]]*[[:digit:] ][[:digit:]]% (' $(docs-log) > $(missing-docs) || true
	@rm -f $(docs-log)
	@(find . -empty -wholename $(missing-docs) | grep "^" &> /dev/null) || ( \
	  echo $(count) && \
	  echo -e "\nMissing documetation!\nThe following $$(wc -l $(missing-docs) | cut -d ' ' -f1) files have incomplete documetation coverage:\n" && \
	  cat $(missing-docs) && \
	  echo "" && \
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
	@rm -f cabal.project.local~?*
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
	@rm -fR $(tmp-dir)
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

install-stan: make-bin-dir
	@ls $(formatter) &> /dev/null || (\
	  echo "Downloading stan..." && \
	  cabal update && \
	  cabal install stan $(with-compiler-flags) $(cabal-install-flags) \
	)

install-stylish-haskell: make-bin-dir
	@ls $(formatter) &> /dev/null || (\
	  echo "Downloading stylish-haskell" && \
	  cabal update && \
	  cabal install stylish-haskell $(with-compiler-flags) $(cabal-install-flags) \
	)

install-weeder: make-bin-dir
	@ls $(weeder-exe) &> /dev/null || (\
	  echo "Downloading weeder..." && \
	  cabal update && \
	  cabal install weeder $(with-compiler-flags) $(cabal-install-flags) \
	)

make-bin-dir:
	@ls | grep $(bin-dir) || mkdir -p $(bin-dir)

make-tmp-dir:
	@ls | grep $(tmp-dir) || mkdir -p $(tmp-dir)

.PHONY: all build clean lint prof test
