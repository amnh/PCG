# Installation of PCG

### Getting the source code
The source code for PCG can be obtained in a number of ways. It is publicly hosted on [GitHub](https://github.com/amnh/PCG/) which it can be freely downloaded. You can find the source code of the latest release [here](https://github.com/amnh/PCG/releases/latest). Alternatively, you can dowload the source code and checkout the latest release using [`git`](https://git-scm.com/).
```
git clone https://github.com/amnh/PCG 
cd PCG
git checkout $(git describe --tags $(git rev-list --tags --max-count=1)) # get the lastest release
```

### Getting `cabal`
PCG can be build using Haskell's `cabal` package management tool. See the [`cabal` website](https://www.haskell.org/cabal/) for getting `cabal` set up on you machine.

First, make sure that the `cabal` package index is up to date:
```
cabal update
```

### Build PCG:
```
cabal build pcg
```

### Build the haddock documentation:
```
cabal haddock all --haddock-hyperlink-source --haddock-internal
```
This will create haddock documentation with hyperlinks to PCG's dependencies. 

### Build and run the test suites
```
cabal test
```

### Build the run the integration tests
```
cabal run integration-tests
```
The integration test are different from the test suites. The test suites test small segments of code and check for *individually* correct behavior. The integration tests run the entire PCG program and check for *holistically* correct behavior. The integration tests will take much longer than the unit tests.
