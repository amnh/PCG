# Haskell C++ FFI example

To build this, do the following:

Make the C++ test library (the `make install` step installs it to
`/usr/local/c++-ffi-example` which you can clean up when you're done):

```
cd c++-lib
make
sudo make install
```

Make the Haskell library and test program:

```
cd ..
cabal configure
cabal build
```

Run the test program (setting up the shared library load path to pick
up the C++ library):

```
export LD_LIBRARY_PATH=/usr/local/c++-ffi-example/lib:$LD_LIBRARY_PATH
./dist/build/tst-prog/tst-prog
```
