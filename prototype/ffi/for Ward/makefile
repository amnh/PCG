### Note I changed original gcc calls to clang, because ghc calls ld which relies on clang.
## Remember when doing this to have .hsc file named differently from .c file, because otherwise
## .o files overwrite each other and you end up with weird linking errors.

clean :
	rm -f *.o *.hi
	rm -f yuAlignForHaskell.c

############ more simple case, structs, pointers, etc.
simple_ffi : mySimpleTestC.c mySimpleTestC.h mySimpleFfi.hs
	clang -c mySimpleTestC.c
	ghc -O --make mySimpleFfi.hs mySimpleTestC.o

########## more complex case, structs, pointers, etc.
In this case, first call hsc2hs to build .hs file from .hsc file.
.hsc file is weird mix of Haskell and C.
complex_ffi : myComplexTestC.c myComplexTestC.h myComplexFFI.hsc
	rm -f myComplex.hs
	hsc2hs myComplexFFI.hsc
	clang -c myComplexTestC.c
	ghc -O --make myComplexFFI.hs myComplexTestC.o