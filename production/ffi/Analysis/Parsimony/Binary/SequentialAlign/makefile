### Note I changed original gcc calls to clang, because ghc calls ld which relies on clang.
## Remember when doing this to have .hsc file named differently from .c file, because otherwise
## .o files overwrite each other and you end up with weird linking errors.

all : my_ffi

clean :
	rm -f *.o *.hi
	rm -f seqAlignForHaskell.c

############ more simple case, structs, pointers, etc.
# my_ffi : mySimpleTestC.c mySimpleTestC.h mySimpleFfi.hs
# 	clang -c mySimpleTestC.c
# 	ghc -O --make mySimpleFfi.hs mySimpleTestC.o

############ more complex case, structs, pointers, etc.
# In this case, first call hsc2hs to build .hs file from .hsc file.
# .hsc file is weird mix of Haskell and C.
# my_ffi : myComplexTestC.c myComplexTestC.h myComplexFFI.hsc
# 	rm -f myComplex.hs
# 	hsc2hs myComplexFFI.hsc
# 	clang -c myComplexTestC.c
# 	ghc -O --make myComplexFFI.hs myComplexTestC.o


#################### Yu's case, structs, pointers, etc.
# Extremely similar to complex case, above.
# may need to make use of  --cross-compile in final builds
my_ffi : seqAlign_ffi.c seqAlign_ffi.h FFI.hsc processSeqAlignCCode.py
	python3 processSeqAlignCCode.py
	hsc2hs FFI.hsc
	clang -c seqAlignForHaskell.c
	ghc -O --make FFI.hs seqAlignForHaskell.o
