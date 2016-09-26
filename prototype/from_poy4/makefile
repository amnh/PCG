### Note that multiple calls to gcc to create .o files are needed, followed by multiple additional calls for linking and final compilation

all : align

clean :
	rm -f *.o *.hi

# to run POY alignment C code
align : driver.c algn.c algn.h avl.c avl.h cm.c cm.h matrices.c matrices.h seq.c seq.h zarr.c zarr.h POYalign.hsc
	rm -f POYalign.hs
	rm -f *.o
	gcc -c algn.c avl.c cm.c matrices.c seq.c zarr.c
	ghc -O --make POYalign.hs algn.c

########## more complex case, structs, pointers, etc.
# In this case, first call hsc2hs to build .hs file from .hsc file.
# .hsc file is weird mix of Haskell and C.
complex_ffi : myComplexTestC.c myComplexTestC.h myComplexFFI.hsc
	rm -f myComplex.hs
	hsc2hs myComplexFFI.hsc
	clang -c myComplexTestC.c
	ghc -O --make myComplexFFI.hs myComplexTestC.o

just_c_code : driver.c algn.c algn.h avl.c avl.h cm.c cm.h matrices.c matrices.h seq.c seq.h zarr.c zarr.h
	rm -f *.o
	gcc -static -g -c algn.c avl.c cm.c matrices.c seq.c zarr.c
	gcc -static -g driver.c algn.o avl.c cm.o matrices.o seq.o zarr.o
