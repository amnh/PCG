### Note that multiple calls to gcc to create .o files are needed, followed by multiple additional calls for linking and final compilation

all : align

clean :
	rm -f *.o *.hi *.out
	rm -rf *.dSYM
	rm -f alignment_driver

# to run POY alignment C code
align : driver.c algn.c algn.h avl.c avl.h costMatrix.c costMatrix.h \
		nwMatrices.c nwMatrices.h seq.c seq.h zarr.c zarr.h POYalign.hsc
	rm -f POYalign.hs
	rm -f *.o
	gcc -c algn.c avl.c costMatrix.c nwMatrices.c seq.c zarr.c
	ghc -O --make POYalign.hs algn.c

########## more complex case, structs, pointers, etc.
# In this case, first call hsc2hs to build .hs file from .hsc file.
# .hsc file is weird mix of Haskell and C.
externalCAlignment : myComplexTestC.c myComplexTestC.h externalCAlignment.hsc
	rm -f myComplex.hs
	hsc2hs myComplexFFI.hsc
	clang -c myComplexTestC.c
	ghc -O --make myComplexFFI.hs myComplexTestC.o

just_c_code : driver.c algn.c algn.h costMatrix.c costMatrix.h \
			   nwMatrices.c nwMatrices.h seq.c seq.h ukkCheckp.c ukkCheckp.h \
			   ukkCommon.c ukkCommon.h
	rm -f *.o
	gcc -g -c algn.c costMatrix.c nwMatrices.c seq.c ukkCheckp.c ukkCommon.c
	gcc -o alignment_driver -g driver.c algn.o costMatrix.o nwMatrices.o seq.o ukkCheckp.c ukkCommon.o
