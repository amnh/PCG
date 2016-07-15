### Note that two calls to gcc to create .o files are needed, followed by two calls to compile

all : bit-ops

clean :
	rm -f *.o

bit-ops : myComplexBitArrayTestC.c myComplexBitArrayTestC.h myComplexBitArrayFFI.hs
	rm -f myComplexBitArrayFFI.hs
	hsc2hs myComplexBitArrayFFI.hsc
	gcc -c myComplexBitArrayTestC.c
	ghc -O --make myComplexBitArrayFFI.hs myComplexBitArrayTestC.o
