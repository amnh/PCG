### Note that two calls to gcc to create .o files are needed, followed by two calls to compile

all : bit-ops

clean :
	rm -f *.o

bit-ops : myComplexBitArrayTestC.c myComplexBitArrayTestC.h dynamicCharacterOperations.c dynamicCharacterOperations.h
	rm -f *.o
	gcc -c myComplexBitArrayTestC.c dynamicCharacterOperations.c
	gcc myComplexBitArrayTestC.o dynamicCharacterOperations.o
