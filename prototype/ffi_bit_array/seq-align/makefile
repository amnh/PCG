### Note that two calls to gcc to create .o files are needed, followed by two calls to compile

all : bit-ops

clean :
	rm -f *.o
	rm -f *.out

bit-ops : bitArrayExampleC.c bitArrayExampleC.h dynamicCharacterOperations.c dynamicCharacterOperations.h
	rm -f *.o
	gcc -c bitArrayExampleC.c dynamicCharacterOperations.c
	gcc bitArrayExampleC.o dynamicCharacterOperations.o

cost-matrix: costMtxTest


# compiler flags:
#  -g    adds debugging information to the executable file
#  -Wall turns on most, but not all, compiler warnings
costMtxTest: costMtxTest.cpp costMatrix.h costMatrix.cpp dynamicCharacterOperations.h dynamicCharacterOperations.c
	g++ -g -Wall -o costMtxTest costMtxTest.cpp