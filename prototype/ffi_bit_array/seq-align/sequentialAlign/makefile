### Note that two calls to gcc to create .o files are needed, followed by two calls to compile

all : bit-ops

clean :
	rm -f *.o
	rm -f *.out

bit-ops : bitArrayExampleC.c bitArrayExampleC.h dynamicCharacterOperations.c dynamicCharacterOperations.h
	rm -f *.o
	gcc -std=c11 -c bitArrayExampleC.c dynamicCharacterOperations.c
	gcc -std=c11 bitArrayExampleC.o dynamicCharacterOperations.o

# compiler flags:
#  -g    adds debugging information to the executable file
#  -Wall turns on most, but not all, compiler warnings
costMtxTest: costMtxTest.cpp costMatrix.h costMatrix.cpp dynamicCharacterOperations.h dynamicCharacterOperations.c
	gcc -std=c11 -c dynamicCharacterOperations.c
	g++ -std=c++14 -c costMatrix.cpp
	g++ -std=c++14 -c costMtxTest.cpp
	g++ -std=c++14 -g -Wall -o costMtxTest dynamicCharacterOperations.o costMatrix.o costMtxTest.o

yu_code: main.c seqAlignForHaskell.c seqAlignForHaskell.h dynamicCharacterOperations.c dynamicCharacterOperations.h
	gcc -std=c11 -c seqAlignForHaskell.c dynamicCharacterOperations.c
	gcc -std=c11 main.c seqAlignForHaskell.o dynamicCharacterOperations.o
