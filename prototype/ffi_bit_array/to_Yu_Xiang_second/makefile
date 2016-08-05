### Note that two calls to gcc to create .o files are needed, followed by two calls to compile

all : bit-ops

clean :
	rm -f *.o

bit-ops : bitArrayExampleC.c bitArrayExampleC.h dynamicCharacterOperations.c dynamicCharacterOperations.h
	rm -f *.o
	gcc -c bitArrayExampleC.c dynamicCharacterOperations.c
	gcc bitArrayExampleC.o dynamicCharacterOperations.o
