### Note that two calls to gcc to create .o files are needed, followed by two calls to compile

all : bit-ops

clean :
	rm -f *.o
	rm -f *.out
	rm -rf *.dSYM

bit-ops : bitArrayExampleC.c bitArrayExampleC.h $(dynamicChar)
	rm -f *.o
	gcc -std=c11 -c bitArrayExampleC.c dynamicCharacterOperations.c
	gcc -std=c11 bitArrayExampleC.o dynamicCharacterOperations.o


dynamicChar = dynamicCharacterOperations.h \
              dynamicCharacterOperations.c

