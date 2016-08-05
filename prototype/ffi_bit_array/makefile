### Note that two calls to gcc to create .o files are needed, followed by two calls to compile

all : bit-ops

clean :
	rm -f *.o

bit-ops : bitArrayTestC.c bitArrayTestC.h dynamicCharacterOperations.cpp dynamicCharacterOperations.h
	rm -f *.o
	g++ -c bitArrayTestC.c dynamicCharacterOperations.cpp
	g++ bitArrayTestC.o dynamicCharacterOperations.o
