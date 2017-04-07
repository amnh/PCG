### Note that two calls to gcc to create .o files are needed, followed by two calls to compile

dynamicChar   = ../memoized_tcm/dynamicCharacterOperations.h \
                ../memoized_tcm/dynamicCharacterOperations.c

costMatrix    = ../memoized_tcm/costMatrix.h \
                ../memoized_tcm/costMatrix.cpp

wrapper       = ../memoized_tcm/costMatrixWrapper.h \
                ../memoized_tcm/costMatrixWrapper.c

seqAlignFiles = seqAlignForHaskell.h \
                seqAlignForHaskell.c \
                seqAlignInterface.h \
                seqAlignInterface.c

all : sequentialAlign

clean :
	rm -f *.o
	rm -f *.out
	rm -rf *.dSYM

bit-ops : bitArrayExampleC.c bitArrayExampleC.h $(dynamicChar)
	rm -f *.o
	gcc -std=c11 -c bitArrayExampleC.c dynamicCharacterOperations.c
	gcc -std=c11 bitArrayExampleC.o dynamicCharacterOperations.o

sequentialAlign :  $(seqAlignFiles) $(costMatrix) $(wrapper) $(dynamicChar)
	rm -f seqAlign
	gcc -std=c11 -c $(dynamicChar)
	# g++ -std=c++14 -c costMatrix.h
	g++ -std=c++14 -c $(costMatrix)
	# gcc -std=c11 -c costMatrixWrapper.h
	gcc -std=c11 -c $(wrapper)
	gcc -std=c11 -c $(seqAlignFiles)
	# in next line, need -lc++ so that it calls correct linker: we need C compiler, but C++ libraries in scope.
	gcc -std=c11 -o seqAlign -Wall seqAlignInterface.c seqAlignForHaskell.o dynamicCharacterOperations.o costMatrix.o costMatrixWrapper.o -lstdc++
	# gcc -std=c11 -c -v -Wall test_c_interface.c # dynamicCharacterOperations.o costMatrix.o costMatrixWrapper.o
	# ld -demangle -dynamic -arch x86_64 -o test_c_interface test_c_interface.o dynamicCharacterOperations.o costMatrix.o costMatrixWrapper.o -lc++ -lSystem



# seqAlign: costMatrix.h costMatrix.cpp costMatrixWrapper.h costMatrixWrapper.c seqAlignForHaskell.h seqAlignForHaskell.c dynamicCharacterOperations.h dynamicCharacterOperations.c
# 	gcc -std=c11 -c dynamicCharacterOperations.c
# 	# g++ -std=c++14 -c costMatrixWrapper.cpp
# 	# gcc -std=c11 -c seqAlignForHaskell.c
# 	g++ -std=c++14 -c costMatrix.cpp
# 	g++ -std=c++14 -c costMatrixWrapper.cpp
# 	g++ -std=c++14 -g -o costMtxTest costMtxTest.c dynamicCharacterOperations.o costMatrix.o # costMatrixWrapper.o # seqAlignForHaskell.o
