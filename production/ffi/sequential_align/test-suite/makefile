### Note that two calls to gcc to create .o files are needed, followed by two calls to compile

all : sequentialAlign

clean :
	rm -f *.o
	rm -f *.out
	rm -rf *.dSYM

bit-ops : bitArrayExampleC.c bitArrayExampleC.h $(dynamicChar)
	rm -f *.o
	gcc -std=c11 -c bitArrayExampleC.c dynamicCharacterOperations.c
	gcc -std=c11 bitArrayExampleC.o dynamicCharacterOperations.o

# compiler flags:
#  -g    adds debugging information to the executable file
#  -Wall turns on most, but not all, compiler warnings
testCostMtx : test_costMtx.cpp costMatrix.h costMatrix.cpp $(dynamicChar)
	rm -f test_costMtx
	gcc -std=c11 -c -g dynamicCharacterOperations.c
	g++ -std=c++14 -c -g costMatrix.cpp
	# g++ -std=c++14 -c test_costMtx.cpp
	g++ -std=c++14 -g -Wall -o test_costMtx test_costMtx.cpp dynamicCharacterOperations.o costMatrix.o


testCInterface : test_c_interface.c $(costMatrix) $(dynamicChar)
	rm -f test_c_interface
	gcc -std=c11 -g -c dynamicCharacterOperations.c
	# g++ -std=c++14 -c costMatrix.h
	g++ -std=c++14 -g -c costMatrix.cpp
	# gcc -std=c11 -c costMatrixWrapper.h
	gcc -std=c11 -g -c costMatrixWrapper.c
	# in next line, need -lc++ so that it calls correct linker: we need C compiler, but C++ libraries in scope.
	gcc -std=c11 -g -o test_c_interface -Wall test_c_interface.c dynamicCharacterOperations.o costMatrix.o costMatrixWrapper.o -lstdc++
	# gcc -std=c11 -c -v -Wall test_c_interface.c # dynamicCharacterOperations.o costMatrix.o costMatrixWrapper.o
	# ld -demangle -dynamic -arch x86_64 -o test_c_interface test_c_interface.o dynamicCharacterOperations.o costMatrix.o costMatrixWrapper.o -lc++ -lSystem


testSequentialAlign : seqAlignTest.c $(seqAlignFiles) $(costMatrix) $(dynamicChar)
	rm -f testSeqAlign
	gcc -std=c11 -g -c dynamicCharacterOperations.c
	# g++ -std=c++14 -c costMatrix.h
	g++ -std=c++14 -c costMatrix.cpp
	# gcc -std=c11 -c costMatrixWrapper.h
	gcc -std=c11 -g -c costMatrixWrapper.c
	gcc -std=c11 -g -c seqAlignForHaskell.c
	gcc -std=c11 -g -c seqAlignInterface.c
	# in next line, need -lc++ so that it calls correct linker: we need C compiler, but C++ libraries in scope.
	gcc -std=c11 -g -o testSeqAlign -Wall seqAlignTest.c seqAlignInterface.o seqAlignForHaskell.o costMatrixWrapper.o dynamicCharacterOperations.o costMatrix.o -lstdc++



sequentialAlign :  $(seqAlignFiles) $(costMatrix) $(dynamicChar)
	rm -f seqAlign
	gcc -std=c11 -c dynamicCharacterOperations.c
	# g++ -std=c++14 -c costMatrix.h
	g++ -std=c++14 -c costMatrix.cpp
	# gcc -std=c11 -c costMatrixWrapper.h
	gcc -std=c11 -c costMatrixWrapper.c
	gcc -std=c11 -c seqAlignForHaskell.c
	# in next line, need -lc++ so that it calls correct linker: we need C compiler, but C++ libraries in scope.
	gcc -std=c11 -o seqAlign -Wall seqAlignInterface.c seqAlignForHaskell.o dynamicCharacterOperations.o costMatrix.o costMatrixWrapper.o -lstdc++
	# gcc -std=c11 -c -v -Wall test_c_interface.c # dynamicCharacterOperations.o costMatrix.o costMatrixWrapper.o
	# ld -demangle -dynamic -arch x86_64 -o test_c_interface test_c_interface.o dynamicCharacterOperations.o costMatrix.o costMatrixWrapper.o -lc++ -lSystem


dynamicChar = dynamicCharacterOperations.h \
			dynamicCharacterOperations.c

costMatrix = costMatrix.h \
			costMatrix.cpp \
			costMatrixWrapper.h \
			costMatrixWrapper.c

seqAlignFiles = seqAlignForHaskell.h \
				seqAlignForHaskell.c \
				seqAlignInterface.h \
				seqAlignInterface.c


# seqAlign: costMatrix.h costMatrix.cpp costMatrixWrapper.h costMatrixWrapper.c seqAlignForHaskell.h seqAlignForHaskell.c dynamicCharacterOperations.h dynamicCharacterOperations.c
# 	gcc -std=c11 -c dynamicCharacterOperations.c
# 	# g++ -std=c++14 -c costMatrixWrapper.cpp
# 	# gcc -std=c11 -c seqAlignForHaskell.c
# 	g++ -std=c++14 -c costMatrix.cpp
# 	g++ -std=c++14 -c costMatrixWrapper.cpp
# 	g++ -std=c++14 -g -o costMtxTest costMtxTest.c dynamicCharacterOperations.o costMatrix.o # costMatrixWrapper.o # seqAlignForHaskell.o
