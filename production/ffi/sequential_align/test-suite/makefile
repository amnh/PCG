### Note that two calls to gcc to create .o files are needed, followed by two calls to compile

all : test_sequential_align

clean :
	rm -f *.o
	rm -f *.out
	rm -rf *.dSYM


# compiler flags:
#  -g    adds debugging information to the executable file
#  -Wall turns on most, but not all, compiler warnings
test_cost_matrix : test_cost_matrix.cpp ../costMatrix.h ../costMatrix.cpp $(dynamicChar)
	rm -f test_cost_matrix
	gcc -std=c11 -c -g ../dynamicCharacterOperations.c
	g++ -std=c++14 -c -g ../costMatrix.cpp
	# g++ -std=c++14 -c test_costMtx.cpp
	g++ -std=c++14 -g -Wall -o test_cost_matrix test_cost_matrix.cpp dynamicCharacterOperations.o costMatrix.o


test_c_interface : test_c_interface.c $(costMatrix) $(dynamicChar)
	rm -f test_c_interface
	gcc -std=c11 -g -c ../dynamicCharacterOperations.c
	# g++ -std=c++14 -c costMatrix.h
	g++ -std=c++14 -g -c ../costMatrix.cpp
	# gcc -std=c11 -c costMatrixWrapper.h
	gcc -std=c11 -g -c ../costMatrixWrapper.c
	# in next line, need -lc++ so that it calls correct linker: we need C compiler, but C++ libraries in scope.
	gcc -std=c11 -g -o test_c_interface -Wall test_c_interface.c dynamicCharacterOperations.o costMatrix.o costMatrixWrapper.o -lstdc++
	# gcc -std=c11 -c -v -Wall test_c_interface.c # dynamicCharacterOperations.o costMatrix.o costMatrixWrapper.o
	# ld -demangle -dynamic -arch x86_64 -o test_c_interface test_c_interface.o dynamicCharacterOperations.o costMatrix.o costMatrixWrapper.o -lc++ -lSystem


test_sequential_align : test_sequential_align.c $(seqAlignFiles) $(costMatrix) $(dynamicChar)
	rm -f .\test_sequential_align
	gcc -std=c11 -g -c ../dynamicCharacterOperations.c
	# g++ -std=c++14 -c costMatrix.h
	g++ -std=c++14 -c ../costMatrix.cpp
	# gcc -std=c11 -c costMatrixWrapper.h
	gcc -std=c11 -g -c ../costMatrixWrapper.c
	gcc -std=c11 -g -c ../seqAlignForHaskell.c
	gcc -std=c11 -g -c ../seqAlignInterface.c
	# in next line, need -lc++ so that it calls correct linker: we need C compiler, but C++ libraries in scope.
	gcc -std=c11 -g -o test_sequential_align -Wall test_sequential_align.c seqAlignInterface.o seqAlignForHaskell.o costMatrixWrapper.o dynamicCharacterOperations.o costMatrix.o -lstdc++


dynamicChar = ../dynamicCharacterOperations.h \
			../dynamicCharacterOperations.c

costMatrix = ../costMatrix.h \
			../costMatrix.cpp \
			../costMatrixWrapper.h \
			../costMatrixWrapper.c

seqAlignFiles = ../seqAlignForHaskell.h \
				../seqAlignForHaskell.c \
				../seqAlignInterface.h \
				../seqAlignInterface.c


# seqAlign: costMatrix.h costMatrix.cpp costMatrixWrapper.h costMatrixWrapper.c seqAlignForHaskell.h seqAlignForHaskell.c dynamicCharacterOperations.h dynamicCharacterOperations.c
# 	gcc -std=c11 -c dynamicCharacterOperations.c
# 	# g++ -std=c++14 -c costMatrixWrapper.cpp
# 	# gcc -std=c11 -c seqAlignForHaskell.c
# 	g++ -std=c++14 -c costMatrix.cpp
# 	g++ -std=c++14 -c costMatrixWrapper.cpp
# 	g++ -std=c++14 -g -o costMtxTest costMtxTest.c dynamicCharacterOperations.o costMatrix.o # costMatrixWrapper.o # seqAlignForHaskell.o
