### Note that two calls to gcc to create .o files are needed, followed by two calls to compile

sanityWarnings = -Wall -Wextra -pedantic

dynamicChar    = ../dynamicCharacterOperations.h \
                 ../dynamicCharacterOperations.c

costMatrix     = ../costMatrix.h \
                 ../costMatrix.cpp

wrapper        = ../costMatrixWrapper.h \
                 ../costMatrixWrapper.c

exe_matrix     = test_cost_matrix
exe_interface  = test_c_interface

all : test_c_interface test_cost_matrix

clean :
	rm -f *.o
	rm -f *.out
	rm -rf *.dSYM
	rm -f $(exe_matrix)
	rm -f $(exe_interface)

# compiler flags:
#  -g    adds debugging information to the executable file
#  -Wall turns on most, but not all, compiler warnings
test_cost_matrix : test_cost_matrix.cpp $(costMatrix) $(dynamicChar)
	gcc -std=c11   $(sanityWarnings) -c -g $(dynamicChar)
	g++ -std=c++14 $(sanityWarnings) -c -g $(costMatrix)
	# g++ -std=c++14 -c test_costMtx.cpp
	g++ -std=c++14 $(sanityWarnings) -g -Wall -o $(exe_matrix) test_cost_matrix.cpp dynamicCharacterOperations.o costMatrix.o


test_c_interface : test_c_interface.c $(costMatrix) $(dynamicChar) $(wrapper)
	gcc -std=c11   $(sanityWarnings) -g -c $(dynamicChar)
	# g++ -std=c++14 -c costMatrix.h
	g++ -std=c++14 $(sanityWarnings) -g -c $(costMatrix)
	# gcc -std=c11 -c costMatrixWrapper.h
	gcc -std=c11   $(sanityWarnings) -g -c $(wrapper)
	# in next line, need -lc++ so that it calls correct linker: we need C compiler, but C++ libraries in scope.
	gcc -std=c11   $(sanityWarnings) -g -o $(exe_interface) -Wall test_c_interface.c dynamicCharacterOperations.o costMatrix.o costMatrixWrapper.o -lstdc++
	# gcc -std=c11 -c -v -Wall test_c_interface.c # dynamicCharacterOperations.o costMatrix.o costMatrixWrapper.o
	# ld -demangle -dynamic -arch x86_64 -o test_c_interface test_c_interface.o dynamicCharacterOperations.o costMatrix.o costMatrixWrapper.o -lc++ -lSystem
