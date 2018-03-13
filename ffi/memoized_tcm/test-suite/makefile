### Note that two calls to gcc to create .o files are needed, followed by two calls to compile
### Default is to compile 2d, so no 2d files are renamed with _2d suffix. This is because 3d will eventually (
### probably) be merged into the 2d code.


sanityWarnings = -Wall -Wextra -pedantic

dynamicChar    = ../dynamicCharacterOperations.h \
                 ../dynamicCharacterOperations.c

costMatrix_2d  = ../costMatrix_2d.h \
                 ../costMatrix_2d.cpp

costMatrix_3d  = ../costMatrix_3d.h \
                 ../costMatrix_3d.cpp \
                 $(costMatrix)

wrapper_2d     = ../costMatrixWrapper_2d.h \
                 ../costMatrixWrapper_2d.c

wrapper_3d     = ../costMatrixWrapper_3d.h \
                 ../costMatrixWrapper_3d.c

exe_matrix_2d    = test_cost_matrix_2d
exe_matrix_3d    = test_cost_matrix_3d
exe_interface_2d = test_c_interface_2d
exe_interface_3d = test_c_interface_3d


# all only compiles 2d
all : test_c_interface_2d test_cost_matrix_2d

clean :
	rm -f *.o
	rm -f *.out
	rm -rf *.dSYM
	rm -f $(exe_matrix_2d)
	rm -f $(exe_matrix_3d)
	rm -f $(exe_interface_2d)
	rm -f $(exe_interface_3d)

# compiler flags:
#  -g    adds debugging information to the executable file
#  -Wall turns on most, but not all, compiler warnings
#  Note that in C++ mode we don't compile .h files as we do in C, because they're
#  automatically included
test_cost_matrix_2d : test_cost_matrix_2d.cpp ../costMatrix_2d.cpp ../dynamicCharacterOperations.c
	gcc -std=c11   $(sanityWarnings) -c -g ../dynamicCharacterOperations.c
	g++ -std=c++14 $(sanityWarnings) -c -g ../costMatrix_2d.cpp
	g++ -std=c++14 $(sanityWarnings) -g -Wall -o $(exe_matrix_2d) test_cost_matrix_2d.cpp dynamicCharacterOperations.o costMatrix_2d.o


test_c_interface_2d : test_c_interface_2d.c $(costMatrix_2d) $(dynamicChar) $(wrapper_2d)
	gcc -std=c11   $(sanityWarnings) -g -c $(dynamicChar)
	# g++ -std=c++14 -c costMatrix.h
	g++ -std=c++14 $(sanityWarnings) -g -c ../costMatrix_2d.cpp
	# gcc -std=c11 -c costMatrixWrapper.h
	gcc -std=c11   $(sanityWarnings) -g -c $(wrapper_2d)
	# in next line, need -lc++ so that it calls correct linker: we need C compiler, but C++ libraries in scope.
	gcc -std=c11   $(sanityWarnings) -g -o $(exe_interface_2d) test_c_interface_2d.c dynamicCharacterOperations.o costMatrix_2d.o costMatrixWrapper_2d.o -lstdc++
	# gcc -std=c11 -c -v -Wall test_c_interface_2d.c # dynamicCharacterOperations.o costMatrix_2d.o costMatrixWrapper_2d.o
	# ld -demangle -dynamic -arch x86_64 -o test_c_interface_2d test_c_interface_2d.o dynamicCharacterOperations.o costMatrix_2d.o costMatrixWrapper_2d.o -lc++ -lSystem


test_cost_matrix_3d : test_cost_matrix_3d.cpp ../costMatrix_2d.cpp ../costMatrix_3d.cpp ../dynamicCharacterOperations.c
	gcc -std=c11   $(sanityWarnings) -c -g ../dynamicCharacterOperations.c
	g++ -std=c++14 $(sanityWarnings) -c -g ../costMatrix_2d.cpp
	g++ -std=c++14 $(sanityWarnings) -c -g ../costMatrix_3d.cpp
	g++ -std=c++14 $(sanityWarnings) -g -Wall -o $(exe_matrix_3d) test_cost_matrix_3d.cpp dynamicCharacterOperations.o costMatrix_2d.o costMatrix_3d.o


test_c_interface_3d : test_c_interface_3d.c $(costMatrix_3d) $(costMatrix_2d) $(dynamicChar) $(wrapper_3d)
	gcc -std=c11   $(sanityWarnings) -g -c $(dynamicChar)
	# g++ -std=c++14 -c costMatrix_2d.h
	g++ -std=c++14 $(sanityWarnings) -g -c ../costMatrix_2d.cpp
	g++ -std=c++14 $(sanityWarnings) -g -c ../costMatrix_3d.cpp
	# gcc -std=c11 -c costMatrixWrapper.h
	gcc -std=c11   $(sanityWarnings) -g -c $(wrapper_3d)
	# in next line, need -lc++ so that it calls correct linker: we need C compiler, but C++ libraries in scope.
	gcc -std=c11   $(sanityWarnings) -g -o $(exe_interface_3d) -Wall test_c_interface_3d.c dynamicCharacterOperations.o costMatrix_2d.o costMatrix_3d.o costMatrixWrapper_3d.o -lstdc++
	# gcc -std=c11 -c -v -Wall test_c_interface.c # dynamicCharacterOperations.o costMatrix_2d.o costMatrixWrapper.o
	# ld -demangle -dynamic -arch x86_64 -o test_c_interface test_c_interface.o dynamicCharacterOperations.o costMatrix_2d.o costMatrixWrapper.o -lc++ -lSystem
