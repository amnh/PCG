### Note that two calls to gcc to create .o files are needed, followed by two calls to compile
### Default is to compile 2d, so no 2d files are renamed with _2d suffix. This is because 3d will eventually (
### probably) be merged into the 2d code.


sanityWarnings = -Wall -Wextra -pedantic

dynamicChar    = ../dynamicCharacterOperations.h \
                 ../dynamicCharacterOperations.c

costMatrix_2d  = ../costMatrix_2d.hpp \
                 ../costMatrix_2d.cpp

costMatrix_3d  = ../costMatrix_3d.hpp \
                 ../costMatrix_3d.cpp

wrapper        = ../costMatrixWrapper.h \
                 ../costMatrixWrapper.c

test_matrix_2d  = test_cost_matrix_2d
test_matrix_3d  = test_cost_matrix_3d
test_interface  = test_c_interface


all : test_cost_matrix_2d test_cost_matrix_3d test_c_interface

clean :
	rm -f *.o
	rm -f *.out
	rm -rf *.dSYM
	rm -f $(test_matrix_2d)
	rm -f $(test_matrix_3d)
	rm -f $(test_interface)


# compiler flags:
#  -g    adds debugging information to the executable file
#  Note that in C++ mode we don't compile .h files as we do in C, because they're
#  automatically included
$(test_matrix_2d) : test_cost_matrix_2d.cpp $(costMatrix_2d) $(dynamicChar)
	gcc -std=c11   $(sanityWarnings) -c -g $(dynamicChar)
	g++ -std=c++14 $(sanityWarnings) -c -g $(costMatrix_2d)
	g++ -std=c++14 $(sanityWarnings) -g -Wall -o $(test_matrix_2d) test_cost_matrix_2d.cpp *.o


$(test_matrix_3d) : test_cost_matrix_3d.cpp $(costMatrix_2d) $(costMatrix_3d) $(dynamicChar)
	gcc -std=c11   $(sanityWarnings) -c -g $(dynamicChar)
	g++ -std=c++14 $(sanityWarnings) -c -g $(costMatrix_2d)
	g++ -std=c++14 $(sanityWarnings) -c -g $(costMatrix_3d)
	g++ -std=c++14 $(sanityWarnings) -g -Wall -o $(test_matrix_3d) test_cost_matrix_3d.cpp *.o


$(test_interface) : test_c_interface.c $(costMatrix_3d) $(costMatrix_2d) $(dynamicChar) $(wrapper)
	gcc -std=c11   $(sanityWarnings) -g -c $(dynamicChar)
	g++ -std=c++14 $(sanityWarnings) -g -c $(costMatrix_2d)
	g++ -std=c++14 $(sanityWarnings) -g -c $(costMatrix_3d)
	gcc -std=c11   $(sanityWarnings) -g -c $(wrapper)
	# in next line, need -lstdc++ so that it calls correct linker: we need C compiler, but C++ libraries in scope.
	gcc -std=c11   $(sanityWarnings) -g -o $(test_interface) test_c_interface.c *.o -lstdc++
