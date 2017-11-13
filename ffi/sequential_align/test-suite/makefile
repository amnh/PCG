### Note that two calls to gcc to create .o files are needed, followed by two calls to compile

sanityWarnings = -Wall -Wextra -pedantic

dynamicChar    = ../../memoized_tcm/dynamicCharacterOperations.h \
                 ../../memoized_tcm/dynamicCharacterOperations.c

costMatrix     = ../../memoized_tcm/costMatrix.h \
                 ../../memoized_tcm/costMatrix.cpp

wrapper        = ../../memoized_tcm/costMatrixWrapper.h \
                 ../../memoized_tcm/costMatrixWrapper.c

seqAlignFiles  = ../sequentialAlign.h \
                 ../sequentialAlign.c \
                 ../sequentialAlignInterface.h \
                 ../sequentialAlignInterface.c \
                 ../sequentialAlignOutputTypes.h

exe_align      = test_sequential_align


all : test_sequential_align

rebuild: clean test_sequential_align

clean :
	rm -f *.o
	rm -f *.out
	rm -rf *.dSYM
	rm -f $(exe_align)


# compiler flags:
#  -g    adds debugging information to the executable file
#  -Wall turns on most, but not all, compiler warnings
test_sequential_align : test_sequential_align.c $(seqAlignFiles) $(costMatrix) $(wrapper) $(dynamicChar)
	gcc -std=c11   $(sanityWarnings) -g -c $(dynamicChar)
	# g++ -std=c++14 -c costMatrix.h
	g++ -std=c++14 $(sanityWarnings) -c $(costMatrix)
	# gcc -std=c11 -c costMatrixWrapper.h
	gcc -std=c11   $(sanityWarnings) -g -c $(wrapper)
	gcc -std=c11   $(sanityWarnings) -g -c $(seqAlignFiles)
#	gcc -std=c11 -g -c ../seqAlignInterface.c
	# in next line, need -lc++ so that it calls correct linker: we need C compiler, but C++ libraries in scope.
	gcc -std=c11   $(sanityWarnings) -g -o $(exe_align) -Wall test_sequential_align.c sequentialAlignInterface.o sequentialAlign.o costMatrixWrapper.o dynamicCharacterOperations.o costMatrix.o -lstdc++
