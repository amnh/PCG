### Note that multiple calls to gcc to create .o files are needed, followed by multiple additional calls for linking and final compilation

sanity-warnings   = -Wall -Wextra -pedantic -fdiagnostics-color

necessary_c_files = ../../c_alignment_interface.c \
                    ../../c_code_alloc_setup.c \
                    ../../costMatrix.c \
                    ../../alignmentMatrices.c \
                    ../../dyn_character.c \
                    ../../alignCharacters.c \
                    ../../ukkCheckPoint.c \
                    ../../ukkCommon.c

necessary_h_files = ../../c_alignment_interface.h \
                    ../../c_code_alloc_setup.h \
                    ../../costMatrix.h \
                    ../../alignmentMatrices.h \
                    ../../dyn_character.h \
                    ../../alignCharacters.h \
                    ../../ukkCommon.h

object_files      = c_alignment_interface.o \
                    c_code_alloc_setup.o \
                    costMatrix.o \
                    alignmentMatrices.o \
                    dyn_character.o \
                    alignCharacters.o \
                    ukkCheckPoint.o \
                    ukkCommon.o

executables       = test_initialization \
                    test_interface \
                    test_interface_3d \
                    test_interface_long \
                    test_character \
                    test_just_c \
                    POYalign.hs




all : test_interface_for_python # test_just_c # test_initialization


clean :
	rm -f *.o *.hi *.out *.so
	rm -rf *.dSYM
	rm -f $(executables)


##########
external_c_alignment : externalCAlignment.hsc $(necessary_c_files) $(necessary_h_files)
	hsc2hs externalCAlignment.hsc
	gcc -std=c11 $(sanity-warnings) -c externalCAlignment.c
	ghc -O --make externalCAlignment.hs $(object_files)


######### Test only c functions that allocate and initialize various
######### internal structs. c_alignment_interface.c relies on these.
test_initialization : test_initialization.c $(necessary_c_files) $(necessary_h_files)
	gcc -std=c11 -g $(sanity-warnings) -c $(necessary_c_files)
	gcc -std=c11 -g $(sanity-warnings) test_initialization.c $(object_files) -o test_initialization


#########
test_interface : test_interface.c $(necessary_c_files) $(necessary_h_files)
	gcc -std=c11 -g $(sanity-warnings) -c $(necessary_c_files)
	gcc -std=c11 -g $(sanity-warnings) test_interface.c $(object_files) -o test_interface


#########
test_interface_3D_for_python : test_interface_3d_for_python.c $(necessary_c_files) $(necessary_h_files)
	gcc -std=c11 -fPIC -O3 -c $(sanity-warnings) $(necessary_c_files)
	gcc -std=c11 -fPIC -O3 -c $(sanity-warnings) test_interface_3d_for_python.c -o intermediate_lib.o
	# Next, build .so files for Python ffi. Note that we need -fPIC in gcc calls above if this is going to work correctly.
	gcc -shared -O3 $(sanity-warnings) intermediate_lib.o $(object_files) -o test_interface_3d_for_python.so
