### To make just C testing executables. To make

### Note that multiple calls to gcc to create .o files are needed, followed by multiple additional calls for linking and final compilation

sanity-warnings   = -Wall -Wextra -pedantic -fdiagnostics-color

necessary_c_files = ../../alignCharacters.c \
                    ../../alignmentMatrices.c \
                    ../../c_alignment_interface.c \
                    ../../c_code_alloc_setup.c \
                    ../../costMatrix.c \
                    ../../dyn_character.c \
                    ../../ukkCheckPoint.c \
                    ../../ukkCommon.c

necessary_h_files = ../../alignCharacters.h \
                    ../../alignmentMatrices.h \
                    ../../c_alignment_interface.h \
                    ../../c_code_alloc_setup.h \
                    ../../costMatrix.h \
                    ../../dyn_character.h \
                    ../../ukkCheckPoint.h \
                    ../../ukkCommon.h

object_files      = alignCharacters.o \
                    alignmentMatrices.o \
                    c_alignment_interface.o \
                    c_code_alloc_setup.o \
                    costMatrix.o \
                    dyn_character.o \
                    ukkCheckPoint.o \
                    ukkCommon.o

executables       = test_character \
                    test_initialization \
                    test_interface \
                    test_interface_3d \
                    test_interface_long \
                    test_just_c \
                    test_powell_for_python \
                    test_powell_only \
                    POYalign.hs

all : test_powell_for_python # test_interface # test_initialization

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


######### Test the actual C interface that Hakell interact with.
test_interface : test_interface.c $(necessary_c_files) $(necessary_h_files)
	gcc -std=c11 -g $(sanity-warnings) -c $(necessary_c_files)
	gcc -std=c11 -g $(sanity-warnings) test_interface.c $(object_files) -o test_interface


######### Test the actual C interface that Hakell interact with.
character_test : character_test.c $(necessary_c_files) $(necessary_h_files)
	rm -f *.o
	gcc -std=c11 $(sanity-warnings) -g -c  $(necessary_c_files)
	gcc -std=c11 $(sanity-warnings) -o character_test -g character_test.c $(object_files)


######### Create a .so file that can be called from a Python script, thereby allowing batch scripting
test_interface_3D_for_python : test_interface_3D_for_python.c $(necessary_c_files) $(necessary_h_files)
	gcc -std=c11 -fPIC -O3 -c $(sanity-warnings) $(necessary_c_files)
	gcc -std=c11 -fPIC -O3 -c $(sanity-warnings) test_interface_3D_for_python.c -o intermediate_lib.o
	# Next, build .so files for Python ffi. Note that we need -fPIC in gcc calls above if this is going to work correctly.
	gcc -shared -O3 $(sanity-warnings) intermediate_lib.o $(object_files) -o test_interface_3D_for_python.so


########################## The following are  for testing only the Powell 3D, and not the interface ################################

######### To compile to .so for Python ffi.
test_powell_for_python : ../../ukkCheckPoint.c ../../ukkCommon_for_python.c ../../ukkCommon.h
	gcc         -fPIC -std=c11 -g $(sanity-warnings) -c ../../ukkCheckPoint.c -o intermediatelib.o
	gcc -shared -fPIC -std=c11 -g $(sanity-warnings) ../../ukkCommon_for_python.c intermediatelib.o -o test_powell_for_python.so


######### To compile to exe for valgrinding on Ubuntu.
test_powell : ../../ukkCheckPoint.c ../../ukkCommon.c ../../ukkCommon.h
	rm -rf test_powell
	gcc -std=c11 -g $(sanity-warnings) -c ../../ukkCheckPoint.c
	gcc -std=c11 -g $(sanity-warnings) ukkCheckPoint.o ../../ukkCommon_with_main.c -o test_powell_only
