### Note that multiple calls to gcc to create .o files are needed, followed by multiple additional calls for linking and final compilation

sanity-warnings   = -Wall -Wextra -pedantic -fdiagnostics-color

necessary_c_files = ../c_alignment_interface.c \
                    ../c_code_alloc_setup.c \
                    ../costMatrix.c \
                    ../nwMatrices.c \
                    ../seq.c \
                    ../alignSequences.c \
                    ../ukkCheckp.c \
                    ../ukkCommon.c

necessary_h_files = ../c_alignment_interface.h \
                    ../c_code_alloc_setup.h \
                    ../costMatrix.h \
                    ../nwMatrices.h \
                    ../seq.h \
                    ../alignSequences.h \
                    ../ukkCheckp.h \
                    ../ukkCommon.h

object_files      = c_alignment_interface.o \
                    c_code_alloc_setup.o \
                    costMatrix.o \
                    nwMatrices.o \
                    seq.o \
                    alignSequences.o \
                    ukkCheckp.o \
                    ukkCommon.o


all : test_interface # test_initialization

clean :
	rm -f *.o *.hi *.out
	rm -rf *.dSYM
	rm -f initialization_driver interface_driver

##########
external_c_alignment : externalCAlignment.hsc $(necessary_c_files) $(necessary_h_files)
	rm -f externalCAlignment.hs
	hsc2hs externalCAlignment.hsc
	gcc -std=c11  $(sanity-warnings) -c externalCAlignment.c
	ghc -O --make externalCAlignment.hs $(object_files)

######### Test only c functions that allocate and initialize various
######### internal structs. c_alignment_interface.c relies on these.
test_initialization : initialization_driver.c $(necessary_c_files) $(necessary_h_files)
	rm -f *.o
	gcc -std=c11 $(sanity-warnings) -g -c $(necessary_c_files)
	gcc -std=c11 $(sanity-warnings) -o initialization_driver -g initialization_driver.c $(object_files)

######### Test the actual C interface that Hakell interact with.
test_interface : interface_driver.c $(necessary_c_files) $(necessary_h_files)
	rm -f *.o
	gcc -std=c11 $(sanity-warnings) -g -c  $(necessary_c_files)
	gcc -std=c11 $(sanity-warnings) -o interface_driver -g interface_driver.c $(object_files)
