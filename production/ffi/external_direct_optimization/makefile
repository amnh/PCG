### Note that multiple calls to gcc to create .o files are needed, followed by multiple additional calls for linking and final compilation

all : externalCAlignment

clean :
	rm -f *.o *.hi *.out
	rm -rf *.dSYM
	rm -f initialization_driver interface_driver

##########
external_c_alignment : externalCAlignment.hsc $(necessary_c_files) $(necessary_h_files) \
					c_alignment_interface.c c_alignment_interface.h
	rm -f externalCAlignment.hs
	hsc2hs externalCAlignment.hsc
	gcc -c externalCAlignment.c
	ghc -O --make externalCAlignment.hs alignment_interface.o $(object_files)

######### Test only c functions that allocate and initialize various
######### internal structs. c_alignment_interface.c relies on these.
test_initialization : initialization_driver.c $(necessary_c_files) $(necessary_h_files)
	rm -f *.o
	gcc -g -c $(necessary_c_files)
	gcc -o initialization_driver -g initialization_driver.c $(object_files)

######### Test the actual C interface that Hakell interact with.
test_interface : interface_driver.c c_alignment_interface.c c_alignment_interface.h \
				$(necessary_c_files) $(necessary_h_files)
	rm -f *.o
	gcc -g -c c_alignment_interface.c $(necessary_c_files)
	gcc -o interface_driver -g interface_driver.c c_alignment_interface.o $(object_files)



necessary_c_files = c_code_alloc_setup.c \
					costMatrix.c \
					nwMatrices.c \
					seq.c \
					seqAlign.c \
					ukkCheckp.c \
					ukkCommon.c

necessary_h_files = c_code_alloc_setup.h \
					costMatrix.h \
					nwMatrices.h \
					seq.h \
					seqAlign.h \
					ukkCheckp.h \
					ukkCommon.h

object_files = c_code_alloc_setup.o \
				costMatrix.o \
				nwMatrices.o \
				seq.o \
				seqAlign.o \
				ukkCheckp.o \
				ukkCommon.o