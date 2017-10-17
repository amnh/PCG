### Note that multiple calls to gcc to create .o files are needed, followed by multiple additional calls for linking and final compilation

sanity-warnings   = -Wall -Wextra -pedantic -fdiagnostics-color

necessary_c_files = c_alignment_interface.c \
                    c_code_alloc_setup.c \
                    costMatrix.c \
                    alignmentMatrices.c \
                    dyn_character.c \
                    alignCharacters.c \
                    ukkCheckPoint.c \
                    ukkCommon.c

necessary_h_files = /c_alignment_interface.h \
                    /c_code_alloc_setup.h \
                    /costMatrix.h \
                    /alignmentMatrices.h \
                    /dyn_character.h \
                    /alignCharacters.h \
                    /ukkCheckPoint.h \
                    /ukkCommon.h

object_files      = c_alignment_interface.o \
                    c_code_alloc_setup.o \
                    costMatrix.o \
                    alignmentMatrices.o \
                    dyn_character.o \
                    alignCharacters.o \
                    ukkCheckPoint.o \
                    ukkCommon.o


all : debug_just_c # test_initialization



clean :
	rm -f *.o *.hi *.out *.so
	rm -rf *.dSYM
	rm -f initialization_driver interface_driver debug_just_c

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


######### Test the actual C interface that Hakell interact with.
test_interface_long : interface_driver_long_seqs.c $(necessary_c_files) $(necessary_h_files)
	rm -f *.o
	gcc -std=c11 $(sanity-warnings) -g -c  $(necessary_c_files)
	gcc -std=c11 $(sanity-warnings) -o interface_driver_long -g interface_driver_long_seqs.c $(object_files)


######### Test the actual C interface that Hakell interact with.
character_test : character_test.c $(necessary_c_files) $(necessary_h_files)
	rm -f *.o
	gcc -std=c11 $(sanity-warnings) -g -c  $(necessary_c_files)
	gcc -std=c11 $(sanity-warnings) -o character_test -g character_test.c $(object_files)


######### Run POY alignment C code using Haskell FFI.
align : debug_just_c.c algn.c algn.h avl.c avl.h cm.c cm.h \
		matrices.c matrices.h seq.c seq.h zarr.c zarr.h POYalign.hsc
	rm -f POYalign.hs
	rm -f *.o
	gcc -c algn.c avl.c cm.c matrices.c seq.c zarr.c
	ghc -O --make POYalign.hs algn.c


######### Test just the C code, without the C interface to Haskell.
debug_just_c : debug_just_c.c algn.c algn.h cm.c cm.h \
			   matrices.c matrices.h seq.c seq.h ukkCheckp.c ukkCheckp.h \
			   ukkCommon.c ukkCommon.h
	rm -f *.o
	gcc -g -c algn.c cm.c matrices.c seq.c ukkCheckp.c ukkCommon.c
	gcc -o debug_just_c -g debug_just_c.c algn.o cm.o matrices.o seq.o ukkCheckp.o ukkCommon.o


######### Test just the C code, without the C interface to Haskell. Send in multiple arrays from Python,
######### to avoid C IO. Also, just wanted to learn Python FFI.
debug_just_c_for_python : debug_just_c_for_python.c algn.c algn.h cm.c cm.h \
						  matrices.c matrices.h seq.c seq.h ukkCheckp.c ukkCheckp.h \
						  ukkCommon.c ukkCommon.h
	rm -f *.o *.a
	gcc -fPIC -O3 -c algn.c cm.c matrices.c seq.c ukkCheckp.c ukkCommon.c
	gcc -fPIC -O3 -c debug_just_c_for_python.c -o intermediate_lib.o
	# ar rvs combined_deps.a algn.o cm.o matrices.o seq.o ukkCheckp.o ukkCommon.o
	# ld intermediate_lib.o -l combined_deps.a -o debug_just_c_for_python.o
	gcc -shared -O3 intermediate_lib.o algn.o cm.o matrices.o seq.o ukkCheckp.o ukkCommon.o -o debug_just_c_for_python.so
