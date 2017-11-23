CC = gcc

sanity-warnings = -Wall -Wextra -pedantic -fdiagnostics-color
to_remove = *.o *.so test_powell *.dSYM

all: test_powell_for_python

######### To compile to .so for Python ffi.
test_powell_for_python : ukkCheckPoint.c ukkCommon_for_python.c ukkCommon.h
	gcc         -fPIC -std=c11 -g $(sanity-warnings) -c ukkCheckPoint.c -o intermediatelib.o
	gcc -shared -fPIC -std=c11 -g $(sanity-warnings) ukkCommon_for_python.c intermediatelib.o -o test_powell_for_python.so

######### To compile to exe for valgrinding on Ubuntu.
test_powell : ukkCheckPoint.c ukkCommon.c ukkCommon.h
	rm -rf test_powell
	gcc -std=c11 -g $(sanity-warnings) -c ukkCheckPoint.c
	gcc -std=c11 -g $(sanity-warnings) ukkCheckPoint.o ukkCommon_with_main.c -o test_powell

clean:
	rm -rf $(to_remove)

