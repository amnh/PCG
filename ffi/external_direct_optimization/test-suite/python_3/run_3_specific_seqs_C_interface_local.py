#! /usr/bin/env python

''' A script to run POY on twice on three DO characters. First run regular DO, then run 3d (iterative: exact) DO on the same three characters. This is to be able to compare the times for the runs of each. These times can then be compared to just the C 3d DO
code to see if the times are comparable.'''

from cffi import FFI
ffi = FFI()           # bind all FFI calls to ffi variable

from random     import randrange
from subprocess import check_output
from sys        import argv
from time       import time


def main():

    ffi.cdef("""
                int wrapperFunction(int *firstSeq, int firstSeqLen, int *secondSeq, int secondSeqLen, int *thirdSeq, int thirdSeqLen);
             """)
    lib = ffi.dlopen( "../C_source/test_interface_3d_for_python.so" )

    intArrays = [ [26, 8],
                  [15, 24],
                  [15, 24]
                ]

    inLen1 = len( intArrays[0] )
    inLen2 = len( intArrays[1] )
    inLen3 = len( intArrays[2] )

    lib.wrapperFunction( intArrays[0], inLen1, intArrays[1], inLen2, intArrays[2], inLen3 )

if __name__ == '__main__' : main()
