#! /usr/bin/env python2

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
    lib = ffi.dlopen("../C_source/interface_driver_3d_for_python.so")

    intArrays = [[8, 2, 1, 8, 1],
                 [8, 2, 1, 8, 1],
                 [8, 2, 1, 8, 1]]

    shortLen  = len(intArrays[2])
    middleLen = len(intArrays[1])
    longLen   = len(intArrays[0])

    lib.wrapperFunction(intArrays[0], longLen, intArrays[2], shortLen, intArrays[1], middleLen)

if __name__ == '__main__' : main()
