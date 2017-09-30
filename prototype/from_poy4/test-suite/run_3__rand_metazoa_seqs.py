#! /usr/bin/env python2

from cffi import FFI
ffi = FFI()    # bind all FFI calls to ffi variables

from random import randrange
from subprocess import check_output

import sys
import time


print (sys.version)

def picklines(thefile, whatlines):
  return [x for i, x in enumerate(thefile) if i in whatlines]


def charToInt(inArr, conversionDict):
    return map(inArr, lambda x: conversionDict(x))


def main():
    iupacDict = {'A': 1, 'C': 2, 'G': 4, 'T': 8, 'U': 8, 'M': 3, 'R': 5, 'S': 6, 'V': 7, 'W': 9, 'Y': 10, 'H': 11, 'K': 12, 'D': 13, 'B': 14, 'N': 15, '.': 16, '-': 16}


    filename = "2metazoa18s-short.fasta"

    wcOutput = ''.join( map(bytes, check_output(['wc', '-l', filename])) ).split()[0]
    numSeqs  = int( (int(wcOutput) + 1) / 2) # for some reason `wc -l` is returning numLines - 1. Maybe it's only counting newlines?

    # print(wcOutput)

    whatlines = list()

    for i in range(3):
        lineNum = randrange(numSeqs)
        whatlines.append(2 * lineNum  + 1) # first line in file is 0

    whatlines.sort()

    infile    = open(filename)

    charArr   = picklines(infile, whatlines) # need to translate this to ints

    intArrays = []

    for i in range(3):
        intArrays.append( list(map( lambda x: iupacDict[x], charArr[i][:-1] )) )
        print(intArrays[i])

    infile.close()
    arr1Len = len(intArrays[0])
    arr2Len = len(intArrays[1])
    arr3Len = len(intArrays[2])
    print("{}, {}, {}".format(arr1Len, arr2Len, arr3Len))

    inputArray1 = ffi.new( "int[]", arr1Len )
    inputArray2 = ffi.new( "int[]", arr2Len )
    inputArray3 = ffi.new( "int[]", arr3Len )

##### Now start ffi call
    start = time.time()

    ffi.cdef("""
       int wrapperFunction(int *firstSeq, int firstSeqLen, int *secondSeq, int secondSeqLen, int *thirdSeq, int thirdSeqLen);
    """)
    lib = ffi.dlopen("debug_just_c_for_python.so")
    lib.wrapperFunction(intArrays[0], arr1Len, intArrays[1], arr2Len, intArrays[2], arr3Len)

    end = time.time()

    print(start - end) # total time the C code ran

if __name__ == '__main__' : main()