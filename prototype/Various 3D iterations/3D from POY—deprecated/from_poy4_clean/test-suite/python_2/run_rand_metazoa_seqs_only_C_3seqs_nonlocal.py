#! /usr/bin/env python2

''' A script to run POY on twice on three DO characters. First run regular DO, then run 3d (iterative: exact) DO on the same three characters. This is to be able to compare the times for the runs of each. These times can then be compared to just the C 3d DO
code to see if the times are comparable.'''

from cffi import FFI
ffi = FFI()           # bind all FFI calls to ffi variable

from random import randrange
from subprocess import check_output
from sys import argv
from time import time


# this will fail if `whatlines` isn't sorted _and_ monotonically increasing.
def picklines(thefile, whatlines):
  return [x[:-1] for i, x in enumerate(thefile) if i in whatlines]


def charToInt(inArr, conversionDict):
    return map(inArr, lambda x: conversionDict(x))


def main():
    if len(argv) < 3:
        print "First arg is # processors, second is which processor this is running on, starting at 0."
        exit()

    howManyProcessors = int(argv[1])
    whichProcessor    = int(argv[2])

    numRuns = 10

    seqFileName = "../data/2metazoa18s-short.fasta"

    iupacDict = {'A': 1, 'C': 2, 'G': 4, 'T': 8, 'U': 8, 'M': 3, 'R': 5, 'S': 6, 'V': 7, 'W': 9, 'Y': 10, 'H': 11, 'K': 12, 'D': 13, 'B': 14, 'N': 15, '.': 16, '-': 16}

    ffi.cdef("""
           int wrapperFunction(int *firstSeq, int firstSeqLen, int *secondSeq, int secondSeqLen, int *thirdSeq, int thirdSeqLen);
        """)
    lib = ffi.dlopen("../C_source/debug_just_c_for_python.so")

    timesFile = open("../data/times_just_C_run_{}.txt".format(whichProcessor), "w")

    average = 0

    for runNum in range(numRuns):
        randFile = open('../data/randseeds.txt')

        # skip to correct line in random number file (always 3 seqs)
        startLine = whichProcessor + runNum * numRuns
        for i in range(0, startLine):
            x = randFile.readline()

        # get next 3 seqs
        whatlines = []
        for i in range(3): # there are always three seqs being sent in
            # get 3 random numbers from random number file
            whatlines.append(int(randFile.readline()) * 2 + 1) # Extra math because we're not getting that line, but that _sequence_
        randFile.close()
        whatlines.sort()
        print "whatlines: ", whatlines

        inputSeqFile = open(seqFileName)
        charArr      = picklines(inputSeqFile, whatlines) # need to translate this to ints
        charArr.sort(key=lambda a: len(a))
        for i in range(3):
            print "character {}: length: {}".format(i, len(charArr[i]))
        print "/n"
        # print charArr

        intArrays = []
        for i in range(3):
            intArrays.append( list(map( lambda x: iupacDict[x.capitalize()], charArr[i][:-1] )) )

        shortLen  = len(intArrays[0])
        middleLen = len(intArrays[1])
        longLen   = len(intArrays[2])
        # print "seq lengths: {}, {}, {}".format(shortLen, middleLen, longLen)

    #     # don't need these, because [int] translates directly to C int[]
    #     # inputArray1 = ffi.new( "int[]", shortLen )
    #     # inputArray2 = ffi.new( "int[]", middleLen )
    #     # inputArray3 = ffi.new( "int[]", longLen )

    ##### Now start ffi call
        start = time()

        # They go in long, short middle order for use in driver, which also serves to drive 2D code.
        lib.wrapperFunction(intArrays[2], longLen, intArrays[0], shortLen, intArrays[1], middleLen)

        end = time()

        current  = end - start
        average += current
        timesFile.write("Run number {} time: {}\n".format(runNum, current)) # total time the C code ran
        timesFile.flush()

    average = average / numRuns
    timesFile.write("Average: {}".format(average))
    inputSeqFile.close()
    timesFile.close()

if __name__ == '__main__' : main()
