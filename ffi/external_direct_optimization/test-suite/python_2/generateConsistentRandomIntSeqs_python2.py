#! /usr/bin/env python

from sys import argv
from time import time
from random import random, randrange, seed


errorMsg = "\nFirst arg is data file (metazoa or chel); \nSecond is # processors; \nThird is number of runs per processor.\n"
if len(argv) < 4:
    print errorMsg
    exit()

dataFile = argv[1]
possible_files = {"metazoa": ["2metazoa18s-short.fasta", 208], "chel": ["chel.fasta", 17]}
if dataFile not in possible_files:
    print("Data file must be one of the following:", possible_files)
    exit()

try:
    numParallelRuns     = int(argv[2])
    numRunsPerProcesser = int(argv[3])
    numSeqs             = int(possible_files[dataFile][1] / 2)
except:
    print errorMsg
    exit()

# print("\n\n\ngenerating\n\n\n")

seed_var = int(time())
seed(seed_var)
# seed(1507299148)

# print(int(.543276826982 * 100) % 24)

randSeedFile = open("../data/" + dataFile + "_randseeds.txt", "w")
lastNum     = -1
lastLastNum = -1
for i in range(numParallelRuns * numRunsPerProcesser * 3): # 3 because 3 seqs
    # outputs mod numSeqs, which is half the number of seqs in file. In run script we'll will have to double and add 1 to make it odd (seqs are on odd lines.)
    thisNum = int(random() * 1000) % numSeqs
    # have to do this because getting three seqs at at time
    while thisNum == lastNum or thisNum == lastLastNum:
        thisNum = int(random() * 1000) % numSeqs
    randSeedFile.write("{}\n".format(thisNum))
    lastLastNum = lastNum
    lastNum = thisNum
    # print "{} {}".format(i, thisNum)

randSeedFile.close()
