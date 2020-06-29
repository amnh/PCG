#! /usr/bin/env python

from sys import argv

from time import time

from random import random, randrange, seed

if len(argv) < 4:
    print( "First arg is # of processors, second is # of runs per processors running on, third is number of sequences in data file." )
    exit()

# print argv

try:
    numParallelRuns     = int(argv[1])
    numRunsPerProcesser = int(argv[2])
    numSeqs             = int(argv[3])
except:
    print( "First arg is # of processors, second is # of runs per processors running on, third is number of sequences in data file." )
    exit()

# print("\n\n\ngenerating\n\n\n")

seed_var = int(time())
seed(seed_var)
# seed(1507299148)

# print(int(.543276826982 * 100) % 24)

randSeedFile = open("../data/randseeds.txt", "w")
lastNum     = -1
lastLastNum = -1
for i in range(numParallelRuns * numRunsPerProcesser * 3): # 3 because 3 seqs
    # outputs mod 208, which is half the number of seqs in file. In run script we'll will have to double and add 1 to make it odd (seqs are on odd lines.)
    thisNum = int(random() * 1000) % numSeqs
    while thisNum == lastNum or thisNum == lastLastNum:
        thisNum = int(random() * 1000) % numSeqs
    randSeedFile.write("{}\n".format(thisNum))
    lastLastNum = lastNum
    lastNum     = thisNum
    # print( thisNum )

randSeedFile.close()
