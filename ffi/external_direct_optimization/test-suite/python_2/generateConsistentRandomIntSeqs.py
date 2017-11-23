#! /usr/bin/env python

from sys import argv

from time import time

from random import random, randrange, seed

if len(argv) < 3:
    print "First arg is # of processors, second is # of runs per processors running on."
    exit()

# print argv

numParallelRuns     = int(argv[1])
numRunsPerProcesser = int(argv[2])

seed_var = int(time())
seed(seed_var)
# seed(1507299148)

# print(int(.543276826982 * 100) % 24)

randSeedFile = open("../data/randseeds.txt", "w")
lastNum = -1
for i in range(numParallelRuns * numRunsPerProcesser * 3): # 3 because 3 seqs
    # outputs mod 208, which is half the length of 2metazoa. In run script we'll will have to double and add 1 to make it odd (seqs are on odd lines.)
    thisNum = int(random() * 1000) % 208
    while thisNum == lastNum:
        thisNum = int(random() * 1000) % 208
    randSeedFile.write("{}\n".format(thisNum))
    lastNum = thisNum
    # print "{} {}".format(i, thisNum)
