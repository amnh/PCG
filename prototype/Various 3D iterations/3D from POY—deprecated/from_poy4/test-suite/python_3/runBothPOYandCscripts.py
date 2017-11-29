#! usr/bin/env python

from subprocess import call
from sys import argv

if len(argv) < 3:
    print "Need two int arguments to determine how many processors to run parallel scripts on. The first number is the number of runs to do for each call. The secodn number will be 1/2 the total number of processors, as  there are two scripts, one for C only and one for POY."
    exit()

numProcessors = int(argv[1])
numRuns       = int(argv[2])

# call POY script
for processor in range(numProcessors):
    call(['./generateConsistentRandomIntSeqs.py', argv[1], argv[2]])

for processor in range(numProcessors):
    # open a new screen
    call(['screen', '-q'])
    for run in range(numRuns):


# call C only script

