#! /usr/bin/env python2

''' A script to run POY on twice on three DO characters. First run regular DO, then run 3d (iterative: exact) DO on the same three characters. This is to be able to compare the times for the runs of each. These times can then be compared to just the C 3d DO
code to see if the times are comparable.'''



from random import randrange
from subprocess import call
from sys import argv
from time import time


def picklines(thefile, whatlines):
  return [x[:-1] for i, x in enumerate(thefile) if i in whatlines]


''' Output a POY file. Only one file needs to be created for each variation. They can be used for muliple runs.
    Get infile, which is already a file handle. Write out to that file.'''
def createPOYfileText(poyFile, outputFile_name, processor, run, version):
    #calls POY and runs it for time. Writes best trees out to outfilename
    poyFile.write( 'read("../data/2metazoa_cutdown_processor_{}_run_{}.fasta")\n'.format(processor, run) )
    poyFile.write( "transform(tcm:(1,1))\n" )
    poyFile.write( 'read("../data/sample_3_taxa.tre")\n' )
    if version == 3:
        poyFile.write('set(iterative: exact)\n' )
    poyFile.write( 'report("' + outputFile_name + '", trees:(total))\n' )
    poyFile.write( 'exit()' )
    return poyFile

def main():
    if len(argv) < 3 or len(argv) > 3:
        print "First arg is # processors, second is which processor this is running on, starting at 0."
        exit()

    howManyProcessors = int(argv[1])
    whichProcessor    = int(argv[2])

    numRuns = 10

    seqFileName  = "../data/2metazoa18s-short.fasta"

    timesFile = open("../data/times_poy_processor_{}.txt".format(whichProcessor), "w")

    average = 0
    for runNum in range(numRuns):
        randFile  = open('../data/randseeds.txt')

        # skip to correct line in random number file (always 3 seqs)
        startLine = (whichProcessor * numRuns * 3) + (runNum * 3)
        for i in range(0, startLine):
            x = randFile.readline()

        # get next 3 seqs
        whatlines = []
        for i in range(3): # there are always three seqs being sent in
            # get 3 random numbers from random number file
            whatlines.append( int(randFile.readline()) * 2 + 1 ) # Extra math because we're not getting that line, but that _sequence_
        randFile.close()
        whatlines.sort()

        poy_file_2d = open( "../POY/infile_2d_processor_{}_run_{}.poy".format(whichProcessor, runNum), "w" )
        poy_file_3d = open( "../POY/infile_3d_processor_{}_run_{}.poy".format(whichProcessor, runNum), "w" )

        poy_file_2d = createPOYfileText(poy_file_2d, "../data/poy_output_2d_run.txt", whichProcessor, runNum, 2)
        poy_file_3d = createPOYfileText(poy_file_3d, "../data/poy_output_3d_run.txt", whichProcessor, runNum, 3)

        poy_file_3d.close()
        poy_file_2d.close()

        inputSeqFile = open(seqFileName)
        charArr      = picklines(inputSeqFile, whatlines)
        outputFastaFile = open("../data/2metazoa_cutdown_processor_{}_run_{}.fasta".format(whichProcessor, runNum), "w")

        for i in range(3):
            # print "{}\n".format(lineNum)
            outputFastaFile.write(">{}\n{}\n".format(i, charArr[i]))
            # print ">{}\n{}\n".format(i, charArr[i])

        outputFastaFile.close()


        # print("whatlines: ", whatlines)

        # note that in subprocess.call, arguments is list of strings

        # non-3d run
        start_2d = time()
        call(['poy5', "infile_2d_processor_{}_run_{}.poy".format(whichProcessor, runNum)])
        end_2d = time()

        # 3d (iterative: exact) run
        start_3d = time()
        call(['poy5', "infile_3d_processor_{}_run_{}.poy".format(whichProcessor, runNum)])
        end_3d = time()
        current = (end_3d - start_3d) - (end_2d - start_2d)
        timesFile.write("Run number {} time: {}\n".format(runNum, current)) # total time the C code ran
        timesFile.flush()
        average += current

    average /= numRuns
    timesFile.write("Average: {}".format(average))
    timesFile.close()
if __name__ == '__main__' : main()