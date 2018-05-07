#! /usr/bin/env python2

''' A script to run POY on twice on three DO characters. First run regular DO, then run 3d (iterative: exact) DO on the same three characters. This is to be able to compare the times for the runs of each. These times can then be compared to just the C 3d DO
code to see if the times are comparable.'''



from random import randrange
from subprocess import call
from sys import argv
from time import time


# this will fail if `whatlines` isn't sorted _and_ monotonically increasing.
def picklines(thefile, whatlines):
  return [x[:-1] for i, x in enumerate(thefile) if i in whatlines]


''' Output a POY file. Only one file needs to be created for each variation. They can be used for muliple runs.
    Get infile, which is already a file handle. Write out to that file.'''
def createPOYfileText(poyFile, filesuffix, version):
    #calls POY and runs it for time. Writes best trees out to outfilename

    outputFile_name = "../data/poy_{}_output_{}.txt".format(version, filesuffix)

    poyFile.write( 'read("../data/{}.fasta")\n'.format(filesuffix) )
    poyFile.write( "transform(tcm:(1,1))\n" )
    poyFile.write( 'read("../data/sample_3_taxa.tre")\n' )
    if version == 3:
        poyFile.write('set(iterative: exact)\n' )
    poyFile.write( 'report("' + outputFile_name + '", ia)\n' )
    poyFile.write( 'exit()' )
    return poyFile

def main():
    errorMsg = "\nFirst arg is data file (metazoa or chel); \nSecond is # processors; \nThird is number of runs per processor; \nFourth is which processor this is running on, starting at 0.\n"
    if len(argv) < 5:
        print errorMsg
        exit()

    dataFile = argv[1]
    possible_files = {"metazoa": ["2metazoa18s-short", "208"], "chel": ["chel", "17"]}
    if dataFile not in possible_files:
        print "Data file must be one of the following:", possible_files
        exit()

    try:
        howManyProcessors = int(argv[2])
        numRuns           = int(argv[3])
        whichProcessor    = int(argv[4])
    except:
        print errorMsg
        exit()

    seqFileName = possible_files[dataFile][0]
    timesFile = open("../data/times_poy_processor_{}.txt".format(whichProcessor), "w")

    averageTime = 0

    # set up seeds list
    randFile = open('../data/' + dataFile + '_randseeds.txt')
    seeds    = randFile.readlines()
    randFile.close()

    curIdx   = whichProcessor * numRuns * 3  # current index into seeds list
    # print "Start index:", curIdx

    for runNum in range(numRuns):
        # get next 3 seqs
        whatlines = []
        for i in range(3): # there are always three seqs being sent in
            # get 3 random numbers from random number file
            thisLine = seeds[curIdx]
            try:
                # print thisLine, end=""
                whatlines.append(int(thisLine) * 2 + 1) # Extra math because we're not getting that line, but that _sequence_
                curIdx += 1
            except:
                print "Randseed read failed. Run number:", runNum, "read number:", i, " value:", thisLine

        whatlines.sort() # so picklines() works
        print "Sequence filename: ", seqFileName
        print "whatlines: "        , whatlines

        # create input fasta file for POY
        inputSeqFile = open("../data/" + seqFileName + ".fasta")
        charArr      = picklines(inputSeqFile, whatlines)
        inputSeqFile.close()

        # suffixes for input .fasta files, output .txt files and .poy files
        filesuffix = "{}_processor_{}_run_{}".format(seqFileName, whichProcessor, runNum)
        outputFastaFile = open("../data/{}.fasta".format(filesuffix), "w")

        for i in range(3):
            # print "{}\n".format(lineNum)
            outputFastaFile.write(">{}\n{}\n".format(i, charArr[i]))
            # print ">{}\n{}\n".format(i, charArr[i])

        outputFastaFile.close()

        poy_file_2d = open( "../POY/infile_2d_{}.poy".format(filesuffix), "w" )
        poy_file_3d = open( "../POY/infile_3d_{}.poy".format(filesuffix), "w" )

        poy_file_2d = createPOYfileText( poy_file_2d
                                       , filesuffix
                                       , 2
                                       )
        poy_file_3d = createPOYfileText( poy_file_3d
                                       , filesuffix
                                       , 3
                                       )

        poy_file_2d.close()
        poy_file_3d.close()

        # note that in subprocess.call, arguments is list of strings

        # non-3d run
        start_2d = time()
        call(['poy5', "../POY/infile_2d_{}.poy".format(filesuffix)])
        end_2d = time()

        # 3d (iterative: exact) run
        start_3d = time()
        call(['poy5', "../POY/infile_3d_{}.poy".format(filesuffix)])
        end_3d  = time()
        current = (end_3d - start_3d) - (end_2d - start_2d)
        timesFile.write("Run number {} time: {}\n".format(runNum, current)) # total time the POY code ran, which is
                                                                            # 3d run - 2d run, to eliminate input and setup time.
        timesFile.flush()
        averageTime += current

    averageTime /= numRuns
    timesFile.write("Average: {}".format(averageTime))
    timesFile.close()

if __name__ == '__main__' : main()
