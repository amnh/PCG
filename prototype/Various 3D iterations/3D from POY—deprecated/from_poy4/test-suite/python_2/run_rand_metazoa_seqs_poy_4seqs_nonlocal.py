#! /usr/bin/env python

''' A script to run POY on twice on three DO characters. First run regular DO, then run 3d (iterative: exact) DO on the same three characters. This is to be able to compare the times for the runs of each. These times can then be compared to just the C 3d DO
code to see if the times are comparable.

"Nonlocal" in the filename means it runs on a different server, which only has Python 2 installed. Actually, I guess I could use it on a copputer with Python 3, too, and just _call_ Python 2...'''



from random import randrange
from subprocess import check_output

import subprocess
import time



''' Output a POY file. Only one file needs to be created for each variation. They can be used for muliple runs.
    Get infile, which is already a file handle. Write out to that file.'''
def createPOYfileText(poyFile, outputFile_name, version):
    #calls POY and runs it for time. Writes best trees out to outfilename
    poyFile.write( 'read("2metazoa_cutdown_poy_4seqs.fasta")\n' )
    poyFile.write("transform(tcm:(1,1))\n")
    poyFile.write('read("./sample_4_taxa.tre")\n')
    # poyFile.write('report("four_tree_poy_output.txt", diagnosis, trees)\n')
    #outfile.write( 'build(1)\n' )
    #outfile.write( 'swap(tbr)\n' )
    if version == 3:
        poyFile.write('set(iterative: exact)')
    poyFile.write( 'report("' + outputFile_name + '", trees:(total))\n' )
    poyFile.write( 'exit()' )
    return poyFile

def main():
    # print sys.version
    numRuns = 10

    # iupacDict = {'A': 1, 'C': 2, 'G': 4, 'T': 8, 'U': 8, 'M': 3, 'R': 5, 'S': 6, 'V': 7, 'W': 9, 'Y': 10, 'H': 11, 'K': 12, 'D': 13, 'B': 14, 'N': 15, '.': 16, '-': 16}

    infile_name  = "2metazoa18s-short.fasta"

    poy_file_2d = open( "../POY/infile_2d_4seqs.poy", "w" )
    poy_file_3d = open( "../POY/infile_3d_4seqs.poy", "w" )

    poy_file_2d = createPOYfileText(poy_file_2d, "poy_output_2d_4seqs_tree.txt", 2)
    poy_file_3d = createPOYfileText(poy_file_3d, "poy_output_3d_4seqs_tree.txt", 3)

    poy_file_3d.close()
    poy_file_2d.close()

    # get number of lines in fasta file
    wcOutput = ''.join( map(bytes, check_output(['wc', '-l', infile_name])) ).split()[0]
    numLines  = (int(wcOutput) + 1) # for some reason `wc -l` is returning numLines - 1. Maybe it's only counting newlines?

    inLines = []
    infile  = open(infile_name)
    # print numLines
    for i in range(numLines / 2):
        infile.readline()
        # print i
        line = infile.readline()
        # print line
        inLines.append(line)
    # print "total chars: {}".format(len(inLines))
    infile.close()

    # print(wcOutput)

    timesFile = open("times_poy_4_seqs.txt", "w")

    average = 0
    for run in range(numRuns):
        fastaFile = open("2metazoa_cutdown_poy_4seqs.fasta", "w")
        for i in range(4):
            lineNum = randrange(numLines / 2)
            # print "{}\n".format(lineNum)
            fastaFile.write(">{}\n{}\n".format(i, inLines[lineNum]))
            # print ">{}\n{}\n".format(i, inLines[lineNum])

        fastaFile.close()


        # print("whatlines: ", whatlines)

        # note that in subprocess.call, arguments is list of strings

        # non-3d run
        start_2d = time.time()
        subprocess.call(['poy5', 'infile_2d_4seqs.poy'])
        end_2d = time.time()

        # 3d (iterative: exact) run
        start_3d = time.time()
        subprocess.call(['poy5', 'infile_3d_4seqs.poy'])
        end_3d = time.time()
        current = (end_3d - start_3d) - (end_2d - start_2d)
        timesFile.write("Run number {} time: {}\n".format(run, current)) # total time the C code ran
        timesFile.flush()
        average += current

    average /= numRuns
    timesFile.write("Average: {}".format(average))
    timesFile.close()
if __name__ == '__main__' : main()