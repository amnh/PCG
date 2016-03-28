import random
import re

#make random sequences
def makeSeqs():
    allSeqs = []
    alph = ["A", "C", "G", "T"]
    for i in range(46):
        random.seed(i)
        myStr = ""
        for j in range(300):   
            myStr += random.choice(alph)
        allSeqs.append(myStr)

    print allSeqs

#make a random sequence of some length
def makeAASeq(inLen):
    alph = ["A", "C", "G", "T", "-"]
    myStr = ""
    for i in range(inLen):
        myStr += random.choice(alph)
    return myStr

#check the resulting output
def checkSeqs():
    fasNames = []
    with open("fakeArtmor.fas", "r") as infile:
        infile.next()
        initLen = len(infile.next())
        index = 1
        for line in infile:
            if line[0] != ">":
                if len(line) != initLen:
                    print "Length of line " + str(index)+ " does not match "
            else:
                fasNames.append(line[1:-1])
            index +=1

    with open("SIngleArtmor.tre", "r") as tree:
        treeNames = re.findall("[A-Za-z]+[0-9]*", tree.read())


    for tn in treeNames:
        if tn not in fasNames:
            print "Taxa not present in fasta " + tn

    for fn in fasNames:
        if fn not in treeNames:
            print "Fasta sequence not present in tree " + fn

#make a random file with the taxa names grabbed from the artmor
def makeArtmorFile():
    names = []
    with open("fakeArtmor.fas", "r") as infile:
        for line in infile:
            if line[0] == ">":
                names.append(line)

    with open("smallArtmor.fas", "w") as outfile:
        for n in names:
            outfile.write(n)
            seq = makeAASeq(5)
            outfile.write(seq + "\n")

checkSeqs()