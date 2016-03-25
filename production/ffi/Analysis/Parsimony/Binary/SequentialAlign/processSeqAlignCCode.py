#! /usr/bin/env python3

infile = open('./seqAlign_ffi_c.c')
outfile = open('./seqAlignForHaskell.c', 'w')
inMain = False
done = False
braceCtr = 0
for line in infile:
    if not done and line.find('int main()') > -1:
        inMain = True
    if inMain:
        idxOpen = line.find('{')
        idxClose = line.find('}')
        idxComment = line.find('//')
        if idxOpen > -1 and idxOpen > idxComment:
            braceCtr += 1
        if idxClose > -1 and idxClose > idxComment:
            braceCtr -= 1
        outfile.write("// ")
        if braceCtr == 0:
            inMain = False
            done = True
    outfile.write(line)

infile.close()
outfile.close()
