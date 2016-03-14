import random

allSeqs = []
alph = ["A", "C", "G", "T"]
for i in range(46):
    random.seed(i)
    myStr = ""
    for j in range(300):   
        myStr += random.choice(alph)
    allSeqs.append(myStr)

print allSeqs