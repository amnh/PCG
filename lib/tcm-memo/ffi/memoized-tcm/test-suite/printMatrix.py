alphabetSize = 10

print('{', end='')
for i in range(alphabetSize):
    for j in range(alphabetSize):
        if i == j:
            print('0', end='')
        else:
            print('1', end='')
        if j != alphabetSize - 1:
            print(', ', end='')
    print( '}\n{', end='')
