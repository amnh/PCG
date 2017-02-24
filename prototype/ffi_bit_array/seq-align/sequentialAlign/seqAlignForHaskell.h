//
//  seqAlignForHaskell.h
//  version_Haskell_bit
//
//  Created by Yu Xiang on 11/1/16.
//  Copyright © 2016 Yu Xiang. All rights reserved.
//

#ifndef YUALIGN_H
#define YUALIGN_H

#include <stddef.h>

#include "costMatrixWrapper.h"
#include "dynamicCharacterOperations.h"

typedef void* costMatrix_p;

struct align {
    int partialWt;
    int partialTrueWt;
 //   char* partialAlign; // EDIT: made it dynamically allocable.
    int* partialAlign; // EDIT: made it dynamically allocable.
    // char partialAlign[20];
    int posStringA;   // position at stringA
    int posStringB;   // position at stringB
    int posTrueA;     // position without gap
    int posTrueB;     // position without gap
    int flagWhichTree;  // belongs to first or second tree
};


int trueWt(struct align *path, costMatrix_p, int len, size_t alphSize);

// EDIT: rectified with .c file.
//int aligner(char*, char*, int, int, struct retType*);
int aligner(uint64_t *seq1, size_t seq1Len, uint64_t *seq2, size_t seq2Len, size_t alphSize,
            costMatrix_p tcm, retType_t *retAlign);

void freeRetType(retType_t* toFree);


#endif /* YUALIGN_H */
