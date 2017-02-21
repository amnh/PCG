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

#include "dynamicCharacterOperations.h"

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


//int trueWt(struct align *path, const int alphSize, int wtInsertDel, int wtSub, int len);
int trueWt(struct align *path, costMtx_t* tcm, size_t alphSize, int len);

// EDIT: rectified with .c file.
//int aligner(char*, char*, int, int, struct retType*);
int aligner(uint64_t *seq1, size_t seq1Len, uint64_t *seq2, size_t seq2Len, size_t alphSize,
            costMtx_t *tcm, retType_t* retAlign);

/** no longer in use. Use costMatrixWrapper.getCost instead.
int getCost(uint64_t lhs, uint64_t rhs, costMtx_t* tcm, size_t alphSize)
*/

void freeRetType(retType_t* toFree);


#endif /* YUALIGN_H */
