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

typedef struct align {
    int       partialWt;
    int       partialTrueWt;
    uint64_t *partialAlign;   // EDIT: made it dynamically allocable.
    uint64_t *partialAlignB;  // EDIT: made it dynamically allocable.
    size_t    posStringA;     // position at stringA
    size_t    posStringB;     // position at stringB
    size_t    posTrueA;       // position without gap
    size_t    posTrueB;       // position without gap
    int       flagWhichTree;  // belongs to first or second tree
} alignment_t;

alignment_t *initAlignment( int       in_partialWt
                          , int       in_partialTrueWt
                          , size_t    in_posStringA
                          , size_t    in_posStringB
                          , size_t    in_posTrueA
                          , size_t    in_posTrueB
                          , int       in_flagWhichTree
                          , size_t    initLength );

int trueWt( alignment_t  *path
          , costMatrix_p  tcm
          , size_t        offset
          , size_t        maxLen
          , size_t        alphSize );

// EDIT: rectified with .c file.
//int aligner(char*, char*, int, int, struct retType*);
int aligner( uint64_t     *seq1
           , size_t        seq1Len
           , uint64_t     *seq2
           , size_t        seq2Len
           , size_t        alphSize
           , costMatrix_p  tcm
           , retType_t    *retAlign
           );

void freeRetType(retType_t* toFree);

void copyAligmentStruct ( alignment_t   *copyFrom
                         , size_t        copyFromIdx
                         , alignment_t  *copyTo
                         , size_t        copyToIdx
                         , const size_t  initLength );

void printCostBuffer(int *buffer, size_t bufLen, char *prefix);

void printBuffer(uint64_t *buffer, size_t bufLen, char *prefix);

/** no longer in use. Use costMatrixWrapper.getCost instead.
int getCost(uint64_t lhs, uint64_t rhs, costMtx_t* tcm, size_t alphSize)
*/
#endif /* YUALIGN_H */
