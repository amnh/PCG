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

// EDIT: Added this struct so the job on my end would be easier.
// Wasn't sure what the final weight was, so copied partialTrueWt.

// I updated retType so that it returns two sequences. I thought I'd done that before, but I guess not.
// Since sequences are int arrays, they're not terminated by \0, and therefore the two lengths are also necessary
typedef struct retType {
    int weight;
  //  char* seq1;
    int* seq1;
    size_t seq1Len;
  //  char* seq2;
    int* seq2;
    size_t seq2Len;
    long int alignmentLength;
} retType_t;

int trueWt(struct align *path, const int alphSize, int wtInsertDel, int wtSub, int len);

// EDIT: rectified with .c file.
//int aligner(char*, char*, int, int, struct retType*);
int aligner(uint64_t *seq1, size_t seq1Len, uint64_t *seq2, size_t seq2Len, size_t alphSize,
            int wtInsertDel, int wtSub, retType_t* retAlign);

void freeRetType(retType_t* toFree);

int getCost(uint64_t lhs, uint64_t rhs, costMtx_t* tcm, size_t alphSize);


#endif /* YUALIGN_H */
