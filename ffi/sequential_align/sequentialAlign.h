//
//  sequentialAlign.h
//  version_Haskell_bit
//
//  Created by Yu Xiang on 11/1/16.
//  Copyright © 2016 Yu Xiang. All rights reserved.
//

#ifndef SEQUENTIAL_ALIGN_H
#define SEQUENTIAL_ALIGN_H

#include <stddef.h>

#include "../memoized_tcm/costMatrixWrapper.h"
#include "../memoized_tcm/dynamicCharacterOperations.h"
#include "sequentialAlignOutputTypes.h"

typedef void* costMatrix_p;

/** Holds intermediate alignments and current pointers into those arrays. */
typedef struct align {
    int       gapped_partialCost;        //TODO: figure out what these do.
    int       ungapped_partialCost;
    uint64_t *partialAlign_A;
    uint64_t *partialAlign_B;
    size_t    aligned_character_A_end_ptr;     // current index to end of aligned characterA
    size_t    aligned_character_B_end_ptr;     // current index to end of aligned characterB

    // For each input character there are multiple alignments; we need a pointer for each:
    size_t    input_character_A_ptr;           // current index to beginning of unconsumed portion of original characterA
    size_t    input_character_B_ptr;           // current index to beginning of unconsumed portion of original characterB
    int       flagWhichTree;                   // belongs to first or second tree
} alignment_t;

/** for use in updateCharacters(), to know whether I'm going A -> GAP, GAP -> B, A -> B */
enum transition { A_TO_GAP, GAP_TO_B, A_TO_B };

/** Allocs enough space to hold four characters: two gapped and two ungapped.
 *  Also assigns input values to pointers into each character
 */
alignment_t *initAlignment( int    in_gapped_partialCost
                          , int    in_ungapped_partialCost
                          , size_t in_aligned_character_A_end_ptr
                          , size_t in_aligned_character_B_end_ptr
                          , size_t in_input_character_A_ptr
                          , size_t in_input_character_B_ptr
                          , int    in_flagWhichTree
                          , size_t initLength
                          );

/** Computes the quadratic (?) cost of the gapped alignment to that point. */
int currentAlignmentCost( alignment_t  *path
              , costMatrix_p  tcm
              , size_t        maxLen
              , size_t        alphSize
              );

/** Does actual alignment */
int aligner( uint64_t     *char1
           , size_t        char1Len
           , uint64_t     *char2
           , size_t        char2Len
           , size_t        alphSize
           , costMatrix_p  tcm
           , retType_t    *retAlign
           );

void freeRetType(retType_t *toFree);

/** copy contents of one alignment struct into another */
void copyAligmentStruct ( alignment_t  *copyTo
                        , size_t        copyToIdx
                        , alignment_t  *copyFrom
                        , size_t        copyFromIdx
                        , const size_t  initLength
                        );

/** sort two arrays at once, using the values in valArray as the sort keys */
void doubleBubbleSort(int *valArray, int *secondArray, size_t number_Elements);

void printCostBuffer(int *buffer, size_t bufLen, char *prefix);

void printBuffer(uint64_t *buffer, size_t bufLen, char *prefix);

/** increment value, making sure that new value is < bound */
size_t boundedIncrement(size_t value, size_t bound);

/** Takes as input a path, which has two characters---the original input characters, and a variable
 *  which determines what is getting aligned (A/Gap, Gap/B or A/B). Updates the current cost of
 *  the new alignment, which is either the current cost summed with the passed-in cost (determined
 *  by the new aligned characters), or by finding the accumulated cost of the alignment thus far.
 *  Also updates the two characters by adding the appropriate character at the end of each, based on
 *  the input direction.
 */
int updateCharacters( alignment_t       *path
                    , uint64_t          *charA
                    , size_t             lengthCharA
                    , uint64_t          *charB
                    , size_t             lengthCharB
                    , uint64_t           GAP
                    , size_t             SEQ_MAX_LEN
                    , const costMatrix_p tcm
                    , size_t             alphSize
                    , size_t             flagEmpty
                    , enum transition    whichSub
                    , int                cost
                    );


#endif /* SEQUENTIAL_ALIGN */
