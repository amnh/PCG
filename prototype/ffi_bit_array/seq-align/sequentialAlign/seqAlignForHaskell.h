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
#include "seqAlignOutputTypes.h"

typedef void* costMatrix_p;

/** Holds intermediate alignments and current pointers into those arrays. */
typedef struct align {
    int       gapped_partialCost;        //TODO: figure out what these do.
    int       ungapped_partialCost;
    uint64_t *partialAlign_A;
    uint64_t *partialAlign_B;
    size_t    aligned_sequence_A_end_ptr;     // current index in aligned sequenceA
    size_t    aligned_sequence_B_end_ptr;     // current index in aligned sequenceB
    size_t    input_sequence_A_ptr;           // current index in input sequenceA   -- for input sequences, since there are
    size_t    input_sequence_B_ptr;           // current index in input sequenceB   -- we need a pointer for each of multiple alignments
    int       flagWhichTree;                  // belongs to first or second tree
} alignment_t;

/** for use in updateSequences(), to know whether I'm going A -> GAP, GAP -> B, A -> B */
enum transition { A_TO_GAP, GAP_TO_B, A_TO_B };

/** Allocs enough space to hold four sequences: two gapped and two ungapped.
 *  Also assigns input values to pointers into each sequence
 */
alignment_t *initAlignment( int    in_gapped_partialCost
                          , int    in_ungapped_partialCost
                          , size_t in_aligned_sequence_A_end_ptr
                          , size_t in_aligned_sequence_B_end_ptr
                          , size_t in_input_sequence_A_ptr
                          , size_t in_input_sequence_B_ptr
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
int aligner( uint64_t     *seq1
           , size_t        seq1Len
           , uint64_t     *seq2
           , size_t        seq2Len
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

/** sort two arrays at once, using the values in firstArray as the sort keys */
void doubleBubbleSort(int *valArray, int *secondArray, size_t number_Elements);

void printCostBuffer(int *buffer, size_t bufLen, char *prefix);

void printBuffer(uint64_t *buffer, size_t bufLen, char *prefix);

/** increment value, making sure that new value is < bound */
size_t boundedIncrement(size_t value, size_t bound);

/** Honestly not yet sure what this does. It shuffles a bunch of values back and forth inside path
 *  and updates path->partialCost.
 */
int updateSequences( alignment_t       *path
                   , uint64_t          *seqA
                   , size_t             lengthSeqA
                   , uint64_t          *seqB
                   , size_t             lengthSeqB
                   , uint64_t           GAP
                   , size_t             SEQ_MAX_LEN
                   , const costMatrix_p tcm
                   , size_t             alphSize
                   , size_t             flagEmpty
                   , enum transition    whichSub
                   , int                cost);


#endif /* YUALIGN_H */
