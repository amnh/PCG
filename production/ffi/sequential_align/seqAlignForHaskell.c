//  aligner.c
//  version_Haskell_bit
//
//  Created by Yu Xiang on 11/1/22.
//  Copyright © 2016 Yu Xiang. All rights reserved.

#include <inttypes.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "dynamicCharacterOperations.h"
#include "seqAlignForHaskell.h"

// this and <inttypes.h> so I can use PRIu64 instead of llu
#define __STDC_FORMAT_MACROS


int aligner( uint64_t *inSeq1
           , size_t    inSeq1Len
           , uint64_t *inSeq2
           , size_t    inSeq2Len
           , size_t    alphSize
           , costMatrix_p tcm
           , retType_t *retAlign)
{
    setbuf(stdout, NULL);

    const uint64_t GAP        = CANONICAL_ONE << (alphSize - 1);
    // printf("alphSize: %zu  %" PRIu64 "\n", alphSize, GAP);

    const size_t SEQ_MAX_LEN  = inSeq1Len + inSeq2Len;  // Maximum alignment length of either seqA or seqB (pathological case)

    size_t i, j;                                            // because i and j are being used a lot,
                                                            // and they were declared later on anyway
    int cost;                                               // This will be used later to send in to update sequence fn.

    size_t lengthSeqA = inSeq1Len,
           lengthSeqB = inSeq2Len;

    if (SEQ_MAX_LEN == 0) {
        // printf("The lengths of the both sequences are zero!\n");
        return 2;
    }

    //Yu_Edit  dynamically allocate struct
    //*******************  initialize struct ***************************
    alignment_t path[3];
    path[0] = *initAlignment(0, 0, 0, 0, 0, 0, 1, SEQ_MAX_LEN);
    path[1] = *initAlignment(0, 0, 0, 0, 0, 0, 1, SEQ_MAX_LEN);
    path[2] = *initAlignment(0, 0, 0, 0, 0, 0, 1, SEQ_MAX_LEN);

    /************************************************ original metric *****************************************************/
    // a*(\sum z_i)^2 + b*(\sum z_i) + c*(\sum z_i^2)
    // WLOG let a = b = c = 1
    //   (\sum z_i)^2 +   (\sum z_i) +   (\sum z_i^2)


    // first metric
    // \sum z_i

    // second metric
    // \sum z_i^2

    alignment_t pathTempFirst[3];
    pathTempFirst[0] = *initAlignment(0, 0, 0, 0, 0, 0, 1, SEQ_MAX_LEN);
    pathTempFirst[1] = *initAlignment(0, 0, 0, 0, 0, 0, 1, SEQ_MAX_LEN);
    pathTempFirst[2] = *initAlignment(0, 0, 0, 0, 0, 0, 1, SEQ_MAX_LEN);

    alignment_t pathTempSecond[3];
    pathTempSecond[0] = *initAlignment(0, 0, 0, 0, 0, 0, 1, SEQ_MAX_LEN);
    pathTempSecond[1] = *initAlignment(0, 0, 0, 0, 0, 0, 1, SEQ_MAX_LEN);
    pathTempSecond[2] = *initAlignment(0, 0, 0, 0, 0, 0, 1, SEQ_MAX_LEN);


    alignment_t pathFirstInfinite  = *initAlignment(100000, 100000, 0, 0, 0, 0, 1, SEQ_MAX_LEN);

    alignment_t pathSecondInfinite = *initAlignment(100000, 100000, 0, 0, 0, 0, 2, SEQ_MAX_LEN);

    alignment_t finalAlign         = *initAlignment(0,      0,      0, 0, 0, 0, 1, SEQ_MAX_LEN);

    uint64_t *seqA = calloc( lengthSeqA, sizeof(uint64_t) );
    uint64_t *seqB = calloc( lengthSeqB, sizeof(uint64_t) );

    // Now, test for allocation. Return 1 if it fails.
    if( seqA == NULL || seqB == NULL ) {
        return 1;
    }

    // This looks good, making sure not to clobber your input
    memcpy( seqA, inSeq1, sizeof(uint64_t) * (lengthSeqA) );
    memcpy( seqB, inSeq2, sizeof(uint64_t) * (lengthSeqB) );

    printBuffer(seqA, lengthSeqA, "SeqA");
    printBuffer(seqB, lengthSeqB, "SeqB");

    int flag         = 0;
    int flagEmpty[2] = {1,1};   // indicator for the case when one tree becomes empty,
                                // i.e., all the candidates nodes have converged to the other tree

    int indicatorFirst, indicatorSecond, indicatorInitial, indicatorMix;
    int kInitial, kFirst, kSecond, kMix;
    int iMatchFirst[3]  = { 0, 0, 0};
    int iMatchSecond[3] = { 0, 0, 0};

    uint64_t *alignFinalA = calloc( SEQ_MAX_LEN, sizeof(uint64_t) );
    uint64_t *alignFinalB = calloc( SEQ_MAX_LEN, sizeof(uint64_t) );
    if (alignFinalA == NULL || alignFinalB == NULL) {
        return 1;
    }

    int iFirst  = 0,
        iSecond = 0;


    //*******************************  Initialization first level generation for both trees ****************************//
    // under \sum z_i measure

    int aToB0,    // Following will hold getCost values, so we don't have to call getCost() over and over.
        aToGap0,  // 0 values get cost using sequence at 0 index in array of structs, etc.
        gapToB0,
        aToB1,
        aToGap1,
        gapToB1,
        aToB2,
        aToGap2,
        gapToB2;

    printf("1st   a: %2" PRIu64 " b: %2" PRIu64 "\n", seqA[0], seqB[0]);

    aToB0   = getCost(seqA[0], seqB[0], tcm, alphSize);
    aToGap0 = getCost(seqA[0], GAP,     tcm, alphSize);
    gapToB0 = getCost(GAP,     seqB[0], tcm, alphSize);

    alignment_t pathFirst[3];
    pathFirst[0] = *initAlignment(aToB0,   aToB0   + 2 * aToB0   * aToB0,   1, 1, 1, 1, 1, SEQ_MAX_LEN);
    pathFirst[1] = *initAlignment(aToGap0, aToGap0 + 2 * aToGap0 * aToGap0, 1, 1, 1, 0, 1, SEQ_MAX_LEN);
    pathFirst[2] = *initAlignment(gapToB0, gapToB0 + 2 * gapToB0 * gapToB0, 1, 1, 0, 1, 1, SEQ_MAX_LEN);


    pathFirst[0].partialAlignA[0] = seqA[0];
    pathFirst[0].partialAlignB[0] = seqB[0];

    pathFirst[1].partialAlignA[0] = seqA[0];
    pathFirst[1].partialAlignB[0] = GAP;

    pathFirst[2].partialAlignA[0] = GAP;
    pathFirst[2].partialAlignB[0] = seqB[0];

    // !! the two weights (wtSub, wtInsertDel) are the same as in pathFirst
    // !! wtSub=getCost(seqA[0],seqB[0]);
    // !! wtInsertDel=getCost(seqa[0], '-');

    //  under \sum z_i^2 measure

    printf("2nd   a: %2" PRIu64 " b: %2" PRIu64 "\n", seqA[0], seqB[0]);
    aToB0   = getCost(seqA[0], seqB[0], tcm, alphSize);
    aToGap0 = getCost(seqA[0], GAP,     tcm, alphSize);
    gapToB0 = getCost(GAP,     seqB[0], tcm, alphSize);

    alignment_t pathSecond[3];
    pathSecond[0] = *initAlignment(aToB0   * aToB0,   aToB0   + 2 * aToB0   * aToB0,   1, 1, 1, 1, 2, SEQ_MAX_LEN);
    pathSecond[1] = *initAlignment(aToGap0 * aToGap0, aToGap0 + 2 * aToGap0 * aToGap0, 1, 1, 1, 0, 2, SEQ_MAX_LEN);
    pathSecond[2] = *initAlignment(gapToB0 * gapToB0, gapToB0 + 2 * gapToB0 * gapToB0, 1, 1, 0, 1, 2, SEQ_MAX_LEN);

    pathSecond[0].partialAlignA[0] = seqA[0];
    pathSecond[0].partialAlignB[0] = seqB[0];

    pathSecond[1].partialAlignA[0] = seqA[0];
    pathSecond[1].partialAlignB[0] = GAP;

    pathSecond[2].partialAlignA[0] = GAP;
    pathSecond[2].partialAlignB[0] = seqB[0];


    printf("3rd   a: %2" PRIu64 " b: %2" PRIu64 "\n", seqA[0], seqB[0]);

    /* Next three values are unchanged from previous setting. */
    // aToB0   = getCost(seqA[0], seqB[0], tcm, alphSize);
    // aToGap0 = getCost(seqA[0], GAP,     tcm, alphSize);
    // gapToB0 = getCost(GAP,     seqB[0], tcm, alphSize);

    int arrayInitial[2][6] = {
        { 10, 20, 30, 11, 21, 31 },
        { aToB0   + 2 * aToB0   * aToB0   // gapped partial weight of pathSecond[0] (see above)
        , aToGap0 + 2 * aToGap0 * aToGap0 // gapped partial weight of pathSecond[1]
        , gapToB0 + 2 * gapToB0 * gapToB0 // gapped partial weight of pathSecond[2]
        , aToB0   + 2 * aToB0   * aToB0   // gapped partial weight of pathSecond[0]
        , aToGap0 + 2 * aToGap0 * aToGap0 // gapped partial weight of pathSecond[1]
        , gapToB0 + 2 * gapToB0 * gapToB0 // gapped partial weight of pathSecond[2]
        }
    };
    printf("arrayInitial[0][0]: %2d\n", arrayInitial[0][0]);
    // printCostBuffer(arrayInitial[0], 6, "arrayInitial[0]");
    // printCostBuffer(arrayInitial[1], 6, "arrayInitial[1]");

            //printCostBuffer(arrayInitial[1], 6, "arrayInitial[1]");

    // Bubble sort, keeping values in arrayInitial[0] in same relative order as arrayInitial[1]
    doubleBubbleSort(arrayInitial[1], arrayInitial[0], 6);

            //printCostBuffer(arrayInitial[0], 6, "arrayInitial[0]");
            //printCostBuffer(arrayInitial[1], 6, "arrayInitial[1]");

    for (i = 0; i < 3; i++) {

        indicatorInitial = arrayInitial[0][i];           // decide which operation to make
        kInitial         = indicatorInitial % 10;        // decide which path it belongs to
        printf("arrayInitial[0]: %2d\n", arrayInitial[0][i]);
        printf("kInitial: %2d indicatorInitial %2d\n", kInitial, indicatorInitial);

        // if lowest cost is a -> b transition
        if (kInitial == 0 && indicatorInitial >= 9 && indicatorInitial < 12) {
            copyAligmentStruct(path, i, pathFirst, 0, SEQ_MAX_LEN);
        }
        // if lowest cost is
        else if (kInitial == 0 && indicatorInitial >= 19 && indicatorInitial < 22) {
            copyAligmentStruct(path, i, pathFirst, 1, SEQ_MAX_LEN);

        } else if (kInitial == 0 && indicatorInitial >= 29 && indicatorInitial < 32) {
            copyAligmentStruct(path, i, pathFirst, 2, SEQ_MAX_LEN);

        } else if (kInitial == 1 && indicatorInitial >= 9 && indicatorInitial < 12) {
            copyAligmentStruct(path, i, pathSecond, 0, SEQ_MAX_LEN);

        } else if (kInitial == 1 && indicatorInitial >= 19 && indicatorInitial < 22) {
            copyAligmentStruct(path, i, pathSecond, 1, SEQ_MAX_LEN);

        } else {
            copyAligmentStruct(path, i, pathSecond, 2, SEQ_MAX_LEN);
        }

    }

    for (i = 0; i < 3; i++) {
        copyAligmentStruct( pathFirst,  i, &pathFirstInfinite,  0, SEQ_MAX_LEN );
        copyAligmentStruct( pathSecond, i, &pathSecondInfinite, 0, SEQ_MAX_LEN );
    }

    for (i = 0; i < 3; i++) {              // assign three candidate nodes to the two trees and other nodes are infinite nodes
        if (path[i].flagWhichTree == 1) {
            copyAligmentStruct( pathFirst, iFirst, path, i, SEQ_MAX_LEN );
            iFirst++;

        } else if (path[i].flagWhichTree == 2) {
            copyAligmentStruct( pathSecond, iSecond, path, i, SEQ_MAX_LEN );
            iSecond++;
        }
    }

    //test function

    int temp = ungappedWt(&pathFirst[0], tcm, SEQ_MAX_LEN, alphSize);

    //********************************************************************************************************
    //******************************** grow both trees based on initialization *******************************
    //********************************************************************************************************

    do {
        //************************************ GROW TWO TREES BASED ON TWO METRICS  ***************************************************

        // grow tree according to first order metric: first tree

        for (i = 0; i < 3; i++) {
            if (    seqA[pathFirst[i].ungapped_sequenceA_ptr] == seqB[pathFirst[i].ungapped_sequenceB_ptr]
                 && pathFirst[i].ungapped_sequenceA_ptr + 1 <= lengthSeqA
                 && pathFirst[i].ungapped_sequenceB_ptr + 1 <= lengthSeqB) {
                iMatchFirst[i] = 0;
            } else if (    seqA[pathFirst[i].ungapped_sequenceA_ptr] != seqB[pathFirst[i].ungapped_sequenceB_ptr]
                        && pathFirst[i].ungapped_sequenceA_ptr + 1 <= lengthSeqA
                        && pathFirst[i].ungapped_sequenceB_ptr + 1 <= lengthSeqB) {
                iMatchFirst[i] = 1;
            } else {
                iMatchFirst[i] = 1000; // TODO: why 1000?
            }
        }

        printf("4th   a[%zu]: %2" PRIu64 " b[%zu]: %2" PRIu64 "\n", pathFirst[0].ungapped_sequenceA_ptr, seqA[pathFirst[0].ungapped_sequenceA_ptr], pathFirst[0].ungapped_sequenceB_ptr, seqB[pathFirst[0].ungapped_sequenceB_ptr]);
        printf("5th   a[%zu]: %2" PRIu64 " b[%zu]: %2" PRIu64 "\n", pathFirst[1].ungapped_sequenceA_ptr, seqA[pathFirst[1].ungapped_sequenceA_ptr], pathFirst[1].ungapped_sequenceB_ptr, seqB[pathFirst[1].ungapped_sequenceB_ptr]);
        printf("6th   a[%zu]: %2" PRIu64 " b[%zu]: %2" PRIu64 "\n", pathFirst[1].ungapped_sequenceA_ptr, seqA[pathFirst[2].ungapped_sequenceA_ptr], pathFirst[2].ungapped_sequenceB_ptr, seqB[pathFirst[2].ungapped_sequenceB_ptr]);

        aToB0   = getCost( seqA[pathFirst[0].ungapped_sequenceA_ptr], seqB[pathFirst[0].ungapped_sequenceB_ptr], tcm, alphSize );
        aToGap0 = getCost( seqA[pathFirst[0].ungapped_sequenceA_ptr], GAP,                                       tcm, alphSize );
        gapToB0 = getCost( GAP,                                       seqB[pathFirst[0].ungapped_sequenceB_ptr], tcm, alphSize );

        aToB1   = getCost( seqA[pathFirst[1].ungapped_sequenceA_ptr], seqB[pathFirst[1].ungapped_sequenceB_ptr], tcm, alphSize );
        aToGap1 = getCost( seqA[pathFirst[1].ungapped_sequenceA_ptr], GAP,                                       tcm, alphSize );
        gapToB1 = getCost( GAP,                                       seqB[pathFirst[1].ungapped_sequenceB_ptr], tcm, alphSize );

        aToB2   = getCost( seqA[pathFirst[2].ungapped_sequenceA_ptr], seqB[pathFirst[2].ungapped_sequenceB_ptr], tcm, alphSize );
        aToGap2 = getCost( seqA[pathFirst[2].ungapped_sequenceA_ptr], GAP,                                       tcm, alphSize );
        gapToB2 = getCost( GAP,                                       seqB[pathFirst[2].ungapped_sequenceB_ptr], tcm, alphSize );


        int arrayFirst[2][9]= { { 10, 20, 30, 11, 21, 31, 12, 22, 32 }
                              , {  aToB0   + pathFirst[0].gapped_partialWt
                                ,  aToGap0 + pathFirst[0].gapped_partialWt
                                ,  gapToB0 + pathFirst[0].gapped_partialWt
                                ,  aToB1   + pathFirst[1].gapped_partialWt
                                ,  aToGap1 + pathFirst[1].gapped_partialWt
                                ,  gapToB1 + pathFirst[1].gapped_partialWt
                                ,  aToB2   + pathFirst[2].gapped_partialWt
                                ,  aToGap2 + pathFirst[2].gapped_partialWt
                                ,  gapToB2 + pathFirst[2].gapped_partialWt
                                }
                              };

        // grow tree according to second order metric: second tree


        for (i = 0; i < 3; i++) {
            if (    seqA[pathSecond[i].ungapped_sequenceA_ptr] == seqB[pathSecond[i].ungapped_sequenceB_ptr]
                 && pathSecond[i].ungapped_sequenceA_ptr + 1 <= lengthSeqA
                 && pathSecond[i].ungapped_sequenceB_ptr + 1 <= lengthSeqB ) {
                iMatchSecond[i] = 0;
            } else if (    seqA[pathSecond[i].ungapped_sequenceA_ptr] != seqB[pathSecond[i].ungapped_sequenceB_ptr]
                        && pathSecond[i].ungapped_sequenceA_ptr + 1 <= lengthSeqA
                        && pathSecond[i].ungapped_sequenceB_ptr + 1 <= lengthSeqB ) {
                iMatchSecond[i] = 1;
            } else {
                iMatchSecond[i] = 1000; // TODO: why 1000?
            }
        }

        printf("7th   a[%zu]: %2" PRIu64 " b[%zu]: %2" PRIu64 "\n", pathFirst[0].ungapped_sequenceA_ptr, seqA[pathFirst[0].ungapped_sequenceA_ptr], pathFirst[0].ungapped_sequenceB_ptr, seqB[pathFirst[0].ungapped_sequenceB_ptr]);
        printf("8th   a[%zu]: %2" PRIu64 " b[%zu]: %2" PRIu64 "\n", pathFirst[1].ungapped_sequenceA_ptr, seqA[pathFirst[1].ungapped_sequenceA_ptr], pathFirst[1].ungapped_sequenceB_ptr, seqB[pathFirst[1].ungapped_sequenceB_ptr]);
        printf("9th   a[%zu]: %2" PRIu64 " b[%zu]: %2" PRIu64 "\n", pathFirst[2].ungapped_sequenceA_ptr, seqA[pathFirst[2].ungapped_sequenceA_ptr], pathFirst[2].ungapped_sequenceB_ptr, seqB[pathFirst[2].ungapped_sequenceB_ptr]);

        aToB0   = getCost( seqA[pathFirst[0].ungapped_sequenceA_ptr], seqB[pathFirst[0].ungapped_sequenceB_ptr], tcm, alphSize);
        aToB1   = getCost( seqA[pathFirst[1].ungapped_sequenceA_ptr], seqB[pathFirst[1].ungapped_sequenceB_ptr], tcm, alphSize);
        aToB2   = getCost( seqA[pathFirst[2].ungapped_sequenceA_ptr], seqB[pathFirst[2].ungapped_sequenceB_ptr], tcm, alphSize);

        aToGap0 = getCost( seqA[pathFirst[0].ungapped_sequenceA_ptr], GAP,                                       tcm, alphSize);
        aToGap1 = getCost( seqA[pathFirst[1].ungapped_sequenceA_ptr], GAP,                                       tcm, alphSize);
        aToGap2 = getCost( seqA[pathFirst[2].ungapped_sequenceA_ptr], GAP,                                       tcm, alphSize);

        gapToB0 = getCost( GAP,                                       seqB[pathFirst[0].ungapped_sequenceB_ptr], tcm, alphSize);
        gapToB1 = getCost( GAP,                                       seqB[pathFirst[1].ungapped_sequenceB_ptr], tcm, alphSize);
        gapToB2 = getCost( GAP,                                       seqB[pathFirst[2].ungapped_sequenceB_ptr], tcm, alphSize);

        // TODO: Is there any way for this ordering to be different from that of arraySecond[1], which was sorted above?
        int arraySecond[2][9]= { { 10, 20, 30, 11, 21, 31, 12, 22, 32 }
                               , {  aToB0   * aToB0   + pathFirst[0].gapped_partialWt
                                 ,  aToGap0 * aToGap0 + pathFirst[0].gapped_partialWt
                                 ,  gapToB0 * gapToB0 + pathFirst[0].gapped_partialWt
                                 ,  aToB1   * aToB1   + pathFirst[1].gapped_partialWt
                                 ,  aToGap1 * aToGap1 + pathFirst[1].gapped_partialWt
                                 ,  gapToB1 * gapToB1 + pathFirst[1].gapped_partialWt
                                 ,  aToB2   * aToB2   + pathFirst[2].gapped_partialWt
                                 ,  aToGap2 * aToGap2 + pathFirst[2].gapped_partialWt
                                 ,  gapToB2 * gapToB2 + pathFirst[2].gapped_partialWt
                                 }
                               };


        //************************************  SORT TWO TREES  *********************************************************

        // IMPORTANT TO BE UPDATED: WHEN THE THREE NODES CONVERGE TO ONE TREE, WE CAN FOCUS ON THAT TREE

                // printCostBuffer(arrayFirst[1], 9, "arrayFirst[1]");
        // first tree: sort both rows using bubble sort of values in the second row
        doubleBubbleSort(arrayFirst[1], arrayFirst[0], 9);

                // printCostBuffer(arrayFirst[0], 9, "arrayFirst[0]");
                // printCostBuffer(arrayFirst[1], 9, "arrayFirst[1]");

                // printCostBuffer(arraySecond[1], 9, "arraySecond[1]");
        // second tree: sort both rows using bubble sort of values in the second row
        doubleBubbleSort(arraySecond[1], arraySecond[0], 9);

                // printCostBuffer(arraySecond[0], 9, "arraySecond[0]");
                // printCostBuffer(arraySecond[1], 9, "arraySecond[1]");


        //*************************************** grow first tree by obaining the three good nodes in first tree  ******************


        for (j = 0; j < 3; j++) {        // make a copy of previous paths, this is crutial since we need to keep track of the path
            copyAligmentStruct( pathTempFirst, j, pathFirst, j, SEQ_MAX_LEN );
        }


        for (i = 0; i < 3; i++) {

            indicatorFirst = arrayFirst[0][i];           // decide which operation to make
            kFirst         = indicatorFirst % 10;        // decide which path it belongs to

            copyAligmentStruct(pathFirst, i, pathTempFirst, kFirst, SEQ_MAX_LEN);

            if (indicatorFirst > 9 && indicatorFirst < 15) { // substitution
                cost = getCost( seqA[pathFirst[kFirst].ungapped_sequenceA_ptr]
                              , seqB[pathFirst[kFirst].ungapped_sequenceB_ptr]
                              , tcm
                              , alphSize );

                flag = updateSequences( &pathFirst[i]
                                      ,  seqA
                                      ,  lengthSeqA
                                      ,  seqB
                                      ,  lengthSeqB
                                      ,  GAP
                                      ,  SEQ_MAX_LEN
                                      ,  tcm
                                      ,  alphSize
                                      ,  flagEmpty[0]
                                      ,  A_TO_B
                                      ,  cost );
                if(flag) {
                    memcpy(alignFinalA, pathFirst[i].partialAlignA, sizeof(uint64_t) * SEQ_MAX_LEN);
                    memcpy(alignFinalB, pathFirst[i].partialAlignB, sizeof(uint64_t) * SEQ_MAX_LEN);
                       printBuffer(alignFinalA, SEQ_MAX_LEN, "From alignFinalA (1), indicatorFirst in [10, 14]:");
                       printBuffer(alignFinalB, SEQ_MAX_LEN, "From alignFinalB (1), indicatorFirst in [10, 14]:");
                    break;
                }
            }

            if ( indicatorFirst > 19 && indicatorFirst < 25) {   // gap in seqB
                printf("n+1th a: %2" PRIu64 " b: %2" PRIu64 " \n", seqA[pathFirst[kFirst].ungapped_sequenceA_ptr], seqB[pathFirst[kFirst].ungapped_sequenceB_ptr]);
                printf("n+1th a[%zu]: %2" PRIu64 " b[X]: %2" PRIu64 ") \n", pathFirst[kFirst].ungapped_sequenceA_ptr, seqA[pathFirst[kFirst].ungapped_sequenceA_ptr], GAP);
                pathFirst[i].gapped_partialWt += getCost(seqA[pathFirst[kFirst].ungapped_sequenceA_ptr]
                                                        , GAP
                                                        , tcm
                                                        , alphSize);

                int cost =   getCost(seqA[pathFirst[kFirst].ungapped_sequenceA_ptr]
                                    , GAP
                                    , tcm
                                    , alphSize );

                flag = updateSequences( &pathFirst[i]
                                      ,  seqA
                                      ,  lengthSeqA
                                      ,  seqB
                                      ,  lengthSeqB
                                      ,  GAP
                                      ,  SEQ_MAX_LEN
                                      ,  tcm
                                      ,  alphSize
                                      ,  flagEmpty[0]
                                      ,  A_TO_GAP
                                      ,  cost );
                if(flag) {
                    memcpy(alignFinalA, pathFirst[i].partialAlignA, sizeof(uint64_t) * SEQ_MAX_LEN);
                    memcpy(alignFinalB, pathFirst[i].partialAlignB, sizeof(uint64_t) * SEQ_MAX_LEN);
                    break;
                }
            }

            if (indicatorFirst > 29 && indicatorFirst < 35)    // gap in seqA
            {
                printf("n+2th a[X]: %2" PRIu64 " b[%zu]: %2" PRIu64 " \n", GAP, pathFirst[kFirst].ungapped_sequenceB_ptr, seqB[pathFirst[kFirst].ungapped_sequenceB_ptr]);

                cost = getCost( GAP
                              , seqB[pathFirst[kFirst].ungapped_sequenceB_ptr]
                              , tcm
                              , alphSize );

                flag = updateSequences( &pathFirst[i]
                                      ,  seqA
                                      ,  lengthSeqA
                                      ,  seqB
                                      ,  lengthSeqB
                                      ,  GAP
                                      ,  SEQ_MAX_LEN
                                      ,  tcm
                                      ,  alphSize
                                      ,  flagEmpty[0]
                                      ,  GAP_TO_B
                                      ,  cost );
                if(flag) {
                    memcpy(alignFinalA, pathFirst[i].partialAlignA, sizeof(uint64_t) * SEQ_MAX_LEN);
                    memcpy(alignFinalB, pathFirst[i].partialAlignB, sizeof(uint64_t) * SEQ_MAX_LEN);
                    break;
                }
            }
        }


        //*************************************** grow second tree by obtaining the three good nodes in second tree  ******************


        if (flag == 0) {

            for (j = 0; j < 3; j++) {        // make a copy of previous paths, this is crucial since we need to keep track of the path
                // in order to decide whether there is a match or substitution in the next position
                copyAligmentStruct(pathTempSecond, j, pathSecond, j, SEQ_MAX_LEN);
                copyAligmentStruct(pathTempFirst,  j, pathFirst,  j, SEQ_MAX_LEN);
            }


            for (i = 0; i < 3; i++){
                indicatorSecond = arraySecond[0][i];     // decide which operation to make
                kSecond         = indicatorSecond % 10;  // decide which path it belongs to

                copyAligmentStruct(pathSecond, i, pathTempSecond, kSecond, SEQ_MAX_LEN);

                if (indicatorSecond > 9 && indicatorSecond < 15) { // substitution
                    cost = getCost( seqA[pathFirst[kSecond].ungapped_sequenceA_ptr]
                                  , seqB[pathSecond[kSecond].ungapped_sequenceB_ptr]
                                  , tcm
                                  , alphSize );

                    flag = updateSequences( &pathSecond[i]
                                          ,  seqA
                                          ,  lengthSeqA
                                          ,  seqB
                                          ,  lengthSeqB
                                          ,  GAP
                                          ,  SEQ_MAX_LEN
                                          ,  tcm
                                          ,  alphSize
                                          ,  flagEmpty[1]
                                          ,  A_TO_B
                                          ,  cost * cost );
                    if(flag) {
                        memcpy(alignFinalA, pathFirst[i].partialAlignA, sizeof(uint64_t) * SEQ_MAX_LEN);
                        memcpy(alignFinalB, pathFirst[i].partialAlignB, sizeof(uint64_t) * SEQ_MAX_LEN);
                        break;
                    }

                }

                if ( indicatorSecond > 19 && indicatorSecond < 25) {   // gap in seqB
                    cost = getCost(seqA[pathFirst[kSecond].ungapped_sequenceA_ptr]
                                  , GAP
                                  , tcm
                                  , alphSize );

                    flag = updateSequences( &pathSecond[i]
                                          ,  seqA
                                          ,  lengthSeqA
                                          ,  seqB
                                          ,  lengthSeqB
                                          ,  GAP
                                          ,  SEQ_MAX_LEN
                                          ,  tcm
                                          ,  alphSize
                                          ,  flagEmpty[1]
                                          ,  A_TO_GAP
                                          ,  cost * cost );
                    if(flag) {
                        memcpy(alignFinalA, pathFirst[i].partialAlignA, sizeof(uint64_t) * SEQ_MAX_LEN);
                        memcpy(alignFinalB, pathFirst[i].partialAlignB, sizeof(uint64_t) * SEQ_MAX_LEN);
                        break;
                    }
                }

                if ( indicatorSecond > 29 && indicatorSecond < 35) {   // gap in seqA
                    cost = getCost( GAP
                                  , seqB[pathSecond[kSecond].ungapped_sequenceB_ptr]
                                  , tcm
                                  , alphSize );

                    flag = updateSequences( &pathFirst[i]
                                          ,  seqA
                                          ,  lengthSeqA
                                          ,  seqB
                                          ,  lengthSeqB
                                          ,  GAP
                                          ,  SEQ_MAX_LEN
                                          ,  tcm
                                          ,  alphSize
                                          ,  flagEmpty[1]
                                          ,  GAP_TO_B
                                          ,  cost * cost );
                    if(flag) {
                        memcpy(alignFinalA, pathFirst[i].partialAlignA, sizeof(uint64_t) * SEQ_MAX_LEN);
                        memcpy(alignFinalB, pathFirst[i].partialAlignB, sizeof(uint64_t) * SEQ_MAX_LEN);
                        break;
                    }
                }
            }
        }

        //******************************   sort the six nodes according to the ungapped metric   *************************************

        int arrayMix[2][6]= { { 10, 20, 30, 11, 21, 31 }
                            , { pathFirst[0].ungapped_partialWt
                              , pathFirst[1].ungapped_partialWt
                              , pathFirst[2].ungapped_partialWt
                              , pathSecond[0].ungapped_partialWt
                              , pathSecond[1].ungapped_partialWt
                              , pathSecond[2].ungapped_partialWt
                              }
                            };

                // printCostBuffer(arrayMix[1], 6, "arrayMix[1]");
        doubleBubbleSort(arrayMix[1], arrayMix[0], 6);
                // printCostBuffer(arrayMix[0], 6, "arrayMix[0]");
                // printCostBuffer(arrayMix[1], 6, "arrayMix[1]");

        for (i = 0; i < 3; i++) {

            indicatorMix = arrayMix[0][i];     // decide which operation to make
            kMix         = indicatorMix % 10;  // decide which path it belongs to
            if (kMix == 0) {
                if (indicatorMix >= 9 && indicatorMix < 12) {
                    copyAligmentStruct(path, i, pathFirst, 0, SEQ_MAX_LEN);

                } else if (indicatorMix >= 19 && indicatorMix < 22) {
                    copyAligmentStruct(path, i, pathFirst, 1, SEQ_MAX_LEN);

                } else { // indicatorMix >= 29 && indicatorMix < 32
                    copyAligmentStruct(path, i, pathFirst, 2, SEQ_MAX_LEN);
                }
            } else { // kMix == 1
                if (indicatorMix >= 9 && indicatorMix < 12) {
                    copyAligmentStruct(path, i, pathSecond, 0, SEQ_MAX_LEN);

                } else if (indicatorMix >= 19 && indicatorMix < 22) {
                    copyAligmentStruct(path, i, pathSecond, 1, SEQ_MAX_LEN);

                } else { // indicatorMix >= 29 && indicatorMix < 32
                    copyAligmentStruct(path, i, pathSecond, 2, SEQ_MAX_LEN);
                }
            }
        }



        //****************************************   assign nodes for the next round *************************************************


        iFirst  = 0;
        iSecond = 0;


        for (i = 0; i < 3; i++) {            // set all six nodes to be infinite nodes
            copyAligmentStruct ( pathFirst,  i, &pathFirstInfinite,  0, SEQ_MAX_LEN );
            copyAligmentStruct ( pathSecond, i, &pathSecondInfinite, 0, SEQ_MAX_LEN );
        }

        for (i = 0; i < 3; i++) {            // assign three candidate nodes to the two trees and other nodes are infinite nodes
            if (path[i].flagWhichTree == 1) {
                copyAligmentStruct( pathFirst, iFirst, path, i, SEQ_MAX_LEN );
                iFirst++;

            } else if (path[i].flagWhichTree == 2) {
                copyAligmentStruct( pathSecond, iSecond, path, i, SEQ_MAX_LEN );
                iSecond++;
            }
        }

        for (i = 0; i < 3; i++) {
            if (path[i].flagWhichTree == 1) {
                flagEmpty[0] = 0;
                break;
            }
        }

        for (i = 0; i < 3; i++) {
            if (path[i].flagWhichTree == 2) {
                flagEmpty[1] = 0;
                break;
            }
        }
    } while( flag == 0 );

    memcpy(finalAlign.partialAlignA, alignFinalA, sizeof(uint64_t) * SEQ_MAX_LEN);
    memcpy(finalAlign.partialAlignB, alignFinalB, sizeof(uint64_t) * SEQ_MAX_LEN);

    finalAlign.gapped_partialWt = 0;

    // TODO: check this ending condition; should it have a 0?
    for (i = 0; i < SEQ_MAX_LEN && finalAlign.partialAlignA[i] && finalAlign.partialAlignB[i]; i++) {
      //        printf("n+6th a: %2" PRIu64 " b: %2" PRIu64 " \n", finalAlign.partialAlignA[i], finalAlign.partialAlignB[i + LENGTH]);

        if (finalAlign.partialAlignA[i] == GAP || finalAlign.partialAlignB[i] == GAP) {
            finalAlign.gapped_partialWt += getCost(GAP, GAP, tcm, alphSize);

        } else if (finalAlign.partialAlignA[i] == finalAlign.partialAlignB[i]) {
            finalAlign.gapped_partialWt += getCost(finalAlign.partialAlignA[i], finalAlign.partialAlignB[i], tcm, alphSize);

        } else {
            finalAlign.gapped_partialWt += getCost(finalAlign.partialAlignA[i], finalAlign.partialAlignB[i], tcm, alphSize);
        }
    }

    //TODO: for now can't use memcpy, because I don't know what the lengths of the arrays are

    //int strIdx = 0;
    // memcpy(retAlign->seq1, finalAlign.partialAlignA, finalAlign.gapped_sequenceA_ptr);
    // retAlign->seq1Len = finalAlign.gapped_sequenceA_ptr;
    // printf("seq1Len: %zu\n", retAlign->seq1Len);

    for(i = 0; i < SEQ_MAX_LEN && finalAlign.partialAlignA[i] != 0; ++i) {
        retAlign->seq1[i] = finalAlign.partialAlignA[i];
    }
    retAlign->seq1Len = i;

    // memcpy(retAlign->seq2, finalAlign.partialAlignB, finalAlign.gapped_sequenceB_ptr);
    // retAlign->seq2Len = finalAlign.gapped_sequenceB_ptr;
    // printf("seq2Len: %zu\n", retAlign->seq2Len);

    for(i = 0; i < SEQ_MAX_LEN && finalAlign.partialAlignB[i] != 0; ++i) {
        retAlign->seq2[i] = finalAlign.partialAlignB[i];
    }
    retAlign->seq2Len = i;

    retAlign->weight = finalAlign.gapped_partialWt;

    // TODO: why would I need a ternary here? Shouldn't they be the same?
    retAlign->alignmentLength = (retAlign->seq1Len < retAlign->seq2Len) ? retAlign->seq1Len : retAlign->seq2Len;

    // for (i = 0; i < SEQ_MAX_LEN; ++i) {
    //   printf("buf[%d]: %lu\n", i, finalAlign.partialAlignA[i]);
    // }

    // for (i = 0; i < retAlign->seq1Len ; ++i) {
    //   printf("seq1[%d]: %lu\n", i, retAlign->seq1[i]);
    // }

    // for (i = 0; i < retAlign->seq2Len ; ++i) {
    //   printf("seq2[%d]: %lu\n", i, retAlign->seq2[i]);
    // }

    // printf("s1    len: %zu\n", retAlign->seq1Len        );
    // printf("s2    len: %zu\n", retAlign->seq2Len        );
    // printf("align len: %zu\n", retAlign->alignmentLength);
    // printf("cost  val: %d\n" , retAlign->weight         );


    for (i = 0; i < 3; i++) {
        free( path[i].partialAlignA );
        free( path[i].partialAlignB );

        //  free(pathFirst[i].partialAlignA);
        //  free(pathFirst[i].partialAlignB);

        free( pathSecond[i].partialAlignA );
        free( pathSecond[i].partialAlignB );

        free( pathTempFirst[i].partialAlignA );
        free( pathTempSecond[i].partialAlignB );
    }
    free( pathFirstInfinite.partialAlignA );
    free( pathFirstInfinite.partialAlignB );
    free( pathSecondInfinite.partialAlignA );
    free( pathSecondInfinite.partialAlignB );
    free( finalAlign.partialAlignA );

    free( seqA );
    free( seqB );


    // EDIT: returning success code.
    return 0;
}

/**************************************   COMBINE SORT CANDIDATES ACCORDING TO TRUE METRIC  *********************************************/


int ungappedWt(alignment_t *path, costMatrix_p tcm, size_t maxLen, size_t alphSize)
{
    size_t i;

    int wtTempFirst  = 0,
        wtTempSecond = 0,
        wtTemp,
        cost;

    // TODO: check that bounds checking is appropriate here
    for(i = 0; i < path->gapped_sequenceA_ptr && i < maxLen; i++) {
        // printf("n+7th a: %2" PRIu64 " b: %2" PRIu64 " (not indexing seqA or seqB)\n", path->partialAlignA[i], path->partialAlignB[i]);
        cost = getCost(path->partialAlignA[i], path->partialAlignB[i], tcm, alphSize);
        wtTempFirst  += cost;
        wtTempSecond += cost * cost;
    }

    wtTempFirst += wtTempFirst * wtTempFirst;

    return wtTempFirst + wtTempSecond;;
}

void freeRetType(retType_t* toFree)
{
    free(toFree->seq1);
    free(toFree->seq2);
    free(toFree);
    toFree = NULL;
}

void copyAligmentStruct ( alignment_t *copyTo
                        , size_t       copyToIdx
                        , alignment_t *copyFrom
                        , size_t       copyFromIdx
                        , const size_t initLength
                        )
{
    copyTo[copyToIdx].gapped_partialWt       = copyFrom[copyFromIdx].gapped_partialWt;
    copyTo[copyToIdx].ungapped_partialWt     = copyFrom[copyFromIdx].ungapped_partialWt;
    copyTo[copyToIdx].gapped_sequenceA_ptr   = copyFrom[copyFromIdx].gapped_sequenceA_ptr;
    copyTo[copyToIdx].gapped_sequenceB_ptr   = copyFrom[copyFromIdx].gapped_sequenceB_ptr;
    copyTo[copyToIdx].ungapped_sequenceA_ptr = copyFrom[copyFromIdx].ungapped_sequenceA_ptr;
    copyTo[copyToIdx].ungapped_sequenceB_ptr = copyFrom[copyFromIdx].ungapped_sequenceB_ptr;
    copyTo[copyToIdx].flagWhichTree          = copyFrom[copyFromIdx].flagWhichTree;
    //TODO: and here
    memcpy(copyTo[copyToIdx].partialAlignA, copyFrom[copyFromIdx].partialAlignA, sizeof(uint64_t) * initLength);
    memcpy(copyTo[copyToIdx].partialAlignB, copyFrom[copyFromIdx].partialAlignB, sizeof(uint64_t) * initLength);
}

alignment_t *initAlignment( int    in_partialWt
                          , int    in_ungapped_partialWt
                          , size_t in_gapped_sequenceA_ptr
                          , size_t in_gapped_sequenceB_ptr
                          , size_t in_ungapped_sequenceA_ptr
                          , size_t in_ungapped_sequenceB_ptr
                          , int    in_flagWhichTree
                          , size_t initLength )
{
    alignment_t *output   = malloc( sizeof(alignment_t) );
    if( output == NULL || output == NULL ) {
        printf("Out of memory\n");
        fflush(stdout);
        exit(1);
    }

    output->gapped_partialWt       = in_partialWt;
    output->ungapped_partialWt     = in_ungapped_partialWt;
    output->gapped_sequenceA_ptr   = in_gapped_sequenceA_ptr;
    output->gapped_sequenceB_ptr   = in_gapped_sequenceB_ptr;
    output->ungapped_sequenceA_ptr = in_ungapped_sequenceA_ptr;
    output->ungapped_sequenceB_ptr = in_ungapped_sequenceB_ptr;
    output->flagWhichTree          = in_flagWhichTree;

    output->partialAlignA = calloc( initLength, sizeof(uint64_t) );
    output->partialAlignB = calloc( initLength, sizeof(uint64_t) );

    if( output->partialAlignA == NULL || output->partialAlignB == NULL ) {
        printf("Out of memory\n");
        fflush(stdout);
        exit(1);
    }
    return output;
}


void doubleBubbleSort(int *valArray, int *secondArray, size_t number_Elements)
{
    int swapA, swapB;
    size_t lastIdx = number_Elements - 1;
    for (size_t i = 0; i < lastIdx; i++) {
        for (size_t j = 0; j < lastIdx - i; j++) {
            if (valArray[j] > valArray[j + 1]) {
                swapA           = valArray[j];
                valArray[j]     = valArray[j + 1];
                valArray[j + 1] = swapA;

                swapB              = secondArray[j];
                secondArray[j]     = secondArray[j + 1];
                secondArray[j + 1] = swapB;

            }
        }
    }
}


void printCostBuffer(int *buffer, size_t bufLen, char *prefix)
{
    printf("\nwhich buffer: %s\n", prefix);
    printf("[ ");
    for(size_t i = 0; i < bufLen; i++) {
        printf("%d, ", buffer[i]);
    }
    printf("]\n");
}


void printBuffer(uint64_t *buffer, size_t bufLen, char *prefix)
{
    printf("which buffer: %s\n", prefix);
    printf("[ ");
    for(size_t i = 0; i < bufLen; i++) {
        printf("%" PRIu64 ", ", buffer[i]);
    }
    printf("]\n");
}


size_t boundedIncrement(size_t value, size_t bound)
{
  size_t incrementedValue = value++;
  if(incrementedValue >= bound) {
      printf("exceeded bound!!! bound: %zu value: %zu\n", bound, value);
  }
  return incrementedValue < bound
       ? incrementedValue
       : value;
}

int updateSequences( alignment_t        *path
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
                   , int                cost)
{

    size_t j = 0;
    int flag = 0;

    path->gapped_partialWt += cost;

    switch (whichSub) {
        case A_TO_GAP:
            path->partialAlignA[path->gapped_sequenceA_ptr] = seqA[path->ungapped_sequenceA_ptr];
            path->partialAlignB[path->gapped_sequenceB_ptr] = GAP;
            path->ungapped_sequenceA_ptr = boundedIncrement(path->ungapped_sequenceA_ptr, lengthSeqA);
            break;
        case GAP_TO_B:
            path->partialAlignA[path->gapped_sequenceA_ptr] = GAP;
            path->partialAlignB[path->gapped_sequenceB_ptr] = seqB[path->ungapped_sequenceB_ptr];
            path->ungapped_sequenceB_ptr = boundedIncrement(path->ungapped_sequenceB_ptr, lengthSeqB);
            break;
        case A_TO_B:
            path->partialAlignA[path->gapped_sequenceA_ptr] = seqA[path->ungapped_sequenceA_ptr];
            path->partialAlignB[path->gapped_sequenceB_ptr] = seqB[path->ungapped_sequenceB_ptr];

            printBuffer(path->partialAlignA, SEQ_MAX_LEN, "partialAlignA (2)");
            printBuffer(path->partialAlignB, SEQ_MAX_LEN, "partialAlignB (2)");

            path->ungapped_sequenceA_ptr = boundedIncrement(path->ungapped_sequenceA_ptr, lengthSeqA );
            path->ungapped_sequenceB_ptr = boundedIncrement(path->ungapped_sequenceB_ptr, lengthSeqB );
            break;
    }

    path->gapped_sequenceA_ptr   = boundedIncrement(path->gapped_sequenceA_ptr, SEQ_MAX_LEN);
    path->gapped_sequenceB_ptr   = boundedIncrement(path->gapped_sequenceB_ptr, SEQ_MAX_LEN);

    if (flagEmpty == 0) {
        path->ungapped_partialWt = ungappedWt (path, tcm, SEQ_MAX_LEN, alphSize);
    }

    if (path->ungapped_sequenceA_ptr >= lengthSeqA - 1) {
        for (; j < lengthSeqB - path->ungapped_sequenceB_ptr; j++) {
            path->partialAlignA[ path->gapped_sequenceA_ptr + j ] = GAP;
            path->partialAlignB[ path->gapped_sequenceB_ptr + j ] = seqB[path->ungapped_sequenceB_ptr + j];
        }
        flag = 1;
    }

    if (path->ungapped_sequenceB_ptr >= lengthSeqB - 1) {
        for (j = 0; j < lengthSeqA - path->ungapped_sequenceA_ptr; j++) {
            path->partialAlignA[ path->gapped_sequenceA_ptr + j ] = seqA[path->ungapped_sequenceA_ptr + j];
            path->partialAlignB[ path->gapped_sequenceB_ptr + j ] = GAP;
        }
        flag = 1;
    }

    return flag;
}