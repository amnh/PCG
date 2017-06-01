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

#include "../memoized_tcm/dynamicCharacterOperations.h"
#include "sequentialAlign.h"

// this and <inttypes.h> so I can use PRIu64 instead of llu
#define __STDC_FORMAT_MACROS


int aligner( uint64_t     *inChar1
           , size_t        inChar1Len
           , uint64_t     *inChar2
           , size_t        inChar2Len
           , size_t        alphSize
           , costMatrix_p  tcm
           , retType_t    *retAlign)
{
    setbuf(stdout, NULL);

    const uint64_t GAP        = CANONICAL_ONE << (alphSize - 1);
    // printf("alphSize: %zu  %" PRIu64 "\n", alphSize, GAP);

    const size_t SEQ_MAX_LEN  = inChar1Len + inChar2Len;  // Maximum alignment length of either charA or charB (pathological case)

    size_t i, j;                                          // because i and j are being used a lot,
                                                          // and they were declared later on anyway
    int cost;                                             // This will be used later to send into update character fn.

    size_t lengthCharA = inChar1Len,
           lengthCharB = inChar2Len;

    if (SEQ_MAX_LEN == 0) {
        // printf("The lengths of the both characters are zero!\n");
        return 2;
    }

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

    uint64_t *charA = calloc( lengthCharA, sizeof(uint64_t) );
    uint64_t *charB = calloc( lengthCharB, sizeof(uint64_t) );

    // Now, test for allocation. Return 1 if it fails.
    if( charA == NULL || charB == NULL ) {
        return 1;
    }

    // This looks good, making sure not to clobber your input
    memcpy( charA, inChar1, sizeof(uint64_t) * (lengthCharA) );
    memcpy( charB, inChar2, sizeof(uint64_t) * (lengthCharB) );

    printBuffer(charA, lengthCharA, "CharA");
    printBuffer(charB, lengthCharB, "CharB");

    int flag         = 0;       // Has all of one of the input characters been consumed?
    int flagEmpty[2] = {1,1};   // indicator for the case when one tree becomes empty,
                                // i.e., all the candidates nodes have converged to the other tree

    int indicatorFirst, indicatorSecond, indicatorInitial, indicatorMix;
    int kInitial, kFirst, kSecond, kMix;
    int iMatchFirst[3]  = { 0, 0, 0 };
    int iMatchSecond[3] = { 0, 0, 0 };

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
        aToGap0,  // 0 values get cost using character at 0 index in array of structs, etc.
        gapToB0,
        aToB1,
        aToGap1,
        gapToB1,
        aToB2,
        aToGap2,
        gapToB2;

    printf("1st   a: %2" PRIu64 " b: %2" PRIu64 "\n", charA[0], charB[0]);

    aToB0   = getCost(charA[0], charB[0], tcm, alphSize);
    aToGap0 = getCost(charA[0], GAP,     tcm, alphSize);
    gapToB0 = getCost(GAP,     charB[0], tcm, alphSize);

    alignment_t pathFirst[3];
    pathFirst[0] = *initAlignment(aToB0,   aToB0   + 2 * aToB0   * aToB0,   1, 1, 1, 1, 1, SEQ_MAX_LEN);
    pathFirst[1] = *initAlignment(aToGap0, aToGap0 + 2 * aToGap0 * aToGap0, 1, 1, 1, 0, 1, SEQ_MAX_LEN);
    pathFirst[2] = *initAlignment(gapToB0, gapToB0 + 2 * gapToB0 * gapToB0, 1, 1, 0, 1, 1, SEQ_MAX_LEN);


    pathFirst[0].partialAlign_A[0] = charA[0];
    pathFirst[0].partialAlign_B[0] = charB[0];

    pathFirst[1].partialAlign_A[0] = charA[0];
    pathFirst[1].partialAlign_B[0] = GAP;

    pathFirst[2].partialAlign_A[0] = GAP;
    pathFirst[2].partialAlign_B[0] = charB[0];

    // !! the two costs (costSub, costInsertDel) are the same as in pathFirst
    // !! costSub=getCost(charA[0],charB[0]);
    // !! costInsertDel=getCost(chara[0], '-');

    //  under \sum z_i^2 measure

    printf("2nd   a: %2" PRIu64 " b: %2" PRIu64 "\n", charA[0], charB[0]);
    aToB0   = getCost(charA[0], charB[0], tcm, alphSize);
    aToGap0 = getCost(charA[0], GAP,     tcm, alphSize);
    gapToB0 = getCost(GAP,     charB[0], tcm, alphSize);
    printf("costs: %d %d %d\n", aToB0, aToGap0, gapToB0);

    alignment_t pathSecond[3];
    pathSecond[0] = *initAlignment(aToB0   * aToB0,   aToB0   + 2 * aToB0   * aToB0,   1, 1, 1, 1, 2, SEQ_MAX_LEN);
    pathSecond[1] = *initAlignment(aToGap0 * aToGap0, aToGap0 + 2 * aToGap0 * aToGap0, 1, 1, 1, 0, 2, SEQ_MAX_LEN);
    pathSecond[2] = *initAlignment(gapToB0 * gapToB0, gapToB0 + 2 * gapToB0 * gapToB0, 1, 1, 0, 1, 2, SEQ_MAX_LEN);

    pathSecond[0].partialAlign_A[0] = charA[0];
    pathSecond[0].partialAlign_B[0] = charB[0];

    pathSecond[1].partialAlign_A[0] = charA[0];
    pathSecond[1].partialAlign_B[0] = GAP;

    pathSecond[2].partialAlign_A[0] = GAP;
    pathSecond[2].partialAlign_B[0] = charB[0];


    printf("3rd   a: %2" PRIu64 " b: %2" PRIu64 "\n", charA[0], charB[0]);

    /* Next three values are unchanged from previous setting. */
    // aToB0   = getCost(charA[0], charB[0], tcm, alphSize);
    // aToGap0 = getCost(charA[0], GAP,     tcm, alphSize);
    // gapToB0 = getCost(GAP,     charB[0], tcm, alphSize);

    int arrayInitial[2][6] = {
        { 10, 20, 30, 11, 21, 31 },
        { aToB0   + 2 * aToB0   * aToB0   // gapped partial cost of pathSecond[0] (see above)
        , aToGap0 + 2 * aToGap0 * aToGap0 // gapped partial cost of pathSecond[1]
        , gapToB0 + 2 * gapToB0 * gapToB0 // gapped partial cost of pathSecond[2]
        , aToB0   + 2 * aToB0   * aToB0   // gapped partial cost of pathSecond[0]
        , aToGap0 + 2 * aToGap0 * aToGap0 // gapped partial cost of pathSecond[1]
        , gapToB0 + 2 * gapToB0 * gapToB0 // gapped partial cost of pathSecond[2]
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

    currentAlignmentCost(&pathFirst[0], tcm, SEQ_MAX_LEN, alphSize);

    /********************************************************************************************************/
    /******************************** grow both trees based on initialization *******************************/
    /********************************************************************************************************/

    do {
        /************************ GROW TWO TREES BASED ON TWO METRICS  **************************************/

        // grow tree according to first order metric: first tree

        for (i = 0; i < 3; i++) {
            if (    charA[pathFirst[i].input_character_A_ptr] == charB[pathFirst[i].input_character_B_ptr]
                 && pathFirst[i].input_character_A_ptr + 1 <= lengthCharA
                 && pathFirst[i].input_character_B_ptr + 1 <= lengthCharB) {
                iMatchFirst[i] = 0;
            } else if (    charA[pathFirst[i].input_character_A_ptr] != charB[pathFirst[i].input_character_B_ptr]
                        && pathFirst[i].input_character_A_ptr + 1 <= lengthCharA
                        && pathFirst[i].input_character_B_ptr + 1 <= lengthCharB) {
                iMatchFirst[i] = 1;
            } else {
                iMatchFirst[i] = 1000; // TODO: why 1000?
            }
        }

        printf("4th   a[%zu]: %2" PRIu64 " b[%zu]: %2" PRIu64 "\n", pathFirst[0].input_character_A_ptr, charA[pathFirst[0].input_character_A_ptr], pathFirst[0].input_character_B_ptr, charB[pathFirst[0].input_character_B_ptr]);
        printf("5th   a[%zu]: %2" PRIu64 " b[%zu]: %2" PRIu64 "\n", pathFirst[1].input_character_A_ptr, charA[pathFirst[1].input_character_A_ptr], pathFirst[1].input_character_B_ptr, charB[pathFirst[1].input_character_B_ptr]);
        printf("6th   a[%zu]: %2" PRIu64 " b[%zu]: %2" PRIu64 "\n", pathFirst[1].input_character_A_ptr, charA[pathFirst[2].input_character_A_ptr], pathFirst[2].input_character_B_ptr, charB[pathFirst[2].input_character_B_ptr]);

        aToB0   = getCost( charA[pathFirst[0].input_character_A_ptr], charB[pathFirst[0].input_character_B_ptr], tcm, alphSize );
        aToGap0 = getCost( charA[pathFirst[0].input_character_A_ptr], GAP,                                       tcm, alphSize );
        gapToB0 = getCost( GAP,                                       charB[pathFirst[0].input_character_B_ptr], tcm, alphSize );

        aToB1   = getCost( charA[pathFirst[1].input_character_A_ptr], charB[pathFirst[1].input_character_B_ptr], tcm, alphSize );
        aToGap1 = getCost( charA[pathFirst[1].input_character_A_ptr], GAP,                                       tcm, alphSize );
        gapToB1 = getCost( GAP,                                       charB[pathFirst[1].input_character_B_ptr], tcm, alphSize );

        aToB2   = getCost( charA[pathFirst[2].input_character_A_ptr], charB[pathFirst[2].input_character_B_ptr], tcm, alphSize );
        aToGap2 = getCost( charA[pathFirst[2].input_character_A_ptr], GAP,                                       tcm, alphSize );
        gapToB2 = getCost( GAP,                                       charB[pathFirst[2].input_character_B_ptr], tcm, alphSize );


        int arrayFirst[2][9]= { { 10, 20, 30, 11, 21, 31, 12, 22, 32 }
                              , {  aToB0   + pathFirst[0].gapped_partialCost
                                ,  aToGap0 + pathFirst[0].gapped_partialCost
                                ,  gapToB0 + pathFirst[0].gapped_partialCost
                                ,  aToB1   + pathFirst[1].gapped_partialCost
                                ,  aToGap1 + pathFirst[1].gapped_partialCost
                                ,  gapToB1 + pathFirst[1].gapped_partialCost
                                ,  aToB2   + pathFirst[2].gapped_partialCost
                                ,  aToGap2 + pathFirst[2].gapped_partialCost
                                ,  gapToB2 + pathFirst[2].gapped_partialCost
                                }
                              };

        // grow tree according to second order metric: second tree


        for (i = 0; i < 3; i++) {
            if (    charA[pathSecond[i].input_character_A_ptr] == charB[pathSecond[i].input_character_B_ptr]
                 && pathSecond[i].input_character_A_ptr + 1 <= lengthCharA
                 && pathSecond[i].input_character_B_ptr + 1 <= lengthCharB ) {
                iMatchSecond[i] = 0;
            } else if (    charA[pathSecond[i].input_character_A_ptr] != charB[pathSecond[i].input_character_B_ptr]
                        && pathSecond[i].input_character_A_ptr + 1 <= lengthCharA
                        && pathSecond[i].input_character_B_ptr + 1 <= lengthCharB ) {
                iMatchSecond[i] = 1;
            } else {
                iMatchSecond[i] = 1000; // TODO: why 1000?
            }
        }

        printf("7th   a[%zu]: %2" PRIu64 " b[%zu]: %2" PRIu64 "\n", pathFirst[0].input_character_A_ptr, charA[pathFirst[0].input_character_A_ptr], pathFirst[0].input_character_B_ptr, charB[pathFirst[0].input_character_B_ptr]);
        printf("8th   a[%zu]: %2" PRIu64 " b[%zu]: %2" PRIu64 "\n", pathFirst[1].input_character_A_ptr, charA[pathFirst[1].input_character_A_ptr], pathFirst[1].input_character_B_ptr, charB[pathFirst[1].input_character_B_ptr]);
        printf("9th   a[%zu]: %2" PRIu64 " b[%zu]: %2" PRIu64 "\n", pathFirst[2].input_character_A_ptr, charA[pathFirst[2].input_character_A_ptr], pathFirst[2].input_character_B_ptr, charB[pathFirst[2].input_character_B_ptr]);

        aToB0   = getCost( charA[pathFirst[0].input_character_A_ptr], charB[pathFirst[0].input_character_B_ptr], tcm, alphSize);
        aToB1   = getCost( charA[pathFirst[1].input_character_A_ptr], charB[pathFirst[1].input_character_B_ptr], tcm, alphSize);
        aToB2   = getCost( charA[pathFirst[2].input_character_A_ptr], charB[pathFirst[2].input_character_B_ptr], tcm, alphSize);

        aToGap0 = getCost( charA[pathFirst[0].input_character_A_ptr], GAP,                                       tcm, alphSize);
        aToGap1 = getCost( charA[pathFirst[1].input_character_A_ptr], GAP,                                       tcm, alphSize);
        aToGap2 = getCost( charA[pathFirst[2].input_character_A_ptr], GAP,                                       tcm, alphSize);

        gapToB0 = getCost( GAP,                                       charB[pathFirst[0].input_character_B_ptr], tcm, alphSize);
        gapToB1 = getCost( GAP,                                       charB[pathFirst[1].input_character_B_ptr], tcm, alphSize);
        gapToB2 = getCost( GAP,                                       charB[pathFirst[2].input_character_B_ptr], tcm, alphSize);

        // TODO: Is there any way for this ordering to be different from that of arraySecond[1], which was sorted above?
        int arraySecond[2][9]= { { 10, 20, 30, 11, 21, 31, 12, 22, 32 }
                               , {  aToB0   * aToB0   + pathFirst[0].gapped_partialCost
                                 ,  aToGap0 * aToGap0 + pathFirst[0].gapped_partialCost
                                 ,  gapToB0 * gapToB0 + pathFirst[0].gapped_partialCost
                                 ,  aToB1   * aToB1   + pathFirst[1].gapped_partialCost
                                 ,  aToGap1 * aToGap1 + pathFirst[1].gapped_partialCost
                                 ,  gapToB1 * gapToB1 + pathFirst[1].gapped_partialCost
                                 ,  aToB2   * aToB2   + pathFirst[2].gapped_partialCost
                                 ,  aToGap2 * aToGap2 + pathFirst[2].gapped_partialCost
                                 ,  gapToB2 * gapToB2 + pathFirst[2].gapped_partialCost
                                 }
                               };


        /*****************************************************  SORT TWO TREES  *****************************************************/

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


        /****************************** grow first tree by obaining the three good nodes in first tree ******************************/


        for (j = 0; j < 3; j++) {        // make a copy of previous paths, this is crucial since we need to keep track of the path
            copyAligmentStruct( pathTempFirst, j, pathFirst, j, SEQ_MAX_LEN );
        }


        for (i = 0; i < 3; i++) {

            indicatorFirst = arrayFirst[0][i];           // decide which operation to make
            kFirst         = indicatorFirst % 10;        // decide which path it belongs to

            copyAligmentStruct(pathFirst, i, pathTempFirst, kFirst, SEQ_MAX_LEN);

            if (indicatorFirst > 9 && indicatorFirst < 15) { // substitution
                cost = getCost( charA[pathFirst[kFirst].input_character_A_ptr]
                              , charB[pathFirst[kFirst].input_character_B_ptr]
                              , tcm
                              , alphSize );

                // TODO: make sure that this accumulator is being reset properly, as I don't believe it now is.
                flag = updateCharacters( &pathFirst[i]
                                       ,  charA
                                       ,  lengthCharA
                                       ,  charB
                                       ,  lengthCharB
                                       ,  GAP
                                       ,  SEQ_MAX_LEN
                                       ,  tcm
                                       ,  alphSize
                                       ,  flagEmpty[0]
                                       ,  A_TO_B
                                       ,  cost
                                       );
                if(flag) {
                    memcpy(alignFinalA, pathFirst[i].partialAlign_A, sizeof(uint64_t) * SEQ_MAX_LEN);
                    memcpy(alignFinalB, pathFirst[i].partialAlign_B, sizeof(uint64_t) * SEQ_MAX_LEN);

                    printBuffer(alignFinalA, SEQ_MAX_LEN, "From alignFinalA (1), indicatorFirst in [10, 14]:");
                    printBuffer(alignFinalB, SEQ_MAX_LEN, "From alignFinalB (1), indicatorFirst in [10, 14]:");
                    break;
                }
            }

            if ( indicatorFirst > 19 && indicatorFirst < 25) {   // gap in charB
                printf("n+1th a: %2" PRIu64 " b: %2" PRIu64 " \n", charA[pathFirst[kFirst].input_character_A_ptr], charB[pathFirst[kFirst].input_character_B_ptr]);
                printf("n+1th a[%zu]: %2" PRIu64 " b[X]: %2" PRIu64 ") \n", pathFirst[kFirst].input_character_A_ptr, charA[pathFirst[kFirst].input_character_A_ptr], GAP);
                pathFirst[i].gapped_partialCost += getCost(charA[pathFirst[kFirst].input_character_A_ptr]
                                                        , GAP
                                                        , tcm
                                                        , alphSize);

                int cost =   getCost(charA[pathFirst[kFirst].input_character_A_ptr]
                                    , GAP
                                    , tcm
                                    , alphSize );

                flag = updateCharacters( &pathFirst[i]
                                       ,  charA
                                       ,  lengthCharA
                                       ,  charB
                                       ,  lengthCharB
                                       ,  GAP
                                       ,  SEQ_MAX_LEN
                                       ,  tcm
                                       ,  alphSize
                                       ,  flagEmpty[0]
                                       ,  A_TO_GAP
                                       ,  cost );
                if(flag) {
                    memcpy(alignFinalA, pathFirst[i].partialAlign_A, sizeof(uint64_t) * SEQ_MAX_LEN);
                    memcpy(alignFinalB, pathFirst[i].partialAlign_B, sizeof(uint64_t) * SEQ_MAX_LEN);
                    break;
                }
            }

            if (indicatorFirst > 29 && indicatorFirst < 35)    // gap in charA
            {
                printf("n+2th a[X]: %2" PRIu64 " b[%zu]: %2" PRIu64 " \n",
                       GAP,
                            pathFirst[kFirst].input_character_B_ptr,
                       charB[pathFirst[kFirst].input_character_B_ptr]);

                cost = getCost( GAP
                              , charB[pathFirst[kFirst].input_character_B_ptr]
                              , tcm
                              , alphSize
                              );

                flag = updateCharacters( &pathFirst[i]
                                       ,  charA
                                       ,  lengthCharA
                                       ,  charB
                                       ,  lengthCharB
                                       ,  GAP
                                       ,  SEQ_MAX_LEN
                                       ,  tcm
                                       ,  alphSize
                                       ,  flagEmpty[0]
                                       ,  GAP_TO_B
                                       ,  cost
                                       );
                if(flag) {
                    memcpy(alignFinalA, pathFirst[i].partialAlign_A, sizeof(uint64_t) * SEQ_MAX_LEN);
                    memcpy(alignFinalB, pathFirst[i].partialAlign_B, sizeof(uint64_t) * SEQ_MAX_LEN);
                    break;
                }
            }
        }


        /**************************** grow second tree by obtaining the three good nodes in second tree  ****************************/


        if (flag == 0) {

            for (j = 0; j < 3; j++) {        // make a copy of previous paths, this is crucial since we need to keep track of the path
                                             // in order to decide whether there is a match or substitution in the next position
                copyAligmentStruct(pathTempFirst,  j, pathFirst,  j, SEQ_MAX_LEN);
                copyAligmentStruct(pathTempSecond, j, pathSecond, j, SEQ_MAX_LEN);
            }


            for (i = 0; i < 3; i++){
                indicatorSecond = arraySecond[0][i];     // decide which operation to make
                kSecond         = indicatorSecond % 10;  // decide which path it belongs to

                copyAligmentStruct(pathSecond, i, pathTempSecond, kSecond, SEQ_MAX_LEN);

                if (indicatorSecond > 9 && indicatorSecond < 15) { // substitution
                    cost = getCost( charA[pathFirst[kSecond].input_character_A_ptr]
                                  , charB[pathSecond[kSecond].input_character_B_ptr]
                                  , tcm
                                  , alphSize
                                  );

                    flag = updateCharacters( &pathSecond[i]
                                           ,  charA
                                           ,  lengthCharA
                                           ,  charB
                                           ,  lengthCharB
                                           ,  GAP
                                           ,  SEQ_MAX_LEN
                                           ,  tcm
                                           ,  alphSize
                                           ,  flagEmpty[1]
                                           ,  A_TO_B
                                           ,  cost * cost
                                           );
                    if(flag) {
                        memcpy(alignFinalA, pathSecond[i].partialAlign_A, sizeof(uint64_t) * SEQ_MAX_LEN);
                        memcpy(alignFinalB, pathSecond[i].partialAlign_B, sizeof(uint64_t) * SEQ_MAX_LEN);
                        break;
                    }

                }

                if ( indicatorSecond > 19 && indicatorSecond < 25) {   // gap in charB
                    cost = getCost(charA[pathFirst[kSecond].input_character_A_ptr]
                                  , GAP
                                  , tcm
                                  , alphSize );

                    flag = updateCharacters( &pathSecond[i]
                                           ,  charA
                                           ,  lengthCharA
                                           ,  charB
                                           ,  lengthCharB
                                           ,  GAP
                                           ,  SEQ_MAX_LEN
                                           ,  tcm
                                           ,  alphSize
                                           ,  flagEmpty[1]
                                           ,  A_TO_GAP
                                           ,  cost * cost );
                    if(flag) {
                        memcpy(alignFinalA, pathSecond[i].partialAlign_A, sizeof(uint64_t) * SEQ_MAX_LEN);
                        memcpy(alignFinalB, pathSecond[i].partialAlign_B, sizeof(uint64_t) * SEQ_MAX_LEN);
                        break;
                    }
                }

                if ( indicatorSecond > 29 && indicatorSecond < 35) {   // gap in charA
                    cost = getCost( GAP
                                  , charB[pathSecond[kSecond].input_character_B_ptr]
                                  , tcm
                                  , alphSize );

                    flag = updateCharacters( &pathFirst[i]
                                           ,  charA
                                           ,  lengthCharA
                                           ,  charB
                                           ,  lengthCharB
                                           ,  GAP
                                           ,  SEQ_MAX_LEN
                                           ,  tcm
                                           ,  alphSize
                                           ,  flagEmpty[1]
                                           ,  GAP_TO_B
                                           ,  cost * cost );
                    if(flag) {
                        memcpy(alignFinalA, pathFirst[i].partialAlign_A, sizeof(uint64_t) * SEQ_MAX_LEN);
                        memcpy(alignFinalB, pathFirst[i].partialAlign_B, sizeof(uint64_t) * SEQ_MAX_LEN);
                        break;
                    }
                }
            }
        }

        //******************************   sort the six nodes according to the ungapped metric   *************************************

        int arrayMix[2][6]= { { 10, 20, 30, 11, 21, 31 }
                            , { pathFirst[0].ungapped_partialCost
                              , pathFirst[1].ungapped_partialCost
                              , pathFirst[2].ungapped_partialCost
                              , pathSecond[0].ungapped_partialCost
                              , pathSecond[1].ungapped_partialCost
                              , pathSecond[2].ungapped_partialCost
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

    memcpy(finalAlign.partialAlign_A, alignFinalA, sizeof(uint64_t) * SEQ_MAX_LEN);
    memcpy(finalAlign.partialAlign_B, alignFinalB, sizeof(uint64_t) * SEQ_MAX_LEN);

    finalAlign.gapped_partialCost = 0;

    // TODO: check this ending condition; should it have a 0?
    for (i = 0; i < SEQ_MAX_LEN && finalAlign.partialAlign_A[i] && finalAlign.partialAlign_B[i]; i++) {
      //        printf("n+6th a: %2" PRIu64 " b: %2" PRIu64 " \n", finalAlign.partialAlign_A[i], finalAlign.partialAlign_B[i + LENGTH]);

        if (finalAlign.partialAlign_A[i] == GAP || finalAlign.partialAlign_B[i] == GAP) {
            finalAlign.gapped_partialCost += getCost(GAP, GAP, tcm, alphSize);

        } else if (finalAlign.partialAlign_A[i] == finalAlign.partialAlign_B[i]) {
            finalAlign.gapped_partialCost += getCost(finalAlign.partialAlign_A[i], finalAlign.partialAlign_B[i], tcm, alphSize);

        } else {
            finalAlign.gapped_partialCost += getCost(finalAlign.partialAlign_A[i], finalAlign.partialAlign_B[i], tcm, alphSize);
        }
    }

    //TODO: for now can't use memcpy, because I don't know what the lengths of the arrays are

    //int strIdx = 0;
    // memcpy(retAlign->char1, finalAlign.partialAlign_A, finalAlign.aligned_character_A_end_ptr);
    // retAlign->char1Len = finalAlign.aligned_character_A_end_ptr;
    // printf("char1Len: %zu\n", retAlign->char1Len);

    for(i = 0; i < SEQ_MAX_LEN && finalAlign.partialAlign_A[i] != 0; ++i) {
        retAlign->char1[i] = finalAlign.partialAlign_A[i];
    }
    retAlign->char1Len = i;

    // memcpy(retAlign->char2, finalAlign.partialAlign_B, finalAlign.aligned_character_B_end_ptr);
    // retAlign->char2Len = finalAlign.aligned_character_B_end_ptr;
    // printf("char2Len: %zu\n", retAlign->char2Len);

    for(i = 0; i < SEQ_MAX_LEN && finalAlign.partialAlign_B[i] != 0; ++i) {
        retAlign->char2[i] = finalAlign.partialAlign_B[i];
    }
    retAlign->char2Len = i;

    retAlign->cost = finalAlign.gapped_partialCost;

    // TODO: why would I need a ternary here? Shouldn't they be the same?
    retAlign->alignmentLength = (retAlign->char1Len < retAlign->char2Len) ? retAlign->char1Len : retAlign->char2Len;

    // for (i = 0; i < SEQ_MAX_LEN; ++i) {
    //   printf("buf[%d]: %lu\n", i, finalAlign.partialAlign_A[i]);
    // }

    // for (i = 0; i < retAlign->char1Len ; ++i) {
    //   printf("char1[%d]: %lu\n", i, retAlign->char1[i]);
    // }

    // for (i = 0; i < retAlign->char2Len ; ++i) {
    //   printf("char2[%d]: %lu\n", i, retAlign->char2[i]);
    // }

    // printf("s1    len: %zu\n", retAlign->char1Len        );
    // printf("s2    len: %zu\n", retAlign->char2Len        );
    // printf("align len: %zu\n", retAlign->alignmentLength);
    // printf("cost  val: %d\n" , retAlign->cost         );


    for (i = 0; i < 3; i++) {
        free( path[i].partialAlign_A );
        free( path[i].partialAlign_B );

        //  free(pathFirst[i].partialAlign_A);
        //  free(pathFirst[i].partialAlign_B);

        free( pathSecond[i].partialAlign_A );
        free( pathSecond[i].partialAlign_B );

        free( pathTempFirst[i].partialAlign_A );
        free( pathTempSecond[i].partialAlign_B );
    }
    free( pathFirstInfinite.partialAlign_A );
    free( pathFirstInfinite.partialAlign_B );
    free( pathSecondInfinite.partialAlign_A );
    free( pathSecondInfinite.partialAlign_B );
    free( finalAlign.partialAlign_A );

    free( charA );
    free( charB );


    // EDIT: returning success code.
    return 0;
}

/************************************   COMBINE SORT CANDIDATES ACCORDING TO TRUE METRIC  *******************************************/


int currentAlignmentCost(alignment_t *path, costMatrix_p tcm, size_t maxLen, size_t alphSize)
{
    size_t i;

    int costTempFirst  = 0,
        costTempSecond = 0,
        cost;

    // TODO: check that bounds checking is appropriate here
    for(i = 0; i < path->aligned_character_A_end_ptr && i < maxLen; i++) {
        // printf("n+7th a: %2" PRIu64 " b: %2" PRIu64 " (not indexing charA or charB)\n", path->partialAlign_A[i], path->partialAlign_B[i]);
        cost = getCost(path->partialAlign_A[i], path->partialAlign_B[i], tcm, alphSize);
        costTempFirst  += cost;
        costTempSecond += cost * cost;
    }

    costTempFirst += costTempFirst * costTempFirst;

    return costTempFirst + costTempSecond;;
}

void freeRetType(retType_t* toFree)
{
    free(toFree->char1);
    free(toFree->char2);
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
    copyTo[copyToIdx].gapped_partialCost     = copyFrom[copyFromIdx].gapped_partialCost;
    copyTo[copyToIdx].ungapped_partialCost   = copyFrom[copyFromIdx].ungapped_partialCost;
    copyTo[copyToIdx].aligned_character_A_end_ptr   = copyFrom[copyFromIdx].aligned_character_A_end_ptr;
    copyTo[copyToIdx].aligned_character_B_end_ptr   = copyFrom[copyFromIdx].aligned_character_B_end_ptr;
    copyTo[copyToIdx].input_character_A_ptr = copyFrom[copyFromIdx].input_character_A_ptr;
    copyTo[copyToIdx].input_character_B_ptr = copyFrom[copyFromIdx].input_character_B_ptr;
    copyTo[copyToIdx].flagWhichTree          = copyFrom[copyFromIdx].flagWhichTree;
    //TODO: and here
    memcpy(copyTo[copyToIdx].partialAlign_A, copyFrom[copyFromIdx].partialAlign_A, sizeof(uint64_t) * initLength);
    memcpy(copyTo[copyToIdx].partialAlign_B, copyFrom[copyFromIdx].partialAlign_B, sizeof(uint64_t) * initLength);
}

alignment_t *initAlignment( int    in_partialCost
                          , int    in_ungapped_partialCost
                          , size_t in_aligned_character_A_end_ptr
                          , size_t in_aligned_character_B_end_ptr
                          , size_t in_input_character_A_ptr
                          , size_t in_input_character_B_ptr
                          , int    in_flagWhichTree
                          , size_t initLength )
{
    alignment_t *output   = malloc( sizeof(alignment_t) );
    if( output == NULL || output == NULL ) {
        printf("Out of memory\n");
        fflush(stdout);
        exit(1);
    }

    output->gapped_partialCost     = in_partialCost;
    output->ungapped_partialCost   = in_ungapped_partialCost;
    output->aligned_character_A_end_ptr   = in_aligned_character_A_end_ptr;
    output->aligned_character_B_end_ptr   = in_aligned_character_B_end_ptr;
    output->input_character_A_ptr = in_input_character_A_ptr;
    output->input_character_B_ptr = in_input_character_B_ptr;
    output->flagWhichTree          = in_flagWhichTree;

    output->partialAlign_A = calloc( initLength, sizeof(uint64_t) );
    output->partialAlign_B = calloc( initLength, sizeof(uint64_t) );

    if( output->partialAlign_A == NULL || output->partialAlign_B == NULL ) {
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

/** Takes as input a path, which has two characters---the original input characters, and a variable
 *  which determines what is getting aligned (A/Gap, Gap/B or A/B). Updates the current cost of
 *  the new alignment, which is either the current cost summed with the passed-in cost (determined
 *  by the new aligned characters), or by finding the accumulated cost of the alignment thus far.
 *  Also updates the two characters by adding the appropriate character at the end of each, based on
 *  the input direction.
 */
int updateCharacters(       alignment_t *path
                    ,       uint64_t    *charA
                    ,       size_t       lengthCharA
                    ,       uint64_t    *charB
                    ,       size_t       lengthCharB
                    ,       uint64_t     GAP
                    ,       size_t       SEQ_MAX_LEN
                    , const costMatrix_p tcm
                    ,       size_t       alphSize
                    ,       size_t       flagEmpty
                    , enum transition    whichSub
                    ,       int          cost)
{

    size_t j = 0;    // loop index to be used later
    int done = 0;    // Has all of one of the input characters been consumed?

    path->gapped_partialCost += cost;

    // assign gap or appropriate element from characters A and B to appropriate gapped characters.
    // also increment pointer into input character A
    switch (whichSub) {
        case A_TO_GAP:
            path->partialAlign_A[ path->aligned_character_A_end_ptr ] = charA[path->input_character_A_ptr];
            path->partialAlign_B[ path->aligned_character_B_end_ptr ] = GAP;

            path->input_character_A_ptr = boundedIncrement(path->input_character_A_ptr, lengthCharA);
            break;
        case GAP_TO_B:
            path->partialAlign_A[path->aligned_character_A_end_ptr] = GAP;
            path->partialAlign_B[path->aligned_character_B_end_ptr] = charB[path->input_character_B_ptr];

            path->input_character_B_ptr = boundedIncrement(path->input_character_B_ptr, lengthCharB);
            break;
        case A_TO_B:
            path->partialAlign_A[path->aligned_character_A_end_ptr] = charA[path->input_character_A_ptr];
            path->partialAlign_B[path->aligned_character_B_end_ptr] = charB[path->input_character_B_ptr];

            printBuffer(path->partialAlign_A, SEQ_MAX_LEN, "partialAlign_A (2)");
            printBuffer(path->partialAlign_B, SEQ_MAX_LEN, "partialAlign_B (2)");

            path->input_character_A_ptr = boundedIncrement(path->input_character_A_ptr, lengthCharA );
            path->input_character_B_ptr = boundedIncrement(path->input_character_B_ptr, lengthCharB );
            break;
    }

    // increment both pointers into gapped characters, since we're just added elements to each
    path->aligned_character_A_end_ptr = boundedIncrement(path->aligned_character_A_end_ptr, SEQ_MAX_LEN);
    path->aligned_character_B_end_ptr = boundedIncrement(path->aligned_character_B_end_ptr, SEQ_MAX_LEN);

    //TODO: figure out what the meaning of this is.
    if (flagEmpty == 0) {
        path->ungapped_partialCost = currentAlignmentCost (path, tcm, SEQ_MAX_LEN, alphSize);
    }

    // If input character A's end has been reached, fill aligned character A with gaps and aligned char B with
    // end of input character B.
    if (path->input_character_A_ptr >= lengthCharA - 1) {
        for (; j < lengthCharB - path->input_character_B_ptr; j++) {
            path->partialAlign_A[ path->aligned_character_A_end_ptr + j ] = GAP;
            path->partialAlign_B[ path->aligned_character_B_end_ptr + j ] = charB[path->input_character_B_ptr + j];
        }
        done = 1;
    }

    // If input character B's end has been reached, fill aligned character B with gaps and aligned char A with
    // end of input character A.
    if (path->input_character_B_ptr >= lengthCharB - 1) {
        for (j = 0; j < lengthCharA - path->input_character_A_ptr; j++) {
            path->partialAlign_A[ path->aligned_character_A_end_ptr + j ] = charA[path->input_character_A_ptr + j];
            path->partialAlign_B[ path->aligned_character_B_end_ptr + j ] = GAP;
        }
        done = 1;
    }

    return done;
}
