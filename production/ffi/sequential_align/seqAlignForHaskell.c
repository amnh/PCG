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

#define __STDC_FORMAT_MACROS

// EDIT: removed all // printf()s, as fn needs to be pure

// EDIT: return code 1 or 0 (failure or success), depending on whether malloc fails
// EDIT: also added pointer to alignment_t finalAlign, memory for which will be allocated in Haskell
// code, and which will hold the result

// EDIT: I changed the fn name to something more evocative.
// Obviously, feel free to make it yet more so.

int aligner(uint64_t *seq1, size_t seq1Len, uint64_t *seq2, size_t seq2Len, size_t alphSize,
            costMatrix_p tcm, retType_t *retAlign)
{
    setbuf(stdout, NULL);


    //  const size_t LENGTH = seq1Len + seq2Len - 5; // strlen(seq1) + strlen(seq2);

    const uint64_t GAP = 1 << (alphSize - 1);

    const size_t SEQ_MAX_LEN   = seq1Len + seq2Len; // Maximum alignment length of either seqA and seqB (pathological)

    const size_t BUFFER_PAD    = 1; // Number of beffer cells between seqA and seqB
    
    const size_t BUFFER_OFFSET = SEQ_MAX_LEN + BUFFER_PAD; //Used for buffer offset so we don't overwrite seqA later

    //Yu_Edit: changed the length of INIT_LENGTH
    const size_t INIT_LENGTH = (2 * SEQ_MAX_LEN) + BUFFER_PAD; // Will be used to initialize all subsequent alignment arrays.
    // int arrays are not terminated with a special character, so must know length of each array. Also, don't need extra
    // space for terminal character.
    // I updated retType so that it returns two sequences. I thought I'd done that before, but I guess not.
    // Since sequences are int arrays, they're not terminated by \0, and therefore the two lengths are also necessary

    // Since a single array will hold both seqs, and each seq has length
    // alignmentLength, this number needs to be twice that, plus 1 for NULL.
    // However, the length was originally padded for a NULL, so one of those needs
    // to be removed.

    size_t i, j; // because i and j are being used a lot, and they were declared later on anyway

    //printf("length is %ld\n", LENGTH);
    if (SEQ_MAX_LEN == 0) {
        // printf("The lengths of the both sequences are zero!\n");
        return 2;
    }

    //We don't need this because we have calloc
    //uint64_t *initArr = calloc( INIT_LENGTH, sizeof(uint64_t) );

    // Now, test for allocation. Return 1 if it fails.
    /*
    if( initArr == NULL ) {
        printf("Out of memory\n");
        fflush(stdout);
        return 1;
    }
    */
    
    // for( i = 0; i < INIT_LENGTH; i++ ) {
    //     initArr[i] = 0;      // in bit representation, use 0 to replace * in the previous version
    // }

    //Yu_Edit  dynamically allocate struct
    //*******************  initialize struct ***************************
    alignment_t path[3] = {
        { .partialWt     = 0,
          .partialTrueWt = 0,
          .posStringA    = 0,
          .posStringB    = 0,
          .posTrueA      = 0,
          .posTrueB      = 0,
	  .flagWhichTree = 1
        },
        { .partialWt     = 0,
          .partialTrueWt = 0,
          .posStringA    = 0,
          .posStringB    = 0,
          .posTrueA      = 0,
          .posTrueB      = 0,
          .flagWhichTree = 1
        },
        { .partialWt     = 0,
          .partialTrueWt = 0,
          .posStringA    = 0,
          .posStringB    = 0,
          .posTrueA      = 0,
          .posTrueB      = 0,
          .flagWhichTree = 1
        }
    };

    for (i = 0; i < 3; i++) {
        path[i].partialAlign = calloc( INIT_LENGTH, sizeof(uint64_t) );
        if( path[i].partialAlign == NULL ) {
            return 1;
        }
        // No need, already zeros from calloc
        // memcpy(path[i].partialAlign, initArr, sizeof(uint64_t) * INIT_LENGTH);
    }
    
    // original metric
    // a*(\sum z_i)^2 + b*(\sum z_i) + c*(\sum z_i^2)
    // WLOG let a = b = c = 1
    //   (\sum z_i)^2 +   (\sum z_i) +   (\sum z_i^2)


    // first metric
    // \sum z_i

    // second metric
    // \sum z_i^2

    alignment_t pathTempFirst[3] =
    {
        { .partialWt     = 0,
          .partialTrueWt = 0,
          .posStringA    = 0,
          .posStringB    = 0,
          .posTrueA      = 0,
          .posTrueB      = 0,
          .flagWhichTree = 1
        },
        { .partialWt     = 0,
          .partialTrueWt = 0,
          .posStringA    = 0,
          .posStringB    = 0,
          .posTrueA      = 0,
          .posTrueB      = 0,
          .flagWhichTree = 1
        },
        { .partialWt     = 0,
          .partialTrueWt = 0,
          .posStringA    = 0,
          .posStringB    = 0,
          .posTrueA      = 0,
          .posTrueB      = 0,
          .flagWhichTree = 1
        }
    };

    for (i = 0; i < 3; i++) {
        pathTempFirst[i].partialAlign = calloc( INIT_LENGTH, sizeof(uint64_t) );
        if( pathTempFirst[i].partialAlign == NULL ) {
            return 1;
        }
    }

    // No need, source array is already zeroes from calloc, just like the desitination from calloc
    /*
    for (i = 0; i < 3; i++) {
        memcpy(pathTempFirst[i].partialAlign, initArr, sizeof(uint64_t) * INIT_LENGTH);
    }
    */
    
    alignment_t pathTempSecond[3] = {
                                       { .partialWt     = 0,
                                         .partialTrueWt = 0,
                                         .posStringA    = 0,
                                         .posStringB    = 0,
                                         .posTrueA      = 0,
                                         .posTrueB      = 0,
                                         .flagWhichTree = 1
                                       },
                                       { .partialWt     = 0,
                                         .partialTrueWt = 0,
                                         .posStringA    = 0,
                                         .posStringB    = 0,
                                         .posTrueA      = 0,
                                         .posTrueB      = 0,
                                         .flagWhichTree = 1
                                       },
                                       { .partialWt     = 0,
                                         .partialTrueWt = 0,
                                         .posStringA    = 0,
                                         .posStringB    = 0,
                                         .posTrueA      = 0,
                                         .posTrueB      = 0,
                                         .flagWhichTree = 1
                                       }
                                     };

    for (i = 0; i < 3; i++) {
        pathTempSecond[i].partialAlign = calloc( INIT_LENGTH, sizeof(uint64_t) );
        if( pathTempSecond[i].partialAlign == NULL ) {
            return 1;
        }
    }

    // No need, source array is already zeroes from calloc, just like the desitination from calloc
    /*
    for (i = 0; i < 3; i++) {
        memcpy(pathTempSecond[i].partialAlign, initArr, sizeof(uint64_t) * INIT_LENGTH);
    }
    */
    
    alignment_t pathFirstInfinite = { .partialWt     = 100000,
                                      .partialTrueWt = 100000,
                                      .posStringA    = 0,
                                      .posStringB    = 0,
                                      .posTrueA      = 0,
                                      .posTrueB      = 0,
                                      .flagWhichTree = 1
                                    };

    pathFirstInfinite.partialAlign = calloc( INIT_LENGTH, sizeof(uint64_t) );

    if( pathFirstInfinite.partialAlign == NULL ) {
        return 1;
    }

    // No need, source array is already zeroes from calloc, just like the desitination from calloc
    //memcpy(pathFirstInfinite.partialAlign, initArr, sizeof(uint64_t) * INIT_LENGTH);

    alignment_t pathSecondInfinite = { .partialWt     = 100000,
                                       .partialTrueWt = 100000,
                                       .posStringA    = 0,
                                       .posStringB    = 0,
                                       .posTrueA      = 0,
                                       .posTrueB      = 0,
                                       .flagWhichTree = 2
                                     };

    pathSecondInfinite.partialAlign = calloc( INIT_LENGTH, sizeof(uint64_t) );

    if( pathSecondInfinite.partialAlign == NULL ) {
        return 1;
    }

    // No need, source array is already zeroes from calloc, just like the desitination from calloc
    //memcpy(pathSecondInfinite.partialAlign, initArr, sizeof(uint64_t) * INIT_LENGTH);

    alignment_t finalAlign = { .partialWt     = 0,
                               .partialTrueWt = 0,
                               .posStringA    = 0,
                               .posStringB    = 0,
                               .posTrueA      = 0,
                               .posTrueB      = 0,
                               .flagWhichTree = 1
                             };


    finalAlign.partialAlign = calloc( INIT_LENGTH, sizeof(uint64_t) );

    if( finalAlign.partialAlign == NULL ) {
        return 1;
    }

    // No need, source array is already zeroes from calloc, just like the desitination from calloc
    //memcpy(finalAlign.partialAlign, initArr, sizeof(uint64_t) * INIT_LENGTH);

    uint64_t *seqA = calloc(seq1Len, sizeof(uint64_t) );

    // Now, test for allocation. Return 1 if it fails.
    if( seqA == NULL ) {
        return 1;
    }

    uint64_t *seqB = calloc(seq2Len, sizeof(uint64_t) );

    // Now, test for allocation. Return 1 if it fails.
    if( seqB == NULL ) {
        return 1;
    }

    // This looks good, making sure not to clobber your input
    memcpy( seqA, seq1, sizeof(uint64_t) * (seq1Len) );
    memcpy( seqB, seq2, sizeof(uint64_t) * (seq2Len) );

    printBuffer(seqA, seq1Len, "SeqA");
    printBuffer(seqB, seq2Len, "SeqB");


    
    int flag = 0;
    int flagEmpty[2] = {1,1};   // indicator for the case when one tree becomes empty,
                                // i.e., all the candidates nodes have converged to the other tree

    int c,
        d,
        n = 9,
        swapA,
        swapB;  // for sorting

    int indicatorFirst, indicatorSecond, indicatorInitial, indicatorMix;
    int kInitial, kFirst, kSecond, kMix;
    int iMatchFirst[3]  = { 0, 0, 0};
    int iMatchSecond[3] = { 0, 0, 0};
    size_t lengthSeqA, lengthSeqB;


    uint64_t *alignFinal = calloc( INIT_LENGTH, sizeof(uint64_t) );
    if( alignFinal == NULL ) {
        return 1;
    }
    
    // No need, source array is already zeroes from calloc, just like the desitination from calloc
    //memcpy(alignFinal, initArr, sizeof(uint64_t) * INIT_LENGTH);


    int iFirst = 0,
        iSecond = 0;
    lengthSeqA = seq1Len;
    lengthSeqB = seq1Len;


    //*******************************  Initialization first level generation for both trees ****************************//

    // under \sum z_i measure
    int aToB,
        aToGap,
        gapToB;

    printf("1st   a: %2llu b: %2llu\n", seqA[0], seqB[0]);

    aToB   = getCost(seqA[0], seqB[0], tcm, alphSize);
    aToGap = getCost(seqA[0], GAP,     tcm, alphSize);
    gapToB = getCost(GAP,     seqB[0], tcm, alphSize);

    alignment_t pathFirst[3] = {
        { .partialWt     = aToB,
          .partialTrueWt = aToB + 2 * aToB * aToB,
          .posStringA    = 1,
          .posStringB    = 1,
          .posTrueA      = 1,
          .posTrueB      = 1,
          .flagWhichTree = 1
        },
        { .partialWt     = aToGap,
          .partialTrueWt = aToGap + 2 * aToGap * aToGap,
          .posStringA    = 1,
          .posStringB    = 1,
          .posTrueA      = 1,
          .posTrueB      = 0,
          .flagWhichTree = 1
        },
        { .partialWt     = gapToB,
          .partialTrueWt = gapToB + 2 * gapToB * gapToB,
          .posStringA    = 1,
          .posStringB    = 1,
          .posTrueA      = 0,
          .posTrueB      = 1,
          .flagWhichTree = 1
        }
    };

    for (i = 0; i < 3; i++) {
        pathFirst[i].partialAlign = calloc( INIT_LENGTH, sizeof(uint64_t) );
        if( pathFirst[i].partialAlign == NULL ) {
            return 1;
        }
	// No need, source array is already zeroes from calloc, just like the desitination from calloc
        //memcpy(pathFirst[i].partialAlign, initArr, sizeof(uint64_t) * INIT_LENGTH);
    }

    pathFirst[0].partialAlign[0]             = seqA[0];
    pathFirst[0].partialAlign[BUFFER_OFFSET] = seqB[0];

    pathFirst[1].partialAlign[0]             = seqA[0];
    pathFirst[1].partialAlign[BUFFER_OFFSET] = GAP;

    pathFirst[2].partialAlign[0]             = GAP;
    pathFirst[2].partialAlign[BUFFER_OFFSET] = seqB[0];



    // !! the two weights (wtSub, wtInsertDel) are the same as in pathFirst
    //!! wtSub=getCost(seqA[0],seqB[0]);
    //!! wtInsertDel=getCost(seqa[0], '-');

    //  under \sum z_i^2 measure

    // printf("2nd   a: %2llu b: %2llu\n", seqA[0], seqB[0]);
    aToB   = getCost(seqA[0], seqB[0], tcm, alphSize);
    aToGap = getCost(seqA[0], GAP, tcm, alphSize);
    gapToB = getCost(GAP, seqB[0], tcm, alphSize);

    alignment_t pathSecond[3] = {
        { .partialWt     = aToB * aToB,
          .partialTrueWt = aToB + 2 * aToB * aToB,
          .posStringA    = 1,
          .posStringB    = 1,
          .posTrueA      = 1,
          .posTrueB      = 1,
          .flagWhichTree = 2
        },
        { .partialWt     = aToGap * aToGap,
          .partialTrueWt = aToGap + 2 * aToGap * aToGap,
          .posStringA    = 1,
          .posStringB    = 1,
          .posTrueA      = 1,
          .posTrueB      = 0,
          .flagWhichTree = 2
        },
        { .partialWt     = gapToB * gapToB,
          .partialTrueWt = gapToB + 2 * gapToB * gapToB,
          .posStringA    = 1,
          .posStringB    = 1,
          .posTrueA      = 0,
          .posTrueB      = 1,
          .flagWhichTree = 2
        }
    };

    for (i = 0; i < 3; i++) {
        pathSecond[i].partialAlign = calloc( INIT_LENGTH, sizeof(uint64_t) );
        if( pathSecond[i].partialAlign == NULL ) {
            return 1;
        }
	// No need, source array is already zeroes from calloc, just like the desitination from calloc
        //memcpy(pathSecond[i].partialAlign, initArr, sizeof(uint64_t) * INIT_LENGTH);

    }


    pathSecond[0].partialAlign[0]             = seqA[0];
    pathSecond[0].partialAlign[BUFFER_OFFSET] = seqB[0];

    pathSecond[1].partialAlign[0]             = seqA[0];
    pathSecond[1].partialAlign[BUFFER_OFFSET] = GAP;

    pathSecond[2].partialAlign[0]             = GAP;
    pathSecond[2].partialAlign[BUFFER_OFFSET] = seqB[0];

    printf("3rd   a: %2llu b: %2llu\n", seqA[0], seqB[0]);
    aToB   = getCost(seqA[0], seqB[0], tcm, alphSize);
    aToGap = getCost(seqA[0], GAP,     tcm, alphSize);
    gapToB = getCost(GAP,     seqB[0], tcm, alphSize);

    int arrayInitial[2][6]= {
        { 10, 20, 30, 11, 21, 31 },
        { aToB   + 2 * aToB   * aToB
        , aToGap + 2 * aToGap * aToGap
        , gapToB + 2 * gapToB * gapToB
        , aToB   + 2 * aToB   * aToB
        , aToGap + 2 * aToGap * aToGap
        , gapToB + 2 * gapToB * gapToB
        }
    };



    for (c = 0 ; c < ( 6 - 1 ); c++) {
        for (d = 0 ; d < 6 - c - 1; d++) {
            if (arrayInitial[1][d] > arrayInitial[1][d + 1]) {
                swapA                  = arrayInitial[1][d];
                arrayInitial[1][d]     = arrayInitial[1][d + 1];
                arrayInitial[1][d + 1] = swapA;

                swapB                  = arrayInitial[0][d];
                arrayInitial[0][d]     = arrayInitial[0][d + 1];
                arrayInitial[0][d + 1] = swapB;

            }
        }
    }



    for (i = 0; i < 3; i++) {

        indicatorInitial = *( *(arrayInitial + 0) + i);           // decide which operation to make
        kInitial         = *( *(arrayInitial + 0) + i) % 10;      // decide which path it belongs to

        if (kInitial == 0 && 9 < indicatorInitial && indicatorInitial < 12) {
            copyAligmentStruct(pathFirst, 0, path, i, INIT_LENGTH);
        } else if (kInitial == 0 && 19 < indicatorInitial && indicatorInitial < 22) {
            copyAligmentStruct(pathFirst, 1, path, i, INIT_LENGTH);
        } else if (kInitial == 0 && 29 < indicatorInitial && indicatorInitial < 32) {
            copyAligmentStruct(pathFirst, 2, path, i, INIT_LENGTH);
        } else if (kInitial == 1 && 9 < indicatorInitial && indicatorInitial < 12) {
            copyAligmentStruct(pathSecond, 0, path, i, INIT_LENGTH);
        } else if (kInitial == 1 && 19 < indicatorInitial && indicatorInitial < 22) {
            copyAligmentStruct(pathSecond, 1, path, i, INIT_LENGTH);
        } else{
            copyAligmentStruct(pathSecond, 2, path, i, INIT_LENGTH);
        }

    }

    for (i = 0; i < 3; i++) {
        pathFirst[i].partialWt     = pathFirstInfinite.partialWt;
        pathFirst[i].partialTrueWt = pathFirstInfinite.partialTrueWt;
        pathFirst[i].posStringA    = pathFirstInfinite.posStringA;
        pathFirst[i].posStringB    = pathFirstInfinite.posStringB;
        pathFirst[i].posTrueA      = pathFirstInfinite.posTrueA;
        pathFirst[i].posTrueB      = pathFirstInfinite.posTrueB;
        pathFirst[i].flagWhichTree = pathFirstInfinite.flagWhichTree;
        memcpy(pathFirst[i].partialAlign, pathFirstInfinite.partialAlign, sizeof(uint64_t) * INIT_LENGTH);

        pathSecond[i].partialWt     = pathSecondInfinite.partialWt;
        pathSecond[i].partialTrueWt = pathSecondInfinite.partialTrueWt;
        pathSecond[i].posStringA    = pathSecondInfinite.posStringA;
        pathSecond[i].posStringB    = pathSecondInfinite.posStringB;
        pathSecond[i].posTrueA      = pathSecondInfinite.posTrueA;
        pathSecond[i].posTrueB      = pathSecondInfinite.posTrueB;
        pathSecond[i].flagWhichTree = pathSecondInfinite.flagWhichTree;
        memcpy(pathSecond[i].partialAlign, pathSecondInfinite.partialAlign, sizeof(uint64_t) * INIT_LENGTH);
    }


    for (i = 0; i < 3; i++) {              // assign three candidate nodes to the two trees and other nodes are infinite nodes
        if (path[i].flagWhichTree == 1) {
            copyAligmentStruct(path, i, pathFirst, iFirst, INIT_LENGTH);
            iFirst++;
        }
        else if(path[i].flagWhichTree == 2) {
            copyAligmentStruct(path, i, pathSecond, iSecond, INIT_LENGTH);
            iSecond++;
        }
    }




    //test function

    int temp;
    temp = trueWt(&pathFirst[0], tcm, BUFFER_OFFSET, alphSize);

    //********************************************************************************************************
    //********************************   grow both trees based on initialization ***************************
    //********************************************************************************************************

    do {

        //*************************************** GROW TWO TREES BASED ON TWO METRICS  ******************************************************



        // grow tree according to first order metric: first tree

        for (i = 0; i < 3; i++) {
            if (      seqA[pathFirst[i].posTrueA] == seqB[pathFirst[i].posTrueB]
                   && pathFirst[i].posTrueA + 1 <= lengthSeqA
                   && pathFirst[i].posTrueB + 1 <= lengthSeqB) {
                iMatchFirst[i] = 0;
            } else if (    seqA[pathFirst[i].posTrueA] != seqB[pathFirst[i].posTrueB]
                        && pathFirst[i].posTrueA + 1 <= lengthSeqA
                        && pathFirst[i].posTrueB + 1<= lengthSeqB) {
                iMatchFirst[i] = 1;
            } else {
                iMatchFirst[i] = 1000;
            }
        }

        printf("4th   a[%d]: %2llu b[%d]: %2llu\n", pathFirst[0].posTrueA, seqA[pathFirst[0].posTrueA], pathFirst[0].posTrueB, seqB[pathFirst[0].posTrueB]);
        printf("5th   a[%d]: %2llu b[%d]: %2llu\n", pathFirst[1].posTrueA, seqA[pathFirst[1].posTrueA], pathFirst[1].posTrueB, seqB[pathFirst[1].posTrueB]);
        printf("6th   a[%d]: %2llu b[%d]: %2llu\n", pathFirst[1].posTrueA, seqA[pathFirst[2].posTrueA], pathFirst[2].posTrueB, seqB[pathFirst[2].posTrueB]);


        int arrayFirst[2][9]= {
            { 10, 20, 30, 11, 21, 31, 12, 22, 32 },
            {  getCost( seqA[pathFirst[0].posTrueA], seqB[pathFirst[0].posTrueB], tcm, alphSize ) + pathFirst[0].partialWt,
               getCost( seqA[pathFirst[0].posTrueA], GAP,                         tcm, alphSize ) + pathFirst[0].partialWt,
               getCost( GAP,                         seqB[pathFirst[0].posTrueB], tcm, alphSize ) + pathFirst[0].partialWt,
               getCost( seqA[pathFirst[1].posTrueA], seqB[pathFirst[1].posTrueB], tcm, alphSize ) + pathFirst[1].partialWt,
               getCost( seqA[pathFirst[1].posTrueA], GAP,                         tcm, alphSize ) + pathFirst[1].partialWt,
               getCost( GAP,                         seqB[pathFirst[1].posTrueB], tcm, alphSize ) + pathFirst[1].partialWt,
               getCost( seqA[pathFirst[2].posTrueA], seqB[pathFirst[2].posTrueB], tcm, alphSize ) + pathFirst[2].partialWt,
               getCost( seqA[pathFirst[2].posTrueA], GAP,                         tcm, alphSize ) + pathFirst[2].partialWt,
               getCost( GAP,                         seqB[pathFirst[2].posTrueB], tcm, alphSize ) + pathFirst[2].partialWt
            }
        };

        // grow tree according to second order metric: second tree


        for (i = 0; i < 3; i++) {
            if (     seqA[pathSecond[i].posTrueA] == seqB[pathSecond[i].posTrueB]
                     && pathSecond[i].posTrueA + 1 <= lengthSeqA
                     && pathSecond[i].posTrueB + 1 <= lengthSeqB ) {
                iMatchSecond[i] = 0;
            } else if ( seqA[pathSecond[i].posTrueA] != seqB[pathSecond[i].posTrueB]
                        && pathSecond[i].posTrueA + 1 <= lengthSeqA
                        && pathSecond[i].posTrueB + 1 <= lengthSeqB ) {
                iMatchSecond[i] = 1;
            } else {
                iMatchSecond[i] = 1000;
            }
        }

        printf("7th   a[%d]: %2llu b[%d]: %2llu\n", pathFirst[0].posTrueA, seqA[pathFirst[0].posTrueA], pathFirst[0].posTrueB, seqB[pathFirst[0].posTrueB]);
        printf("8th   a[%d]: %2llu b[%d]: %2llu\n", pathFirst[1].posTrueA, seqA[pathFirst[1].posTrueA], pathFirst[1].posTrueB, seqB[pathFirst[1].posTrueB]);
        printf("9th   a[%d]: %2llu b[%d]: %2llu\n", pathFirst[2].posTrueA, seqA[pathFirst[2].posTrueA], pathFirst[2].posTrueB, seqB[pathFirst[2].posTrueB]);

        aToB      = getCost( seqA[pathFirst[0].posTrueA], seqB[pathFirst[0].posTrueB], tcm, alphSize);
        int aToB1 = getCost( seqA[pathFirst[1].posTrueA], seqB[pathFirst[1].posTrueB], tcm, alphSize);
        aToGap = getCost( seqA[pathFirst[0].posTrueA], GAP,                         tcm, alphSize);
        int aToGap1 = getCost( seqA[pathFirst[1].posTrueA], GAP,                         tcm, alphSize);
        gapToB = getCost( GAP,                         seqB[pathFirst[0].posTrueB], tcm, alphSize);
        int arraySecond[2][9]= {
            { 10, 20, 30, 11, 21, 31, 12, 22, 32 },
            {   aToB * aToB + pathFirst[0].partialWt
            ,   aToGap * 2 + pathFirst[0].partialWt
            ,   gapToB * 2 + pathFirst[0].partialWt
            ,   aToB1 * 2 + pathFirst[1].partialWt
            ,   getCost( seqA[pathFirst[1].posTrueA], GAP,                         tcm, alphSize)
              * getCost( seqA[pathFirst[1].posTrueA], GAP,                         tcm, alphSize)
              + pathFirst[1].partialWt
            ,   getCost( GAP,                         seqB[pathFirst[1].posTrueB], tcm, alphSize)
              * getCost( GAP,                         seqB[pathFirst[1].posTrueB], tcm, alphSize)
              + pathFirst[1].partialWt
            ,   getCost( seqA[pathFirst[2].posTrueA], seqB[pathFirst[2].posTrueB], tcm, alphSize)
              * getCost( seqA[pathFirst[2].posTrueA], seqB[pathFirst[2].posTrueB], tcm, alphSize)
              + pathFirst[2].partialWt
            ,   getCost( seqA[pathFirst[2].posTrueA], GAP,                         tcm, alphSize)
              * getCost( seqA[pathFirst[2].posTrueA], GAP,                         tcm, alphSize)
              + pathFirst[2].partialWt
            ,   getCost( GAP, seqB[pathFirst[2].posTrueB],                         tcm, alphSize)
              * getCost( GAP, seqB[pathFirst[2].posTrueB],                         tcm, alphSize)
              + pathFirst[2].partialWt
            }
        };


        //************************************  SORT TWO TREES  *********************************************************

        // IMPORTANT TO BE UPDATED: WHEN THE THREE NODES CONVERGE TO ONE TREE, WE CAN FOCUS ON THAT TREE

        // first tree: sorting the second row by bubble and the first row change accordingly

        for (c = 0 ; c < ( n - 1 ); c++)
        {
            for (d = 0 ; d < n - c - 1; d++)
            {
                if (arrayFirst[1][d] > arrayFirst[1][d + 1]) {
                    swapA                = arrayFirst[1][d];
                    arrayFirst[1][d]     = arrayFirst[1][d + 1];
                    arrayFirst[1][d + 1] = swapA;

                    swapB                = arrayFirst[0][d];
                    arrayFirst[0][d]     = arrayFirst[0][d + 1];
                    arrayFirst[0][d + 1] = swapB;

                }
            }
        }

        // second tree: sorting the second row by bubble and the first row change accordingly

        for (c = 0; c < (n - 1); c++) {
            for (d = 0 ; d < n - c - 1; d++) {
                if (arraySecond[1][d] > arraySecond[1][d + 1]) {
                    swapA                 = arraySecond[1][d];
                    arraySecond[1][d]     = arraySecond[1][d + 1];
                    arraySecond[1][d + 1] = swapA;

                    swapB                 = arraySecond[0][d];
                    arraySecond[0][d]     = arraySecond[0][d + 1];
                    arraySecond[0][d + 1] = swapB;

                }
            }
        }



        //*************************************** grow first tree by obaining the three good nodes in first tree  ******************


        for (j = 0; j < 3; j++) {        // make a copy of previous paths, this is crutial since we need to keep track of the path
            copyAligmentStruct(pathFirst, j, pathTempFirst, j, INIT_LENGTH);
        }


        for (i = 0; i < 3; i++) {

            // TODO: figure out what's going on here and fix dereferencing
            indicatorFirst = *( arrayFirst[0] + i);           // decide which operation to make

            kFirst         = *( arrayFirst[0] + i) % 10;      // decide which path it belongs to

            copyAligmentStruct(pathTempFirst, kFirst, pathFirst, i, INIT_LENGTH);

            if ( indicatorFirst > 9 && indicatorFirst < 15) { // substitution
                printBuffer(pathFirst[i].partialAlign, INIT_LENGTH - 1, "pathFirst[i] (1), indicatorFirst in [10, 14]:");

                printf("nth   a[%d]: %2llu b[%d]: %2llu \n", pathFirst[kFirst].posTrueA, seqA[pathFirst[kFirst].posTrueA], pathFirst[kFirst].posTrueA, seqB[pathFirst[kFirst].posTrueB]);
                pathFirst[i].partialWt = pathFirst[i].partialWt
                                       + getCost( seqA[pathFirst[kFirst].posTrueA], seqB[pathFirst[kFirst].posTrueB], tcm, alphSize );

                pathFirst[i].partialAlign[pathFirst[i].posStringA]          = seqA[pathFirst[i].posTrueA];
		printBuffer(pathFirst[i].partialAlign, INIT_LENGTH, "partialAlign (1)");
                pathFirst[i].partialAlign[BUFFER_OFFSET + pathFirst[i].posStringB] = seqB[pathFirst[i].posTrueB];
		printBuffer(pathFirst[i].partialAlign, INIT_LENGTH, "partialAlign (2)");
                pathFirst[i].posStringA++;
                pathFirst[i].posStringB++;
                //LOOK: It gets incremented here!
                pathFirst[i].posTrueA = pathFirst[i].posTrueA + 1 >= seq1Len ? pathFirst[i].posTrueA : pathFirst[i].posTrueA + 1;
                pathFirst[i].posTrueB = pathFirst[i].posTrueB + 1 >= seq2Len ? pathFirst[i].posTrueB : pathFirst[i].posTrueB + 1;

                if (flagEmpty[0] == 0) {
                    pathFirst[i].partialTrueWt = trueWt(&pathFirst[i], tcm, BUFFER_OFFSET, alphSize);
                }

                if (pathFirst[i].posTrueA >= lengthSeqA || pathFirst[i].posTrueB >= lengthSeqB ) {
                    if (pathFirst[i].posTrueA >= lengthSeqA) {
                        for (j = 0; j < lengthSeqB - pathFirst[i].posTrueB; j++) {
                            pathFirst[i].partialAlign[pathFirst[i].posStringA + j]          = GAP;
                            pathFirst[i].partialAlign[BUFFER_OFFSET + pathFirst[i].posStringB + j] = seqB[pathFirst[i].posTrueB + j];
                        }
                    }
                    if (pathFirst[i].posTrueB >= lengthSeqB) {
                        for (j = 0; j < lengthSeqA-pathFirst[i].posTrueA; j++) {
                            pathFirst[i].partialAlign[BUFFER_OFFSET + pathFirst[i].posStringB + j] = GAP;
                            pathFirst[i].partialAlign[pathFirst[i].posStringA + j]          = seqA[pathFirst[i].posTrueA + j];
                        }
                    }

                    memcpy(alignFinal, pathFirst[i].partialAlign, sizeof(uint64_t) * INIT_LENGTH);
		    printBuffer(alignFinal, INIT_LENGTH - 1, "From pathFirst (1), indicatorFirst in [10, 14]:");
                    flag = 1;
                    break;
                }

            }

            if ( indicatorFirst > 19 && indicatorFirst < 25) {   // gap in seqB
	      //printf("n+1th a: %2llu b: %2llu \n", seqA[pathFirst[kFirst].posTrueA], seqB[pathFirst[kFirst].posTrueB]);
	      printf("n+1th a[%d]: %2llu b[X]: %2llu) \n", pathFirst[kFirst].posTrueA, seqA[pathFirst[kFirst].posTrueA], GAP);
                pathFirst[i].partialWt = pathFirst[i].partialWt
                                       + getCost(seqA[pathFirst[kFirst].posTrueA], GAP, tcm, alphSize);
                pathFirst[i].partialAlign[pathFirst[i].posStringA]          = seqA[pathFirst[i].posTrueA];
                pathFirst[i].partialAlign[BUFFER_OFFSET + pathFirst[i].posStringB] = GAP;
                pathFirst[i].posStringA++;
                pathFirst[i].posStringB++;
		pathFirst[i].posTrueA = pathFirst[i].posTrueA >= seq1Len ? pathFirst[i].posTrueA : pathFirst[i].posTrueA + 1;
		
                if (flagEmpty[0] == 0) {
                    pathFirst[i].partialTrueWt = trueWt(&pathFirst[i], tcm, BUFFER_OFFSET, alphSize);
                }

                if (pathFirst[i].posTrueA >= lengthSeqA || pathFirst[i].posTrueB >= lengthSeqB ) {

                    if (pathFirst[i].posTrueA >= lengthSeqA) {
                        for (j = 0; j < lengthSeqB-pathFirst[i].posTrueB; j++) {
                            pathFirst[i].partialAlign[pathFirst[i].posStringA + j] = GAP;
                            pathFirst[i].partialAlign[BUFFER_OFFSET + pathFirst[i].posStringB + j] = seqB[pathFirst[i].posTrueB + j];
                        }
                    }

                    if (pathFirst[i].posTrueB >= lengthSeqB) {
                        for (j = 0; j < lengthSeqA-pathFirst[i].posTrueA; j++) {
                            pathFirst[i].partialAlign[BUFFER_OFFSET + pathFirst[i].posStringB + j] = GAP;
                            pathFirst[i].partialAlign[pathFirst[i].posStringA + j] = seqA[pathFirst[i].posTrueA + j];
                        }
                    }

                    memcpy(alignFinal, pathFirst[i].partialAlign, sizeof(uint64_t) * INIT_LENGTH);
		    //printBuffer(alignFinal, INIT_LENGTH - 1, "From pathFirst (1), indicatorFirst in [20, 24]:");

                    flag = 1;
                    break;
                }
            }

            if ( indicatorFirst > 29 && indicatorFirst < 35)    // gap in seqA
            {
                printf("n+2th a[X]: %2llu b[%d]: %2llu \n", GAP, pathFirst[kFirst].posTrueB, seqB[pathFirst[kFirst].posTrueB]);

                pathFirst[i].partialWt = pathFirst[i].partialWt
                                       + getCost(GAP, seqB[pathFirst[kFirst].posTrueB], tcm, alphSize);
                pathFirst[i].partialAlign[pathFirst[i].posStringA] = GAP;
                pathFirst[i].partialAlign[BUFFER_OFFSET + pathFirst[i].posStringB] = seqB[pathFirst[i].posTrueB];
                pathFirst[i].posStringA++;
                pathFirst[i].posStringB++;
		//LOOK: incremented
		pathFirst[i].posTrueB = pathFirst[i].posTrueB + 1 >= seq2Len ? pathFirst[i].posTrueB : pathFirst[i].posTrueB + 1;

		if (flagEmpty[0] == 0) {

                    pathFirst[i].partialTrueWt = trueWt(&pathFirst[i], tcm, BUFFER_OFFSET, alphSize);
                }

                if (pathFirst[i].posTrueA >= lengthSeqA || pathFirst[i].posTrueB >= lengthSeqB ) {


                    if (pathFirst[i].posTrueA >= lengthSeqA) {
                        for (j = 0; j < lengthSeqB-pathFirst[i].posTrueB; j++) {
                            pathFirst[i].partialAlign[pathFirst[i].posStringA + j]          = GAP;
                            pathFirst[i].partialAlign[BUFFER_OFFSET + pathFirst[i].posStringB + j] = seqB[pathFirst[i].posTrueB + j];
                        }
                    }
                    if (pathFirst[i].posTrueB >= lengthSeqB) {
                        for (j = 0; j < lengthSeqA-pathFirst[i].posTrueA; j++) {
                            pathFirst[i].partialAlign[BUFFER_OFFSET + pathFirst[i].posStringB + j] = GAP;
                            pathFirst[i].partialAlign[pathFirst[i].posStringA + j] = seqA[pathFirst[i].posTrueA + j];
                        }
                    }

                    memcpy(alignFinal, pathFirst[i].partialAlign, sizeof(uint64_t) * INIT_LENGTH);
    		    printBuffer(alignFinal, INIT_LENGTH - 1, "From pathFirst (1), indicatorFirst in [30, 34]:");
                    flag = 1;
                    break;
                }
            }


        }


        //*************************************** grow second tree by obaining the three good nodes in second tree  ******************



        if (flag == 0) {

            for (j = 0; j < 3; j++) {        // make a copy of previous paths, this is crucial since we need to keep track of the path
                // in order to decide whether there is a match or substitution in the next position
                copyAligmentStruct(pathSecond, j, pathTempSecond, j, INIT_LENGTH);
                copyAligmentStruct(pathFirst, j, pathTempFirst, j, INIT_LENGTH);
            }


            for (i = 0; i < 3; i++){
                indicatorSecond = *( * (arraySecond + 0) + i);   // decide which operation to make
                kSecond = *( * (arraySecond + 0) + i) % 10;      // decide which path it belongs to

                copyAligmentStruct(pathTempSecond, kSecond, pathSecond, i, INIT_LENGTH);

                if ( indicatorSecond > 9 && indicatorSecond < 15) { // substitution
		  printf("n+3th a[%d]: %2llu b[%d]: %2llu \n", pathFirst[kFirst].posTrueA, seqA[pathFirst[kFirst].posTrueA], pathFirst[kFirst].posTrueB, seqB[pathFirst[kFirst].posTrueB]);

                    pathSecond[i].partialWt = pathSecond[i].partialWt
                                            + getCost(seqA[pathFirst[kSecond].posTrueA], seqB[pathSecond[kSecond].posTrueB], tcm, alphSize)
                                            * getCost(seqA[pathFirst[kSecond].posTrueA], seqB[pathSecond[kSecond].posTrueB], tcm, alphSize);

                    pathSecond[i].partialAlign[pathSecond[i].posStringA]          = seqA[pathSecond[i].posTrueA];
                    pathSecond[i].partialAlign[BUFFER_OFFSET + pathSecond[i].posStringB] = seqB[pathSecond[i].posTrueB];
                    pathSecond[i].posStringA++;
                    pathSecond[i].posStringB++;
		    //LOOK: Inc
		    pathSecond[i].posTrueA = pathSecond[i].posTrueA + 1 >= seq1Len ? pathSecond[i].posTrueA : pathSecond[i].posTrueA + 1;
		    pathSecond[i].posTrueB = pathSecond[i].posTrueB + 1 >= seq2Len ? pathSecond[i].posTrueB : pathSecond[i].posTrueB + 1;

                    if (flagEmpty[1] == 0) {
                        pathSecond[i].partialTrueWt = trueWt(&pathSecond[i], tcm, BUFFER_OFFSET, alphSize);

                    }

                    if (pathSecond[i].posTrueA >= lengthSeqA || pathSecond[i].posTrueB >= lengthSeqB ) {


                        if (pathSecond[i].posTrueA >= lengthSeqA) {
                            for (j = 0; j < lengthSeqB-pathSecond[i].posTrueB; j++) {
                                pathSecond[i].partialAlign[pathSecond[i].posStringA + j] = GAP;
                                pathSecond[i].partialAlign[BUFFER_OFFSET + pathSecond[i].posStringB + j] = seqB[pathSecond[i].posTrueB + j];
                            }
                        }
                        if (pathSecond[i].posTrueB >= lengthSeqB) {
                            for (j = 0; j < lengthSeqA-pathSecond[i].posTrueA; j++) {
                                pathSecond[i].partialAlign[BUFFER_OFFSET + pathSecond[i].posStringB + j] = GAP;
                                pathSecond[i].partialAlign[pathSecond[i].posStringA + j] = seqA[pathSecond[i].posTrueA + j];
                            }
                        }
                        memcpy(alignFinal, pathFirst[i].partialAlign, sizeof(uint64_t) * INIT_LENGTH);
 	                printBuffer(alignFinal, INIT_LENGTH - 1, "From pathFirst (2), indicatorFirst in [10, 14]:");

                        flag = 1;
                        break;
                    }

                }

                if ( indicatorSecond > 19 && indicatorSecond < 25) {   // gap in seqB
                    printf("n+4th a[%d]: %2llu b[X]: %2llu \n", pathFirst[kFirst].posTrueA, seqA[pathFirst[kFirst].posTrueA], GAP);

                    aToGap = getCost(seqA[pathFirst[kSecond].posTrueA], GAP, tcm, alphSize);
                    pathSecond[i].partialWt                                       = pathSecond[i].partialWt  + aToGap * aToGap;
                    pathSecond[i].partialAlign[pathSecond[i].posStringA]          = seqA[pathSecond[i].posTrueA];
                    pathSecond[i].partialAlign[BUFFER_OFFSET + pathSecond[i].posStringB] = GAP;
                    pathSecond[i].posStringA++;
                    pathSecond[i].posStringB++;
		    pathSecond[i].posTrueA = pathSecond[i].posTrueA + 1 >= seq1Len ? pathSecond[i].posTrueA : pathSecond[i].posTrueA + 1;

                    if (flagEmpty[1] == 0) {
                        pathSecond[i].partialTrueWt = trueWt(&pathSecond[i], tcm, BUFFER_OFFSET, alphSize);
                    }

                    // printf("partialTrueWt: %d\n", pathSecond[i].partialTrueWt);


                    if (pathSecond[i].posTrueA >= lengthSeqA || pathSecond[i].posTrueB >= lengthSeqB ) {

                        if (pathSecond[i].posTrueA >= lengthSeqA) {
                            for (j = 0; j < lengthSeqB - pathSecond[i].posTrueB; j++) {
                                pathSecond[i].partialAlign[pathSecond[i].posStringA + j]          = GAP;
                                pathSecond[i].partialAlign[BUFFER_OFFSET + pathSecond[i].posStringB + j] = seqB[pathSecond[i].posTrueB + j];
                            }
                        }
                        if (pathSecond[i].posTrueB >= lengthSeqB) {
                            for (j = 0; j < lengthSeqA - pathSecond[i].posTrueA; j++) {
                                pathSecond[i].partialAlign[BUFFER_OFFSET + pathSecond[i].posStringB + j] = GAP;
                                pathSecond[i].partialAlign[pathSecond[i].posStringA + j]          = seqA[pathSecond[i].posTrueA + j];
                            }
                        }
                        memcpy(alignFinal, pathFirst[i].partialAlign, sizeof(uint64_t) * INIT_LENGTH);
                        printBuffer(alignFinal, INIT_LENGTH - 1, "From pathFirst (2), indicatorFirst in [20, 24]:");

                        flag = 1;
                        break;
                    }
                }

                if ( indicatorSecond > 29 && indicatorSecond < 35) {   // gap in seqA
		    printf("n+5th a[X]: %2llu b[%d]: %2llu \n", GAP, pathFirst[kFirst].posTrueB, seqB[pathFirst[kFirst].posTrueB]);
                    gapToB = getCost(GAP, seqB[pathSecond[kSecond].posTrueB], tcm, alphSize);
                    pathSecond[i].partialWt                                       = pathSecond[i].partialWt  + gapToB * gapToB;
                    pathSecond[i].partialAlign[pathSecond[i].posStringA]          = GAP;
                    pathSecond[i].partialAlign[BUFFER_OFFSET + pathSecond[i].posStringB] = seqB[pathSecond[i].posTrueB];
                    pathSecond[i].posStringA++;
                    pathSecond[i].posStringB++;
		    //LOOK: Inc
		    pathSecond[i].posTrueB = pathSecond[i].posTrueB + 1 >= seq2Len ? pathSecond[i].posTrueB : pathSecond[i].posTrueB + 1;

                    if (flagEmpty[1] == 0) {
                        pathSecond[i].partialTrueWt = trueWt(&pathSecond[i], tcm, BUFFER_OFFSET, alphSize);

                    }

                    if (pathSecond[i].posTrueA >= lengthSeqA || pathSecond[i].posTrueB >= lengthSeqB ) {
                        if (pathSecond[i].posTrueA >= lengthSeqA) {
                            for (j = 0; j < lengthSeqB-pathSecond[i].posTrueB; j++) {
                                pathSecond[i].partialAlign[pathSecond[i].posStringA + j]          = GAP;
                                pathSecond[i].partialAlign[BUFFER_OFFSET + pathSecond[i].posStringB + j] = seqB[pathSecond[i].posTrueB + j];
                            }
                        }
                        if (pathSecond[i].posTrueB >= lengthSeqB) {
                            for (j = 0; j < lengthSeqA - pathSecond[i].posTrueA; j++) {
                                pathSecond[i].partialAlign[BUFFER_OFFSET + pathSecond[i].posStringB + j] = GAP;
                                pathSecond[i].partialAlign[pathSecond[i].posStringA + j]          = seqA[pathSecond[i].posTrueA + j];
                            }
                        }
                        memcpy(alignFinal, pathFirst[i].partialAlign, sizeof(uint64_t) * INIT_LENGTH);
                        printBuffer(alignFinal, INIT_LENGTH - 1, "From pathFirst (2), indicatorFirst in [30, 34]:");

                        flag = 1;
                        break;
                    }
                }
            }
        }

        //****************************************   sort the six nodes according to the true metric *************************************************

        int arrayMix[2][9]= {
            {10, 20, 30, 11, 21, 31},
            {pathFirst[0].partialTrueWt, pathFirst[1].partialTrueWt, pathFirst[2].partialTrueWt,
                pathSecond[0].partialTrueWt, pathSecond[1].partialTrueWt, pathSecond[2].partialTrueWt}
        };

        for (c = 0 ; c < ( 6 - 1 ); c++)
        {
            for (d = 0 ; d < 6 - c - 1; d++)
            {
                if (arrayMix[1][d] > arrayMix[1][d + 1]) {
                    swapA              = arrayMix[1][d];
                    arrayMix[1][d]     = arrayMix[1][d + 1];
                    arrayMix[1][d + 1] = swapA;

                    swapB              = arrayMix[0][d];
                    arrayMix[0][d]     = arrayMix[0][d + 1];
                    arrayMix[0][d + 1] = swapB;

                }
            }
        }


        for (i = 0; i < 3; i++) {

            indicatorMix = *( *(arrayMix + 0) + i);   // decide which operation to make
            kMix = *( *(arrayMix + 0) + i) % 10;      // decide which path it belongs to

            if (kMix == 0 && 9 < indicatorMix && indicatorMix < 12) {
                copyAligmentStruct(pathFirst, 0, path, i, INIT_LENGTH);
            } else if (kMix == 0 && 19 < indicatorMix && indicatorMix < 22) {
                copyAligmentStruct(pathFirst, 1, path, i, INIT_LENGTH);
            } else if (kMix == 0 && 29 < indicatorMix && indicatorMix <32) {
                copyAligmentStruct(pathFirst, 2, path, i, INIT_LENGTH);
            } else if (kMix == 1 && 9< indicatorMix && indicatorMix <12) {
                copyAligmentStruct(pathSecond, 0, path, i, INIT_LENGTH);
            } else if (kMix == 1 && 19< indicatorMix && indicatorMix <22) {
                copyAligmentStruct(pathSecond, 1, path, i, INIT_LENGTH);
            } else {
                copyAligmentStruct(pathSecond, 2, path, i, INIT_LENGTH);
            }

        }



        //****************************************   assign nodes for the next round *************************************************


        iFirst  = 0;
        iSecond = 0;


        for (i = 0; i < 3; i++) {                            // set all six nodes to be infinite nodes
            pathFirst[i].partialWt     = pathFirstInfinite.partialWt;
            pathFirst[i].partialTrueWt = pathFirstInfinite.partialTrueWt;
            pathFirst[i].posStringA    = pathFirstInfinite.posStringA;
            pathFirst[i].posStringB    = pathFirstInfinite.posStringB;
            pathFirst[i].posTrueA      = pathFirstInfinite.posTrueA;
            pathFirst[i].posTrueB      = pathFirstInfinite.posTrueB;
            pathFirst[i].flagWhichTree = pathFirstInfinite.flagWhichTree;
            memcpy(pathFirst[i].partialAlign, pathFirstInfinite.partialAlign, sizeof(uint64_t) * INIT_LENGTH);

            //    pathSecond[i] = pathSecondInfinite;
            pathSecond[i].partialWt     = pathSecondInfinite.partialWt;
            pathSecond[i].partialTrueWt = pathSecondInfinite.partialTrueWt;
            pathSecond[i].posStringA    = pathSecondInfinite.posStringA;
            pathSecond[i].posStringB    = pathSecondInfinite.posStringB;
            pathSecond[i].posTrueA      = pathSecondInfinite.posTrueA;
            pathSecond[i].posTrueB      = pathSecondInfinite.posTrueB;
            pathSecond[i].flagWhichTree = pathSecondInfinite.flagWhichTree;
            memcpy(pathSecond[i].partialAlign, pathSecondInfinite.partialAlign, sizeof(uint64_t) * INIT_LENGTH);
        }

        for (i = 0; i < 3; i++) {                            // assign three candidate nodes to the two trees and other nodes are infinite nodes
            if (path[i].flagWhichTree == 1) {
                copyAligmentStruct(path, i, pathFirst, iFirst, INIT_LENGTH);
                iFirst++;
            }
            else if(path[i].flagWhichTree == 2) {
                copyAligmentStruct(path, i, pathSecond, iSecond, INIT_LENGTH);
                Second].partialAlign, path[i].partialAlign, sizeof(uint64_t) * INIT_LENGTH);
                iSecond++;
            }
        }

        for (i = 0; i < 3; i++) {
            if(path[i].flagWhichTree == 1){
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


    memcpy(finalAlign.partialAlign, alignFinal, sizeof(uint64_t) * INIT_LENGTH);

    finalAlign.partialWt = 0;

    for(i = 0; i < SEQ_MAX_LEN && finalAlign.partialAlign[i] != 0 && finalAlign.partialAlign[BUFFER_OFFSET + i]; i++){
      //        printf("n+6th a: %2llu b: %2llu \n", finalAlign.partialAlign[i], finalAlign.partialAlign[i + LENGTH]);

        if (finalAlign.partialAlign[i] == GAP || finalAlign.partialAlign[i + BUFFER_OFFSET] == GAP) {
            finalAlign.partialWt = finalAlign.partialWt
                                 + getCost(GAP,                        GAP,                                 tcm, alphSize);
        } else if (finalAlign.partialAlign[i] == finalAlign.partialAlign[i + BUFFER_OFFSET]) {
            finalAlign.partialWt = finalAlign.partialWt
                                 + getCost(finalAlign.partialAlign[i], finalAlign.partialAlign[i + BUFFER_OFFSET], tcm, alphSize);
        } else {
            finalAlign.partialWt = finalAlign.partialWt
                                 + getCost(finalAlign.partialAlign[i], finalAlign.partialAlign[i + BUFFER_OFFSET], tcm, alphSize);
        }

    }

    // EDIT: here I'm assigning to retAlign. You might have a better way to do this.

    //int strIdx = 0;
    for(i = 0; i < SEQ_MAX_LEN && finalAlign.partialAlign[i] != 0; ++i) {
        retAlign->seq1[i] = finalAlign.partialAlign[i];
    }
    retAlign->seq1Len = i;
    //retAlign->seq1[strIdx] = '\0';
    /*
    while( finalAlign.partialAlign[strIdx] == 0 && strIdx < INIT_LENGTH - 1) {
        strIdx++;
    }
    int normalizer = strIdx;
    */
    for(i = 0; i < SEQ_MAX_LEN && finalAlign.partialAlign[BUFFER_OFFSET + i] != 0; ++i) {
        retAlign->seq2[i] = finalAlign.partialAlign[BUFFER_OFFSET + i];
    }
    retAlign->seq2Len = i;
    //retAlign->seq2[strIdx - normalizer] = '\0';

    retAlign->weight = finalAlign.partialWt;

    retAlign->alignmentLength = (retAlign->seq1Len < retAlign->seq2Len) ? retAlign->seq1Len : retAlign->seq2Len;

    for (i = 0; i < INIT_LENGTH; ++i) {
      printf("buf[%d]: %lu\n", i, finalAlign.partialAlign[i]);
    }

    for (i = 0; i < retAlign->seq1Len ; ++i) {
      printf("seq1[%d]: %lu\n", i, retAlign->seq1[i]);
    }

    for (i = 0; i < retAlign->seq2Len ; ++i) {
      printf("seq2[%d]: %lu\n", i, retAlign->seq2[i]);
    }

    printf("s1    len: %zu\n", retAlign->seq1Len        );
    printf("s2    len: %zu\n", retAlign->seq2Len        );
    printf("align len: %zu\n", retAlign->alignmentLength);
    printf("cost  val: %d\n" , retAlign->weight         );
    

    //free(initArr);
    for (i = 0; i < 3; i++) {
        free(path[i].partialAlign);

    }
    free(pathFirstInfinite.partialAlign);
    free(pathSecondInfinite.partialAlign);

    for (i = 0; i < 3; i++) {
      //  free(pathFirst[i].partialAlign);
        free(pathSecond[i].partialAlign);
    }
    for (i = 0; i < 3; i++) {
        free(pathTempFirst[i].partialAlign);
        free(pathTempSecond[i].partialAlign);
    }

    free(seqA);
    free(seqB);
    free(finalAlign.partialAlign);
    //free(alignFinal);

    // EDIT: returning success code.
    return 0;
}

/**************************************   COMBINE SORT CANDIDATES ACCORDING TO TRUE METRIC  *********************************************/


int trueWt(alignment_t *path, costMatrix_p tcm, size_t len, size_t alphSize)
{
    size_t i;

    int wtTempFirst = 0, wtTempSecond = 0;
    int wtTemp;

    for(i = 0; i < path->posStringA ; i++) {
        printf("n+7th a: %2llu b: %2llu (not indexing seqA or seqB)\n", path->partialAlign[i], path->partialAlign[i+len]);
        wtTempFirst = getCost(path->partialAlign[i], path->partialAlign[i+len], tcm, alphSize) + wtTempFirst;
    }
//  }

    for(i = 0; i < path->posStringA ; i++) {
        printf("n+8th a: %2llu b: %2llu (not indexing seqA or seqB)\n", path->partialAlign[i], path->partialAlign[i+len]);
        wtTempSecond = getCost(path->partialAlign[i], path->partialAlign[i+len], tcm, alphSize)
                     * getCost(path->partialAlign[i], path->partialAlign[i+len], tcm, alphSize)
                     + wtTempSecond;
    }

    wtTempFirst = wtTempFirst * wtTempFirst + wtTempFirst;
    wtTemp      = wtTempFirst + wtTempSecond;

    return wtTemp;

}

void freeRetType(retType_t* toFree)
{
    free(toFree->seq1);
    free(toFree->seq2);
    free(toFree);
    toFree = NULL;
}

void copyAligmentStruct ( alignment_t *copyFrom
                        , size_t copyFromIdx
                        , alignment_t *copyTo
                        , size_t copyToIdx
                        , const size_t initLength
                        )
{
    copyTo[copyToIdx].partialWt     = copyFrom[copyFromIdx].partialWt;
    copyTo[copyToIdx].partialTrueWt = copyFrom[copyFromIdx].partialTrueWt;
    copyTo[copyToIdx].posStringA    = copyFrom[copyFromIdx].posStringA;
    copyTo[copyToIdx].posStringB    = copyFrom[copyFromIdx].posStringB;
    copyTo[copyToIdx].posTrueA      = copyFrom[copyFromIdx].posTrueA;
    copyTo[copyToIdx].posTrueB      = copyFrom[copyFromIdx].posTrueB;
    copyTo[copyToIdx].flagWhichTree = copyFrom[copyFromIdx].flagWhichTree;
    memcpy(copyTo[copyToIdx].partialAlign, copyFrom[copyFromIdx].partialAlign, sizeof(uint64_t) * initLength);
}

void printBuffer(uint64_t *buffer, size_t bufLen, char *prefix)
{
    printf("which buffer: %s\n", prefix);
    printf("[ ");
    for(size_t i = 0; i < bufLen; i++) {
        printf("%llu, ", buffer[i]);
    }
    printf("]\n");
}
