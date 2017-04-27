//
//  main.c
//  version_Haskell_bit
//
//  Created by Yu Xiang on 11/1/16.
//  Copyright © 2016 Yu Xiang. All rights reserved.
//


// recent changes: 1. Changed all malloc()s to calloc()s. calloc() initializes memory, as well as allocating.
//                    This could very easily be changed back for speed efficiency, but it kept showing up as an error in valgrind.
//                 2. Added a return value of 2. Now 0 is success, 1 is memory allocation error, 2 is inputs were both 0 length.
//                 3. Changed strncpy(retAlign, _, _) to be a series of loops, each terminating at '*'.
//                    Also terminated both strings with '\0', for good measure.
//                 4. Made INIT_LENGTH a const.
//                 5. INIT_LENGTH = 2 * retAlign->alignmentLength - 1.
//                 6. Made LENGTH a const.
//                 7. Some style changes to enhance legibility for myself.

#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "../../memoized_tcm/costMatrixWrapper.h"
#include "../../memoized_tcm/dynamicCharacterOperations.h"
#include "../seqAlignForHaskell.h"
#include "../seqAlignOutputTypes.h"

#define __STDC_FORMAT_MACROS

#define TCM_LENGTH 25

int main() {

    int tcm[TCM_LENGTH] = {0,1,1,1,1, 1,0,1,1,1, 1,1,0,1,1, 1,1,1,0,1, 1,1,1,1,0};
    size_t alphabetSize = 5;
    if ( TCM_LENGTH != alphabetSize * alphabetSize ) {
        printf("tcm wrong size\n");
        exit(1);
    }

    costMatrix_p costMatrix = matrixInit(alphabetSize, tcm);

    uint64_t seqA_main[] = {1, 16};
    size_t seqALen = 2;

    uint64_t seqB_main[] = {15, 1};
    size_t seqBLen = 2;



    int success = 1;
    retType_t* retAlign = malloc( sizeof(retType_t) );

    size_t length = sizeof(seqA_main)/sizeof(seqA_main[0]) + sizeof(seqB_main)/sizeof(seqB_main[0]) + 5;

 //   retAlign->seq1 = calloc(length, sizeof(char));
    retAlign->seq1 = calloc(length, sizeof(uint64_t));
  //  retAlign->seq2 = calloc(length, sizeof(char));
    retAlign->seq2 = calloc(length, sizeof(uint64_t));

    if( retAlign->seq1 == NULL || retAlign->seq2 == NULL ) {
        printf("Memory failure!\n");
        return 1;
    }
    retAlign->alignmentLength = length;

    success = aligner(seqA_main, seqALen, seqB_main, seqBLen, alphabetSize, costMatrix, retAlign);

    if (success == 0) {
        printf("\nSuccess!\n\n");
        printf("The aligned sequences are:\n");
        printf("  sequence 1:  [");
        for(size_t i = 0; i < length; ++i) {
            printf("%2" PRIu64 ", ", retAlign->seq1[i]);
        }
        printf("]\n  sequence 2:  [");
        for(size_t i = 0; i < length; ++i) {
            printf("%2" PRIu64 ", ", retAlign->seq2[i]);
        }
        printf("]\n");
        printf("The cost of the alignment is: %d\n", retAlign->cost);

    } else {
        printf("Fail!\n");
    }
    free(retAlign->seq1);
    free(retAlign->seq2);
    matrixDestroy(costMatrix);

}


/**
 *  A sample program that takes in two dynamic characters and returns two aligned dynamic chars,
 *  in the form of an alignResult_t. The third character is allocated on Haskell side and passed in by reference.
 *  Returns 0 on correct exit, 1 on allocation failure. This was used to test the Haskell FFI.
 */
/*
int exampleInterfaceFn(dynChar_t* seqA, dynChar_t* seqB, alignResult_t* result) {

    uint64_t* seqA_main = dynCharToIntArr(seqA);
    uint64_t* seqB_main = dynCharToIntArr(seqB);

    retType_t* retAlign = malloc( sizeof(retType_t) );

    long int length = seqA->numElems + seqB->numElems + 5;
    retAlign->seq1 = calloc(length, sizeof(int));
    retAlign->seq2 = calloc(length, sizeof(int));
    if( retAlign->seq1 == NULL || retAlign->seq2 == NULL ) {
        printf("Memory failure!\n");
        return 1;
    }
    retAlign->alignmentLength = length;

    // Call aligner with TCM here: int success = aligner(seqA_main, seqB_main, costMtx_t* tcm, &retAlign);
    // The following as an example
    int success = aligner(seqA_main, seqA->numElems, seqB_main, seqB->numElems, seqA->slphSize, tcm, retAlign);
    result->finalChar1 = intArrToBitArr (seqA->slphSize, retAlign->alignmentLength, retAlign->seq1);
    result->finalChar2 = intArrToBitArr (seqA->slphSize, retAlign->alignmentLength, retAlign->seq2);

    result->finalCost    = retAlign->cost;
    result->finalLength  = retAlign->alignmentLength;

    freeRetType(retAlign);
    free(retAlign);

    return 0;
}
*/
