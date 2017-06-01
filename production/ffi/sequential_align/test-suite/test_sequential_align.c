//
//  test_sequential_align.c



#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "../../memoized_tcm/costMatrixWrapper.h"
#include "../../memoized_tcm/dynamicCharacterOperations.h"
#include "../sequentialAlign.h"
#include "../sequentialAlignOutputTypes.h"

#define __STDC_FORMAT_MACROS

#define TCM_LENGTH 25

int main() {

    int tcm[TCM_LENGTH] = {0,1,1,1,2, 1,0,1,1,2, 1,1,0,1,2, 1,1,1,0,2, 2,2,2,2,0};
    size_t alphabetSize = 5;
    if ( TCM_LENGTH != alphabetSize * alphabetSize ) {
        printf("tcm wrong size\n");
        exit(1);
    }

    costMatrix_p costMatrix = matrixInit(alphabetSize, tcm);

    uint64_t charA_main[] = {1, 14, 2, 4};
    size_t charALen = 4;

    uint64_t charB_main[] = {15, 1};
    size_t charBLen = 2;



    int success = 1;
    retType_t* retAlign = malloc( sizeof(retType_t) );

    size_t length = sizeof(charA_main) / sizeof(charA_main[0]) + sizeof(charB_main) / sizeof(charB_main[0]) + 5;

 //   retAlign->char1 = calloc(length, sizeof(char));
    retAlign->char1 = calloc(length, sizeof(uint64_t));
  //  retAlign->char2 = calloc(length, sizeof(char));
    retAlign->char2 = calloc(length, sizeof(uint64_t));

    if( retAlign->char1 == NULL || retAlign->char2 == NULL ) {
        printf("Memory failure!\n");
        return 1;
    }
    retAlign->alignmentLength = length;

    success = aligner(charA_main, charALen, charB_main, charBLen, alphabetSize, costMatrix, retAlign);

    if (success == 0) {
        printf("\nSuccess!\n\n");
        printf("The aligned sequences are:\n");
        printf("  sequence 1:  [");
        for(size_t i = 0; i < length; ++i) {
            printf("%2" PRIu64 ", ", retAlign->char1[i]);
        }
        printf("]\n  sequence 2:  [");
        for(size_t i = 0; i < length; ++i) {
            printf("%2" PRIu64 ", ", retAlign->char2[i]);
        }
        printf("]\n");
        printf("The cost of the alignment is: %d\n", retAlign->cost);

    } else {
        printf("Fail!\n");
    }
    free(retAlign->char1);
    free(retAlign->char2);
    matrixDestroy(costMatrix);

}


/**
 *  A sample program that takes in two dynamic characters and returns two aligned dynamic chars,
 *  in the form of an alignResult_t. The third character is allocated on Haskell side and passed in by reference.
 *  Returns 0 on correct exit, 1 on allocation failure. This was used to test the Haskell FFI.
 */
/*
int exampleInterfaceFn(dynChar_t* charA, dynChar_t* charB, alignResult_t* result) {

    uint64_t* charA_main = dynCharToIntArr(charA);
    uint64_t* charB_main = dynCharToIntArr(charB);

    retType_t* retAlign = malloc( sizeof(retType_t) );

    long int length = charA->numElems + charB->numElems + 5;
    retAlign->char1 = calloc(length, sizeof(int));
    retAlign->char2 = calloc(length, sizeof(int));
    if( retAlign->char1 == NULL || retAlign->char2 == NULL ) {
        printf("Memory failure!\n");
        return 1;
    }
    retAlign->alignmentLength = length;

    // Call aligner with TCM here: int success = aligner(charA_main, charB_main, costMtx_t* tcm, &retAlign);
    // The following as an example
    int success = aligner(charA_main, charA->numElems, charB_main, charB->numElems, charA->slphSize, tcm, retAlign);
    result->finalChar1 = intArrToBitArr (charA->slphSize, retAlign->alignmentLength, retAlign->char1);
    result->finalChar2 = intArrToBitArr (charA->slphSize, retAlign->alignmentLength, retAlign->char2);

    result->finalCost    = retAlign->cost;
    result->finalLength  = retAlign->alignmentLength;

    freeRetType(retAlign);
    free(retAlign);

    return 0;
}
*/
