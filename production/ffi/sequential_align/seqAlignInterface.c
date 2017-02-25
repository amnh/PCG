#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

//#include "costMatrixWrapper.h"
#include "dynamicCharacterOperations.h"
#include "seqAlignForHaskell.h"

int performSequentialAlignment(dynChar_t* seqA, dynChar_t* seqB, costMatrix_p costMatrix, alignResult_t* result) {

    //printf("Successfully entered Yu Xiang's code on the other side of the FFI.\n");
    //fflush(stdout);

    uint64_t* seqA_main = dynCharToIntArr(seqA);
    uint64_t* seqB_main = dynCharToIntArr(seqB);

    retType_t* retAlign = malloc( sizeof(retType_t) );

    size_t length = seqA->numElems + seqB->numElems + 5;
    retAlign->seq1 = calloc(length, sizeof(int));
    retAlign->seq2 = calloc(length, sizeof(int));
    if( retAlign->seq1 == NULL || retAlign->seq2 == NULL ) {
        printf("Memory failure!\n");
        return 1;
    }
    retAlign->alignmentLength = length;

    /**
    getCost(1,  1, costMatrix, seqA->alphSize);
    getCost(5,  7, costMatrix, seqA->alphSize);
    getCost(9, 15, costMatrix, seqA->alphSize);
    **/
    /**/

    //printDynChar(seqA);
    //printDynChar(seqB);
    
    //printf("Before 'Aligner' call\n");
    //fflush(stdout);

    int success = aligner(seqA_main, seqA->numElems, seqB_main, seqB->numElems, seqA->alphSize, costMatrix, retAlign);

    //printf("After  'Aligner' call\n");
    //fflush(stdout);

    /**/
    result->finalChar1  = intArrToBitArr (seqA->alphSize, retAlign->alignmentLength, retAlign->seq1);
    result->finalChar2  = intArrToBitArr (seqA->alphSize, retAlign->alignmentLength, retAlign->seq2);
    result->medianChar  = seqA->dynChar;  //findMedian(result->finalChar1, result->finalChar2, );
    result->finalWt     = retAlign->weight;
    result->finalLength = retAlign->alignmentLength;

    // freeRetType(retAlign); NO! It's pointers all the way down!

    return 0;//success;
}
