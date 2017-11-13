#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "../memoized_tcm/costMatrixWrapper.h"
#include "../memoized_tcm/dynamicCharacterOperations.h"
#include "sequentialAlign.h"
#include "sequentialAlignInterface.h"
#include "sequentialAlignOutputTypes.h"

#define __STDC_FORMAT_MACROS

int performSequentialAlignment(dynChar_t *seqA, dynChar_t *seqB, costMatrix_p costMatrix, alignResult_t *result)
{

    uint64_t* seqA_main = dynCharToIntArr(seqA);
    uint64_t* seqB_main = dynCharToIntArr(seqB);

    size_t length       = seqA->numElems + seqB->numElems + 5;
    // printf("retAlign buffer length: %zu\n", length);
    size_t alphSize     = seqA->alphSize;

    retType_t *retAlign       = malloc( sizeof(retType_t) );
    retAlign->char1            = calloc(length, sizeof(uint64_t));
    retAlign->char2            = calloc(length, sizeof(uint64_t));
    retAlign->alignmentLength = 0;

    if( retAlign->char1 == NULL || retAlign->char2 == NULL ) {
        printf("Memory failure!\n");
        return 1;
    }

    int success              = aligner(seqA_main, seqA->numElems, seqB_main, seqB->numElems, alphSize, costMatrix, retAlign);

    // size_t finalBufferLength = retAlign->alignmentLength * dcElemSize(alphSize);

    result->finalChar1  = intArrToBitArr (alphSize, retAlign->alignmentLength, retAlign->char1);
    result->finalChar2  = intArrToBitArr (alphSize, retAlign->alignmentLength, retAlign->char2);

    result->medianChar  = getMedian(retAlign->char1, retAlign->char2, retAlign->alignmentLength, alphSize, costMatrix);

    result->finalCost   = retAlign->cost;
    result->finalLength = retAlign->alignmentLength;

    //printf("Median result construction:\n");
    //printDynChar(result->medianChar);

    // freeRetType(retAlign); NO! It's pointers all the way down!

    return success;
}

packedChar *getMedian(const packedChar *const lhs, const packedChar *const rhs, const size_t length, const size_t alphSize, costMatrix_p costMatrix)
{
    dcElement_t *median = malloc(sizeof(dcElement_t));
    median->alphSize    = alphSize;
    median->element     = calloc(length, sizeof(packedChar));

    uint64_t *integralStateBuffer = calloc(length, sizeof(packedChar));

    for( size_t i = 0; i < length; i++ ) {
        getCostInternal(lhs[i], rhs[i], costMatrix, alphSize, median);
        uint64_t value = *median->element;
        printf("%zu: a: %" PRIu64 ", b: %" PRIu64 ", median: %" PRIu64 "\n", i, lhs[i], rhs[i], value);
        integralStateBuffer[i] = value;
    }

    free(median);
    return intArrToBitArr(alphSize, length, integralStateBuffer);
}
