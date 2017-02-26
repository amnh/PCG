#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

//#include "costMatrixWrapper.h"
#include "dynamicCharacterOperations.h"
#include "seqAlignForHaskell.h"
#include "seqAlignInterface.h"

int performSequentialAlignment(dynChar_t *seqA, dynChar_t *seqB, costMatrix_p costMatrix, alignResult_t *result)
{

    uint64_t* seqA_main = dynCharToIntArr(seqA);
    uint64_t* seqB_main = dynCharToIntArr(seqB);

    size_t length       = seqA->numElems + seqB->numElems + 5;
    printf("retAlign buffer length: %zu\n", length);
    size_t alphSize     = seqA->alphSize;

    retType_t *retAlign       = malloc( sizeof(retType_t) );
    retAlign->seq1            = calloc(length, sizeof(uint64_t));
    retAlign->seq2            = calloc(length, sizeof(uint64_t));
    retAlign->alignmentLength = 0;

    if( retAlign->seq1 == NULL || retAlign->seq2 == NULL ) {
        printf("Memory failure!\n");
        return 1;
    }

    int success         = aligner(seqA_main, seqA->numElems, seqB_main, seqB->numElems, alphSize, costMatrix, retAlign);

    size_t finalBufferLength = retAlign->alignmentLength * dcElemSize(alphSize);

    // TODO: wrong types, not dyn char, need just the buffers
    //result->finalChar1  = intArrToBitArr (alphSize, retAlign->alignmentLength, retAlign->seq1);
    result->finalChar1  = makeDynamicChar(alphSize, retAlign->alignmentLength, retAlign->seq1);

    //result->finalChar2  = intArrToBitArr (alphSize, retAlign->alignmentLength, retAlign->seq2);
    result->finalChar2  = makeDynamicChar(alphSize, retAlign->alignmentLength, retAlign->seq2);

    printf("Char 1 result construction:\n");
    printDynChar(result->finalChar1);
    printf("Char 2 result construction:\n");
    printDynChar(result->finalChar2);
    
    //    result->medianChar  = malloc(sizeof(dynChar_t));
    //result->medianChar  = malloc(sizeof(dynChar_t));

    result->finalWt     = retAlign->weight;
    result->finalLength = retAlign->alignmentLength;

    getMedian(result, costMatrix, alphSize);

    printf("Median result construction:\n");
    printDynChar(result->medianChar);

    // freeRetType(retAlign); NO! It's pointers all the way down!

    return 0;//success;
}

void getMedian(alignResult_t *input, costMatrix_p costMatrix, size_t alphSize)
{
    dcElement_t *key1   = malloc(sizeof(dcElement_t));
    key1->alphSize      = alphSize;
    key1->element       = input->finalChar1;

    dcElement_t *key2   = malloc(sizeof(dcElement_t));
    key2->alphSize      = alphSize;
    key2->element       = input->finalChar2;

    dcElement_t *median = malloc(sizeof(dcElement_t));
    median->alphSize    = alphSize;
    median->element     = calloc(input->finalLength, sizeof(packedChar));

    uint64_t *integralStateBuffer = calloc( input->finalLength, sizeof(packedChar));

    for( size_t i = 0; i < input->finalLength; i++ ) {
        getCostAndMedian(key1, key2, median, costMatrix);
	integralStateBuffer[i] = *median->element;
    }

    input->medianChar = makeDynamicChar(alphSize, input->finalLength, integralStateBuffer); 
    
    free(key1);
    free(key2);
    free(median);
}
