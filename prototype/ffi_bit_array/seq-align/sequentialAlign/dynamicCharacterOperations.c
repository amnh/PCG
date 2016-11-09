/**
 * For function documentation see the header file.
 * Please don't modify the implementation without consulting library maintainer.
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "dynamicCharacterOperations.h"

void SetBit( uint64_t* const arr, const size_t k ) {
    arr[ k / WORD_WIDTH ] |= (CANONICAL_ONE << (k % WORD_WIDTH));
}
void ClearBit( uint64_t* const arr, const size_t k ) {
    arr[ k / WORD_WIDTH ] &= ~(CANONICAL_ONE << (k % WORD_WIDTH));
}

uint64_t TestBit( uint64_t* const arr, const size_t k ) {
    return arr[ k / WORD_WIDTH ] & (CANONICAL_ONE << (k % WORD_WIDTH));
}

size_t dynCharSize(size_t alphSize, size_t numElems) {
    size_t totalBits = numElems * alphSize;
    return (totalBits / WORD_WIDTH) + ((totalBits % WORD_WIDTH) ? 1 : 0);
}

size_t dcElemSize(size_t alphSize) {
    return (alphSize / WORD_WIDTH) + ((alphSize % WORD_WIDTH) ? 1 : 0);
}

uint64_t getGap(const dynChar_t* const character) {
    return 1 << (character->alphSize - 1);
}

int setDCElement( const size_t whichIdx, const dcElement_t* const changeToThis, dynChar_t* const charToBeAltered ) {
    if ( whichIdx >= charToBeAltered->numElems ) {
        return 1;
    }
    if( changeToThis->alphSize != charToBeAltered->alphSize ) {
        return 2;
    }
    size_t start = whichIdx * charToBeAltered->alphSize;
    size_t end   = start + charToBeAltered->alphSize;
    for( size_t dynCharIdx = start, elementIdx = 0; dynCharIdx < end; dynCharIdx++, elementIdx++ ) {
        if( TestBit(changeToThis->element, elementIdx) ) {
            SetBit(charToBeAltered->dynChar, dynCharIdx);
        } else {
            ClearBit(charToBeAltered->dynChar, dynCharIdx);
        }
    }
    return 0;
}

dcElement_t* getDCElement( const size_t whichChar, const dynChar_t* const inDynChar ) {
    
    dcElement_t* output = malloc( sizeof(dcElement_t) );
    output->alphSize = 0;
    output->element  = calloc( dcElemSize( inDynChar->alphSize ), INT_WIDTH );
    if (output->element == NULL) {
        printf("Out of memory.\n");
        fflush(stdout);
        exit(1);
    }

    // fail if prereqs aren't met or alloc failed
    if (  whichChar >= inDynChar->numElems || output->element == NULL ) {
        return output;
    }
    output->alphSize = inDynChar->alphSize;
    
    // copy values
    size_t start = whichChar * inDynChar->alphSize;
    size_t end   = start + inDynChar->alphSize;
    for( size_t getIdx = start, setIdx = 0; getIdx < end; getIdx++, setIdx++ ) {
        if( TestBit(inDynChar->dynChar, getIdx) ) {
            SetBit(output->element, setIdx);
        } else { // do I need this? I calloc'ed
            ClearBit(output->element, setIdx);
        }
    }
    return output;
}

dcElement_t* makeDCElement( const size_t alphSize, const uint64_t value ) {
    // First create dynamic character with empty character field.
    dcElement_t* output = malloc( sizeof(dcElement_t) );
    output->alphSize  = alphSize;
    output->element   = calloc( dcElemSize(alphSize), INT_WIDTH );
    if (output->element == NULL) {
        printf("Out of memory.\n");
        fflush(stdout);
        exit(1);
    }
    
    // need a check here for longer alphabets
    for( size_t bitIdx = 0; bitIdx < alphSize; bitIdx++ ) {
        if( value & (CANONICAL_ONE << bitIdx) ) {
            SetBit(output->element, bitIdx);
        }
    }
    return output;
}

double getCost( const dynChar_t* const inDynChar1, size_t whichElem1, const dynChar_t* const inDynChar2, size_t whichElem2, 
               costMtx_t* tcm, dcElement_t* newElem1 ) {
    if (  newElem1->alphSize != inDynChar1->alphSize 
       // || newElem2->alphSize != inDynChar1->alphSize
       || inDynChar1->alphSize != inDynChar2->alphSize 
       ) {
        return -1;
    }
    dcElement_t* char1Element = getDCElement(whichElem1, inDynChar1);
    dcElement_t* char2Element = getDCElement(whichElem2, inDynChar2);
    
    uint64_t overlap = *(char1Element->element) & *(char2Element->element);
    if( overlap ) {
        printf("%llu\n", overlap);
        *(newElem1->element) = overlap;
        // *(newElem2->element) = overlap;
        return 0;
    } else if ( *(char1Element->element) == getGap(inDynChar1) || *(char2Element->element) == getGap(inDynChar1) ) {
        *(newElem1->element) = *(char1Element->element) | *(char2Element->element);
        return tcm->gapCost;
    } else {
        *(newElem1->element) = *(char1Element->element) | *(char2Element->element);
        return tcm->subCost;
    }

    /* unused now, but will be needed once CostMatrix is finished
    int curCost = 0;
    uint64_t curElem = 0;
    for( size_t i = 1; i < char1Element->alphSize; i++ ) {
        for( size_t j = 1; j < char2Element->alphSize; j++ ) {
            if( TestBit(char1Element->element, i) && TestBit(char2Element->element, j) ) {
                // eventually add lookup here.
                curCost = tcm->subCost;
                SetBit( newElem1->element, i );
                SetBit( newElem1->element, j );
            }
        }
    }
    if( !curCost ) {
        *(newElem1->element) = *(char1Element->element);
        // *(newElem2->element) = *(char2Element->element);
        return tcm->gapCost;
    }
    return curCost;
    */
}

/** 
 *  The following fn should only needed this for testing. 
 *
 *  Takes in a dynamic character (by reference), an alphabet length, the number of static
 *  characters the array should hold, and an array of int values that should be packed into the
 *  the character. Then mutates the passed character to match the inputs.
 *
 *  If this is used, need to free afterwards.
 */
dynChar_t* makeDynamicChar( size_t alphSize, size_t numElems, uint64_t* values ) {
    // allocate dynamic character
    dynChar_t* output  = malloc( sizeof(dynChar_t) );
    if (output == NULL) {
        printf("Out of memory.\n");
        fflush(stdout);
        exit(1);
    }
    output->alphSize   = alphSize;
    output->numElems   = numElems;
    output->dynCharLen = dynCharSize( alphSize, numElems );
    output->dynChar    = calloc( output->dynCharLen, INT_WIDTH );
    if (output->dynChar == NULL) {
        printf("Out of memory.\n");
        fflush(stdout);
        exit(1);
    }

    if( output->dynChar == NULL ) {
        output->alphSize = 0;
        return output;
    }
    for( int elemNum = 0; elemNum < numElems; elemNum++ ) {
        
        for( int bitIdx = 0; bitIdx < alphSize; bitIdx++ ) {
            if( values[elemNum] & (CANONICAL_ONE << bitIdx) ) {

                SetBit(output->dynChar, elemNum * alphSize + bitIdx);
            }
        }
    }
    return output;
}

int* dynCharToIntArr(dynChar_t* input) {
    int* output = calloc(input->numElems, INT_WIDTH);
    if (output != NULL) {
        for( size_t i = 0; i < input->numElems; i++ ) {
            output[i] = (int) *(getDCElement(i, input)->element);
        }
    } else {
        printf("Out of memory.\n");
        fflush(stdout);
        exit(1);
    }
    return output;
}

void intArrToDynChar( size_t alphSize, size_t arrayLen, int* input, dynChar_t* output) {
    dcElement_t* changeToThis = makeDCElement( alphSize, CANONICAL_ZERO );
    output->alphSize   = alphSize;
    output->numElems   = arrayLen;
    output->dynCharLen = dynCharSize(alphSize, arrayLen);
    for( size_t i = 0; i < arrayLen; i++ ) {
        *changeToThis->element = (uint64_t) input[i];
        setDCElement( i, changeToThis, output );
    }
    freeDCElem(changeToThis);
}

uint64_t* intArrToBitArr( size_t alphSize, size_t arrayLen, int* input ) {
    uint64_t* output = calloc( dynCharSize(alphSize, arrayLen), INT_WIDTH );
    if (output == NULL) {
        printf("Out of memory.\n");
        fflush(stdout);
        exit(1);
    }
    uint64_t convert; // to point to int whose bit values are tested

    for( size_t arrIdx = 0, shift = 0; arrIdx < arrayLen; arrIdx++, shift += alphSize ) {
        for( size_t intIdx = 0; intIdx < alphSize; intIdx++ ) {
            convert = (uint64_t) input[arrIdx];
            if( TestBit(&convert, intIdx) ) {
                SetBit(output, shift + intIdx);
            } else {
                ClearBit(output, shift + intIdx);
            }
        }
    }
    return output;
}

void freeDynChar( dynChar_t* p ) {
    free( p->dynChar );
    free( p );
}

void freeDCElem( dcElement_t* p ) {
    free( p->element );
    free( p );
}