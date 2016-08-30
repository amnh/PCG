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

size_t dynCharSize(const dynChar_t* const character) {
    size_t totalBits = character -> numElems * character -> alphSize;
    return (totalBits / WORD_WIDTH) + ((totalBits % WORD_WIDTH) ? 1 : 0);
}

size_t dcElemSize(size_t alphLen) {
    return (alphLen / WORD_WIDTH) + ((alphLen % WORD_WIDTH) ? 1 : 0);
}

int setDCElement( const size_t whichIdx, const dcElement_t* const changeToThis, dynChar_t* const charToBeAltered ) {
    if ( whichIdx >= charToBeAltered -> numElems ) {
        return 1;
    }
    if( changeToThis -> alphSize != charToBeAltered -> alphSize ) {
        return 2;
    }
    size_t start = whichIdx * charToBeAltered -> alphSize;
    size_t end   = start + charToBeAltered -> alphSize;
    for( size_t dynCharIdx = start, elementIdx = 0; dynCharIdx < end; dynCharIdx++, elementIdx++ ) {
        if( TestBit(changeToThis -> element, elementIdx) ) {
            SetBit(charToBeAltered -> dynChar, dynCharIdx);
        } else {
            ClearBit(charToBeAltered -> dynChar, dynCharIdx);
        }
    }
    return 0;
}

dcElement_t* getDCElement( const size_t whichChar, const dynChar_t* const inDynChar ) {
    
    dcElement_t* output = malloc( sizeof(dcElement_t) );
    output -> alphSize = 0;
    output -> element  = calloc( dcElemSize( inDynChar -> alphSize ), INT_WIDTH );

    // fail if prereqs aren't met or alloc failed
    if (  whichChar >= inDynChar -> numElems || output -> element == NULL ) {
        return output;
    }
    output -> alphSize = inDynChar -> alphSize;
    
    // copy values
    size_t start = whichChar * inDynChar -> alphSize;
    size_t end   = start + inDynChar -> alphSize;
    for( size_t getIdx = start, setIdx = 0; getIdx < end; getIdx++, setIdx++ ) {
        if( TestBit(inDynChar -> dynChar, getIdx) ) {
            SetBit(output -> element, setIdx);
        } else { // do I need this? I calloc'ed
            ClearBit(output -> element, setIdx);
        }
    }
    return output;
}

dcElement_t* makeDCElement( const size_t alphLen, const uint64_t value ) {
    // First create dynamic character with empty character field.
    dcElement_t* output = malloc( sizeof(dcElement_t) );
    output -> alphSize  = alphLen;
    output -> element   = calloc( dcElemSize(alphLen), INT_WIDTH ) ;
    
    // need a check here for longer alphabets
    for( size_t bitIdx = 0; bitIdx < alphLen; bitIdx++ ) {
        if( value & (CANONICAL_ONE << bitIdx) ) {
            SetBit(output -> element, bitIdx);
        }
    }
    return output;
}

double getCost( const dynChar_t* const inDynChar1, size_t whichElem1, const dynChar_t* const inDynChar2, size_t whichElem2, costMtx_t* tcm, dcElement_t* newElem1, dcElement_t* newElem2 ) {
    if (  newElem1 -> alphSize != inDynChar1 -> alphSize 
       || newElem2 -> alphSize != inDynChar1 -> alphSize
       || inDynChar1 -> alphSize != inDynChar2 -> alphSize 
       ) {
        return -1;
    }

    uint64_t overlap = *(inDynChar1 -> dynChar) & *(inDynChar2 -> dynChar);
    if( overlap ) {
        *(newElem1 -> element) = overlap;
        *(newElem2 -> element) = overlap;
        return 0;
    }

    int curCost = 0;
    uint64_t curElem = 0;
    for( size_t i = 1; i < inDynChar1 -> alphSize; i++ ) {
        for( size_t j = 1; j < inDynChar2 -> alphSize; j++ ) {
            if( TestBit(inDynChar1 -> dynChar, i) && TestBit(inDynChar1 -> dynChar, i) ) {
                // eventually add lookup here.
                curCost = tcm -> subCost;
                SetBit( newElem1 -> element, i );
                SetBit( newElem2 -> element, j );
            }
        }
    }
    if( !curCost ) {
        *(newElem1 -> element) = *(inDynChar1 -> dynChar);
        *(newElem2 -> element) = *(inDynChar2 -> dynChar);
        return tcm -> gapCost;
    }
    return curCost;
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
dynChar_t* makeDynamicChar( size_t alphLen, size_t numElems, uint64_t* values ) {
    // allocate dynamic character
    dynChar_t* output    = malloc( sizeof(dynChar_t) );
    output -> alphSize   = alphLen;
    output -> numElems   = numElems;
    output -> dynCharLen = dynCharSize( output );
    output -> dynChar    = calloc( output -> dynCharLen, INT_WIDTH );

    if( output -> dynChar == NULL ) {
        output -> alphSize = 0;
        return output;
    }
    for( int elemNum = 0; elemNum < numElems; elemNum++ ) {
        
        for( int bitIdx = 0; bitIdx < alphLen; bitIdx++ ) {
            if( values[elemNum] & (CANONICAL_ONE << bitIdx) ) {

                SetBit(output -> dynChar, elemNum * alphLen + bitIdx);
            }
        }
    }
    return output;
}

void freeDynChar( dynChar_t* p ) {
    free( p -> dynChar );
    free( p );
}

void freeDCElem( dcElement_t* p ) {
    free( p -> element );
    free( p );
}