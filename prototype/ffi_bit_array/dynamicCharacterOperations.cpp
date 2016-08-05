/** 
 * For function documentation see the header file.
 * Don't modify the implementation without consulting library maintainer.
 */

#include <stdint.h>
#include <stdlib.h>
#include "dynamicCharacterOperations.h"

void SetBit( uint64_t* const arr, const unsigned int k ) {
    arr[ k / WORD_WIDTH ] |= (CANONICAL_ONE << (k % WORD_WIDTH)); 
}
void ClearBit( uint64_t* const arr, const unsigned int k ) { 
    arr[ k / WORD_WIDTH ] &= ~(CANONICAL_ONE << (k % WORD_WIDTH)); 
}

uint64_t TestBit( uint64_t* const arr, const unsigned int k ) { 
    return arr[ k / WORD_WIDTH ] & (CANONICAL_ONE << (k % WORD_WIDTH)); 
}

size_t bufferSize(const dynChar_t* const character) {
    unsigned int charLen   = character -> numElems;
    unsigned int alphLen   = character -> alphSize;
    unsigned int totalBits = charLen * alphLen;
    return (totalBits / WORD_WIDTH) + ((totalBits % WORD_WIDTH) ? 1 : 0);
}

int setDCElement( const unsigned int whichIdx, const dcElement_t* const changeToThis, const dynChar_t* const charToBeAltered ) {
    if ( whichIdx >= charToBeAltered -> numElems
       || charToBeAltered -> alphSize != changeToThis -> alphSize 
       ) {
        return 1;
    }
    unsigned int start = whichIdx * charToBeAltered -> alphSize;
    unsigned int end   = start + charToBeAltered -> alphSize;
    for( size_t dynCharIdx = start, elementIdx = 0; dynCharIdx < end; dynCharIdx++, elementIdx++ ) {
        if( TestBit(changeToThis -> dynChar, elementIdx) ) {
            SetBit(charToBeAltered -> dynChar, dynCharIdx);
        } else {
            ClearBit(charToBeAltered -> dynChar, dynCharIdx);
        }
    }
    return 0;
}

int getDCElement( const unsigned int whichChar, const dynChar_t* const inDynChar_t, dcElement_t* const outDCElement ) {
    // fail if prereqs aren't met
    if (  whichChar >= inDynChar_t -> numElems
       || inDynChar_t -> alphSize != outDCElement -> alphSize 
       ) {
        return 1;
    }
    // copy values
    unsigned int start = whichChar * inDynChar_t -> alphSize;
    unsigned int end   = start + inDynChar_t -> alphSize;
    for( size_t getIdx = start, setIdx = 0; getIdx < end; getIdx++, setIdx++ ) {
        if( TestBit(inDynChar_t -> dynChar, getIdx) ) {
            SetBit(outDCElement -> dynChar, setIdx);
        } else {
            ClearBit(outDCElement -> dynChar, setIdx);
        }
    }
    outDCElement -> numElems = 1;
    return 0;
}

void makeDCElement( const unsigned int alphLen, const uint64_t value, dcElement_t* const output ) {
    // First create dynamic character with empty character field.
    output -> alphSize = alphLen;
    output -> numElems = 1;
    output -> arrLen   = bufferSize(output);
    
    // Now figure out how many uint64_t's we'll need in our array.
    // Then declare and initialize a dynChar set to all zeroes.
    output -> dynChar = (uint64_t*) calloc( output -> arrLen, INT_WIDTH );  // all set to 0s
    for( size_t bitIdx = 0; bitIdx < alphLen; bitIdx++ ) {
        if( value & (CANONICAL_ONE << bitIdx) ) {
            SetBit(output -> dynChar, bitIdx);
        }
    }
}

dynChar_t operator&( const dynChar_t& right ) const {
     uint64_t* holder = (uint64_t*) malloc( right.arrLen * INT_WIDTH );
     for( size_t i; i < right.arrLen; i++ ) {
         holder[i] = right.dynChar[i] & dynChar[i];
     }
     dynChar_t anded;
     anded.alphSize = right.alphSize;
     anded.numElems = right.numElems;
     anded.arrLen   = right.arrLen;
     anded.dynChar  = holder;
     return anded;
 }

bool operator==( const dynChar_t& left, const dynChar_t& right ) const {
    if ( !(left.alphSize = right.alphSize && 
           left.numElems = right.numElems &&
           left.arrLen   = right.arrLen   &&
           left.dynChar  = holder) ) {
         return false;
     }
     uint64_t* holder = (uint64_t*) malloc( right.arrLen * INT_WIDTH );
     for( size_t i; i < right.arrLen; i++ ) {
         holder[i] = right.dynChar[i] & dynChar[i];
     }
     
     return true;
 }