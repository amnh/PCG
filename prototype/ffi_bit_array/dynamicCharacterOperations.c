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

unsigned int bufferSize(const DynChar* const character) {
    unsigned int charLen   = character -> dynCharLen;
    unsigned int alphLen   = character -> alphSize;
    unsigned int totalBits = charLen * alphLen;
    return (totalBits / WORD_WIDTH) + ((totalBits % WORD_WIDTH) ? 1 : 0);
}

int setStaticChar( const unsigned int whichIdx, const DynChar* const changeToThis, DynChar* const charToBeAltered ) {
    if ( whichIdx >= charToBeAltered -> dynCharLen
       || charToBeAltered -> alphSize != changeToThis -> alphSize 
       ) {
        return 1;
    }
    unsigned int start = whichIdx * charToBeAltered -> alphSize;
    unsigned int end   = start + charToBeAltered -> alphSize;
    for( unsigned int dynCharIdx = start, staticCharIdx = 0; dynCharIdx < end; dynCharIdx++, staticCharIdx++ ) {
        if( TestBit(changeToThis -> dynChar, staticCharIdx) ) {
            SetBit(charToBeAltered -> dynChar, dynCharIdx);
        } else {
            ClearBit(charToBeAltered -> dynChar, dynCharIdx);
        }
    }
    return 0;
}

int getStaticChar( const unsigned int whichChar, const DynChar* const inDynChar, DynChar* const outStaticChar ) {
    // fail if prereqs aren't met
    if (  whichChar >= inDynChar -> dynCharLen
       || inDynChar -> alphSize != outStaticChar -> alphSize 
       ) {
        return 1;
    }
    // copy values
    unsigned int start = whichChar * inDynChar -> alphSize;
    unsigned int end   = start + inDynChar -> alphSize;
    for( unsigned int getIdx = start, setIdx = 0; getIdx < end; getIdx++, setIdx++ ) {
        if( TestBit(inDynChar -> dynChar, getIdx) ) {
            SetBit(outStaticChar -> dynChar, setIdx);
        } else {
            ClearBit(outStaticChar -> dynChar, setIdx);
        }
    }
    outStaticChar -> dynCharLen = 1;
    return 0;
}

void makeStaticChar( const unsigned int alphLen, const uint64_t value, DynChar* const output ) {
    // First create dynamic character with empty character field.
    output -> alphSize = alphLen;
    output -> dynCharLen  = 1;
    
    // Now figure out how many uint64_t's we'll need in our array.
    // Then declare and initialize a dynChar set to all zeroes.
    unsigned int neededLen = bufferSize(output);
    output -> dynChar = calloc( neededLen, INT_WIDTH );  // all set to 0s
    for( unsigned int bitIdx = 0; bitIdx < alphLen; bitIdx++ ) {
        if( value & (CANONICAL_ONE << bitIdx) ) {
            SetBit(output -> dynChar, bitIdx);
        }
    }
}

