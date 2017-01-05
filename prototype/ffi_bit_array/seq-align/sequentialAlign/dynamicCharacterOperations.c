/**
 *  Module      :  Functions for processing bit-packed character data
 *  Description :  Contains various helper fns for using bit arrays to implement packed dynamic characters.
 *  Copyright   :  (c) 2016 Eric Ford, Division of Invertebrate Zoology, AMNH. All rights reserved.
 *  License     :
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright notice, this
 *     list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright notice,
 *     this list of conditions and the following disclaimer in the documentation
 *     and/or other materials provided with the distribution.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 *  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 *  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 *  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 *  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 *  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *  The views and conclusions contained in the software and documentation are those
 *  of the authors and should not be interpreted as representing official policies,
 *  either expressed or implied, of the FreeBSD Project.
 *
 *  Maintainer  :  Eric Ford <eford@amnh.org>
 *  Stability   :  Interface is stable.
 *  Portability :  Should be portable.
 *
 *  Contains various helper fns for using bit arrays to implement packed dynamic characters.
 *  Individual dynamic character elements are represented using bit arrays, where each bit marks whether
 *  a given character state is present in that element, so [1,0,0,1] would imply that the element is
 *  ambiguously an A or a T.
 *  The length of each element is thus the length of number of possible character states (the alphabet).
 *
 *  To clarify the nomenclature used in this file,
 *  • A dynamic character element (DCElement) is a single, possibly ambiguous phylogenetic character, e.g.,
 *    in the case of DNA, A or {A,G}.
 *  • A series of DCElements are "packed" if they are concatenated directly,
 *    and not stored one to each array position. Each array position might therefore hold many elements.
 *    For instance, one might store characters with alphabet size 4 in an array of int64s.
 *    In that case 16 elements would fit in each int in the array.
 *    Likewise, it's possible that only the first part of an element might fit into a single
 *    position in the array.
 *  • A dynamic character is a packed series of elements. (This isn't the _actual_ definition
 *    of a dynamic character, but will do for our purposes.)
 *
 *  TODO: for this to be useable on |alphabet including gap| > 64, various uint64_t types below will have to be
 *        exchanged out for packedChar_p (i.e. arrays of 64-bit ints).
 *
 *  For function documentation see the header file.
 *  Please don't modify the implementation without consulting library maintainer.
 */

// TODO: NULL check after all callocs/mallocs

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "dynamicCharacterOperations.h"

void SetBit( packedChar_p const arr, const size_t k ) {
    arr[ k / WORD_WIDTH ] |= (CANONICAL_ONE << (k % WORD_WIDTH));
}
void ClearBit( packedChar_p const arr, const size_t k ) {
    arr[ k / WORD_WIDTH ] &= ~(CANONICAL_ONE << (k % WORD_WIDTH));
}

uint64_t TestBit( packedChar_p const arr, const size_t k ) {
    return arr[ k / WORD_WIDTH ] & (CANONICAL_ONE << (k % WORD_WIDTH));
}

void ClearAll( packedChar_p const arr, const size_t packedCharLen) {
    for (size_t i = 0; i < packedCharLen; i++) {
        arr[i] = CANONICAL_ZERO;
    }
}

size_t dynCharSize(size_t alphSize, size_t numElems) {
    size_t totalBits = numElems * alphSize;
    return (totalBits / WORD_WIDTH) + ((totalBits % WORD_WIDTH) ? 1 : 0);
}

size_t dcElemSize(size_t alphSize) {
    return (alphSize / WORD_WIDTH) + ((alphSize % WORD_WIDTH) ? 1 : 0);
}

dcElement_t* getGap(const dynChar_t* const character) {
    dcElement_t* toReturn = malloc(sizeof(dynChar_t));
    toReturn->alphSize    = character->alphSize;
    toReturn->element     = calloc(character->dynCharLen, INT_WIDTH);
    SetBit(toReturn->element, character->alphSize - 1);
    return toReturn;
}

int setDCElement( const size_t whichIdx, const dcElement_t* const changeToThis,
                  dynChar_t* const charToBeAltered ) {
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
    if (output == NULL) {
        printf("Out of memory.\n");
        fflush(stdout);
        exit(1);
    }
    output->element = calloc( inDynChar->dynCharLen, INT_WIDTH );
    if (output->element == NULL) {
        printf("Out of memory.\n");
        fflush(stdout);
        exit(1);
    }
    output->alphSize = inDynChar->alphSize;
    // fail if prereqs aren't met
    if( whichChar >= inDynChar->numElems ) {
        output->alphSize = 0;
        return output;
    }


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

dcElement_t* allocateDCElement( const size_t alphSize ) {
    // First create dynamic character with empty character field.
    dcElement_t* output = malloc( sizeof(dcElement_t) );
    output->alphSize    = alphSize;
    output->element     = calloc( dcElemSize(alphSize), INT_WIDTH );
    if (output->element == NULL) {
        printf("Out of memory.\n");
        fflush(stdout);
        exit(1);
    }
    return output;
}

dcElement_t* makeDCElement( const size_t alphSize, const uint64_t value ) {
    dcElement_t* output = allocateDCElement( alphSize );

    // need a check here for longer alphabets
    for( size_t bitIdx = 0; bitIdx < alphSize; bitIdx++ ) {
        if( value & (CANONICAL_ONE << bitIdx) ) {
            SetBit(output->element, bitIdx);
        }
    }
    return output;
}

double getCostDyn( const dynChar_t* const inDynChar1, size_t whichElem1,
                const dynChar_t* const inDynChar2, size_t whichElem2,
                costMtx_t* tcm, dcElement_t* newElem1 ) {
    // won't work if alphabet sizes don't match
    if ( newElem1->alphSize   != inDynChar1->alphSize ||
         inDynChar1->alphSize != inDynChar2->alphSize ) {
        return -1;
    }
    // Now need to free newElem1->element, in case it was allocated previously,
    // because we're going to allocate below, then assign that allocated array
    // to newElem1->element.
    free(newElem1->element);

    // These will need to be freed later.
    dcElement_t* char1Element = getDCElement(whichElem1, inDynChar1);
    dcElement_t* char2Element = getDCElement(whichElem2, inDynChar2);
    dcElement_t* gapChar = getGap(inDynChar1);

    packedChar_p overlap = packedCharAnd(char1Element->element, char2Element->element, char1Element->alphSize);
    int isOverlap = 0;
    size_t numElems = dcElemSize(inDynChar1->alphSize);
    printf("numElems: %lu\n", numElems);
    for(size_t i = 0; i < numElems; i++ ) {
        if( overlap[i] ) {
            isOverlap = 1;
        }
    }
    if( isOverlap ) {
        newElem1->element = overlap;
        free(overlap);
        freeDCElem(gapChar);
        freeDCElem(char1Element);
        freeDCElem(char2Element);
        return 0;
    } else if ( dcElementEq( char1Element, gapChar ) ||
                dcElementEq( char2Element, gapChar ) ) {
        newElem1->element = packedCharOr(char1Element->element, char2Element->element, char1Element->alphSize);
        free(overlap);
        freeDCElem(gapChar);
        freeDCElem(char1Element);
        freeDCElem(char2Element);
        return tcm->gapCost;
    } else {
        newElem1->element = packedCharOr(char1Element->element, char2Element->element, char1Element->alphSize);
        free(overlap);
        freeDCElem(gapChar);
        freeDCElem(char1Element);
        freeDCElem(char2Element);
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
 *  The following fn should only be needed for testing. It will not work for alphabet sizes > 64.
 *
 *  Takes in a dynamic character (by reference), an alphabet length, the number of static
 *  characters the array should hold, and an array of int values that should be packed into the
 *  the character. Then mutates the passed character to match the inputs.
 *
 *  If this is used, needs to free afterwards.
 */
dynChar_t* makeDynamicChar( size_t alphSize, size_t numElems, packedChar_p values ) {
    // allocate dynamic character
    dynChar_t* output = malloc( sizeof(dynChar_t) );
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
    for( int elemNum = 0; elemNum < numElems; elemNum++ ) {
        for( int bitIdx = 0; bitIdx < alphSize; bitIdx++ ) {
            if( values[elemNum] & (CANONICAL_ONE << bitIdx) ) {
                SetBit(output->dynChar, elemNum * alphSize + bitIdx);
            }
        }
    }
    return output;
}

uint64_t* dynCharToIntArr(dynChar_t* input) {
    uint64_t* output = calloc(input->numElems, sizeof(uint64_t));
    if (output != NULL) {
        for( size_t i = 0; i < input->numElems; i++ ) {
            output[i] = (uint64_t) *(getDCElement(i, input)->element);
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

packedChar_p intArrToBitArr( size_t alphSize, size_t arrayLen, int* input ) {
    packedChar_p output = calloc( dynCharSize(alphSize, arrayLen), INT_WIDTH );
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

// TODO: test the next four fns. And make sure docs in .h file are good.
packedChar_p packedCharAnd(packedChar_p lhs, packedChar_p rhs, size_t alphSize) {
    size_t length = dcElemSize(alphSize);
    printf("length: %lu\n", length);
    packedChar_p toReturn = calloc(length, INT_WIDTH);
    for (size_t i = 0; i < length; i++) {
        toReturn[i] = lhs[i] & rhs[i];
    }
    return toReturn;
}

dcElement_t* dcElementOr(dcElement_t* lhs, dcElement_t* rhs) {
    dcElement_t* toReturn = malloc(sizeof(dcElement_t));
    size_t elementSz      = dcElemSize(lhs->alphSize);
    toReturn->alphSize    = lhs->alphSize;
    toReturn->element     = packedCharOr(lhs->element, rhs->element, lhs->alphSize);
    return toReturn;
}

packedChar_p packedCharOr (packedChar_p lhs, packedChar_p rhs, size_t alphSize) {
    size_t length = dcElemSize(alphSize);
    packedChar_p toReturn = calloc(length, INT_WIDTH);
    for (size_t i = 0; i < length; i++) {
        toReturn[i] = lhs[i] | rhs[i];
    }
    return toReturn;
}

int dcElementEq (dcElement_t* lhs, dcElement_t* rhs) {
    if (lhs->alphSize != rhs->alphSize) {
        return 0;
    }
    size_t numElems = dcElemSize(lhs->alphSize);
    for (size_t i = 0; i < numElems; i++) {
        if (lhs->element[i] != rhs->element[i]) {
            return 0;
        }
    }
    return 1;
}

void freeDynChar( dynChar_t* p ) {
    free( p->dynChar );
    free( p );
}

void freeDCElem( dcElement_t* p ) {
    free( p->element );
    free( p );
}

void printCharBits( const dynChar_t* const input ) {
    printPackedChar(input->dynChar, input->numElems, input->alphSize);
}

void printPackedChar( const packedChar_p input, size_t numElems, size_t alphSize ) {
    printf("[\n");

    for( size_t elemNum = 0; elemNum < numElems; elemNum++ ) {
        for( size_t bitIdx = 0; bitIdx < alphSize; bitIdx++ ) {
            if( TestBit(input, alphSize * elemNum + bitIdx) ) {
                // printf("Bit index:        %lu\n", alphSize * elemNum + bitIdx );
                printf("1,");
            } else {
                printf("0,");
            }
        }
        printf("\n");
    }
    printf("]\n");
}

void printElemBits( const dcElement_t* const input ) {
    printPackedChar( input->element, 1, input->alphSize );
}

void printDynChar( const dynChar_t* const input ) {
    printf("\nAlphabet Size:       %lu\n", input->alphSize);
    printf("Number of Elements:  %lu\n", input->numElems);
    printf("Internal arr Length: %lu ", input->dynCharLen);
    printf("(should need %lu.)\n", dynCharSize(input->alphSize, input->numElems) );
    printCharBits(input);
}