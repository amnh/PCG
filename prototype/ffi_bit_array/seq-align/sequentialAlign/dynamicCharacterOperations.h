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
 *        exchanged out for packedChar *(i.e. arrays of 64-bit ints).
 */

#ifndef DYNAMIC_CHARACTER_OPERATIONS
#define DYNAMIC_CHARACTER_OPERATIONS

/**
 *  stdint is a library that provides int values for all architectures. This will allow the code to
 *  compile even on architectures on which int != 32 bits (and, more to the point, unsigned long int != 64 bits).
 */

#include <stdint.h>
#include <stdlib.h>
#include <stdint.h>

/** Following constants must be static to prevent compilation issues. */
static const size_t   BITS_IN_BYTE   = 8;                    // so bytes are set to 8, for all architectures
static const size_t   INT_WIDTH      = sizeof(uint64_t);     // don't forget: in bytes
static const size_t   WORD_WIDTH     = 8 * sizeof(uint64_t); // BITS_IN_BYTE * INT_WIDTH; <-- because HSC is dumb!
static const uint64_t CANONICAL_ONE  = 1;
static const uint64_t CANONICAL_ZERO = 0;

typedef uint64_t packedChar;
typedef void* costMatrix_p;

/** alignResult_t is where results get put for return to Haskell. For further notes see retType_t */
typedef struct alignResult_t {
    size_t      finalWt;
    size_t      finalLength;
    packedChar *finalChar1;
    packedChar *finalChar2;
    packedChar *medianChar;
} alignResult_t;

/** retType_t differs from alignResult_t in that it's the return type from the C sequence alignment code,
 *  which doesn't return packedChars, but ints. Also, the output from the sequence alignment gets post-processed
 *  to create a median, which is placed in medianChar in alignResult_t.
 */
typedef struct retType_t {
    int weight;
  //  char* seq1;
    int *seq1;
    size_t seq1Len;
  //  char* seq2;
    int *seq2;
    size_t seq2Len;
    long int alignmentLength;
} retType_t;

/**
 *  This holds the array of _possibly ambiguous_ static chars (i.e. a single dynamic character),
 *  along with its alphabet size and the number of "characters" (dcElements) in the dynChar.
 *  See note in .c file for how this is used.
 */
typedef struct dynChar_t {
    size_t      alphSize;
    size_t      numElems;     // how many dc elements are stored
    size_t      dynCharLen;   // how many uint64_ts are necessary to store the elements
    packedChar *dynChar;
} dynChar_t;

/** This is a single element from a dynamic character. It's a separate type because the dynamic character is packed.
 *  This may or may not have been a good judgement call, since both store some similar elements.
 *  The dynChar_t struct cannot just have an array of dcElement_ts, because of the packing.
 */
typedef struct dcElement_t {
    size_t      alphSize;
    packedChar *element;
} dcElement_t;

/** Pointer to a CostMatrix object. Needed in C for both sequential align and Haskell integration. */
typedef struct costMtx_t {
    int subCost;
    int gapCost;
} costMtx_t;

/**
 *  The following three functions taken from http://www.mathcs.emory.edu/~cheung/Courses/255/Syllabus/1-C-intro/bit-array.html
 *  Provides operations to use an array of ints as an array of bits. In essence, creates a mask of length k and uses that
 *  mask to set, clear and test bits.
 */
void SetBit( packedChar *const arr, const size_t k );

void ClearBit( packedChar *const arr, const size_t k );

uint64_t TestBit( const packedChar *const arr, const size_t k );

/** Clear entire packed character: all bits set to 0;
 *  packedCharLen is pre-computed dynCharSize()
 */
void ClearAll( packedChar *const arr, const size_t packedCharLen);

/** Returns size_t holding length a uint64_t array needs to be to hold numElems packed dynamic characters.
 *  Note that this is different from numElems * dcElemSize.
 */
size_t dynCharSize(size_t alphSize, size_t numElems);

/** Returns a size_t holding length a uint64_t array needs to be to hold a single packed dynamic character. */
size_t dcElemSize(size_t alphSize);

/** functions to free memory. Self-explanatory. **/
void freeDynChar( dynChar_t* p );

/** const because I needed it to be const when I free keys_t in CostMatrix.cpp. */
void freeDCElem( const dcElement_t* p );

/** functions to interact directly with DCElements */

/** Returns the correct gap value for this character.
 *  Allocates, so much be deallocated after each use.
 */
dcElement_t* getGap(const dynChar_t* const character);

/**
 *  Takes in a dynamic character to be altered, as well as the index of the element that will
 *  be replaced. A second input is provided, which is the replacement element.
 *  Fails if the position of the element to be replaced is beyond the end of the dynamic character to be altered.
 *  Fails if the alphabet sizes of the two input characters are different.
 *  Makes a copy of value in changeToThis, so can deallocate or mutate changeToThis later with no worries.
 */
int setDCElement( const size_t whichIdx,
                 const dcElement_t* const changeToThis, dynChar_t* const charToBeAltered );

/**
 *  Find and return an element from a packed dynamic character.
 *  Return failure (indicted as an alphabet size of 0) if:
 *  • character requested is beyond end of dynamic character's length
 *  • the alphabet sizes of the input and output characters don't match
 *
 *  This allocates, so must be
 *      a) NULL checked,
 *      b) freed later using deallocations, above.
 */
dcElement_t* getDCElement( const size_t whichChar, const dynChar_t* const indynChar_t);


/** Allocates a dcElement_t. Sets the element to 0 of the appropriate length,
 *  and the alphSize to alphSize.
 */
dcElement_t* allocateDCElement( const size_t alphSize );

/**
 *  Create an empty dynamic character element (i.e., a dynamic character with only one sub-character)
 *  using inputted alphabet length to determine necessary length of internal int array.
 *  Fill internal int array with zeroes.
 *  This allocates, so must be
 *      a) NULL checked,
 *      b) freed later using deallocations, above.
 */
dcElement_t* makeDCElement( const size_t alphSize, const uint64_t value );

/**
 *  Send in two elements. If there's an overlap, put that overlap into return dyn char, return 0.
 *  Otherwise, compute least cost and return that cost and put the median into return dynChar.
 *
 *  If the two characters are not compatible (have different length alphabets—it doesn't check
 *  to see that the alphabets are the same), returns a negative cost.
 *
 *  newElem1
 */
/* double getCostDyn( const dynChar_t* const inDynChar1, size_t whichElem1,
                const dynChar_t* const inDynChar2, size_t whichElem2,
                costMtx_t* tcm, dcElement_t* newElem1 );
*/

/** Allocator for dynChar_t
 *  This (obviously) allocates, so must be
 *      TODO: a) NULL checked,
 *      b) freed later using deallocations, above.
 */
dynChar_t* makeDynamicChar( size_t alphSize, size_t numElems, packedChar *values );

/** takes as input a dynamic character and converts it to a int array. Allocates, so after returned array
 *  is no longer in use it must be deallocated.
 *
 *  Nota bene: limits alphabet size to whatever the width of an int is, likely 2 bytes.
 */
uint64_t* dynCharToIntArr( dynChar_t* input );

/** takes as input an int array and copies its values into a packed dynamic character.
 *  This effectively recapitulates makeDynamicChar(), with one difference, this is intended to
 *  *copy* the contents, so it requires a preallocated dynChar_t. This is so that it can be placed
 *  into a container allocated on the other side of the FFI, and deallocated there, as well.
 */
void intArrToDynChar( size_t alphSize, size_t arrayLen, int* input, dynChar_t* output );

/** Copy input values to already alloced output and return a pointer to output */
void copyPackedChar( packedChar* inChar, packedChar* outChar, size_t alphSize);

/** As above, but only allocates and fills the bit array, not whole dyn char */
packedChar* intArrToBitArr( size_t alphSize, size_t arrayLen, int* input );

/** Takes two packed characters (uint64_t*) and finds the value as if they were bitwise AND'ed.
 *  Allocates, so must call freeDynChar() afterwards.
 */
packedChar* packedCharAnd(packedChar *lhs, packedChar *rhs, size_t alphSize);

/** Takes two dcElements and finds the value if they were bitwise OR'ed.
 *  Allocates, so must call freeDynChar() afterwards. Uses packedCharOr()
 *  to find | of elements.
 */
dcElement_t* dcElementOr (dcElement_t* lhs, dcElement_t* rhs);

/** Takes two packed characters (uint64_t*) and finds the value as if they were bitwise OR'ed.
 *  Allocates, so must call freeDynChar() afterwards.
 */
packedChar* packedCharOr (packedChar *lhs, packedChar *rhs, size_t alphSize);

int dcElementEq (dcElement_t* lhs, dcElement_t* rhs);

/** Print only the bit representation of an element of a dynamic character as a matrix.
 *  Calls printPackedChar().
 */
void printCharBits( const dynChar_t* const input );

/** Print only the bit representation of an element of a dynamic character element.
 * Calls printPackedChar().
 */
void printElemBits( const dcElement_t* const input );

/** Print the bit representation of an element of a dynamic character as a matrix
 *  but also print the alphabet size.
 *  Calls printCharBits().
 */
void printDynChar( const dynChar_t* const input );

/** Print only a packed character. */
void printPackedChar( const packedChar *input, size_t numElems, size_t alphSize );

#endif /* DYNAMIC_CHARACTER_OPERATIONS */
