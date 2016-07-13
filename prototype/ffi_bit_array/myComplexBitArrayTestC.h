/**
 *  Contains various helper fns for using bit arrays to implement packed dynamic characters.
 *  Static characters are represented using bit arrays, where each bit marks whether a given character
 *  state is present in that character, so [1,0,0,1] would imply that the character is ambiguously an A or a T.
 *  The length of each character is thus the length of number of possible character states (the alphabet).
 *
 *  To clarify the nomenclature used in this file,
 *  • A static character is a single, possibly ambiguous phylogenetic character, e.g., in the case of DNA, A or {A,G}.
 *  • A series of static characters are "packed" if they are concatenated directly, and not stored one to each
 *    array position. Each position might therefore hold many characters. For instance, one might store
 *    characters with alphabet size 4 in an array of int64s. In that case 16 characters would fit in each int in the array. 
 *    Likewise, it's possible that only the first part of a character might fit into a single position in the array.
 *    If, for instance, the alphabet length is greater than the number of bits in the array type, only the first x bits
 *    would fit into the first position, and the rest of the character would necessarily flow into the next int in the array. 
 *  • A dynamic character is a packed series of static characters. (This isn't the _actual_ definition
 *    of a dynamic character, but will do for our purposes.)
 */

/** 
 *  stdint is a library that provides int values for all architectures. This will allow the code to
 *  compile even on architectures on which int != 32 bits (and, more to the point, unsigned long int != 64 bits).
 */
#include <stdint.h>

#define BITS_IN_BYTE 8  // so bytes are set to 8, for all architectures
#define INT_WIDTH sizeof(uint64_t)
#define WORD_WIDTH BITS_IN_BYTE * INT_WIDTH
const uint64_t CANONICAL_ONE = 1;

/** 
 *  The following three #defines taken from http://www.mathcs.emory.edu/~cheung/Courses/255/Syllabus/1-C-intro/bit-array.html
 *  Provides operations to use an array of ints as an array of bits. In essence, creates a mask of length k and uses that
 *  mask to set, clear and test bits. 
 */ 
#define SetBit(arr,k)     ( arr[ k / WORD_WIDTH ] |=  (1 << (k % WORD_WIDTH)) )
#define ClearBit(arr,k)   ( arr[ k / WORD_WIDTH ] &= ~(1 << (k % WORD_WIDTH)) )
#define TestBit(arr,k)    ( arr[ k / WORD_WIDTH ] &   (1 << (k % WORD_WIDTH)) )

/* alignResult_t is where results get put for return to Haskell */
typedef struct AlignResult {
    int finalWt;
    int finalLength;
    uint64_t* finalStr;
} AlignResult;

/** 
 *  This holds the array of _possibly ambiguous_ static chars (i.e. a single dynamic character),
 *  along with it's alphabet size and the number of "characters" in the dynChar.
 *  See note in .c file for how this is used. 
 */
typedef struct DynChar {
    int alphSize;
    int dynCharLen;
    uint64_t* dynChar;
} DynChar;

// To be commented out later
int testFN(DynChar*, DynChar*, AlignResult*);

/* figures out how long the int array needs to be to hold a given dynamic character */
int bufferSize(DynChar* character) {
    int charLen    = character -> dynCharLen;
    int alphLen    = character -> alphSize;
    uint64_t totalBits  = (uint64_t) (charLen * alphLen);
    printf("charLen:        %d\n", charLen);
    printf("alphLen:        %d\n", alphLen);
    printf("totalBits:      %llu\n", totalBits);
    printf("bitsInWord:     %llu\n", WORD_WIDTH);
    printf("Primary:        %lu\n", (uint64_t) totalBits / (uint64_t) WORD_WIDTH);
    printf("extra:          %d\n", (totalBits % WORD_WIDTH) ? 1 : 0);
    printf("total uint64_t: %llu\n", (totalBits / WORD_WIDTH) + ((totalBits % WORD_WIDTH) ? 1 : 0) );
    return (totalBits / WORD_WIDTH) + ((totalBits % WORD_WIDTH) ? 1 : 0);
}

/** Takes in a dynamic character to be altered, as well as the index of the static character that will
 *  be replaced. A second input is provided, which is the replacement static character.
 *  Fails if the position of the static char to be replaced is beyond the end of the dynamic character to be altered.
 *  Fails if the alphabet sizes of the two input characters are different.
 */    
int setStaticChar(DynChar* charToBeAltered, int whichIdx, DynChar* changeToThis) {
    if ( whichIdx >= charToBeAltered -> dynCharLen
       || charToBeAltered -> alphSize != changeToThis -> alphSize 
       ) {
        return 1;
    }
    int start = whichIdx * charToBeAltered -> alphSize;
    int end   = start + charToBeAltered -> alphSize;
    for( int dynCharIdx = start, staticCharIdx = 0; dynCharIdx < end; dynCharIdx++, staticCharIdx++ ) {
        if( TestBit(changeToThis -> dynChar, staticCharIdx) ) {
            SetBit(charToBeAltered -> dynChar, dynCharIdx);
        } else {
            ClearBit(charToBeAltered -> dynChar, dynCharIdx);
        }
    }
    return 0;
}

/** 
 *  Find a static character in a packed dynamic character.
 *  Copy that character into a static character (which was passed in by ref).
 *  Return failure if:
 *  • character requested is beyond end of dynamic character's length
 *  • the output character is the wrong length
 *  • the alphabet sizes of the input and output characters don't match
 */
int getStaticChar(DynChar* inDynChar, int which, DynChar* outStaticChar) {
    // get necessary length and tack on isExtra
    int minArrLen = bufferSize(inDynChar);
    // fail if prereqs aren't met
    if (  which >= inDynChar -> dynCharLen
       || outStaticChar -> dynCharLen < minArrLen
       || inDynChar -> alphSize != outStaticChar -> alphSize 
       ) {
        return 1;
    }
    // copy values
    int start = which * inDynChar -> alphSize;
    int end   = start + inDynChar -> alphSize;
    for( int getIdx = start, setIdx = 0; getIdx < end; getIdx++, setIdx++ ) {
        if( TestBit(outStaticChar -> dynChar, getIdx) ) {
            SetBit(outStaticChar -> dynChar, setIdx);
        } else {
            ClearBit(outStaticChar -> dynChar, setIdx);
        }
    }
    outStaticChar -> dynCharLen = 1;
    return 0;
}

/** 
 *  Create an empty static character (i.e., a dynamic character with only one sub-character)
 *  using inputted alphabet length to determine necessary length of internal int array.
 *  Fill internal int array with zeroes.
 */
void makeEmptyStaticChar( DynChar* output, int alphLen ) {
    // First create dynamic character with empty character field.
    output -> alphSize = alphLen;
    output -> dynCharLen  = 1;
    
    // Now figure out how many uint64_t's we'll need in our array.
    // Then declare and initialize a dynChar set to all zeroes.
    int neededLen = bufferSize(output);
    uint64_t interimArr [neededLen];
    for( int i = 0; i < neededLen; i++ ) {
        interimArr[i] = 0;
    }
    output -> dynChar = interimArr;
}

