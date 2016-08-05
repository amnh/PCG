/**
 *  Contains various helper fns for using bit arrays to implement packed dynamic characters.
 *  Individualt dynamic character elements are represented using bit arrays, where each bit marks whether 
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
 */


#ifndef DYNAMIC_CHARACTER
#define DYNAMIC_CHARACTER

/** 
 *  stdint is a library that provides int values for all architectures. This will allow the code to
 *  compile even on architectures on which int != 32 bits (and, more to the point, unsigned long int != 64 bits).
 */
#include <cstdint>
#include <vector>

 // TODO: If I make this a class, I can add a [] operator, as well as ^ and &. 
 // Maybe use vector under the hood, so resizing takes place automatically.
 // Also need concatenation, I think.

// these must be static to prevent compilation issues.
static const size_t   BITS_IN_BYTE   = 8;  // so bytes are set to 8, for all architectures
static const size_t   INT_WIDTH      = sizeof(uint64_t);
static const size_t   WORD_WIDTH     = BITS_IN_BYTE * INT_WIDTH; // BITS_IN_BYTE * INT_WIDTH; <-- because HSC is dumb!
static const uint64_t CANONICAL_ONE  = 1;
static const uint64_t CANONICAL_ZERO = 0;

/** 
 *  This holds the array of _possibly ambiguous_ static chars (i.e. a single dynamic character),
 *  along with it's alphabet size and the number of "characters" in the dynChar.
 *  See note in .c file for how this is used. 
 */
class dynChar_t {
    public: 
        dynChar_t( size_t alphSize = 5, size_t numElems = 1, uint64_t vals = {0} );
        /** takes the pre-computed size of a int vector */
        dynChar_t( size_t characterVectorLen );

        dynChar_t operator& ( dynChar_t const &right );

        dynChar_t operator[]( size_t pos );

        dynChar_t operator==( dynChar_t const &right );

        dynChar_t operator| ( dynChar_t const &right );

        dynChar_t operator^ ( dynChar_t const &right );

    private:
        vector<uint64_t> character; // packed dynamic character
        /** 
         *  Takes in a dynamic character to be altered, as well as the index of the static character that will
         *  be replaced. A second input is provided, which is the replacement static character.
         *  Fails if the position of the static char to be replaced is beyond the end of the dynamic character to be altered.
         *  Fails if the alphabet sizes of the two input characters are different.
         */    
        // int setDCElement( const unsigned int whichIdx, const dynChar_t* const changeToThis, const dynChar_t* const charToBeAltered );

        /** 
         *  Find and return a static character in a packed dynamic character.
         *  Copy that character into a static character (which was passed in by ref).
         *  Return failure if:
         *  • character requested is beyond end of dynamic character's length
         *  • the alphabet sizes of the input and output characters don't match
         *
         *  Nota bene: The outDCElement (created elsewhere) *must* have a dynChar of 
         *             adequate length, or things will go horribly wrong.
         */
        // int getDCElement( const unsigned int whichChar, const dynChar_t* const inDynChar_t, dynChar_t* const outDCElement );

        /** 
         *  Create an empty dynamic character element of appropriate length
         *  using inputted alphabet length to determine necessary length of internal int array.
         *  Fill internal int array with zeroes.
         */
        // void makeDCElement( const unsigned int alphLen, const uint64_t value, dynChar_t* const output );
};

typedef dynChar_t dcElement_t;

/** 
 *  The following three functions taken from http://www.mathcs.emory.edu/~cheung/Courses/255/Syllabus/1-C-intro/bit-array.html
 *  Provides operations to use an array of ints as an array of bits. In essence, creates a mask of length k and uses that
 *  mask to set, clear and test bits. 
 */ 
void SetBit( uint64_t* const arr, const unsigned int k );

void ClearBit( uint64_t* const arr, const unsigned int k );

uint64_t TestBit( uint64_t* const arr, const unsigned int k );

/* figures out how long the int array needs to be to hold a given dynamic character */
size_t bufferSize(const dynChar_t* const character);




#endif /* DYNAMIC_CHARACTER_OPERATIONS */
