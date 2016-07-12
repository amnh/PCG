#ifndef EXPORT_CHARACTER_TYPES
#define EXPORT_CHARACTER_TYPES

#include <stdint.h>

struct dynChar_t {
    int alphSize;      // The number of symbols in the alphabet, each sub character in the sequence is an element the power set.
    int dynCharLen;    // The number of sub characters in the sequence
    uint64_t* dynChar; // The length of the buffer can be calculated by calling "bufferSize"
};

struct alignResult_t {
    int finalWt;
    int finalLength;
    uint64_t* finalStr;
};

int bufferSize(const struct dynChar_t * const character);

int aligner( const struct dynChar_t * const seq1   // 1st sequence as an immutable pointer to an immutable structure
	   , const struct dynChar_t * const seq2   // 2nd sequence as an immutable pointer to an immutable structure
           , const int wtInsertDel                 // Immutable cost of an indel event (insertion or deletion)
           , const int wtSub                       // Immutable cost of an substitution event
           , struct alignResult_t * const retAlign // The return structure to be mutated, the pointer to the structure is immutable.
	   );


// For testing purposes, can be removed.
int testFN(struct dynChar_t*, struct dynChar_t*, struct alignResult_t*);

#endif /* EXPORT_CHARACTER_TYPES */
