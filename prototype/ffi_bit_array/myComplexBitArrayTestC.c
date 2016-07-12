#include <stdio.h>
#include <stdlib.h>
#include <string.h> 
#include "myComplexBitArrayTestC.h"


// takes in two strings, and returns a struct. The string in the struct is wt1 * the first string +
// wt2 * the second string. The int in the struct is wt1 * wt2.
// returns 0 on correct exit, 1 on allocation failure
int testFn(struct dynChar_t* seqA, struct dynChar_t* seqB, struct alignResult_t* result) {
    // get total length of output string
    int charLenA = seqA -> dynCharLen;
    int charLenB = seqB -> dynCharLen;
    int buffLenA = bufferSize(seqA);
    int buffLenB = bufferSize(seqB);
    
    printf("C: alphabet  length  %d\n", seqA->alphSize);
    printf("C: character length  %d\n", seqA->dynCharLen);
    printf("C: character pointer %p\n", seqA->dynChar);
    

    // have to malloc because declaring a length in the formal params 
    // was causing buffer overflows according to valgrind
    // Unless malloc() fails, eventual freeing of memory must be done in Haskell FFI code.
    uint64_t* buffer = calloc( buffLenA + buffLenB, sizeof(uint64_t)); // in bytes

    // Now check that malloc() didn't fail.
    if( buffer == NULL ) {
        return 1;
    }
    
    for(int i = 0; i < buffLenA; i++) {
        buffer[i] = seqA -> dynChar[i];
        printf("C: character A loop, %llu\n", seqA -> dynChar[i]);
    }
    
    for(int i = 0; i < buffLenB; i++) {
        buffer[i + buffLenA] = seqB -> dynChar[i];
        printf("C: character B loop, %llu\n", seqB -> dynChar[i]);
    }

    result->finalWt     = buffLenA + buffLenB; // Real cost goes here later
    result->finalLength = buffLenA + buffLenB;
    result->finalStr    = buffer;
    return 0;
}

#define BITS_IN_BYTE 8;

int bufferSize(struct dynChar_t* character) {
  int charLen    = character -> dynCharLen;
  int alphLen    = character -> alphSize;
  int totalBits  = charLen * alphLen;
  int bitsInWord = sizeof(uint64_t) * BITS_IN_BYTE;
  return (totalBits / bitsInWord) + ((totalBits % bitsInWord) ? 1 : 0);
}

/**
int main() {
    uint64_t* buffer; 
    struct align* emptyStc = malloc(sizeof(uint64_t*) + sizeof(int));
    printf("%lu\n", sizeof(buffer));
    emptyStc->finalWt = 1;
    emptyStc->finalStr = buffer;
    printf("%d\n", testFn("a", "b", 1, 0, emptyStc));
    printf("%s\n", emptyStc->finalStr);
    free( emptyStc->finalStr );
    free( emptyStc );
    return 1;
}
**/
