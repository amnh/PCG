#include <stdio.h>
#include <stdlib.h>
#include <string.h> 
#include "myComplexBitArrayTestC.h"


// takes in two strings, and returns a struct. The string in the struct is wt1 * the first string +
// wt2 * the second string. The int in the struct is wt1 * wt2.
// returns 0 on correct exit, 1 on allocation failure
int testFn(struct dynChar_t* seqA, struct dynChar_t* seqB, struct alignResult_t* result) {
    // get total length of output string
    int lenA = seqA -> dynCharLen;
    int lenB = seqB -> dynCharLen;
    printf("%d\n", seqA->alphSize);
    printf("%d\n", seqA->dynCharLen);
    printf("%p\n", seqA->dynChar);
    

    // have to malloc because declaring a length in the formal params 
    // was causing buffer overflows according to valgrind
    // Unless malloc() fails, eventual freeing of memory must be done in Haskell FFI code.
    uint64_t* buffer = calloc( lenA + lenB + 1, sizeof(uint64_t) * seqA->alphSize); // in bytes, extra 1 for NUL

    // Now check that malloc() didn't fail.
    if( buffer == NULL ) {
        return 1;
    }
    
    for(int i = 0; i < lenA; i++) {
        buffer[i] = seqA -> dynChar[i];
        printf("%llu\n", seqA -> dynChar[i]);
    }
    for(int i = lenA; i < lenA + lenB; i++) {
        buffer[i] = seqB -> dynChar[(i - lenA)];
    }
    result->finalWt = lenA + lenB;
    result->finalLength = lenA + lenB + 1;
    result->finalStr = buffer;
    return 0;
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