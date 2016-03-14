#include <stdio.h>
#include <stdlib.h>
#include <string.h> 
#include "myComplexTestC.h"


// takes in two strings, and returns a struct. The string in the struct is wt1 * the first string +
// wt2 * the second string. The int in the struct is wt1 * wt2.
// returns 0 on correct exit, 1 on allocation failure
int testFn(char* seqA, char* seqB, int wt1, int wt2, struct align* result) {
    // get total length of output string
    int lenA = strlen( seqA ) * wt1;
    int lenB = strlen( seqB ) * wt2;
    

    // have to malloc because declaring a length in the formal params 
    // was causing buffer overflows according to valgrind
    // Unless malloc() fails, eventual freeing of memory must be done in Haskell FFI code.
    char* buffer = malloc(( sizeof(buffer) ) * (lenA + lenB + 1) ); // extra 1 for NUL

    // Now check that neither malloc() failed.
    if( buffer == NULL ) {
        //toReturn->finalWt = -1;    // So I can check for valid weight first.
        //toReturn->finalStr = NULL; 
        return 1;
    }
    
    for(int i = 0; i < wt1; i++) {
        strcat( buffer, seqA );
    }
    for(int i = 0; i < wt2; i++) {
        strcat( buffer, seqB );
    }
    result->finalWt = wt1 * wt2;
    result->finalStr = buffer;
    return 0;
}

/**
int main() {
    char* buffer; 
    struct align* emptyStc = malloc(sizeof(char*) + sizeof(int));
    printf("%lu\n", sizeof(buffer));
    emptyStc->finalWt = 1;
    emptyStc->finalStr = buffer;
    printf("%d\n", testFn("a", "b", 1, 0, emptyStc));
    printf("%s\n", emptyStc->finalStr);
    free( emptyStc->finalStr );
    free( emptyStc );
    return 1;
}
/**/