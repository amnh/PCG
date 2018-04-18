#include <stdlib.h>
#include <string.h> 
#include "mySimpleTestC.h"

char* testFN(char* seqA, char* seqB, int wt1, int wt2) {
    // have to malloc because declaring a length in the formal params 
    // was causing buffer overflows according to valgrind
    int lenA = strlen( seqA );
    int lenB = strlen( seqB );
    char* buffer = malloc(( sizeof(char*) ) * ((lenA + lenB) + 1) ); // one for NUL
    // TODO: check here for allocation. Look at asserts(?) in POY.
    // return some String error if assert fails?
    strcpy( buffer, seqA );
    return strcat( buffer, seqB );
}