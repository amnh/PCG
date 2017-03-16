#include <stdlib.h>
#include <stdio.h>
#include "mySimpleBitArrayTestC.h"
#include "bit-ops.h"
#include <stdint.h> // So we can use 64-bit ints no matter the architecture

int testFN(uint64_t* seqA, int arrLen, int alphLen) {
    // have to malloc because declaring a length in the formal params 
    // was causing buffer overflows according to valgrind
    for( int i = 0; i < arrLen * alphLen; i += alphLen) {
        SetBit(seqA, i);
    }
    
    return 0;
}

// int main() {
//     int i;    // for use in loops, below
//     const int ARR_LEN = 2;
//     unsigned long int seqA[ARR_LEN];

//     for ( i = 0; i < ARR_LEN; i++ ) {
//         seqA[i] = 0;
//         SetBit(seqA, i / ARR_LEN);
//     }
//     testFN(seqA, ARR_LEN, 8);

//     for ( i = 0; i < ARR_LEN * INT_WIDTH; i++ ) {
//         if ( TestBit(seqA, i) ) {
//             printf("Bit %d was set !\n", i);
//         }
//     }
// }