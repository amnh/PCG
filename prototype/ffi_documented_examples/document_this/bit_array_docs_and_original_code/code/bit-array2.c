
/* ========================================
   Bit Operations as macros
   ======================================== */

#include <stdio.h>
#include "bit-ops.h"

int main( int argc, char* argv[] )
{
    unsigned long int A[10];

    int i;                           // Index used in multiple loops
    for ( i = 0; i < 10; i++ ) {
        A[i] = 0;                    // Clear the bit array
    }

    printf("Set bit positions 100, 200 and 300\n");
    SetBit( A, 100 );               // Set 3 bits
    SetBit( A, 200 );
    SetBit( A, 300 );


    // Check if SetBit() works:

    for ( i = 0; i < 10 * INT_WIDTH; i++ ) {
        if ( TestBit(A, i) ) {
            printf("Bit %d was set !\n", i);
        }
    }

    printf("\nClear bit position 200 \n");
    ClearBit( A, 200 );

    // Check if ClearBit() works:

    for ( i = 0; i < 10 * INT_WIDTH; i++ ) {
        if ( TestBit(A, i) ) {
            printf("Bit %d was set !\n", i);
        }
    }
}
