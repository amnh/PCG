

#include <stdio.h>
#include "bit-manipulation.h"

int main( int argc, char* argv[] )
{
   int A[10];
   int i;

   for ( i = 0; i < 10; i++ )
      A[i] = 0;                    // Clear the bit array

   printf("Set bit positions 100, 200 and 300\n");
   SetBit( A, 100 );               // Set 3 bits
   SetBit( A, 200 );
   SetBit( A, 300 );


   // Check if SetBit() works:

   for ( i = 0; i < 320; i++ )
      if ( TestBit(A, i) )
         printf("Bit %d was set !\n", i);

   printf("\nClear bit positions 200 \n");
   ClearBit( A, 200 );

   // Check if ClearBit() works:

   for ( i = 0; i < 320; i++ )
      if ( TestBit(A, i) )
         printf("Bit %d was set !\n", i);
}
