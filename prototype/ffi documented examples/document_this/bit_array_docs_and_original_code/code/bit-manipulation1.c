
#include "bit-manipulation.h"


void  SetBit( int A[ ],  int k )
{
   A[k / 32] |= 1 << (k % 32);      // Set the bit at the k-th position
}

void  ClearBit( int A[ ],  int k )
{
   A[k / 32] &= ~(1 << (k %3 2)) ;  // RESET the bit at the k-th position
}

int TestBit( int A[ ],  int k ) 
{
   return ( (A[k / 32] & (1 << (k % 32) )) != 0 ) ;  // Return TRUE if bit set
}

