
#include "bit-manipulation.h"


void  SetBit( int A[ ],  int k )
{
   int i = k/32;
   int pos = k%32;

   unsigned int flag = 1;  // flag = 0000.....00001

   flag = flag << pos;     // flag = 0000...010...000   (shifted k positions)

   A[i] = A[i] | flag;     // Set the bit at the k-th position in A[i]
}

void  ClearBit( int A[ ],  int k )
{
   int i = k/32;
   int pos = k%32;

   unsigned int flag = 1;  // flag = 0000.....00001

   flag = flag << pos;     // flag = 0000...010...000   (shifted k positions)
   flag = ~flag;           // flag = 1111...101..111

   A[i] = A[i] & flag;     // RESET the bit at the k-th position in A[i]
}

int TestBit( int A[ ],  int k )
{
   int i = k/32;
   int pos = k%32;

   unsigned int flag = 1;  // flag = 0000.....00001

   flag = flag << pos;     // flag = 0000...010...000   (shifted k positions)

   if ( A[i] & flag )      // Test the bit at the k-th position in A[i]
      return 1;
   else
      return 0;
}

