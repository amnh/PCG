
const int INT_WIDTH = 32;
#define SetBit(arr,k)     ( arr[ k / INT_WIDTH ] |=  (1 << (k % INT_WIDTH)) )
#define ClearBit(arr,k)   ( arr[ k / INT_WIDTH ] &= ~(1 << (k % INT_WIDTH)) )
#define TestBit(arr,k)    ( arr[ k / INT_WIDTH ] &   (1 << (k % INT_WIDTH)) )
