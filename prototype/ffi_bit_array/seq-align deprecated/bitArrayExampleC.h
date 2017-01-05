#include <stdio.h> 
#include <stdlib.h>
#include <string.h> 
#include "dynamicCharacterOperations.h"

/** 
 *  The following fn should only needed this for testing, so it's not in the .h file. 
 *
 *  Prints a representation of a dynamic character as a matrix of bits.
 */
void printBits( dynChar_t* input ); 

/** 
 *  A sample program that takes in two dynamic characters, and writes the result of any copmutations to a third
 *  dynamic character. The third character is allocated on Haskell side and passed in by reference.
 *  Returns 0 on correct exit, 1 on allocation failure. This was used to test the Haskell FFI.
 */
int exampleInterfaceFn( dynChar_t* seqA, dynChar_t* seqB, alignResult_t* result );

/** 
 *  The following fn should only needed this for testing, so it's not in the .h file. 
 *
 *  Takes in a dynamic character (by reference), an alphabet length, the number of elements
 *  the array should hold, and an array of int values that should be packed into the
 *  the character. Then mutates the passed character to match the inputs.
 */
dynChar_t* makeDynamicChar( size_t alphLen, size_t numElems, uint64_t* values );
