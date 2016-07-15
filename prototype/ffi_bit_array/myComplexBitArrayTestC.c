#include <stdio.h> 
#include <stdlib.h>
#include <string.h> 
#include "myComplexBitArrayTestC.h"

/** 
 *  The following fn should only needed this for testing, so it's not in the .h file. 
 *
 *  Prints a representation of a dynamic character as a matrix of bits.
 */
void printBits( DynChar* input ) {
    printf("[\n");
    unsigned int alphLen = input -> alphSize;
    unsigned int numStaticChars = input -> dynCharLen;

    for( unsigned int charNum = 0; charNum < numStaticChars; charNum++ ) {
        for( unsigned int bitIdx = 0; bitIdx < alphLen; bitIdx++ ) {
            if( TestBit(input -> dynChar, alphLen * charNum + bitIdx) ) {
                //printf("Bit index:        %d\n", alphLen * charNum + bitIdx );
                printf("1,");
            } else {
                printf("0,");
            }
        }
        printf("\n");
    }
    printf("]\n");
}

/** 
 *  A sample program that takes in two dynamic characters, and wrote the result of any computations to a third
 *  dynamic character. The third character is allocated on Haskell side and passed in by reference.
 *  Returns 0 on correct exit, 1 on allocation failure. This was used to test the Haskell FFI.
 */
int exampleInterfaceFn(DynChar* seqA, DynChar* seqB, AlignResult* result) {
    // Because the characters are packed (meaning multiple characters will be in a single int,
    // we need the total number of ints in our array, which is 
    unsigned int buffLenA = bufferSize(seqA);   
    unsigned int buffLenB = bufferSize(seqB);

    // Using calloc out of an abundance of caution. Shouldn't need it, as we overwrite the entire buffer.
    // This will have to be freed in the Haskell end of things.
    // See note in .h file re: uint64_t
    uint64_t* buffer = calloc( buffLenA + buffLenB, INT_WIDTH); // in bytes. int array, so don't need \0

    // Now check that calloc() didn't fail.
    if( buffer == NULL ) {
        return 1;
    }
    
    // a simple concatenation, to test the input and output. Note that this won't be packed; we're just 
    // sticking the two arrays together.
    for(int i = 0; i < buffLenA; i++) {
        buffer[i] = seqA -> dynChar[i];
    }
    
    for(int i = 0; i < buffLenB; i++) {
        buffer[i + buffLenA] = seqB -> dynChar[i];
    }

    // now assign to struct for retrieval by Haskell FFI
    result->finalWt     = buffLenA + buffLenB; // Real cost goes here later
    result->finalLength = buffLenA + buffLenB;
    result->finalStr    = buffer;
    return 0;
}

/** 
 *  The following fn should only needed this for testing, so it's not in the .h file. 
 *
 *  Takes in a dynamic character (by reference), an alphabet length, the number of static
 *  characters the array should hold, and an array of int values that should be packed into the
 *  the character. Then mutates the passed character to match the inputs.
 */
void makeDynamicChar( DynChar* output, unsigned int alphLen, unsigned int numStaticChars, uint64_t* values ) {
    // First create dynamic character with empty character field.
    output -> alphSize   = alphLen;
    output -> dynCharLen = numStaticChars;
    
    // Now figure out how many uint64_t's we'll need in our array.
    // Then declare and initialize a dynChar.
    unsigned int neededLen = bufferSize(output);

    // need to alloc so that array isn't on the stack, and persist beyond this function call
    output -> dynChar = calloc( neededLen, INT_WIDTH ); 
    // fill array with integer values, 
    for( int charNum = 0; charNum < numStaticChars; charNum++ ) {
        for( int bitIdx = 0; bitIdx < alphLen; bitIdx++ ) {
            if( values[charNum] & (1 << bitIdx) ) {
                SetBit(output -> dynChar, charNum * alphLen + bitIdx);
            }
        }
    }
}

/**
 *  This function purely for testing, and as example usage.
 */
int main() {
    unsigned int numStaticChars = 14;
    unsigned int alphabetLen    = 5;
    uint64_t values [numStaticChars];
    for( unsigned int i = 0; i < numStaticChars; i++ ) {
        values[i] = (uint64_t) i;
    }

    DynChar char1; // alphabet: 5, chars: 14, values: 0 .. 13
    makeDynamicChar(&char1, alphabetLen, numStaticChars, values);
    printf("\nTest bit wrap to next int. Should be 14, then 5, then numbers from 0 to 14 in bits:\n");
    printf("%u\n", char1.dynCharLen);
    printf("%u\n", char1.alphSize);
    printBits(&char1);

    printf("\nTest make static character. Should print 1, then 250, then a matrix 250 wide, all set to 0 except first three:\n");
    alphabetLen = 63; // TODO: fix this so it rolls over. Right now 64 is max.
    DynChar char2; // alphabet: 63, chars: 1, value: 7
    makeStaticChar( (unsigned int) alphabetLen, (uint64_t) 7, &char2 ); // cast because input needs to be unsigned long
    printf("%u\n", char2.dynCharLen);
    printf("%u\n", char2.alphSize);
    printBits( &char2 );

    printf("\nTest accessors:\n");

    alphabetLen = 5;
    printf("\nTest get static character. Should print 1, then 5, then 13 in binary, then error out twice:\n");
    DynChar char3; // alphabet: 5, chars: 1, value: 13
    makeStaticChar( alphabetLen, (uint64_t) 0, &char3 );
    if ( ! getStaticChar((unsigned int) 13, &char1, &char3) ) {
        printf("%u\n", char3.dynCharLen);
        printf("%u\n", char3.alphSize);
        printBits( &char3 );
    } else {
        printf("Error! \n");
    }
    if ( ! getStaticChar( (unsigned int) 17, &char1, &char3) ) {
        printBits( &char3 );
    } else {
        printf("\nError!\n");
    }
    char3.alphSize = 7;
    if ( ! getStaticChar( (unsigned int) 13, &char1, &char3) ) {
        printBits( &char3 );
    } else {
        printf("\nError!\n");
    }

    printf("\nTest set static character. Should print binary ints from 0 to 13, with evens all replace by 13, then error out twice:\n");
    char3.alphSize = 5;
    for( unsigned int i = 0; i < char1.dynCharLen; i+=2 ) {
        if ( setStaticChar( i, &char3, &char1 ) ) {
            printf("Error! \n");
        }
    }
    printBits( &char1 );
    
    if ( ! setStaticChar( (unsigned int) 17, &char1, &char3) ) {
        printBits( &char3 );
    } else {
        printf("\nError!\n");
    }
    char3.alphSize = 7;
    if ( ! setStaticChar( (unsigned int) 13, &char1, &char3) ) {
        printBits( &char3 );
    } else {
        printf("\nError!\n");
    }

}