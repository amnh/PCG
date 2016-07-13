#include <stdio.h>  // no I/O except in testing code, so don't need this anymore
#include <stdlib.h>
#include <string.h> 
#include "myComplexBitArrayTestC.h"

void printBits( uint64_t* input, int alphLen, int numStaticChars ) {
    printf("[\n");
    for( int charNum = 0; charNum < numStaticChars; charNum++ ) {
        for( int bitIdx = 0; bitIdx < alphLen; bitIdx++ ) {
            // printf("char num: %d\t", (alphLen * charNum + bitIdx) / WORD_WIDTH);
            // printf("loc in char: %d\t", (alphLen * charNum + bitIdx) % WORD_WIDTH);
            // printf("value: %d\n", TestBit(input, alphLen * charNum + bitIdx) );
            if( TestBit(input, alphLen * charNum + bitIdx) ) {
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
 *  A sample program that takes in two dynamic characters, and writed the result of any copmutations to a third
 *  dynamic character. The third character is allocated on Haskell side and passed in by reference.
 *  Returns 0 on correct exit, 1 on allocation failure. This was used to test the Haskell FFI.
 */
int testFn(DynChar* seqA, DynChar* seqB, AlignResult* result) {
    // get total length of output string
    int charLenA = seqA -> dynCharLen; // This is the total number of static characters in our first input 
    int charLenB = seqB -> dynCharLen; // This is the total number of static characters in our second input 

    // Because the characters are packed (meaning multiple characters will be in a single int,
    // we need the total number of ints in our array, which is 
    int buffLenA = bufferSize(seqA);   
    int buffLenB = bufferSize(seqB);
    
    /*
    printf("C: alphabet  length  %d\n", seqA->alphSize);
    printf("C: character length  %d\n", seqA->dynCharLen);
    printf("C: character pointer %p\n", seqA->dynChar);
    */

    // using calloc out of an abundance of caution. Shouldn't need it.
    // See note in .h file re: uint64_t
    uint64_t* buffer = calloc( buffLenA + buffLenB, INT_WIDTH); // in bytes. int array, so don't need \0

    // Now check that calloc() didn't fail.
    if( buffer == NULL ) {
        return 1;
    }
    
    // a simple concatenation, to test the input and output
    for(int i = 0; i < buffLenA; i++) {
        buffer[i] = seqA -> dynChar[i];
        // printf("C: character A loop, %llu\n", seqA -> dynChar[i]);
    }
    
    for(int i = 0; i < buffLenB; i++) {
        buffer[i + buffLenA] = seqB -> dynChar[i];
        // printf("C: character B loop, %llu\n", seqB -> dynChar[i]);
    }

    // now assign to struct for retrieval by Haskell FFI
    result->finalWt     = buffLenA + buffLenB; // Real cost goes here later
    result->finalLength = buffLenA + buffLenB;
    result->finalStr    = buffer;
    return 0;
}


/* The following should only needed this for testing, so they're not in the .h file. */
void makeDynamicChar( DynChar* output, int alphLen, int numStaticChars, int* values ) {
    // First create dynamic character with empty character field.
    output -> alphSize   = alphLen;
    output -> dynCharLen = numStaticChars;
    
    // Now figure out how many uint64_t's we'll need in our array.
    // Then declare and initialize a dynChar.
    int neededLen = bufferSize(output);
    output -> dynChar = calloc( neededLen, INT_WIDTH ); // in bytes. int array, so don't need \0=
    printf("\nInitial:\n");
    printBits(output -> dynChar, alphLen, numStaticChars);
    // printf("Needed: %d\n", neededLen);
    for( int charNum = 0; charNum < numStaticChars; charNum++ ) {
        printf("\nCurrent char: %d\n", charNum);
        for( int bitIdx = 0; bitIdx < alphLen; bitIdx++ ) {
            // printf("Bit value:   %d\n", 1 << bitIdx);
            // printf("Bit loc:     %d\n", charNum * alphLen + bitIdx);
            // printf("value: %d, bit loc: %d\n", values[charNum], bitIdx);
            if( values[charNum] & (1 << bitIdx) ) {
                printf("True\n");
                printf("Mask:       %d\n", (1 << bitIdx));
                printf("Bit to set: %d\n", charNum * alphLen + bitIdx);
                SetBit(output -> dynChar, charNum * alphLen + bitIdx);
                printBits(output -> dynChar, alphLen, numStaticChars);
                printf("After set:  %llu\n", output -> dynChar[0] );
            }
        }
    }
}



int main() {
    int numStaticChars = 9;
    int alphabetLen    = 5;
    int values [numStaticChars];
    for( int i = 0; i < numStaticChars; i++ ) {
        values[i] = i;
    }

    DynChar char1;
    makeDynamicChar(&char1, alphabetLen, numStaticChars, values);
    printf("\nFinal:\n");
    printBits(char1.dynChar, alphabetLen, numStaticChars);

}