#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "bitArrayExampleC.h"

/**
 *  The following fn should only needed this for testing, so it's not in the .h file.
 *
 *  Prints a representation of a dynamic character as a matrix of bits.
 */
void printCharBits( const dynChar_t* const input ) {
    printf("[\n");
    size_t alphLen = input->alphSize;

    for( size_t elemNum = 0; elemNum < input->numElems; elemNum++ ) {
        for( size_t bitIdx = 0; bitIdx < alphLen; bitIdx++ ) {
            if( TestBit(input->dynChar, alphLen * elemNum + bitIdx) ) {
                // printf("Bit index:        %lu\n", alphLen * elemNum + bitIdx );
                printf("1,");
            } else {
                printf("0,");
            }
        }
        printf("\n");
    }
    printf("]\n");
}

void printElemBits( const dcElement_t* const input ) {
    size_t alphLen = input->alphSize;

    printf("[ ");
    for( size_t bitIdx = 0; bitIdx < input->alphSize; bitIdx++ ) {
        if( TestBit(input->element, bitIdx) ) {
            //printf("Bit index:        %d\n", alphLen * elemNum + bitIdx );
            printf("1,");
        } else {
            printf("0,");
        }
    }
    printf(" ]\n");
}

void printDynChar( const dynChar_t* const input ) {
    printf("\nAlphabet Size:       %lu\n", input->alphSize);
    printf("Number of Elements:  %lu\n", input->numElems);
    printf("Internal arr Length: %lu ", input->dynCharLen);
    printf("(should need %lu.)\n", dynCharSize(input->alphSize, input->numElems) );
    printCharBits(input);
}

/**
 *  A sample program that takes in two dynamic characters, and writes the result of any computations to a third
 *  dynamic character. The third character is allocated on Haskell side and passed in by reference.
 *  Returns 0 on correct exit, 1 on allocation failure. This was used to test the Haskell FFI.
 *
 *  This is deprecated and has been replaced by exampleInterfaceFn in main.c
 */
/* int exampleInterfaceFnOld(dynChar_t* seqA, dynChar_t* seqB, alignResult_t* result) {
    // Because the characters are packed (meaning multiple characters will be in a single uint64_t),
    // we need the total number of uint64_ts in our array, which is computed in dynCharSize().
    //
    size_t buffLenA = dynCharSize(seqA);
    size_t buffLenB = dynCharSize(seqB);

    // Using calloc out of an abundance of caution. Shouldn't need it, as we overwrite the entire buffer.
    // This will have to be freed in the Haskell end of things.
    // See note in .h file re: uint64_t.
    // Remember to deallocate later.
    uint64_t* buffer = calloc( buffLenA + buffLenB, INT_WIDTH); // in bytes. int array, so don't need \0

    // Now check that calloc() didn't fail.
    if( buffer == NULL ) {
        return 1;
    }

    // a simple concatenation, to test the input and output. Note that this won't be packed; we're just
    // sticking the two arrays together.
    for(int i = 0; i < buffLenA; i++) {
        buffer[i] = seqA->dynChar[i];
    }

    for(int i = 0; i < buffLenB; i++) {
        buffer[i + buffLenA] = seqB->dynChar[i];
    }

    // now assign to struct for retrieval by Haskell FFI
    result->finalWt     = buffLenA + buffLenB; // Real cost goes here later
    result->finalLength = buffLenA + buffLenB;
    memcpy(result->finalChar1, buffer, buffLenA + buffLenB);
    free(buffer);
    return 0;
}
*/


/**
 *  This function purely for testing, and as example usage.
 */
int main() {
    size_t numElems     = 14;
    size_t alphabetSize = 5;
    uint64_t* values    = calloc(numElems, sizeof(uint64_t));
    for( size_t i = 0; i < numElems; i++ ) {
        values[i] = (uint64_t) i;
    }

    // testing makeDynamicChar

    // creating with more than one int necessary in the array
    dynChar_t* char1 = makeDynamicChar( alphabetSize, numElems, values );
    printf("\nTest bit wrap to next int. Should be 14, then 5, then numbers from 0 to 14 in bits:\n");
    printf("number of elems: %zu\n", char1->numElems);
    printf("alphabet size:   %zu\n", char1->alphSize);
    printf("length of array: %zu\n", char1->dynCharLen);
    printCharBits( char1 );


    // testing this with a large alphabet size. Still needs to be amended.
    printf("\nTest make static character. Should print 1, then 67, then a matrix 67 wide, all set to 0 except first three:\n");
    alphabetSize = 67; // TODO: fix this so it rolls over. Right now 64 is max.
    //; // alphabet: 63, chars: 1, value: 7
    dcElement_t* dcElem1;
    printf("just about to start assign values to dcElem1\n");
    dcElem1 = makeDCElement( alphabetSize, (uint64_t) 7 ); // cast because input needs to be unsigned long
    printf("%zu\n", dcElem1->alphSize);
    printElemBits( dcElem1 );

    /** You MUST free memory you're using that's not being passed back to Haskell. **/
    freeDCElem( dcElem1 );

    printf("\nTest accessors:\n");

    // first, try getting a static character from char1 and assigning it into char3
    alphabetSize = 5;
    printf("\nTest get static character. Should print 5, then 13 in binary, then error out:\n");
    dynChar_t* char3 = makeDynamicChar( 5, 1, &values[13] ); // alphabet: 5, chars: 1, value: 13

    free( values );

    dcElement_t* dcElem2 = getDCElement( (size_t) 13, char1 );
    if ( dcElem2->alphSize == 0 ) { // failure to alloc gives an alphabet size of 0
        printf("Error! \n");
    } else {
        printf("%zu\n", dcElem2->alphSize);
        printElemBits( dcElem2 );
    }

    /** already freed, so should be able to reallocate **/
    dcElem1 = getDCElement( (size_t) 17, char1 );

    // now fail because there aren't 17 elements in char1
    if ( dcElem1->alphSize == 0 ) {
        printf("\nError! Not enough elements in dynChar.\n");
    } else {
        printElemBits( dcElem1 );
    }
    freeDCElem( dcElem1 );

    /** alph size is now set inside fn **/
    // // now fail because alphabet sizes aren't the same
    // char3.alphSize = 7;
    // if ( getDCElement( (size_t) 13, &char1, dcElem2) ) {
    //     printf("\nError! Alphabet sizes don't match\n");
    // } else {
    //     printCharBits( &char3 );
    // }


    printf("\nTest setDCElement. Should print binary ints from 0 to 13, with evens all replaced by 13, then error out:\n");

    // now set the value of char3 into char1 at all even positions.
    dcElem2->element[0] = (uint64_t) 13;
    for( size_t i = 0; i < char1->numElems; i += 2 ) {
        if ( setDCElement( i, dcElem2, char1 ) ) {
            printf("Error! Not enough elements.\n");
            break;
        }
    }
    printCharBits( char1 );

    // Fail because index is beyond length of char3
    if ( setDCElement( (size_t) 17, dcElem2, char3) ) {
        printf("\nError! Not enough elements in char1.\n\n");
    } else {
        printCharBits( char3 );
    }
    // freeDCElem( dcElem2 );

    /** alph size is now set inside fn **/
    // // fail because alphabets are different lengths
    // char3.alphSize = 7;
    // if ( setDCElement( (size_t) 13, dcElem2, &char3) ) {
    //     printf("\nError! Alphabets sizes don't match.\n");
    // } else {
    //     printCharBits( &char3 );
    // }

    // now test getCostDyn
    // currently second element in char1 is with 10000, or gap
    // and the first element in char3 is with 10110
    alphabetSize = 5;
    costMtx_t* tcm = malloc( sizeof(costMtx_t) );
    tcm->subCost = 1;
    tcm->gapCost = 2;

    // next to get values back
    dcElement_t* alignElem1 = allocateDCElement( alphabetSize );
    // dcElement_t* alignElem2 = makeDCElement( alphabetSize, CANONICAL_ZERO );

    //getDCElement( alphabetSize, &char3, &char4 );
    int cost = getCostDyn( char1, 1, char3, 0, tcm, alignElem1 );
    if( cost < 0 ) {
        printf("Error: alphabet sizes don't match.\n");
    }

    // test overlap cost
    printf("Testing getCostDyn():");
    printf("\nShould print 0 and 10000.\n");
    printf("%d\n", cost);
    printElemBits( alignElem1 );

    // test sub cost
    dcElem2->element[0] = (uint64_t) 18;
    setDCElement(1, dcElem2, char1);
    cost = getCostDyn( char1, 1, char3, 0, tcm, alignElem1 );
    printf("\nShould print 1 and 11111.\n");
    printf("%d\n", cost);
    printElemBits( alignElem1 );

    // test indel cost
    dcElem2->element[0] = (uint64_t) 16;
    setDCElement(1, dcElem2, char1);
    cost = getCostDyn( char1, 1, char3, 0, tcm, alignElem1 );
    printf("\nShould print 2 and 10111.\n");
    printf("%d\n", cost);
    printElemBits( alignElem1 );


    int* toPrint = dynCharToIntArr(char1);

    printf("\n\ndynamic character to int array:");
    printf("\nInt array after conversion. \nStarting at 0 every even num is 13. Odds are their own indices:\n");
    for(size_t i = 0; i < char1->numElems; i++) {
        printf("%d, ", toPrint[i]);
    }
    printf("\n");

    dcElement_t* dcOr = dcElementOr( getDCElement(3, char1), getDCElement(2, char3) );

    printElemBits(dcOr);

    // packedChar_t pcOr = packedCharOr(getDCElement(char1, 3), getDCElement(char3, 2), alphabetSize);

    dynChar_t* char4 = makeDynamicChar( char1->alphSize, char1->numElems, values );

    printf("\n\nint array to dynamic character:");
    intArrToDynChar( char1->alphSize, char1->numElems, toPrint, char4 );
    printDynChar(char4);

    printf("\n\nint array to bit array:");
    fflush(stdout);
    free(char4->dynChar);

    char4->dynChar = intArrToBitArr( char1->alphSize, char1->numElems, toPrint );

    printDynChar(char4);

    free(toPrint);
    freeDynChar( char1 );
    freeDynChar( char3 );
    freeDynChar( char4 );
    freeDCElem( alignElem1 );
    // freeDCElem( alignElem2 );

}
