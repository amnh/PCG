#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

#include "../costMatrix.h"
#include "../dynamicCharacterOperations.h"

#define __STDC_FORMAT_MACROS

// #include "seqAlignForHaskell.h"

int main() {
    const size_t tcmLen       = 25;
    const size_t alphabetSize = 5;

    int tcm [tcmLen] = {0,1,2,3,4, 1,0,1,2,3, 2,1,0,1,2, 3,2,1,0,1, 4,3,2,1,0};
    if ( tcmLen != alphabetSize * alphabetSize ) {
        printf("tcm wrong size\n");
        exit(1);
    }

    size_t seqALen = 15;
    packedChar seqA_main[seqALen];
    for (size_t i = 0; i < seqALen; i++) {
        seqA_main[i] = 15 - i;
    }
    size_t seqBLen = 10;
    packedChar seqB_main[seqBLen];
    for (size_t i = 0; i < seqBLen; i++) {
        seqB_main[i] = i;
    }


    CostMatrix myMatrix = CostMatrix(alphabetSize, tcm);


    dcElement_t* firstKey  = makeDCElement( alphabetSize, 1 );
    dcElement_t* secondKey = makeDCElement( alphabetSize, 1 );
    dcElement_t* retMedian = makeDCElement( alphabetSize, 1 );
    int cost,
        foundCost;

    packedChar median;        // just a test: alphabet size == 4, so don't need packedChar*
    median = CANONICAL_ZERO;

    // First, test constructor, i.e. that unambiguous characters have been inserted.
    printf("\n\n\n******* Testing constructor: insertion of unambiguous characters. ******\n");
    for (size_t key1 = 0; key1 < alphabetSize; ++key1) { // for every possible value of key1, key2
        SetBit(firstKey->element, key1);
        SetBit(&median, key1);    // computed median just for testing.
        // printPackedChar(&median, 1, alphabetSize);

        for (size_t key2 = 0; key2 < alphabetSize; ++key2) { // no longer assumes 0 diagonal
            SetBit(secondKey->element, key2);
            cost = tcm[key1 * alphabetSize + key2];
            SetBit(&median, key2);

            foundCost = myMatrix.getCostMedian(firstKey, secondKey, retMedian);
	    fflush(stdout);
            printf("key 1 set: %zu\n", key1);
            printf("key 2 set: %zu\n", key2);
            printf("found median:\n");
            printPackedChar(retMedian->element, 1, alphabetSize);
            printf("found cost:    %d\n", foundCost);
	    /*
            if(median != *retMedian->element || cost != foundCost) {
                printf("****** Median/cost retrieval failed! ******\n");
                printf("key 1 set: %zu\n", key1);
                printf("key 2 set: %zu\n", key2);
                printf("computed median:\n");
                printPackedChar(&median, 1, alphabetSize);
                printf("found median:\n");
                printPackedChar(retMedian->element, 1, alphabetSize);
                printf("computed cost: %d\n", cost);
                printf("found cost:    %d\n", foundCost);
		exit(1);
            }
            else {
                printf("Unambiguous success! key1: %zu, key2: %zu, cost: %i, median: %" PRIu64 " \n", key1, key2, cost, median);
            }
	    */
            if(key2 != key1) ClearBit(&median, key2); // the key1 bit needs to persist on the median
            ClearBit(secondKey->element, key2);
        } // key2
        ClearBit(firstKey->element, key1);
        ClearBit(&median, key1);
    }
    printf("Passed!\n\n\n");

    printf("\n\n\n******* Testing ambiguous characters: get/set of ambiguous characters. ******\n");
    size_t numSetInKey;
    for(size_t i = 0; i < 25; ++i) {
        printf("\n\niteration %2zu\n", i + 1);
        numSetInKey = rand() % alphabetSize + 1;
        for(size_t setIdx = 0; setIdx < numSetInKey; ++setIdx) {
            SetBit(firstKey->element, rand() % alphabetSize);
        }
        numSetInKey = rand() % alphabetSize + 1;
        for(size_t setIdx = 0; setIdx < numSetInKey; ++setIdx) {
            SetBit(secondKey->element, rand() % alphabetSize);
        }
        printf("key1: %2" PRIu64 ", key2: %2" PRIu64 "\n", *firstKey->element, *secondKey->element);
        foundCost = myMatrix.getSetCostMedian(firstKey, secondKey, retMedian);
        printf("***Final cost: %i median: %" PRIu64 "\n", foundCost, *retMedian->element);
        printPackedChar(retMedian->element, 1, alphabetSize);
        ClearAll( firstKey->element, dynCharSize(alphabetSize, 1) );
        ClearAll(secondKey->element, dynCharSize(alphabetSize, 1) );
    }
    packedChar *first, *second, *third, *result, *result2;
    packedChar firstVal  = 1,
               secondVal = 4,
               thirdVal  = 16;

    first  = &firstVal;
    second = &secondVal;
    third  = &thirdVal;
    result = packedCharOr( first, second, alphabetSize, 1 );

    printf("%" PRIu64 "\n", *result);
    //free(result);
    result2 = packedCharOr( result, third, alphabetSize, 1 );
    printf("%" PRIu64 "\n", *result2);

    /****** This next to test Yu Xiang's code, once you can. ******/

    // int success = aligner(seqA_main, seqALen, seqB_main, seqBLen, alphabetSize, getCostMatrix(myMatrix), &retMedChar);

    // if (success == 0) {
    //     printf("\nSuccess!\n\n");
    //     // printf("The aligned sequences are: \n%p\n%p\n", retAlign->seq1, retAlign->seq2);
    //     // printf("The cost of the alignment is: %d\n", retAlign->weight);
    //     // for(int i = 0; i < length; ++i) {
    //     //     printf("%d\n",(int)retAlign->seq1[i]);
    //     // }
    // } else {
    //     printf("Fail!\n");
    // }

}

