#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

#include "../costMatrix_2d.hpp"
#include "../costMatrix_3d.hpp"
#include "../dynamicCharacterOperations.h"

#define __STDC_FORMAT_MACROS

// #include "seqAlignForHaskell.h"

int main() {
    const size_t tcmLen       = 25;
    const size_t alphabetSize = 5;

    unsigned int tcm [tcmLen] = {0,1,1,1,1, 1,0,1,1,1, 1,1,0,1,1, 1,1,1,0,1, 1,1,1,1,0};
    if ( tcmLen != alphabetSize * alphabetSize ) {
        printf("tcm wrong size\n");
        exit(1);
    }

    // TODO: I can't remember why these are different lengths.
    // const size_t SEQ_A_LEN = 15;
    // packedChar seqA_main[SEQ_A_LEN];
    // for (size_t i = 0; i < SEQ_A_LEN; i++) {
    //     seqA_main[i] = 15 - i;
    // }
    // const size_t SEQ_B_LEN = 10;
    // packedChar seqB_main[SEQ_B_LEN];
    // for (size_t i = 0; i < SEQ_B_LEN; i++) {
    //     seqB_main[i] = i;
    // }

    // const size_t SEQ_C_LEN = 10;
    // packedChar seqC_main[SEQ_C_LEN];
    // for (size_t i = 0; i < SEQ_C_LEN; i++) {
    //     seqC_main[i] = (10 - i) / 2;
    // }

    CostMatrix_3d myMatrix = CostMatrix_3d(alphabetSize, tcm);

    auto firstKey  = makeDCElement( alphabetSize, 1 );
    auto secondKey = makeDCElement( alphabetSize, 1 );
    auto thirdKey  = makeDCElement( alphabetSize, 1 );
    auto retMedian = makeDCElement( alphabetSize, 1 );
    auto cost{0},
         foundCost{0};

    // just a test: alphabet size == 4, so don't need packedChar*
    auto median = CANONICAL_ZERO;

    // First, test constructor, i.e. that unambiguous characters have been inserted.
    printf("\n\n\n******* Testing constructor: insertion of unambiguous characters. ******\n");
    for (size_t key1 = 0; key1 < alphabetSize; ++key1) { // for every possible value of key1, key2
        SetBit(firstKey->element, key1);
        SetBit(&median, key1);    // computed median just for testing.
        // printPackedChar(&median, 1, alphabetSize);

        for (size_t key2 = 0; key2 < alphabetSize; ++key2) { // no longer assumes 0 diagonal
            SetBit(secondKey->element, key2);
            SetBit(&median, key2);

            for (size_t key3 = 0; key3 < alphabetSize; ++key3) { // no longer assumes 0 diagonal
                SetBit(thirdKey->element, key3);
                SetBit(&median, key3);

                foundCost = myMatrix.costAndMedian3D(firstKey, secondKey, thirdKey, retMedian);
                fflush(stdout);
                printf("key 1 set bit: %zu\n", key1);
                printf("key 2 set bit: %zu\n", key2);
                printf("key 3 set bit: %zu\n", key3);
                printf("computed median:\n");
                printPackedChar(&median, 1, alphabetSize);
                printf("found median:\n");
                printPackedChar(retMedian->element, 1, alphabetSize);
                printf("found cost:    %d\n\n", foundCost);

                if(key3 != key1) ClearBit(&median, key3); // the key1 bit needs to persist on the median
            }
            if(key2 != key1) ClearBit(&median, key2); // the key1 bit needs to persist on the median
            ClearBit(secondKey->element, key2);
        } // key2
        ClearBit(firstKey->element, key1);
        ClearBit(&median, key1);
    }
    printf("Passed!\n\n\n");

    // printf("\n\n\n******* Testing ambiguous characters: get/set of ambiguous characters. ******\n");
    // size_t numSetInKey;
    // for (size_t i = 0; i < 25; ++i) {
    //     printf("\n\niteration %2zu\n", i + 1);
    //     numSetInKey = rand() % alphabetSize + 1;
    //     for(size_t setIdx = 0; setIdx < numSetInKey; ++setIdx) {
    //         SetBit(firstKey->element, rand() % alphabetSize);
    //     }
    //     numSetInKey = rand() % alphabetSize + 1;
    //     for(size_t setIdx = 0; setIdx < numSetInKey; ++setIdx) {
    //         SetBit(secondKey->element, rand() % alphabetSize);
    //     }
    //     numSetInKey = rand() % alphabetSize + 1;
    //     for(size_t setIdx = 0; setIdx < numSetInKey; ++setIdx) {
    //         SetBit(thirdKey->element, rand() % alphabetSize);
    //     }
    //     printf( "key1: %2" PRIu64 ", key2: %2" PRIu64 ", key3: %2" PRIu64 "\n"
    //           , *firstKey->element
    //           , *secondKey->element
    //           , *thirdKey->element
    //           );
    //     foundCost = myMatrix.getSetCostMedian(firstKey, secondKey, thirdKey, retMedian);
    //     printf("***Final cost: %i median: %" PRIu64 "\n", foundCost, *retMedian->element);
    //     printPackedChar(retMedian->element, 1, alphabetSize);
    //     ClearAll( firstKey->element,  dynCharSize(alphabetSize, 1) );
    //     ClearAll( secondKey->element, dynCharSize(alphabetSize, 1) );
    //     ClearAll( thirdKey->element,  dynCharSize(alphabetSize, 1) );
    // }

    //Free objects the test suite creates to help with Valgrin's memory leak diagnostics.
    freeDCElem( firstKey  );
    freeDCElem( secondKey );
    freeDCElem( thirdKey  );
    freeDCElem( retMedian );
    free( firstKey  );
    free( secondKey );
    free( thirdKey  );
    free( retMedian );


    // TODO: Right now this isn't doing anything. It only works for 2d.
    // For 3d something more fancier will need to be done.
    /*
    auto first   = new packedChar{1},
         second  = new packedChar{4},
         third   = new packedChar{16},
         result  = packedCharOr( first, second, alphabetSize, 1 ),
         result2 = packedCharOr( result, third, alphabetSize, 1 );


    printf("%" PRIu64 "\n", *result);
    //free(result);
    printf("%" PRIu64 "\n", *result2);
    */
    /****** This next to test Yu Xiang's code, once you can. ******/

    // int success = aligner(seqA_main, SEQ_A_LEN, seqB_main, SEQ_B_LEN, alphabetSize, getCostMatrix(myMatrix), &retMedChar);

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
