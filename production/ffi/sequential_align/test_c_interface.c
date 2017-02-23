#include <stdio.h>

//#include "costMatrix.h"
#include "dynamicCharacterOperations.h"
#include "costMatrixWrapper.h"
// #include "seqAlignForHaskell.h"

#define TCM_LEN 25

int main() {
    int tcm [TCM_LEN] = {0,1,1,1,2, 1,0,1,1,2, 1,1,0,1,2, 1,1,1,0,2, 2,2,2,2,0};
    size_t alphabetSize = 5;

    if ( TCM_LEN != alphabetSize * alphabetSize ) {
        printf("tcm wrong size\n");
        exit(1);
    }

    int cost,
        foundCost;

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

    // retType_t *retMedChar = malloc(sizeof(retType_t));

    costMatrix_p myMatrix = matrixInit(alphabetSize, tcm);
    cost = getCost(5, 5, myMatrix, 5);

    printf("cost on 5, 5: %i\n", cost);

    cost = getCost(5, 16, myMatrix, 5);

    printf("cost on 5, 16: %i\n", cost);

    cost = getCost(4, 8, myMatrix, 5);

    printf("cost on 4, 8: %i\n", cost);


    dcElement_t* firstKey  = allocateDCElement( alphabetSize );
    dcElement_t* secondKey = allocateDCElement( alphabetSize );
    dcElement_t* retMedian = allocateDCElement( alphabetSize );

//    cost = getCostAndMedian(firstKey, secondKey, retMedian, tcm);

    // printf("median:\n");
    // printElemBits(retMedian);
    // printf("\ncost:%d\n", cost);


    packedChar median;        // just a test: alphabet size == 4, so don't need packedChar*
    median = CANONICAL_ZERO;

    for (size_t key1 = 1; key1 <= alphabetSize; key1++) { // for every possible value of key1, key2
        SetBit(firstKey->element, key1);
        SetBit(&median, key1);    // computed median just for testing.
        // printPackedChar(&median, 1, alphabetSize);

        for (size_t key2 = 1; key2 <= alphabetSize; key2++) { // no longer assumes 0 diagonal
            SetBit(secondKey->element, key2);
            cost = tcm[(key1 - 1) * alphabetSize + (key2 - 1)];
            SetBit(&median, key2);

            foundCost = getCostAndMedian(firstKey, secondKey, retMedian, tcm);

            if(median != *retMedian->element || cost != foundCost) {
                printf("key 1 set: %zu\n", key1);
                printf("key 2 set: %zu\n", key2);
                // printf("computed median:\n");
                // printPackedChar(&median, 1, alphabetSize);
                // printf("found median:\n");
                // printPackedChar(retMedian->element, 1, alphabetSize);
                printf("computed cost: %d\n", cost);
                printf("found cost:    %d\n", foundCost);
            }
            if(key2 != key1) ClearBit(&median, key2);
            ClearBit(secondKey->element, key2);

        } // key2
        ClearBit(firstKey->element, key1);
        ClearBit(&median, key1);
    }

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

