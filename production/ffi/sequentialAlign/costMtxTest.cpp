#include "costMatrix.h"

int main() {
    int tcm [16] = {0,1,1,2,1,0,1,2,1,1,0,2,2,2,2,0};
    size_t alphabetSize = 4;

    CostMatrix myMatrix(alphabetSize, tcm);

    dcElement_t* firstKey  = makeDCElement( alphabetSize, 1 );
    dcElement_t* secondKey = makeDCElement( alphabetSize, 1 );
    dcElement_t* retMedian = makeDCElement( alphabetSize, 1 );
    int cost,
        foundCost;

    packedChar median;        // just a test: alphabet size == 4, so don't need packedChar*
    median = CANONICAL_ZERO;

    for (size_t key1 = 0; key1 < alphabetSize; key1++) { // for every possible value of key1, key2
        SetBit(firstKey->element, key1);
        SetBit(&median, key1);    // computed median just for testing.
        // printPackedChar(&median, 1, alphabetSize);

        for (size_t key2 = 0; key2 < alphabetSize; key2++) { // no longer assumes 0 diagonal
            SetBit(secondKey->element, key2);
            cost = tcm[key1 * alphabetSize + key2];
            SetBit(&median, key2);

            foundCost = myMatrix.getSetCost(firstKey, secondKey, retMedian);

            if(median != *retMedian->element || cost != foundCost) {
                printf("key 1 set: %zu\n", key1);
                printf("key 2 set: %zu\n", key2);
                printf("computed median:\n");
                printPackedChar(&median, 1, alphabetSize);
                printf("found median:\n");
                printPackedChar(retMedian->element, 1, alphabetSize);
                printf("computed cost: %d\n", cost);
                printf("found cost:    %d\n", foundCost);
            }
            if(key2 != key1) ClearBit(&median, key2);
            ClearBit(secondKey->element, key2);

        } // key2
        ClearBit(firstKey->element, key1);
        ClearBit(&median, key1);
    }

}

