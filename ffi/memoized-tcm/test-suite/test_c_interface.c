#include <stdio.h>

#include "../dynamicCharacterOperations.h"
#include "../costMatrixWrapper.h"

#define ALPH_SIZE 5
#define TCM_LEN   ALPH_SIZE * ALPH_SIZE

int main() {
    unsigned int tcm[TCM_LEN];  // gcc weirdly complains this isn't used. Don't know how to suppress this error.

    //***** TCM is indel = 1, sub = 1 *****
    for (size_t i = 0; i < ALPH_SIZE; i++) {
        for (size_t j = 0; j < ALPH_SIZE; j++) {
            tcm[i * ALPH_SIZE + j] = (i == j) ? 0 : 1;
        }
    }

    unsigned int foundCost;
    costMatrix_p myMatrix  = matrixInit(ALPH_SIZE, tcm);
    dcElement_t* firstKey  = allocateDCElement( ALPH_SIZE );
    dcElement_t* secondKey = allocateDCElement( ALPH_SIZE );
    dcElement_t* thirdKey  = allocateDCElement( ALPH_SIZE );
    dcElement_t* retMedian = allocateDCElement( ALPH_SIZE );

    // for every possible value of unambiguous key value
    for (size_t key1 = 0; key1 < ALPH_SIZE; key1++) {
        SetBit(firstKey->element, key1);

        for (size_t key2 = 0; key2 < ALPH_SIZE; key2++) { // no longer assumes 0 diagonal
            SetBit(secondKey->element, key2);
            foundCost = getCostAndMedian2D(firstKey, secondKey, retMedian, myMatrix);

            for (size_t key3 = 0; key3 < ALPH_SIZE; ++key3) {
                SetBit(thirdKey->element, key3);
                foundCost = getCostAndMedian3D(firstKey, secondKey, thirdKey, retMedian, myMatrix);
                ClearBit(thirdKey->element, key3);
            }
            ClearBit(secondKey->element, key2);
        }
        ClearBit(firstKey->element, key1);
    }

    matrixDestroy(myMatrix);

    freeDCElem(firstKey );
    freeDCElem(secondKey);
    freeDCElem(thirdKey);
    freeDCElem(retMedian);
    
    free(firstKey );
    free(secondKey);
    free(thirdKey);
    free(retMedian);
}
