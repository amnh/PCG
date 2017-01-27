#include <stdint.h>

#include "costMatrixWrapper.h"
#include "dynamicCharacterOperations.h"

costMatrix_p matrixInit(size_t alphSize, int *tcm) {
   return (costMatrix_p) construct_CostMatrix_C(alphSize, tcm);
}

void matrixDestroy(costMatrix_p untyped_ptr) {
    destruct_CostMatrix_C(untyped_ptr);
}



int getCost(packedChar elem1, packedChar elem2, costMatrix_p tcm, size_t alphSize){
    // Need to create new pointers, because of copying into cost matrix.
    // TODO: valgrind this.
    packedChar *packedElemRet = (packedChar*) malloc(sizeof(packedChar));
    packedChar *packedElem1   = (packedChar*) malloc(sizeof(packedChar));
    packedChar *packedElem2   = (packedChar*) malloc(sizeof(packedChar));

    *packedElemRet = CANONICAL_ZERO;
    *packedElem1   = elem1;
    *packedElem2   = elem2;

    printPackedChar(packedElem1, 1, alphSize);
    printPackedChar(packedElem2, 1, alphSize);

    dcElement_t retElem = { alphSize, packedElemRet };
    dcElement_t dcElem1 = { alphSize, packedElem1 };
    dcElement_t dcElem2 = { alphSize, packedElem2 };

    int cost = call_getSetCost_C(tcm, &dcElem1, &dcElem2, &retElem);

    free(packedElemRet);

    return cost;
}

// costMatrix_p getCostMatrix(costMatrix_p untyped_self) {
//     return (costMatrix_p) untyped_self;
// }