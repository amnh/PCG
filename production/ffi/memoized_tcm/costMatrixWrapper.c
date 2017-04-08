#include <execinfo.h> // For 'backtrace' and friends
#include <stdint.h>
#include <stdio.h>

#include "costMatrixWrapper.h"
#include "dynamicCharacterOperations.h"

costMatrix_p matrixInit(size_t alphSize, int *tcm) {
   return (costMatrix_p) construct_CostMatrix_C(alphSize, tcm);
}

void matrixDestroy(costMatrix_p untyped_ptr) {
    destruct_CostMatrix_C(untyped_ptr);
}



int getCost(packedChar elem1, packedChar elem2, costMatrix_p tcm, size_t alphSize) {
    // Need to create new pointers, because of copying into cost matrix.
    if (elem1 == 0 || elem2 == 0) {
      // printf("Gave me a zero, kuddos to you brave soul!\n");
      // printf("Element 1: %lu\n", elem1);
      // printf("Element 2: %lu\n", elem2);
      return 1337;
    }


    packedChar *packedElemRet = malloc(sizeof(packedChar));
    dcElement_t *retElem      = malloc(sizeof(dcElement_t));

    *packedElemRet    = CANONICAL_ZERO;
    retElem->alphSize = alphSize;
    retElem->element  = packedElemRet;

    int cost = getCostInternal(elem1, elem2, tcm, alphSize, retElem);

    free(packedElemRet);

    return cost;
}

int getCostInternal(packedChar elem1, packedChar elem2, costMatrix_p tcm, size_t alphSize, dcElement_t *retElem) {
    // Need to create new pointers, because of copying into cost matrix.

    packedChar *packedElem1 = malloc(sizeof(packedChar));
    packedChar *packedElem2 = malloc(sizeof(packedChar));

    *packedElem1 = elem1;    // should be okay, because elem1 and elem2 are just ints, so pass by copy
    *packedElem2 = elem2;

    dcElement_t *dcElem1 = malloc(sizeof(dcElement_t));
    dcElement_t *dcElem2 = malloc(sizeof(dcElement_t));

    dcElem1->alphSize = alphSize;
    dcElem2->alphSize = alphSize;

    dcElem1->element = packedElem1;
    dcElem2->element = packedElem2;

    int cost = call_getSetCost_C(tcm, dcElem1, dcElem2, retElem);

    return cost;
}


int getCostAndMedian(dcElement_t *elem1, dcElement_t *elem2, dcElement_t *retElem, costMatrix_p tcm) {
    // Need to create new pointers, because of copying into cost matrix.
    // TODO: valgrind this.
    //printf("We made it to C LAND!!!!\n"), fflush(stdout);
    //printf("%p\n", elem1), fflush(stdout);
    size_t alphSize = elem1->alphSize;
    //printf("alphSize %d\n",alphSize), fflush(stdout);
    //printf("[%d]\n",elem1->element[0]), fflush(stdout);
    
    // dcElement_t *elem1copy = allocateDCElement( alphSize );
    // dcElement_t *elem2copy = allocateDCElement( alphSize );
    // Can't use allocateDCElement because makePackedCharCopy allocates
    dcElement_t *elem1copy = malloc(sizeof(dcElement_t));
    elem1copy->alphSize    = alphSize;
    dcElement_t *elem2copy = malloc(sizeof(dcElement_t));
    elem2copy->alphSize    = alphSize;
    //printf("We alloced!\n"), fflush(stdout);

    elem1copy->element = makePackedCharCopy( elem1->element, alphSize, 1 );
    elem2copy->element = makePackedCharCopy( elem2->element, alphSize, 1 );
    //printf("We copied!\n"), fflush(stdout);

    //// printf("Here we go:\n");
    // printPackedChar(elem1copy->element, 1, alphSize);
    //printf("Go again:\n");
    // printPackedChar(elem2copy->element, 1, alphSize);

    // printf("Inputs:\n");
    // printPackedChar(  elem1->element, 1, alphSize);
    // printPackedChar(  elem2->element, 1, alphSize);
    // printf("Output buffer:\n");
    // printPackedChar(retElem->element, 1, alphSize);

    int cost = call_getSetCost_C(tcm, elem1copy, elem2copy, retElem);

    return cost;
}

// costMatrix_p getCostMatrix(costMatrix_p untyped_self) {
//     return (costMatrix_p) untyped_self;
// }
