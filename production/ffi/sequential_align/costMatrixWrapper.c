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
    // TODO: valgrind this.
    if (elem1 == 0 || elem2 == 0) {
      return 1337;
    }

    packedChar *packedElemRet = malloc(sizeof(packedChar));
    packedChar *packedElem1   = malloc(sizeof(packedChar));
    packedChar *packedElem2   = malloc(sizeof(packedChar));

    *packedElemRet = CANONICAL_ZERO;
    *packedElem1   = elem1;    // should be okay, because elem1 and elem2 are just ints, so pass by copy
    *packedElem2   = elem2;

    // printf("** %llu\n", elem1);
    // printPackedChar(packedElem1, 1, alphSize);
    // printf("** %llu\n", elem2);
    // printPackedChar(packedElem2, 1, alphSize);


    /**
    dcElement_t retElem = { alphSize, packedElemRet };
    dcElement_t dcElem1 = { alphSize, packedElem1   };
    dcElement_t dcElem2 = { alphSize, packedElem2   };
    **/

    dcElement_t *retElem = malloc(sizeof(dcElement_t));
    dcElement_t *dcElem1 = malloc(sizeof(dcElement_t));
    dcElement_t *dcElem2 = malloc(sizeof(dcElement_t));

    retElem->alphSize = alphSize;
    dcElem1->alphSize = alphSize;
    dcElem2->alphSize = alphSize;

    retElem->element = packedElemRet;
    dcElem1->element = packedElem1;
    dcElem2->element = packedElem2;

    /*
    printf("Before 'getSetCost' call\n");
    printf("Elem 1: %lu\n", elem1);
    printf("Elem 2: %lu\n", elem2);
    fflush(stdout);
    */
    
    int cost = call_getSetCost_C(tcm, dcElem1, dcElem2, retElem);

    /*
    printf("After  'getSetCost' call\n");
    printf("Entering Yu Xiang's context:\n");
    void* callstack[128];
    int i, frames = backtrace(callstack, 128);
    char** strs = backtrace_symbols(callstack, frames);
    for (i = 0; i < frames; ++i) {
      printf("%s\n", strs[i]);
    }
    free(strs);
    fflush(stdout);
    */

    free(packedElemRet);

    return cost;
}


int getCostAndMedian(dcElement_t *elem1, dcElement_t *elem2, dcElement_t *retElem, costMatrix_p tcm) {
    // Need to create new pointers, because of copying into cost matrix.
    // TODO: valgrind this.
    size_t alphSize = elem1->alphSize;
    dcElement_t *elem1copy = allocateDCElement( alphSize );
    dcElement_t *elem2copy = allocateDCElement( alphSize );
//    dcElement_t *elem3copy = allocateDCElement( alphSize );

    copyPackedChar( elem1->element, elem1copy->element, alphSize);
    copyPackedChar( elem2->element, elem2copy->element, alphSize);

    printf("Here we go:\n");
    printPackedChar(elem1copy->element, 1, alphSize);
    printf("Go again:\n");
    printPackedChar(elem2copy->element, 1, alphSize);

    printf("Inputs:\n");
    printPackedChar(  elem1->element, 1, alphSize);
    printPackedChar(  elem2->element, 1, alphSize);
    printf("Output buffer:\n");
    printPackedChar(retElem->element, 1, alphSize);

    printf("Get dat cost:\n");
    int cost = call_getSetCost_C(tcm, elem1copy, elem2copy, retElem);

    printf("success!?");

    //TODO: return a success value
    int success = 0;

    return cost;
}

// costMatrix_p getCostMatrix(costMatrix_p untyped_self) {
//     return (costMatrix_p) untyped_self;
// }
