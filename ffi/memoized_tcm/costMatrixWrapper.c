#include <execinfo.h> // For 'backtrace' and friends
#include <stdint.h>
#include <stdio.h>

#include "costMatrixWrapper.h"
#include "dynamicCharacterOperations.h"

costMatrix_p matrixInit(size_t alphSize, unsigned int *tcm)
{
   return (costMatrix_p) construct_CostMatrix_C(alphSize, tcm);
}


void matrixDestroy(costMatrix_p untyped_ptr)
{
    destruct_CostMatrix_C(untyped_ptr);
}


unsigned int getCostAndMedian2D( dcElement_t *elem1
                               , dcElement_t *elem2
                               , dcElement_t *retElem
                               , costMatrix_p tcm
                               )
{
    // TODO: Pretty sure we *DON'T* need to create new pointers, because of copying into cost matrix.
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

    int cost = call_costAndMedian2D_C(tcm, elem1copy, elem2copy, retElem);

    freeDCElem(elem1copy);
    freeDCElem(elem2copy);
    free(elem1copy);
    free(elem2copy);

    return cost;
}

unsigned int getCostAndMedian3D( dcElement_t *elem1
                               , dcElement_t *elem2
                               , dcElement_t *elem3
                               , dcElement_t *retElem
                               , costMatrix_p tcm
                               )
{
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
    dcElement_t *elem3copy = malloc(sizeof(dcElement_t));
    elem3copy->alphSize    = alphSize;
    //printf("We alloced!\n"), fflush(stdout);

    elem1copy->element = makePackedCharCopy( elem1->element, alphSize, 1 );
    elem2copy->element = makePackedCharCopy( elem2->element, alphSize, 1 );
    elem3copy->element = makePackedCharCopy( elem3->element, alphSize, 1 );
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

    int cost = call_costAndMedian3D_C(tcm, elem1copy, elem2copy, elem3copy, retElem);

    freeDCElem(elem1copy);
    freeDCElem(elem2copy);
    freeDCElem(elem3copy);
    free(elem1copy);
    free(elem2copy);
    free(elem3copy);

    return cost;
}
