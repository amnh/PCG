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
    return call_costAndMedian2D_C(tcm, elem1, elem2, retElem);
}

unsigned int getCostAndMedian3D( dcElement_t *elem1
                               , dcElement_t *elem2
                               , dcElement_t *elem3
                               , dcElement_t *retElem
                               , costMatrix_p tcm
                               )
{
    return call_costAndMedian3D_C(tcm, elem1, elem2, elem3, retElem);
}
