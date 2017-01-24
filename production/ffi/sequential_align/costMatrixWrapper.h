#ifndef _COST_MATRIX_WRAPPER_H
#define _COST_MATRIX_WRAPPER_H

#include <stdint.h>

#include "dynamicCharacterOperations.h"

costMatrix_p matrixInit(size_t alphSize, int *tcm);
void matrixDestroy(costMatrix_p mytype);
int lookUpCost(costMatrix_p untyped_self, dcElement_t *left, dcElement_t *right, dcElement_t *retMedian);
int getCost(packedChar elem1, packedChar elem2, costMatrix_p tcm, size_t alphSize);


costMatrix_p construct_CostMatrix_C(size_t alphSize, int *tcm);
void destruct_CostMatrix_C(costMatrix_p mytype);
int call_getSetCost_C(costMatrix_p untyped_self, dcElement_t *left, dcElement_t *right, dcElement_t *retMedian);

#endif // _COST_MATRIX_WRAPPER_H