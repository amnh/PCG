#ifndef _COST_MATRIX_WRAPPER_3D_H
#define _COST_MATRIX_WRAPPER_3D_H

#include <stdint.h>

#include "dynamicCharacterOperations.h"


/** Initialize a matrix (fill in all values for non-ambiguous chracter
 *  transition costs) using a TCM sent in from an outside source.
 */
costMatrix_p matrixInit_3d(size_t alphSize, int *tcm);


/** C wrapper for cpp destructor */
void matrixDestroy_3d(costMatrix_p untyped_ptr);


// TODO: figure out how to eliminate at least one of the next three.
/** C wrapper for getting and setting values in a memoized cost matrix.
 *  Receives two ints and get a cost back.
 *  Contrast with getCostAndMedian, which also returns a pointer to a
 *  median value.
 */
int getCost_3d(packedChar elem1, packedChar elem2, packedChar elem3, costMatrix_p tcm, size_t alphSize);


/** used by getCost. Allocates. */
int getCostInternal_3d( packedChar elem1
                      , packedChar elem2
                      , packedChar elem3
                      , costMatrix_p tcm
                      , size_t alphSize
                      , dcElement_t *retElem
                      );


/** Like getCost, but also returns a pointer to a median value. */
int getCostAndMedian_3d( dcElement_t *elem1
                       , dcElement_t *elem2
                       , dcElement_t *elem3
                       , dcElement_t *retElem
                       , costMatrix_p tcm
                       );


/** Following three fns are C references to cpp functions found in costMatrix.cpp */
costMatrix_p construct_CostMatrix_3d_C(size_t alphSize, int *tcm);


void destruct_CostMatrix_3d_C(costMatrix_p mytype);


int call_getSetCost_3d_C( costMatrix_p untyped_self
                        , dcElement_t* first
                        , dcElement_t* second
                        , dcElement_t* third
                        , dcElement_t* retMedian
                        );


#endif // _COST_MATRIX_WRAPPER_3D_H
