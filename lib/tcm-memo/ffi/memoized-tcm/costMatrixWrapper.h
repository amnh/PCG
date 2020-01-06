#ifndef _COST_MATRIX_WRAPPER_H
#define _COST_MATRIX_WRAPPER_H

#include <stdint.h>

#include "dynamicCharacterOperations.h"


/** Initialize a matrix (fill in all values for non-ambiguous chracter
 *  transition costs) using a TCM sent in from an outside source.
 */
costMatrix_p matrixInit(size_t alphSize, unsigned int *tcm);


/** C wrapper for cpp destructor */
void matrixDestroy(costMatrix_p untyped_ptr);


unsigned int getCostAndMedian2D( dcElement_t *elem1
                               , dcElement_t *elem2
                               , dcElement_t *retElem
                               , costMatrix_p tcm
                               );


unsigned int getCostAndMedian3D( dcElement_t *elem1
                               , dcElement_t *elem2
                               , dcElement_t *elem3
                               , dcElement_t *retElem
                               , costMatrix_p tcm
                               );


/** Following three fns are C references to cpp functions found in costMatrix.cpp */
costMatrix_p construct_CostMatrix_C(size_t alphSize, unsigned int *tcm);


void destruct_CostMatrix_C(costMatrix_p mytype);


unsigned int call_costAndMedian2D_C( costMatrix_p untyped_self
                                   , dcElement_t* first
                                   , dcElement_t* second
                                   , dcElement_t* retMedian
                                   );


unsigned int call_costAndMedian3D_C( costMatrix_p untyped_self
                                   , dcElement_t* first
                                   , dcElement_t* second
                                   , dcElement_t* third
                                   , dcElement_t* retMedian
                                   );


#endif // _COST_MATRIX_WRAPPER_H
