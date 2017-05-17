#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#ifndef C_CODE_ALLOC_SETUP_H
#define C_CODE_ALLOC_SETUP_H

#include "alignCharacters.h"
#include "debug_constants.h"
#include "costMatrix.h"
#include "alignmentMatrices.h"
//#include "ukkCheckp.h"
//#include "ukkCommon.h"


/** Allocate nw_matrices struct. Assigns initial values where necessary. Calls
 *  mat_setup_size to allocate all internal arrays.
 *
 *  Order of character lengths doesn't matter
 */
void initializeAlignmentMtx( alignment_matrices_t *retMtx
                           , size_t                cap_char1
                           , size_t                cap_char2
                           , size_t                cap_char3
                           , size_t                alphSize
                           );

/** Does internal allocation for a character struct. Also sets character pointers within array to correct positions.
 *
 *  resChar must be alloced before this call. This is because allocation must be done on other side of FFI for pass
 *  by ref to be correct.
 */
void initializeChar(dyn_char_p retChar, size_t allocSize);

/** Resets character array to all 0s.
 *  Makes length 0.
 *  Points beginning of character to end of character array.
 */
void resetCharValues(dyn_char_p retChar);

/** Find distance between an unambiguous nucleotide and an ambiguous ambElem. Return that value and the median.
 *  @param ambElem is ambiguous input.
 *  @param nucleotide is unambiguous.
 *  @param median is used to return the calculated median value.
 *
 *  This fn is necessary because there isn't yet a cost matrix set up, so it's not possible to
 *  look up ambElems, therefore we must loop over possible values of the ambElem
 *  and find the lowest cost median.
 *
 *  Requires symmetric, if not metric, matrix.
 */
int distance( unsigned int const *tcm
            , size_t              alphSize
            , elem_t              nucleotide
            , elem_t              ambElem
            );

/** Take in a cost_matrices_2d, the struct for which has already allocated. Internal arrays are allocated
 *  in call to cm_alloc_seet_costs_2d.
 *
 *  Nota bene:
 *  No longer setting max, as algorithm to do so is unclear: see note below.
 *  Not sure which of two loops to set prepend and tail arrays is correct.
 */
void setUp2dCostMtx( cost_matrices_2d_t *retMtx
                   , unsigned int       *tcm
                   , size_t              alphSize
                   , unsigned int        gap_open
                   );

/** Nearly identical to setUp2dCostMtx. Code duplication necessary in order to have two different return types.
 *  I attempted to do with with a return of void *, but was having trouble with allocation, and was forced to move
 *  it outside this fn.
 */
void setUp3dCostMtx( cost_matrices_3d_t *retMtx
                   , unsigned int       *tcm
                   , size_t              alphSize
                   , unsigned int        gap_open
                   );

void freeCostMtx(void *input, int is_2d);

void freeNWMtx(alignment_matrices_t *input);

void freeChar(dyn_char_p toFree);

#endif // C_CODE_ALLOC_SETUP_H
