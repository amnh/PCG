#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#ifndef C_CODE_ALLOC_SETUP_H
#define C_CODE_ALLOC_SETUP_H

#include "alignCharacters.h"
#include "debug_constants.h"
#include "costMatrix.h"
#include "nwMatrices.h"
//#include "ukkCheckp.h"
//#include "ukkCommon.h"


/** Allocate nw_matrices struct. Assigns initial values where necessary. Calls
 *  mat_setup_size to allocate all internal arrays.
 *
 *  Order of character lengths doesn't matter
 */
void initializeNWMtx(nw_matrices_p retMtx, size_t cap_char1, size_t cap_char2, size_t cap_char3, int alphSize);

/** Does internal allocation for a character struct. Also sets seq pointers within array to correct positions.
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
int distance (int const *tcm, size_t alphSize, int nucleotide, int ambElem);

void setUp2dCostMtx(int* tcm, size_t alphSize, int gap_open, cost_matrices_2d_p retMtx);

void setUp3dCostMtx(int* tcm, size_t alphSize, int gap_open, cost_matrices_3d_p retMtx);

void freeCostMtx(void * input, int is_2d);

void freeNWMtx(nw_matrices_p input);

void freeChar(dyn_char_p toFree);

#endif // C_CODE_ALLOC_SETUP_H
