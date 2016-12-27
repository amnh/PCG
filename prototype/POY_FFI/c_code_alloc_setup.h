#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "algn.h"
#include "debug.h"
#include "costMatrix.h"
#include "matrices.h"
#include "ukkCheckp.h"
#include "ukkCommon.h"

#define SEQ_CAPACITY 64            // TODO: increase this size, or replace it altogether

nw_matrices_p initializeNWMtx();

seq_p initializeSeq(size_t allocSize, const int *vals, size_t length);

void resetSeqValues(seq_p retSeq);

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
int distance (int const *tcm, int alphSize, int nucleotide, int ambElem);

// may return cost_matrices_2d or cost_matrices_3d, so void *
// no longer setting max, as algorithm to do so is unclear: see note in .c file
void * setupCostMtx(int* tcm, int alphSize, int gap_open, int is_2d, seq_p longSeq);

void freeCostMtx(void * input, int is_2d);

void freeNWMtx(nw_matrices_p input);

void freeSeq(seq_p toFree);

