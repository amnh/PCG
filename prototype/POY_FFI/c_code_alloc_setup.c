#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "algn.h"
#include "c_code_alloc_setup.h"
#include "debug.h"
#include "costMatrix.h"
#include "nwMatrices.h"
#include "ukkCheckp.h"
#include "ukkCommon.h"

int* tcm, int alphSize, int gap_open, int is_2d, seq_p longSeq


nw_matrices_p initializeNWMtx() {
    nw_matrices_p newMtx = malloc( sizeof(struct matrices) );

    // in six following allocations all matrices are set to their shortest length because they get realloced in mat_setup_size
    newMtx->len          = 0;  // a suitably small number to trigger realloc, but be larger than len_eff
    newMtx->len_eff      = -1; // len_eff is -1 so that len_eff < len, triggering the realloc
    newMtx->len_pre      = 0;  // again, trigger realloc

    newMtx->nw_costMtx   = malloc ( sizeof( int ) );
    newMtx->dir_mtx_2d   = malloc ( sizeof( DIRECTION_MATRIX ) );
    newMtx->pointers_3d  = malloc ( sizeof( int* ) );     // TODO: Why don't I have to dealloc all pointers in array?
    // newMtx->cube         = malloc ( sizeof( int* ) );  // don't have to allocate these two,
    // newMtx->cube_d       = malloc ( sizeof( int* ) );  // because they're just pointing to nw_costMtx and dir_mtx_2d
    newMtx->precalc      = malloc ( sizeof( int ) );

    return newMtx;
}


seq_p initializeSeq(size_t allocSize, const int *vals, size_t length) {
    seq_p retSeq = malloc( sizeof(struct seq) );

    // assign sequence into sequence struct
    SEQT *seq = calloc(allocSize, sizeof(SEQT));
    if (length > 0) {
        for(size_t i = allocSize - length; i < allocSize; i++) {
            seq[i] = (int) vals[i - allocSize + length];
        }
    }

    retSeq->magic_number = 0; // This was only used in OCaml code
    retSeq->cap          = allocSize; // capacity
    retSeq->len          = length == 0 ? 0 : length;
    retSeq->head         = seq;
    retSeq->begin        = retSeq->head + allocSize - retSeq->len; // because the assigned values are at the end of the array
    retSeq->end          = retSeq->begin + retSeq->len;

    return retSeq;
}



/** Find distance between an ambiguous nucleotide and an unambiguous ambElem. Return that value and the median.
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
int distance (int const *tcm, int alphSize, int nucleotide, int ambElem) {
    int min     = INT_MAX;
    // int max     = 0;
    int curCost = 0;
    for (size_t pos = 0; pos < alphSize; pos++) {
        if (1 << pos & ambElem) { // if pos is set in ambElem, meaning pos is possible value of ambElem
            curCost = tcm[pos * alphSize + nucleotide - 1];
            if (curCost < min) {
                min = curCost;
            }
        }
    }
    return min;
}

// may return cost_matrices_2d or cost_matrices_3d, so void *
// no longer setting max, as algorithm to do so is unclear: see note below
void * setupCostMtx(int* tcm, int alphSize, int gap_open, int is_2d) {
    // first allocate retMatrix
    int combinations = 1;                     // false if matrix is sparse. In this case, it's DNA, so not sparse.
    int do_aff       = gap_open == 0 ? 0 : 2; // The 2 is because affine's cost_model_type is 2, according to my reading of ML code.
                                              // This value set in cm_set_affine().
    int is_metric    = 1;
    int all_elements = (1 << alphSize) - 1;                    // Given data is DNA (plus gap), there are 2^5 - 1 possible character states
    cost_matrices_2d_p retMtx;

    int minCost2d = INT_MAX;
    int minCost3d = INT_MAX;
    SEQT median2d = 0, median3d = 0;          // cumulative median for 2d and 3d; combos of median1, etc., below
    int curCost2d, curCost3d;

    int median1, median2, median3;            // median of a given nucleotide and current ambElem, for each ambElem

    if (is_2d) {
        retMtx = malloc( sizeof(struct cost_matrices_2d) );
        cm_alloc_set_costs_2d( alphSize,
                               combinations,
                               do_aff,
                               gap_open,
                               is_metric,
                               all_elements,
                               retMtx
                              );
    } else {
        retMtx = malloc( sizeof(struct cost_matrices_3d) );
        cm_alloc_set_costs_3d( alphSize,
                               combinations,
                               do_aff,
                               gap_open,
                               all_elements,
                               (cost_matrices_3d_p) retMtx
                              );
    }

    for (SEQT ambElem1 = 1; ambElem1 <= all_elements; ambElem1++) { // for every possible value of ambElem1, ambElem2, ambElem3
        for (SEQT ambElem2 = 1; ambElem2 <= all_elements; ambElem2++) {
            for (SEQT ambElem3 = 1; ambElem3 <= all_elements; ambElem3++) {
                curCost2d = curCost3d = 0;                // don't actually need to do this
                minCost2d = INT_MAX;
                minCost3d = INT_MAX;
                median2d  = median3d = 0;
                median1   = median2  = median3 = 0;
                for (int nucleotide = 1; nucleotide <= alphSize; nucleotide++) {
                    curCost2d = distance (tcm, alphSize, nucleotide, ambElem1) +
                                distance (tcm, alphSize, nucleotide, ambElem2);
                    // now seemingly recreating logic in distance(), but that was to get the cost for each
                    // ambElem; now we're combining those costs get overall cost and median
                    if (curCost2d < minCost2d) {
                        minCost2d = curCost2d;
                        median2d  = 1 << (nucleotide - 1); // median1 | median2;
                    } else if (curCost2d == minCost2d) {
                        median2d |= 1 << (nucleotide - 1); // median1 | median2;
                    }
                    if (!is_2d) {
                        median1   = median2 = median3 = 0;
                        curCost3d = distance (tcm, alphSize, nucleotide, ambElem1) +
                                    distance (tcm, alphSize, nucleotide, ambElem2) +
                                    distance (tcm, alphSize, nucleotide, ambElem3);
                        if (curCost3d < minCost3d) {
                            minCost3d = curCost3d;
                            median3d  = 1 << (nucleotide - 1); // median1 | median2 | median3;
                        } else if (curCost3d == minCost3d) {
                            median3d |= 1 << (nucleotide - 1); // median1 | median2 | median3;
                        }
                    } // end 3d cost assignment
                } // nucleotide

                if (!is_2d) {
                    cm_set_cost_3d   (ambElem1, ambElem2, ambElem3, minCost3d, (cost_matrices_3d_p) retMtx);
                    cm_set_median_3d (ambElem1, ambElem2, ambElem3, median3d,  (cost_matrices_3d_p) retMtx);
                    // cm_set_worst     (ambElem1, ambElem2, max_2d,    (cost_matrices_2d_p) retMtx);    // no worst in 3d
                }
            } // ambElem3
            cm_set_cost_2d   (ambElem1, ambElem2, minCost2d, (cost_matrices_2d_p) retMtx);
            cm_set_median_2d (ambElem1, ambElem2, median2d,  (cost_matrices_2d_p) retMtx);
        } // ambElem2
    } // ambElem1
    if (is_2d) {
        SEQT* seqStart = longSeq->begin;
        int gap        = all_elements;
        int seqElem;
        for ( size_t i = 1; i <= all_elements; i++) {
            // Gap number is alphSize - 1, which makes bit representation
            // i << (alphSize - 1), because first char value is i << 0.
            seqElem = (int) *(seqStart + i);
            cm_set_prepend_2d (i, cm_get_cost(gap, seqElem, retMtx), retMtx);
            cm_set_tail_2d    (cm_get_cost(seqElem, gap, retMtx), i, retMtx);
        }
    }
    return retMtx;
}

/*** Folowing functions free alloc'ed all space allocated in above fns. ***/

void freeCostMtx(void * input, int is_2d) {

    if (is_2d) {
        free( ( (cost_matrices_2d_p) input )->cost);
        free( ( (cost_matrices_2d_p) input )->median);
        free( ( (cost_matrices_2d_p) input )->worst);
        free( ( (cost_matrices_2d_p) input )->prepend_cost);
        free( ( (cost_matrices_2d_p) input )->tail_cost);
    } else {
        free( ( (cost_matrices_3d_p) input )->cost);
        free( ( (cost_matrices_3d_p) input )->median);
    }

    free (input);
}

/**
 *
 * TODO: make sure I'm actually deallocing right here.
 */
void freeNWMtx(nw_matrices_p input) {
    free (input->nw_costMtx);
    free (input->dir_mtx_2d);
    free (input->pointers_3d);
    // free (input->cube);    // don't have to deallocate these two,
    // free (input->cube_d);  // because they're just pointing to nw_costMtx and dir_mtx_2d
    free (input->precalc);

    free(input);
}

void freeSeq(seq_p toFree) {
    free(toFree->head);
    free(toFree);
}

void resetSeqValues(seq_p retSeq) {
    //retSeq->end   = retSeq->begin + retSeq->len;
    retSeq->begin = retSeq->end;
    retSeq->len   = 0;
}

