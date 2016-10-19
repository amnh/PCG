#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "algn.h"
#include "debug.h"
// #include "array_pool.h"
// #include "cm.h"
#include "matrices.h"
#include "ukkCommon.h"

#define SEQ_CAPACITY 64

int power_2 (int input) {
    if (input == 1)  return 1;
    if (input == 2)  return 1;
    if (input == 4)  return 1;
    if (input == 8)  return 1;
    if (input == 16) return 1;
    return 0;
}

nw_matrices_p initializeNWMtx() {
    nw_matrices_p newMtx = malloc( sizeof(struct matrices) );

    // in six following allocations all matrices are set to their shortest length because they get realloced in mat_setup_size
    newMtx->len         = 0;  // a suitably small number to trigger realloc, but be larger than len_eff
    newMtx->len_eff     = -1; // len_eff is -1 so that len_eff < len, triggering the realloc
    newMtx->len_pre     = 0;  // again, trigger realloc

    newMtx->nw_costMtx  = malloc ( sizeof( int ) * 1); // TODO: add reasoning
    newMtx->dir_mtx_2d  = malloc ( sizeof( DIRECTION_MATRIX ) );
    newMtx->pointers_3d = malloc ( sizeof( int* ) );
    newMtx->cube        = malloc ( sizeof( int* ) );
    newMtx->cube_d      = malloc ( sizeof( int* ) );
    newMtx->precalc     = malloc ( sizeof( int ) );

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

void resetSeqValues(seq_p retSeq) {
    //retSeq->end   = retSeq->begin + retSeq->len;
    retSeq->begin = retSeq->end;
    retSeq->len   = 0;
}

/* Original setupCostMtx fns:

void setCosts(int *tcm, int alphSize, int lcm, SEQT base, SEQT median_in, int *min, int *max, SEQT *median_out) {
    int cost;
    // printf("base: %2hhu,    min: %d,    max: %d,    median_out: %hhu\n", base, min, max, *median_out);
    for (size_t i = 1; i <= alphSize; i++) {
        if ( 1 << (i - 1) & base) { // if nucleotide is present in possibly ambiguous base
            for (size_t j = 1; j <= alphSize; j++) { // for each alphabet character possible find cost
                if (1 << (j - 1) & median_in) {
                    cost = tcm[(i - 1) * lcm + j - 1];
                    //printf("cost: %d\n", cost);
                    if( cost < *min ) {
                        *min = cost;
                        // printf("2d minimum   i: %2zu,    j: %2zu,    min:    %2d\n", i, j, min);
                        *median_out = (SEQT) (1 << (i - 1)) | (1 << (j - 1));
                        // printf("2d minimum   i: %2zu,    j: %2zu,    median_out: %2hhu\n", i, j, *median_out);
                    } else if (cost == *min) {
                        *median_out |= (SEQT) (1 << (i - 1)) | (1 << (j - 1));
                        // printf("2d equal     i: %2zu,    j: %2zu,    median_out: %2hhu\n", i, j, *median_out);
                    }
                    if( cost >= *max ) {
                        *max = cost;
                        // printf("maximum   i: %2zu,    j: %2zu,    max:    %2d\n", i, j, cost);
                    }
                }
            }
        }
    }
    if (min < 0) {
        printf("base: %2hhu,    median_in: %2d,    min: %d,    max: %d,    median_out: %2hhu\n", base, median_in, *min, *max, *median_out);
    }
}

void * setupCostMtx(int* tcm, int alphSize, int gap_open, int is_2d) {
    int combinations = 1; // false if matrix is sparse. In this case, it's DNA, so not sparse.
    int do_aff       = gap_open == 0 ? 0 : 2;
    int is_metric    = 1;
    int all_elements = 31; // How is this used?

    cost_matrices_2d_p retMtx;
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

    // set up cost, median and worst matrices.
    int *max_1 = malloc(sizeof(int));
    int *max_2 = malloc(sizeof(int));
    int *max_3 = malloc(sizeof(int)); // max_3 is uneeded, but used as argument for setCosts()
    int *min_1 = malloc(sizeof(int));
    int *min_2 = malloc(sizeof(int));
    int *min_3 = malloc(sizeof(int));
    SEQT *median_1 = malloc(sizeof(SEQT));
    SEQT *median_2 = malloc(sizeof(SEQT));
    SEQT *median_3 = malloc(sizeof(SEQT));
    SEQT median_12;
    SEQT median_23;
    SEQT median_13;

    int min_2d;
    SEQT median_2d;
    int max_2d;  
    int min_3d;
    SEQT median_3d;
    int max_3d;  

    size_t i;

    for (SEQT ambElem1 = 1; ambElem1 <= 31; ambElem1++) {
        for (SEQT ambElem2 = 1; ambElem2 <= 31; ambElem2++) {
            median_12 = ambElem1 | ambElem2;
            if (is_2d) {
                *min_1    = INT_MAX; // this should be largest integer value.
                *max_1    = 0;
                *median_1 = 0;
                setCosts(tcm, alphSize, retMtx->lcm, ambElem1, ambElem2, min_1, max_1, median_1);
                // if (*min_1 != 0) {
                //     printf("ambElem1: %2hhu,    median_23: %2d,    min: %d,    max: %d,    median: %2d\n", 
                //            ambElem1, median_23, *min_1, *max_1, *median_1);
                // }

                *min_2    = INT_MAX;
                *max_2    = 0;
                *median_2 = 0;
                setCosts(tcm, alphSize, retMtx->lcm, ambElem2, ambElem1, min_2, max_2, median_2);

                min_2d    = *min_1 + *min_2;
                median_2d = *median_1 | *median_2;
                // if (min_2d != 0) {
                //     printf("ambElem1: %2hhu,   ambElem2: %2hhu,   ambElem3: %2hhu,    median: %2d,    min: %d,    max: %d\n", 
                //            ambElem1, ambElem2, ambElem3, median_2d, min_2d, max_2d);
                // }
                max_2d    = *max_1 + *max_2; 
                
                // printf("ambElem1: %2hhu,    ambElem2: %2hhu,    ambElem3: %2hhu\n", ambElem1, ambElem2, ambElem3);
                // printf("median: %2d,    min: %2d\n", median_2d, min_2d);
                cm_set_cost_2d   (ambElem1, ambElem2, min_2d,    (cost_matrices_2d_p) retMtx);
                cm_set_median_2d (ambElem1, ambElem2, median_2d, (cost_matrices_2d_p) retMtx);
                cm_set_worst     (ambElem1, ambElem2, max_2d,    (cost_matrices_2d_p) retMtx);
            } else {
                for (SEQT ambElem3 = 1; ambElem3 <= 31; ambElem3++) {
                    median_23 = ambElem2 | ambElem3;
                    median_13 = ambElem1 | ambElem3;

                    *min_1    = INT_MAX; // this should be largest integer value.
                    *max_1    = 0;
                    *median_1 = 0;
                    setCosts(tcm, alphSize, retMtx->lcm, ambElem1, median_23, min_1, max_1, median_1);
                    // if (*min_1 != 0) {
                    //     printf("ambElem1: %2hhu,    median_23: %2d,    min: %d,    max: %d,    median: %2d\n", 
                    //            ambElem1, median_23, *min_1, *max_1, *median_1);
                    // }

                    *min_2    = INT_MAX;
                    *max_2    = 0;
                    *median_2 = 0;
                    setCosts(tcm, alphSize, retMtx->lcm, ambElem2, median_13, min_2, max_2, median_2);
                    // if (*min_2 != 0) {
                    //     printf("ambElem2: %2hhu,    median_13: %2d,    min: %d,    max: %d,    median: %2d\n", 
                    //            ambElem2, median_13, *min_2, *max_2, *median_2);
                    // }

                    *max_3    = 0;
                    *min_3    = INT_MAX; 
                    *median_3 = 0;
                    setCosts(tcm, alphSize, retMtx->lcm, ambElem3, median_12, min_3, max_3, median_3);
                    // if (*min_3 != 0) {
                    //     printf("ambElem3: %2hhu,    median_12: %2d,    min: %d,    max: %d,    median: %2d\n", 
                    //            ambElem3, median_12, *min_3, *max_3, *median_3);
                    // }

                    min_3d    = *min_1 + *min_2 + *min_3;
                    median_3d = *median_1 | *median_2 | *median_3;

                    printf("ambElem1: %2hhu,    ambElem2: %2hhu,    ambElem3: %2hhu    3d median: %2d,    3d cost: %2d\n", 
                               ambElem1, ambElem2, ambElem3, median_3d, min_3d);
                    
                    cm_set_cost_3d   (ambElem1, ambElem2, ambElem3, min_3d,    (cost_matrices_3d_p) retMtx);
                    cm_set_median_3d (ambElem1, ambElem2, ambElem3, median_3d, (cost_matrices_3d_p) retMtx);
                    // no worst in 3d

                    // printf("ambElem1: %hhu, ambElem2: %hhu, ambElem3:\n", ambElem1, ambElem2);
                    
                    
                    // printf("cost_2d: %d, min: %d, max: %d, ambElem1: %hhu, ambElem2: %hhu, median: %d\n", cost, min, max, ambElem1, ambElem2, median );
                } // ambElem3
            }
        } // ambElem2
    } // ambElem1

    // now that 2d cost matrix is set up, use it to calculate prepend and tail matrices
    // remember: no tail or prepend in 3d
    if (is_2d) {
        for ( i = 0; i < retMtx->alphSize; i++) {
            cm_set_prepend_2d (i, cm_get_cost(cm_get_gap_2d (retMtx), i, retMtx), retMtx);
            cm_set_tail_2d    (i, cm_get_cost(i, cm_get_gap_2d (retMtx), retMtx), retMtx);
        }
    }
    return retMtx;
}
*/

/** Find distance between an ambiguous nucleotide and an unambiguous ambElem. Return that value and the median. 
 *  @param ambElem is ambiguous input.
 *  @param nucleotide is unambiguous.
 *  @param median is used to return the calculated median value.
 *
 *  This fn is necessary because there isn't yet a cost matrix set up, so it's not possible to 
 *  look up ambElems; therefore we must loop over possible values of the ambElem
 *  and find the lowest cost median.
 *
 *  Requires symmetric, if not metric, matrix.
 */
int distance (int const *tcm, int alphSize, int lcm, int nucleotide, int ambElem) {
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
    // printf("ambElem:   %2d,   nuc:   %2d,   min: %2d\n", ambElem, nucleotide, min);
    return min;
}

// may return cost_matrices_2d or cost_matrices_3d, so void *
// no longer setting max, as algorithm to do so is unclear: see note below
void * setupCostMtx(int* tcm, int alphSize, int gap_open, int is_2d) {
    // first allocate retMatrix
    int combinations = 1; // false if matrix is sparse. In this case, it's DNA, so not sparse.
    int do_aff       = gap_open == 0 ? 0 : 2;
    int is_metric    = 1;
    int all_elements = 31; // How is this used?
    cost_matrices_2d_p retMtx;

    int minCost2d = INT_MAX;
    int minCost3d = INT_MAX;
    // int maxCost = 0;
    SEQT median2d = 0, median3d = 0; // cumulative median for 2d and 3d; combos of median1, etc., below
    // int cost2d, cost3d;
    int curCost2d, curCost3d;

    int median1, median2, median3; // median of a given nucleotide and current ambElem, for each ambElem

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

    for (SEQT ambElem1 = 1; ambElem1 <= 31; ambElem1++) { // for every possible value of ambElem1, ambElem2, ambElem3
        for (SEQT ambElem2 = 1; ambElem2 <= 31; ambElem2++) {
            for (SEQT ambElem3 = 1; ambElem3 <= 31; ambElem3++) {
                curCost2d = curCost3d = 0; // don't actually need to do this
                minCost2d = INT_MAX; 
                minCost3d = INT_MAX;
                // maxCost   = 0;
                median2d  = median3d = 0;
                median1   = median2  = median3 = 0;
                for (int nucleotide = 1; nucleotide <= alphSize; nucleotide++) {
                    // TODO: if we do maxCost, then we should find individual max's for each distance below?
                    curCost2d = distance (tcm, alphSize, retMtx->lcm, nucleotide, ambElem1) +
                                distance (tcm, alphSize, retMtx->lcm, nucleotide, ambElem2);
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
                        curCost3d = distance (tcm, alphSize, retMtx->lcm, nucleotide, ambElem1) +
                                    distance (tcm, alphSize, retMtx->lcm, nucleotide, ambElem2) + 
                                    distance (tcm, alphSize, retMtx->lcm, nucleotide, ambElem3);
                        if (curCost3d < minCost3d) {
                            minCost3d = curCost3d;
                            median3d  = 1 << (nucleotide - 1); // median1 | median2 | median3;
                            // if( power_2(ambElem1) && power_2(ambElem2) && power_2(ambElem3)) {
                            //     printf("<     seq1: %2d,    seq2: %2d,    seq3: %2d,    cost: %2d,    median: %2d,    nucleotide: %2d\n", 
                            //       ambElem1, ambElem2, ambElem3, minCost3d, median3d, 1 << (nucleotide - 1));
                            // }
                        } else if (curCost3d == minCost3d) {
                            median3d |= 1 << (nucleotide - 1); // median1 | median2 | median3;
                            // if( power_2(ambElem1) && power_2(ambElem2) && power_2(ambElem3)) {
                            //     printf("==    seq1: %2d,    seq2: %2d,    seq3: %2d,    cost: %2d,    median: %2d,    nucleotide: %2d\n", 
                            //       ambElem1, ambElem2, ambElem3, minCost3d, median3d, 1 << (nucleotide - 1));
                            // }
                        }
                    }
                } // nucleotide
                
                if (!is_2d) {
                    // printf("seq1: %d,    seq2: %d,    cost: %d,    median: %d\n", 
                    //        ambElem1, ambElem2, minCost2d, median2d);
                    cm_set_cost_3d   (ambElem1, ambElem2, ambElem3, minCost3d, (cost_matrices_3d_p) retMtx);
                    cm_set_median_3d (ambElem1, ambElem2, ambElem3, median3d,  (cost_matrices_3d_p) retMtx);
                    // cm_set_worst     (ambElem1, ambElem2, max_2d,    (cost_matrices_2d_p) retMtx);    // no worst in 3d
                    // if( power_2(ambElem1) && power_2(ambElem2) && power_2(ambElem3)) {
                    //     printf("3d    seq1: %2d,    seq2: %2d,    seq3: %2d,    cost: %2d,    median: %2d\n", 
                    //       ambElem1, ambElem2, ambElem3, minCost3d, median3d);
                    // }
                }
            } // ambElem3
            // printf("ambElem1:  %2hhu,   ambElem2: %2hhu\n", ambElem1, ambElem2);
            // printf("median: %2d,   min:   %2d\n", median2d, minCost2d);
            cm_set_cost_2d   (ambElem1, ambElem2, minCost2d, (cost_matrices_2d_p) retMtx);
            cm_set_median_2d (ambElem1, ambElem2, median2d,  (cost_matrices_2d_p) retMtx);
            // if (power_2(ambElem1) && power_2(ambElem2)) {
            //     printf("2d    seq1: %d,    seq2: %d,    cost: %d,    median: %2d\n", ambElem1, ambElem2, minCost2d, median2d);
            // }
        } // ambElem2
    } // ambElem1
    if (is_2d) {
        for ( size_t i = 0; i < retMtx->alphSize; i++) {
            cm_set_prepend_2d (i, cm_get_cost(cm_get_gap_2d (retMtx), i, retMtx), retMtx);
            cm_set_tail_2d    (i, cm_get_cost(i, cm_get_gap_2d (retMtx), retMtx), retMtx);
        }
    }
    return retMtx;
}

int main() {


/******************************** set up and allocate all variables and structs ************************************/


/****************  Allocate sequences  ****************/

        //***** for following seqs, affine requires gap at start of sequence!!! *****/

    int alphSize = 5; // includes gap, but no ambiguities

    int longest_vals[SEQ_CAPACITY]  = {16, 1,2,4,8}; // don't forget to change lengths!!!
    int longSeqLen                  = 5;
    int shortest_vals[SEQ_CAPACITY] = {16, 1,2,4,8}; // don't forget to change lengths!!!
    int shortSeqLen                 = 5;
    int middle_vals[SEQ_CAPACITY]   = {16, 1,1,1,1}; // don't forget to change lengths!!!
    int middleSeqLen                = 5;

    seq_p longSeq   = initializeSeq(SEQ_CAPACITY, longest_vals,  longSeqLen);
    seq_p shortSeq  = initializeSeq(SEQ_CAPACITY, shortest_vals, shortSeqLen);
    seq_p mediumSeq = initializeSeq(SEQ_CAPACITY, middle_vals,   middleSeqLen);

    size_t total_poss_align_len = longSeqLen + shortSeqLen + middleSeqLen;
    seq_p retLongSeq   = initializeSeq(total_poss_align_len, 0, 0);
    seq_p retShortSeq  = initializeSeq(total_poss_align_len, 0, 0);
    seq_p retMediumSeq = initializeSeq(total_poss_align_len, 0, 0);



/****************  Allocate NW matrices  ****************/

    nw_matrices_p algn_mtxs2d       = initializeNWMtx();
    nw_matrices_p algn_mtxs2dAffine = initializeNWMtx();
    nw_matrices_p algn_mtxs3d       = initializeNWMtx();



/************  Allocate cost matrices  **************/

    size_t tcm_total_len = alphSize * alphSize; // the size of the input tcm


    // !!!!! if modifying this code, also make sure to change is_metric !!!!!!
    int *tcm = calloc(tcm_total_len, sizeof(int)); // this is the input tcm, not the generated one
    for (size_t i = 0; i < tcm_total_len; i += alphSize) {
        //printf("i: %zu\n", i);
        for (size_t j = 0; j < alphSize; j++) {
            //printf("i: %zu, j: %zu, cost: %lu\n", i, j, 2 * i + 2 * j);
            //tcm[i + j] = 2 * i + 2 * j;
            if ( i == j * alphSize ) {
                // printf("i: %2zu, j: %2zu, cost: 0\n", i, j);
                tcm[i + j] = IDENTITY_COST;    // identity
            } else if (i == (tcm_total_len - alphSize) || j == (alphSize - 1)) {
                // printf("i: %2zu, j: %2zu, cost: 2\n", i, j);
                tcm[i + j] = INDEL_COST;   // indel cost
            } else {
                // printf("i: %2zu, j: %2zu, cost: 1\n", i, j);
                tcm[i + j] = SUB_COST;    // sub cost
            }
         }
    }

    /**
    // Print TCM in pretty format
    const int n = costMtx2d->lcm;
    for (size_t i = 0; i < n; ++i) {
        for (size_t j = 0; j < n; ++j) {
            printf("%2d ",tcm[ n*i + j ]);
        }
        printf("\n");
    }
    **/
    cost_matrices_2d_p costMtx2d;
    cost_matrices_2d_p costMtx2d_affine;
    cost_matrices_3d_p costMtx3d;

    // tcm is tcm; alphSize includes gap; third param is gap opening cost; fourth is is_2d
    if (DO_2D) {
        costMtx2d = setupCostMtx (tcm, alphSize, 0, 1);
        mat_setup_size (algn_mtxs2d, longSeq->len, shortSeq->len, 0, 0, costMtx2d->lcm);
    }
    if (DO_AFF) {
        costMtx2d_affine = setupCostMtx (tcm, alphSize, 2, 1);
        mat_setup_size (algn_mtxs2dAffine, longSeq->len, shortSeq->len, 0, 0, costMtx2d_affine->lcm);
    }
    if (DO_3D) {
        costMtx3d = setupCostMtx (tcm, alphSize, 0, 0);  // last argument means it's not 2d
        // penultimate parameter is ukk flag
        mat_setup_size (algn_mtxs3d, longSeq->len, mediumSeq->len, shortSeq->len, 0, costMtx3d->lcm);
    }
    int algnCost;

    // the following to compute deltawh, which increases the matrix height or width in algn_nw_2d
    // TODO: This has something to do with Ukkonnen. Figure it out and document it.
    // This from ML:
    // TODO: figure out: does this loop, or something?
    int deltawh = 0;
    // TODO: make sure lenLongSeq > lenShortSeq
    int diff = longSeq->len - shortSeq->len;
    int lower_limit = .1 * longSeq->len;
    if (deltawh) {
        deltawh = diff < lower_limit ? lower_limit : deltawh;
    } else {
        deltawh = diff < lower_limit ? lower_limit / 2 : 2;
    }


    // cm_print_3d (costMtx3d);

/**************************************************** Do 2d alignment ********************************************************/

    if (DO_2D) {
        printf("\n\n\n******************** Align 2 sequences **********************\n");

        // printf("Original alignment matrix before algn_nw_2d: \n");
        // algn_print_dynmtrx_2d( longSeq, shortSeq, algn_mtxs2d );

        algnCost = algn_nw_2d( longSeq, shortSeq, costMtx2d, algn_mtxs2d, deltawh ); // TODO: is

        if (DEBUG_MAT) {
            printf("\n\nFinal alignment matrix: \n\n");
            algn_print_dynmtrx_2d( longSeq, shortSeq, algn_mtxs2d );
        }


        printf("Original 2d sequences:\n");
        seq_print(longSeq, 1);
        seq_print(shortSeq, 2);

        algn_backtrace_2d (longSeq, shortSeq, retLongSeq, retShortSeq, algn_mtxs2d, costMtx2d, 0, 0, 1);
        printf("\nAligned 2d sequences\n");
        seq_print(retLongSeq, 1);
        seq_print(retShortSeq, 2);

        printf("Alignment cost: %d\n", algnCost);

        /****  Now get alignments  ****/

        printf("\nAligned sequences:\n");
        int *algnSeqVals = calloc(retLongSeq->len, sizeof(int));
        seq_p algnSeq = initializeSeq(SEQ_CAPACITY, algnSeqVals, retLongSeq->len);
        resetSeqValues(algnSeq);

        // union:
        algn_union (retLongSeq, retShortSeq, algnSeq);
        printf("  Unioned sequence\n  ");
        seq_print(algnSeq, 0);

        // ungapped:
        resetSeqValues(algnSeq);
        algn_get_median_2d_no_gaps (retLongSeq, retShortSeq, costMtx2d, algnSeq);
        printf("\n  Median without gaps\n  ");
        seq_print(algnSeq, 0);

        // gapped:
        resetSeqValues(algnSeq);
        algn_get_median_2d_with_gaps (retLongSeq, retShortSeq, costMtx2d, algnSeq);
        printf("\n  Median with gaps\n  ");
        seq_print(algnSeq, 0);
    }



/************************************************ Do 2d affine alignment *****************************************************/

    /*** must have gap at start of sequence!!! ***/

    if (DO_AFF) {

        resetSeqValues(retLongSeq);
        resetSeqValues(retShortSeq);

        // TODO: document these variables
        int *matrix;                        //
        int *close_block_diagonal;          //
        int *extend_block_diagonal;         //
        int *extend_vertical;               //
        int *extend_horizontal;             //
        int *final_cost_matrix;             //
        int *precalcMtx;                    //
        int *matrix_2d;                     //
        int *gap_open_prec;                 //
        int *s_horizontal_gap_extension;    //
        int lenLongerSeq;                   //
        DIRECTION_MATRIX *direction_matrix;
        size_t lenLongSeq = seq_get_len(longSeq);
        size_t lenShortSeq = seq_get_len(shortSeq);

        // reset return results
        resetSeqValues(retLongSeq);
        resetSeqValues(retShortSeq);

        lenLongerSeq = (lenLongSeq > lenShortSeq) ? lenLongSeq : lenShortSeq;

        //    mat_setup_size (algn_mtxs2dAffine, lenLongerSeq, lenLongerSeq, 0, 0, cm_get_lcm (costMtx2d_affine));
        matrix_2d  = mat_get_2d_nwMtx (algn_mtxs2dAffine);
        precalcMtx = mat_get_2d_prec (algn_mtxs2dAffine);

        // TODO: figure out what the following seven values do/are
        //       also note the int factors, which maybe have something to do with the unexplained 12
        //       that appears in matrices.c?
        // here and in algn.c, "block" refers to a block of gaps, so close_block_diagonal is the cost to
        // end a subsequence of gaps, presumably with a substitution, but maybe by simply switching directions:
        // there was a vertical gap, now there's a horizontal one.
        close_block_diagonal       = (int *)  matrix_2d;
        extend_block_diagonal      = (int *) (matrix_2d + ( 2 * lenLongerSeq));
        extend_vertical            = (int *) (matrix_2d + ( 4 * lenLongerSeq));
        extend_horizontal          = (int *) (matrix_2d + ( 6 * lenLongerSeq));
        final_cost_matrix          = (int *) (matrix_2d + ( 8 * lenLongerSeq));
        gap_open_prec              = (int *) (matrix_2d + (10 * lenLongerSeq));
        s_horizontal_gap_extension = (int *) (matrix_2d + (11 * lenLongerSeq));



        // TODO: empty_medianSeq might not be necessary, as it's unused in ml code:
        size_t medianSeqLen    = lenLongSeq + lenShortSeq + 2;  // 2 because that's how it is in ML code
        seq_p empty_medianSeq  = malloc( sizeof(struct seq) );
        empty_medianSeq->cap   = medianSeqLen;
        empty_medianSeq->head  = calloc( medianSeqLen, sizeof(SEQT));
        empty_medianSeq->len   = 0;
        empty_medianSeq->begin = empty_medianSeq->end = empty_medianSeq->head + medianSeqLen;

        seq_p medianSeq  = malloc( sizeof(struct seq) );
        medianSeq->cap   = medianSeqLen;
        medianSeq->head  = calloc( medianSeqLen, sizeof(SEQT));
        medianSeq->len   = 0;
        medianSeq->begin = medianSeq->end = medianSeq->head + medianSeqLen;

        direction_matrix =  mat_get_2d_direct (algn_mtxs2dAffine);

        printf("\n\n\n***************** Align 2 sequences affine ********************\n\n");

        printf("Original affine 2d sequences:\n");
        
        // seq_p longerSequence = lenLongSeq > lenShortSeq ? longSeq : shortSeq;
        // seq_p shorterSequence = lenLongSeq > lenShortSeq ? shortSeq : longSeq;

        seq_print(longSeq, 1);
        seq_print(shortSeq, 2);

        cm_precalc_4algn(costMtx2d_affine, algn_mtxs2dAffine, longSeq);

        // TODO: consider moving all of this into algn.
        //       the following three fns were initially not declared in algn.h
        initialize_matrices_affine (costMtx2d_affine->gap_open, shortSeq, longSeq, 
                                    costMtx2d_affine, 
                                    close_block_diagonal, extend_block_diagonal, 
                                    extend_vertical, extend_horizontal,
                                    final_cost_matrix, direction_matrix, precalcMtx);

        printf("\n");
        printf("close_block_diagonal      : %d\n", *close_block_diagonal      );
        printf("extend_block_diagonal     : %d\n", *extend_block_diagonal     );
        printf("extend_vertical           : %d\n", *extend_vertical           );
        printf("extend_horizontal         : %d\n", *extend_horizontal         );
        printf("final_cost_matrix         : %d\n", *final_cost_matrix         );
        printf("gap_open_prec             : %d\n", *gap_open_prec             );
        printf("s_horizontal_gap_extension: %d\n", *s_horizontal_gap_extension);
        printf("\n");

       // for (int *i = matrix_2d, j = 0; i < matrix_2d + algn_mtxs2dAffine->len; i++, j++) {
       //     printf("%d, ", *i);
       //     if (j % (lenLongerSeq ) == 0) {
       //         printf("\n");
       //     }
       // }


        // shorter first
        // TODO: why isn't this consistent with next fn call?
        algnCost = algn_fill_plane_3_affine (shortSeq, longSeq, 
                                             shortSeq->len - 1, longSeq->len - 1,
                                             final_cost_matrix, direction_matrix, costMtx2d_affine, 
                                             extend_horizontal, extend_vertical,
                                             close_block_diagonal, extend_block_diagonal, 
                                             precalcMtx, gap_open_prec,
                                             s_horizontal_gap_extension);



        if (DEBUG_MAT) {
            printf("\n\nFinal alignment matrix, affine: \n\n");
            algn_print_dynmtrx_2d( longSeq, shortSeq, algn_mtxs2dAffine );
        }


        // shorter first
        // TODO: fix this to make it consistent
        backtrace_affine (direction_matrix, shortSeq, longSeq, medianSeq, empty_medianSeq,
                          retLongSeq, retShortSeq, costMtx2d_affine);

        printf("\nAligned affine 2d sequences\n");
        if (lenLongSeq > lenShortSeq) {
          seq_print(retShortSeq, 1);
          seq_print(retLongSeq, 2);
        } else {
          seq_print(retLongSeq, 1);
          seq_print(retShortSeq, 2);
        }

        printf("\nAlignment cost: %d\n", algnCost);
    }


/************************************************ Do 3d alignment *************************************************/

    if (DO_3D) {

        printf("\n\n\n******************** Align 3 sequences **********************\n\n");

        // must first reset values in retLongSeq and retShortSeq
        resetSeqValues(retLongSeq);
        resetSeqValues(retShortSeq);

        // algnCost = algn_nw_3d (longSeq, mediumSeq, shortSeq, costMtx3d, algn_mtxs3d, deltawh);
        //printf("Final alignment matrix: \n");
        //algn_print_dynmtrx_2d_2d( longSeq, shortSeq, algn_mtxs3d );

        printf("Original 3d sequences:\n");
        seq_print(longSeq,   1);
        seq_print(mediumSeq, 2);
        seq_print(shortSeq,  3);
        printf("\n");

        powell_3D_align (longSeq,    mediumSeq,    shortSeq, 
                         retLongSeq, retMediumSeq, retShortSeq, 
                         1, 2, 1);

        //algn_backtrace_3d (longSeq, mediumSeq, shortSeq, retLongSeq, retMediumSeq, retShortSeq, costMtx3d, algn_mtxs3d);

        printf("\n\nAligned 3d sequences:\n");
        seq_print(retLongSeq,   1);
        seq_print(retMediumSeq, 2);
        seq_print(retShortSeq,  3);

        printf("\nAlignment cost: %d\n", algnCost);

        printf("\n\n\n");

        // for (SEQT *base = retLongSeq->begin; base != retLongSeq->end; base++) {
        //     printf("a: %c\n", *base);
        // }
        // for (SEQT *base = retShortSeq->begin; base != retShortSeq->end; base++) {
        //     printf("b: %s\n", base);
        // }
    }

    // Next this: algn_get_median_3d (seq_p seq1, seq_p seq2, seq_p seq3, 
    //                cost_matrices_3d_p m, seq_p sm)


    return 1;
}
