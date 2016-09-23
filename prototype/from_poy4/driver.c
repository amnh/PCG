#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "algn.h"
#include "debug.h"
// #include "array_pool.h"
// #include "cm.h"
#include "matrices.h"

#define SEQ_CAPACITY 64

nw_matrices_p initializeNWMtx() {
    nw_matrices_p newMtx = malloc( sizeof(struct matrices) );

    // in three following allocations all matrices are set to their shortest length because they get realloced in mat_setup_size
    // len_eff is likewise set to 0, as that triggers the realloc.
    newMtx->len_eff     = 0;
    newMtx->nw_costMtx  = malloc ( sizeof( int ) );
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

void * setupCostMtx(int* tcm, int alphSize, int gap_open) {
    int combinations = 1; // false if matrix is sparse. In this case, it's DNA, so not sparse.
    int do_aff       = gap_open == 0 ? 0 : 1;
    int is_metric    = 1;
    int all_elements = 31; // How is this used?

    cost_matrices_2d_p retMtx = malloc( sizeof(struct cost_matrices_2d) );
    retMtx = cm_alloc_set_costs_2d( alphSize, 
                                       combinations, 
                                       do_aff, 
                                       gap_open, 
                                       is_metric, 
                                       all_elements, 
                                       retMtx
                                     );

    // set up cost, median and worst matrices.
    // Since arrays in 3d are just pointers to 2d, should work for both.
    int max, cost, min;
    SEQT median;
    size_t i, j;
    // for each element, ambiguous or not, cycle through all other elements
    for (SEQT base1 = 1; base1 <= 31; base1++) {
        for (SEQT base2 = 1; base2 <= 31; base2++) {
            median = 0;
            max = 0;
            min = INT_MAX; // this should be largest integer value.

            // now cycle through single-bit values, 1, 2, 4, etc.
            // if that bit is set in base1, look at all bits in base2,
            // accumulate lowest cost and highest cost of all existing combinations
            // into cost and worst matrices.
            for (i = 1; i <= retMtx->alphSize; i++) {
                if ( 1 << (i - 1) & base1) {
                    //printf("base1: %hhu, i: %zu\n", base1, i);
                    for (j = 1; j < retMtx->alphSize; j++) {
                        //printf("%d\n", costMtx2d->alphSize);
                        if (1 << (j - 1) & base2) {
                            cost = tcm[(i - 1) * retMtx->lcm + j - 1];
                            if( cost < min ) {
                                min = cost;
                                median = (SEQT) (1 << (i - 1)) | (1 << (j - 1));
                                // printf("i: %zu, j: %zu, same %hhu\n", i, j, median);
                                // cm_set_median(base1, base2, median, retMtx);
                            } else if (cost == min) {
                                median |= (SEQT) (1 << (i - 1)) | (1 << (j - 1));
                                // printf("low  %hhu\n", median);
                                // cm_set_median(base1, base2, median, retMtx);
                            }
                            if( cost >= max ) {
                                max = cost;
                            }
                            
                        }
                    }
                }
                
            }
            cm_set_cost  (base1, base2, min,    retMtx);
            cm_set_worst (base1, base2, max,    retMtx);
            cm_set_median(base1, base2, median, retMtx);
            //printf("cost: %d, min: %d, max: %d, base1: %hhu, base2: %hhu, median: %d\n", cost, min, max, base1, base2, median );
        }
    }

    // now that cost matrix is set up, use it to calculate prepend and tail matrices
    for (size_t i = 0; i < retMtx->alphSize; i++) {

        cm_set_prepend(i, cm_get_cost(cm_get_gap(retMtx), i, retMtx), retMtx);
        cm_set_tail(i, cm_get_cost(i, cm_get_gap(retMtx), retMtx), retMtx);
    }
    return retMtx;
}

int main() {

/******************************** set up and allocate all variables and structs ************************************/


/****************  Allocate sequences  ****************/

        //***** for following seqs, affine requires gap at start of sequence!!! *****/

    int s1_vals[SEQ_CAPACITY] = {16, 2,1,8,8,8,8,8,4,4,4,4,4,4,4,4}; // don't forget to change lengths!!!
    int seq1Len               = 16;
    int s2_vals[SEQ_CAPACITY] = {16, 2,1          ,4,4,4,4,4,4,4,4}; // don't forget to change lengths!!!
    int seq2Len               = 11;
    int s3_vals[SEQ_CAPACITY] = {16, 3,1,9,9,8,4,4}; // don't forget to change lengths!!!
    int seq3Len               = 8;

    seq_p seq1    = initializeSeq(SEQ_CAPACITY, s1_vals, seq1Len);
    seq_p seq2    = initializeSeq(SEQ_CAPACITY, s2_vals, seq2Len);
    seq_p seq3    = initializeSeq(SEQ_CAPACITY, s3_vals, seq3Len);
    
    size_t total_poss_align_len = seq1Len + seq2Len + seq3Len;
    seq_p retSeq1 = initializeSeq(total_poss_align_len, 0, 0);
    seq_p retSeq2 = initializeSeq(total_poss_align_len, 0, 0);
    seq_p retSeq3 = initializeSeq(total_poss_align_len, 0, 0);



/****************  Allocate NW matrices  ****************/

    nw_matrices_p algn_mtxs2d       = initializeNWMtx();
    nw_matrices_p algn_mtxs2dAffine = initializeNWMtx();
    nw_matrices_p algn_mtxs3d       = initializeNWMtx();



/************  Allocate cost matrices  **************/


    int alphSize = 5; // includes gap, but no ambiguities
    
    
    size_t tcm_total_len = alphSize * alphSize; // the size of the input tcm

    // printf("%zu\n", tcm_total_len);

    // if modifying this code, also make sure to change is_metric
    int *tcm = calloc(tcm_total_len, sizeof(int)); // this is the input tcm, not the generated one
    for (size_t i = 0; i < tcm_total_len; i += alphSize) {
        //printf("i: %zu\n", i);
        for (size_t j = 0; j < alphSize; j++) {
            //printf("i: %zu, j: %zu, cost: %lu\n", i, j, 2 * i + 2 * j);
            //tcm[i + j] = 2 * i + 2 * j;
            if ( i == j * alphSize ) {
                tcm[i + j] = 0;    // identity
            } else if (i == (tcm_total_len - alphSize) || j == (alphSize - 1)) {
                tcm[i + j] = 2;   // indel cost
            } else {
                tcm[i + j] = 1;    // sub cost
            }
            // printf("i: %zu, j: %zu, cost: %d\n", i, j, tcm[i+j]);

            //cm_set_cost (i, j, or, costMtx2d);
            // printf("should be: %zu, is: %d\n", or, cm_get_cost (i,j,costMtx2d));
            // or += 2;
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
    cost_matrices_2d_p costMtx2d        = setupCostMtx (tcm, alphSize, 0);
    cost_matrices_2d_p costMtx2d_affine = setupCostMtx (tcm, alphSize, 2);
    cost_matrices_3d_p costMtx3d        = setupCostMtx (tcm, alphSize, 0);

    mat_setup_size (algn_mtxs2d,       seq1->len, seq2->len, 0,         0, costMtx2d->lcm);
    mat_setup_size (algn_mtxs2dAffine, seq1->len, seq2->len, 0,         0, costMtx2d_affine->lcm);
    mat_setup_size (algn_mtxs3d,       seq1->len, seq2->len, seq3->len, 0, costMtx3d->lcm);

    
    //cm_print (costMtx2d);

/**************************************************** Do 2d alignment ********************************************************/


    printf("\n\n\n******************** Align 2 sequences **********************\n");
    
    // printf("Original alignment matrix before algn_nw_2d: \n");
    // algn_print_dynmtrx_2d_2d( seq1, seq2, algn_mtxs2d );

    // the following to compute deltawh, which increases the matrix height or width in algn_nw_2d
    // This from ML:
    // TODO: figure out: does this loop, or something?
    int deltawh = 0;
    // TODO: make sure lenSeq1 > lenSeq2
    int diff = seq1->len - seq2->len;
    int lower_limit = .1 * seq1->len;
    if (deltawh) {
        deltawh = diff < lower_limit ? lower_limit : deltawh;
    } else {
        deltawh = diff < lower_limit ? lower_limit / 2 : 2;
    }
    

    int algnCost = algn_nw_2d( seq1, seq2, costMtx2d, algn_mtxs2d, deltawh ); // TODO: is

    if (DEBUG_MAT) {
        printf("\n\nFinal alignment matrix: \n\n");
        algn_print_dynmtrx_2d( seq1, seq2, algn_mtxs2d );
    }


    printf("Original 2d sequences:\n");
    seq_print(seq1, 1);
    seq_print(seq2, 2);

    backtrack_2d (seq1, seq2, retSeq1, retSeq2, algn_mtxs2d, costMtx2d, 0, 0, 1);
    printf("\nAligned 2d sequences\n");
    seq_print(retSeq1, 1);
    seq_print(retSeq2, 2);

    printf("Alignment cost: %d\n", algnCost);

    // for (SEQT *base = retSeq1->begin; base != retSeq1->end; base++) {
    //     printf("a: %c\n", *base);
    // }
    // for (SEQT *base = retSeq2->begin; base != retSeq2->end; base++) {
    //     printf("b: %s\n", base);
    // }

/************************************************ Do 2d affine alignment *****************************************************/

    /*** must have gap at start of sequence!!! ***/



    resetSeqValues(retSeq1);
    resetSeqValues(retSeq2);

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
    size_t lenSeq1 = seq_get_len(seq1);
    size_t lenSeq2 = seq_get_len(seq2);

    // reset return results
    resetSeqValues(retSeq1);
    resetSeqValues(retSeq2);

    lenLongerSeq = (lenSeq1 > lenSeq2) ? lenSeq1 : lenSeq2;


    mat_setup_size (algn_mtxs2dAffine, lenLongerSeq, lenLongerSeq, 0, 0, cm_get_lcm (costMtx2d_affine));
    matrix_2d = mat_get_2d_nwMtx (algn_mtxs2dAffine);
    precalcMtx = mat_get_2d_prec (algn_mtxs2dAffine);

    // TODO: figure out what the following seven values do/are
    //       also note the int factors, which maybe have something to do with the unexplained 12
    //       that appears in matrices.c?
    close_block_diagonal       = (int *)  matrix_2d;
    extend_block_diagonal      = (int *) (matrix_2d + (2 * lenLongerSeq));
    extend_vertical            = (int *) (matrix_2d + (4 * lenLongerSeq));
    extend_horizontal          = (int *) (matrix_2d + (6 * lenLongerSeq));
    final_cost_matrix          = (int *) (matrix_2d + (8 * lenLongerSeq));
    gap_open_prec              = (int *) (matrix_2d + (10 * lenLongerSeq));
    s_horizontal_gap_extension = (int *) (matrix_2d + (12 * lenLongerSeq));



    // TODO: empty_medianSeq might not be necessary, as it's unused in ml code:
    size_t medianSeqLen    = lenSeq1 + lenSeq2 + 2;  // 2 because that's how it is in ML code
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
    seq_print(seq1, 1);
    seq_print(seq2, 2);


    if (lenSeq1 <= lenSeq2) {
        if (DEBUG_AFFINE) printf("seq 1 is shorter, s1: %zu, s2: %zu\n", lenSeq1, lenSeq2);

        cm_precalc_4algn(costMtx2d_affine, algn_mtxs2dAffine, seq1);

        // TODO: consider moving all of this into algn.
        //       the following three fns were initially not declared in algn.h
        initialize_matrices_affine (costMtx2d_affine->gap_open, seq1, seq2, costMtx2d_affine, close_block_diagonal, 
                                    extend_block_diagonal, extend_vertical, extend_horizontal, 
                                    final_cost_matrix, direction_matrix, precalcMtx);
        // shorter first TODO: is this consistent?
        algnCost = algn_fill_plane_3_affine (seq1, seq2, lenSeq1 - 1, lenSeq1 - 1, final_cost_matrix, 
                                           direction_matrix, costMtx2d_affine, extend_horizontal, extend_vertical, 
                                           close_block_diagonal, extend_block_diagonal, precalcMtx, gap_open_prec, 
                                           s_horizontal_gap_extension);
        // shorter first TODO: fix this to make it consistent
        backtrace_affine (direction_matrix, seq1, seq2, medianSeq, empty_medianSeq,
                          retSeq1, retSeq2, costMtx2d_affine);
    } else {
        if (DEBUG_AFFINE) printf("seq 2 is shorter, s1: %zu, s2: %zu\n", lenSeq1, lenSeq2);
    
        cm_precalc_4algn(costMtx2d_affine, algn_mtxs2dAffine, seq2);

        initialize_matrices_affine(costMtx2d_affine->gap_open, seq2, seq1, costMtx2d_affine, close_block_diagonal, 
                                   extend_block_diagonal, extend_vertical, extend_horizontal, 
                                   final_cost_matrix, direction_matrix, precalcMtx);

        printf("close_block_diagonal      : %d\n", *close_block_diagonal      );
        printf("extend_block_diagonal     : %d\n", *extend_block_diagonal     );
        printf("extend_vertical           : %d\n", *extend_vertical           );
        printf("extend_horizontal         : %d\n", *extend_horizontal         );
        printf("final_cost_matrix         : %d\n", *final_cost_matrix         );
        printf("gap_open_prec             : %d\n", *gap_open_prec             );
        printf("s_horizontal_gap_extension: %d\n\n", *s_horizontal_gap_extension);

        // for (int *i = matrix_2d, j = 0; i < matrix_2d + algn_mtxs2dAffine->len; i++, j++) {
        //     printf("%d, ", *i);
        //     if (j % (lenLongerSeq ) == 0) {
        //         printf("\n");
        //     }
            
        // }
        
        algnCost = algn_fill_plane_3_affine (seq2, seq1, lenSeq2 - 1, lenSeq1 - 1, final_cost_matrix, 
                                             direction_matrix, costMtx2d_affine, extend_horizontal, extend_vertical, 
                                             close_block_diagonal, extend_block_diagonal, precalcMtx, gap_open_prec, 
                                             s_horizontal_gap_extension);
        if (DEBUG_MAT) {
            printf("\n\nFinal alignment matrix, affine: \n\n");
            algn_print_dynmtrx_2d( seq1, seq2, algn_mtxs2dAffine );
        }

        // shorter first
        backtrace_affine (direction_matrix, seq2, seq1, medianSeq, empty_medianSeq,
                          retSeq2, retSeq1, costMtx2d_affine);
    }
 

    printf("\nAligned affine 2d sequences\n");
    seq_print(retSeq1, 1);
    seq_print(retSeq2, 2);

    printf("\nAlignment cost: %d\n", algnCost);



/************************************************ Do 3d alignment *************************************************/


    printf("\n\n\n******************** Align 3 sequences **********************\n\n");

    // must first reset values in retSeq1 and retSeq2
    resetSeqValues(retSeq1);
    resetSeqValues(retSeq2);

    algn_nw_3d( seq1, seq2, seq3, costMtx3d, algn_mtxs3d, deltawh );
    //printf("Final alignment matrix: \n");
    //algn_print_dynmtrx_2d_2d( seq1, seq2, algn_mtxs3d );

    printf("Original 3d sequences:\n");
    seq_print(seq1, 1);
    seq_print(seq2, 2);
    seq_print(seq3, 3);

    backtrack_3d (seq1, seq2, seq3, retSeq1, retSeq2, retSeq3, algn_mtxs3d, costMtx3d);

    printf("\nAligned 3d sequences:\n");
    seq_print(retSeq1, 1);
    seq_print(retSeq2, 2);
    seq_print(retSeq3, 3);

    printf("\n\n\n");

    // for (SEQT *base = retSeq1->begin; base != retSeq1->end; base++) {
    //     printf("a: %c\n", *base);
    // }
    // for (SEQT *base = retSeq2->begin; base != retSeq2->end; base++) {
    //     printf("b: %s\n", base);
    // }


    return 1;
}
