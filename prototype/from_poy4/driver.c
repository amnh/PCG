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

    for (SEQT base1 = 1; base1 <= 31; base1++) {
        for (SEQT base2 = 1; base2 <= 31; base2++) {
            median_12 = base1 | base2;
            if (is_2d) {
                *min_1    = INT_MAX; // this should be largest integer value.
                *max_1    = 0;
                *median_1 = 0;
                setCosts(tcm, alphSize, retMtx->lcm, base1, base2, min_1, max_1, median_1);
                // if (*min_1 != 0) {
                //     printf("base1: %2hhu,    median_23: %2d,    min: %d,    max: %d,    median: %2d\n", 
                //            base1, median_23, *min_1, *max_1, *median_1);
                // }

                *min_2    = INT_MAX;
                *max_2    = 0;
                *median_2 = 0;
                setCosts(tcm, alphSize, retMtx->lcm, base2, base1, min_2, max_2, median_2);

                min_2d    = *min_1 + *min_2;
                median_2d = *median_1 | *median_2;
                // if (min_2d != 0) {
                //     printf("base1: %2hhu,   base2: %2hhu,   base3: %2hhu,    median: %2d,    min: %d,    max: %d\n", 
                //            base1, base2, base3, median_2d, min_2d, max_2d);
                // }
                max_2d    = *max_1 + *max_2; 
                
                // printf("base1: %2hhu,    base2: %2hhu,    base3: %2hhu\n", base1, base2, base3);
                // printf("median: %2d,    min: %2d\n", median_2d, min_2d);
                cm_set_cost_2d   (base1, base2, min_2d,    (cost_matrices_2d_p) retMtx);
                cm_set_median_2d (base1, base2, median_2d, (cost_matrices_2d_p) retMtx);
                cm_set_worst     (base1, base2, max_2d,    (cost_matrices_2d_p) retMtx);
            } else {
                for (SEQT base3 = 1; base3 <= 31; base3++) {
                    median_23 = base2 | base3;
                    median_13 = base1 | base3;

                    *min_1    = INT_MAX; // this should be largest integer value.
                    *max_1    = 0;
                    *median_1 = 0;
                    setCosts(tcm, alphSize, retMtx->lcm, base1, median_23, min_1, max_1, median_1);
                    // if (*min_1 != 0) {
                    //     printf("base1: %2hhu,    median_23: %2d,    min: %d,    max: %d,    median: %2d\n", 
                    //            base1, median_23, *min_1, *max_1, *median_1);
                    // }

                    *min_2    = INT_MAX;
                    *max_2    = 0;
                    *median_2 = 0;
                    setCosts(tcm, alphSize, retMtx->lcm, base2, median_13, min_2, max_2, median_2);
                    // if (*min_2 != 0) {
                    //     printf("base2: %2hhu,    median_13: %2d,    min: %d,    max: %d,    median: %2d\n", 
                    //            base2, median_13, *min_2, *max_2, *median_2);
                    // }

                    *max_3    = 0;
                    *min_3    = INT_MAX; 
                    *median_3 = 0;
                    setCosts(tcm, alphSize, retMtx->lcm, base3, median_12, min_3, max_3, median_3);
                    // if (*min_3 != 0) {
                    //     printf("base3: %2hhu,    median_12: %2d,    min: %d,    max: %d,    median: %2d\n", 
                    //            base3, median_12, *min_3, *max_3, *median_3);
                    // }

                    min_3d    = *min_1 + *min_2 + *min_3;
                    median_3d = *median_1 | *median_2 | *median_3;

                    printf("base1: %2hhu,    base2: %2hhu,    base3: %2hhu    3d median: %2d,    3d cost: %2d\n", 
                               base1, base2, base3, median_3d, min_3d);
                    
                    cm_set_cost_3d   (base1, base2, base3, min_3d,    (cost_matrices_3d_p) retMtx);
                    cm_set_median_3d (base1, base2, base3, median_3d, (cost_matrices_3d_p) retMtx);
                    // no worst in 3d

                    // printf("base1: %hhu, base2: %hhu, base3:\n", base1, base2);
                    
                    
                    // printf("cost_2d: %d, min: %d, max: %d, base1: %hhu, base2: %hhu, median: %d\n", cost, min, max, base1, base2, median );
                } // base3
            }
        } // base2
    } // base1

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

int main() {

    const int DO_2D  = 1;
    const int DO_AFF = 0;
    const int DO_3D  = 1;

    const int IDENTITY_COST = 0;
    const int INDEL_COST    = 100;
    const int SUB_COST      = 1;

/******************************** set up and allocate all variables and structs ************************************/


/****************  Allocate sequences  ****************/

        //***** for following seqs, affine requires gap at start of sequence!!! *****/

    int alphSize = 5; // includes gap, but no ambiguities

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
        mat_setup_size (algn_mtxs2d, seq1->len, seq2->len, 0, 0, costMtx2d->lcm);
    }
    if (DO_AFF) {
        costMtx2d_affine = setupCostMtx (tcm, alphSize, 2, 1);
        mat_setup_size (algn_mtxs2dAffine, seq1->len, seq2->len, 0, 0, costMtx2d_affine->lcm);
    }
    if (DO_3D) {
        costMtx3d = setupCostMtx (tcm, alphSize, 0, 0);
        // penultimate parameter is ukk flag
        mat_setup_size (algn_mtxs3d, seq1->len, seq2->len, seq3->len, 0, costMtx3d->lcm);
    }
    int algnCost;

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


    //cm_print (costMtx3d);

/**************************************************** Do 2d alignment ********************************************************/

    if (DO_2D ) {
        printf("\n\n\n******************** Align 2 sequences **********************\n");

        // printf("Original alignment matrix before algn_nw_2d: \n");
        // algn_print_dynmtrx_2d_2d( seq1, seq2, algn_mtxs2d );

        algnCost = algn_nw_2d( seq1, seq2, costMtx2d, algn_mtxs2d, deltawh ); // TODO: is

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
    }



/************************************************ Do 2d affine alignment *****************************************************/

    /*** must have gap at start of sequence!!! ***/

    if (DO_AFF) {

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

        seq_p longerSequence = lenSeq1 > lenSeq2 ? seq1 : seq2;
        seq_p lesserSequence = lenSeq1 > lenSeq2 ? seq2 : seq1;

        cm_precalc_4algn(costMtx2d_affine, algn_mtxs2dAffine, longerSequence);

        // TODO: consider moving all of this into algn.
        //       the following three fns were initially not declared in algn.h
        initialize_matrices_affine (costMtx2d_affine->gap_open, lesserSequence, longerSequence, 
                                    costMtx2d_affine, close_block_diagonal,
                                    extend_block_diagonal, extend_vertical, extend_horizontal,
                                    final_cost_matrix, direction_matrix, precalcMtx);

        printf("\n");
        printf("close_block_diagonal      : %d\n", *close_block_diagonal      );
        printf("extend_block_diagonal     : %d\n", *extend_block_diagonal     );
        printf("extend_vertical           : %d\n", *extend_vertical           );
        printf("extend_horizontal         : %d\n", *extend_horizontal         );
        printf("final_cost_matrix         : %d\n", *final_cost_matrix         );
    //    printf("gap_open_prec             : %d\n", *gap_open_prec             );
    //    printf("s_horizontal_gap_extension: %d\n", *s_horizontal_gap_extension);
        printf("\n");

       // for (int *i = matrix_2d, j = 0; i < matrix_2d + algn_mtxs2dAffine->len; i++, j++) {
       //     printf("%d, ", *i);
       //     if (j % (lenLongerSeq ) == 0) {
       //         printf("\n");
       //     }
       // }


        // shorter first TODO: is this consistent?
        algnCost = algn_fill_plane_3_affine (lesserSequence, longerSequence, 
                                             lesserSequence->len - 1, longerSequence->len - 1,
                                             final_cost_matrix, direction_matrix, costMtx2d_affine, 
                                             extend_horizontal, extend_vertical,
                                             close_block_diagonal, extend_block_diagonal, 
                                             precalcMtx, gap_open_prec,
                                             s_horizontal_gap_extension);



        if (DEBUG_MAT) {
            printf("\n\nFinal alignment matrix, affine: \n\n");
            algn_print_dynmtrx_2d( seq1, seq2, algn_mtxs2dAffine );
        }


        // shorter first TODO: fix this to make it consistent
        backtrace_affine (direction_matrix, lesserSequence, longerSequence, medianSeq, empty_medianSeq,
                          retSeq1, retSeq2, costMtx2d_affine);

        printf("\nAligned affine 2d sequences\n");
        if (lenSeq1 > lenSeq2) {
          seq_print(retSeq2, 1);
          seq_print(retSeq1, 2);
        } else {
          seq_print(retSeq1, 1);
          seq_print(retSeq2, 2);
        }

        printf("\nAlignment cost: %d\n", algnCost);
    }


/************************************************ Do 3d alignment *************************************************/

    if (DO_3D) {

        printf("\n\n\n******************** Align 3 sequences **********************\n\n");

        // must first reset values in retSeq1 and retSeq2
        resetSeqValues(retSeq1);
        resetSeqValues(retSeq2);

        algnCost = algn_nw_3d( seq1, seq2, seq3, costMtx3d, algn_mtxs3d, deltawh );
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

        printf("\nAlignment cost: %d\n", algnCost);

        printf("\n\n\n");

        // for (SEQT *base = retSeq1->begin; base != retSeq1->end; base++) {
        //     printf("a: %c\n", *base);
        // }
        // for (SEQT *base = retSeq2->begin; base != retSeq2->end; base++) {
        //     printf("b: %s\n", base);
        // }
    }


    return 1;
}
