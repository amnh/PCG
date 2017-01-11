#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "seqAlign.h"
#include "c_code_alloc_setup.h"
#include "debug_constants.h"
// #include "costMatrix.h"
#include "nwMatrices.h"
#include "ukkCheckp.h"
#include "ukkCommon.h"

// #define SEQ_CAPACITY 64

int power_2 (int input) {
    if (input == 1)  return 1;
    if (input == 2)  return 1;
    if (input == 4)  return 1;
    if (input == 8)  return 1;
    if (input == 16) return 1;
    return 0;
}



void setSeq(const int *vals, size_t length, seq_p retSeq) {
    // assign sequence into sequence struct
    retSeq->len = length;
    retSeq->seq_begin = retSeq->end - length;
    if (length > 0) {
        for(size_t i = 0; i < length; i++) {
            retSeq->seq_begin[i] = (int) vals[i];
        }
    }
}



int main() {


/******************************** set up and allocate all variables and structs ************************************/


/****************  Allocate sequences  ****************/

        //***** for following seqs, affine requires gap at start of sequence!!! *****/

    const int longSeqLen   = 22;
    const int middleSeqLen = 18;
    const int shortSeqLen  = 17;

    const size_t SEQ_CAPACITY = longSeqLen + shortSeqLen + middleSeqLen;


    int alphSize = 5; // includes gap, but no ambiguities
    int longest_vals [longSeqLen]   = {16, 2, 1, 8, 4, 2, 1, 8, 4, 1, 1, 1, 1, 1, 2, 1, 8, 4, 2, 1, 8, 4}; // don't forget to change lengths!!!
    int middle_vals  [middleSeqLen] = {16, 8, 8, 2, 1, 8, 4, 2, 1, 8, 4, 1, 1, 2, 1, 8, 4, 1};             // don't forget to change lengths!!!
    int shortest_vals[shortSeqLen]  = {16, 2, 1, 8, 4, 2, 1, 8, 4, 2, 1, 8, 4, 2, 1, 8, 4};                // don't forget to change lengths!!!



    seq_p shortSeq     = malloc(sizeof(struct seq));
    initializeSeq(shortSeq, SEQ_CAPACITY);
    setSeq(shortest_vals, shortSeqLen, shortSeq);

    seq_p middleSeq    = malloc(sizeof(struct seq));
    initializeSeq(middleSeq, SEQ_CAPACITY);
    setSeq(middle_vals, middleSeqLen, middleSeq);

    seq_p longSeq      = malloc(sizeof(struct seq));
    initializeSeq(longSeq, SEQ_CAPACITY);
    setSeq(longest_vals, longSeqLen, longSeq);

    seq_p retShortSeq  = malloc( sizeof(struct seq) );
    initializeSeq(retShortSeq,  SEQ_CAPACITY);

    seq_p retMiddleSeq = malloc( sizeof(struct seq) );
    initializeSeq(retMiddleSeq, SEQ_CAPACITY);

    seq_p retLongSeq   = malloc( sizeof(struct seq) );
    initializeSeq(retLongSeq,   SEQ_CAPACITY);






/************  Allocate cost matrices  **************/

    size_t tcm_total_len = alphSize * alphSize; // the size of the input tcm


    // !!!!! if modifying this code, also make sure to change is_metric !!!!!!
    /** TCM is only for non-ambiguous nucleotides, and it used to generate
     *  the entire cost matrix, which includes ambiguous elements.
     *  TCM is row-major, with each row being the left sequence element.
     *  It is therefore indexed not by powers of two, but by cardinal integer.
     *  This particular example is both metric and symmetric. All TCMs must be
     *  symmetric. Metricity is decided by PCG application.
     */
    int *tcm = calloc(tcm_total_len, sizeof(int)); // this is the input tcm, not the generated one
    for (size_t i = 0; i < tcm_total_len; i += alphSize) {
        //printf("i: %zu\n", i);
        for (size_t j = 0; j < alphSize; j++) {
            //printf("i: %zu, j: %zu, cost: %lu\n", i, j, 2 * i + 2 * j);
            //tcm[i + j] = 2 * i + 2 * j;
            if ( i == j * alphSize ) {
                tcm[i + j] = IDENTITY_COST;    // identity
            } else if (i == (tcm_total_len - alphSize) || j == (alphSize - 1)) {
                tcm[i + j] = INDEL_COST;       // indel cost
            } else {
                tcm[i + j] = SUB_COST;         // sub cost
            }
         }
    }

    // Print TCM in pretty format
    const int n = alphSize;
    for (size_t i = 0; i < n; ++i) {
        for (size_t j = 0; j < n; ++j) {
            printf("%2d ", tcm[ n*i + j ]);
        }
        printf("\n");
    }


    cost_matrices_2d_p costMtx2d        = malloc(sizeof(struct cost_matrices_2d));
    cost_matrices_2d_p costMtx2d_affine = malloc(sizeof(struct cost_matrices_2d));
    cost_matrices_3d_p costMtx3d        = malloc(sizeof(struct cost_matrices_3d));
    cost_matrices_3d_p costMtx3d_affine = malloc(sizeof(struct cost_matrices_3d));

    // tcm is tcm; alphSize includes gap; third param is gap opening cost
    setup2dCostMtx (tcm, alphSize, 0,             costMtx2d);
    setup2dCostMtx (tcm, alphSize, GAP_OPEN_COST, costMtx2d_affine);
    setup3dCostMtx (tcm, alphSize, 0,             costMtx3d);
    setup3dCostMtx (tcm, alphSize, GAP_OPEN_COST, costMtx3d_affine);
    //cm_print_2d (costMtx2d);

    /****************  Allocate NW matrices  ****************/
    // in following, penultimate parameter was ukk flag, used only to set up 3d matrices.
    nw_matrices_p algn_mtxs2d       = malloc(sizeof(struct nwMatrices));
    nw_matrices_p algn_mtxs2dAffine = malloc(sizeof(struct nwMatrices));
    nw_matrices_p algn_mtxs3d       = malloc(sizeof(struct nwMatrices));
    nw_matrices_p algn_mtxs3dAffine = malloc(sizeof(struct nwMatrices));

    if (DO_2D) {
        initializeNWMtx(longSeq->len, shortSeq->len,  0,             costMtx2d->lcm,        algn_mtxs2d);
    }
    if (DO_2D_AFF) {
        initializeNWMtx(longSeq->len, shortSeq->len,  0,             costMtx2d_affine->lcm, algn_mtxs2dAffine);
    }
    if (DO_3D) {
        initializeNWMtx(longSeq->len, middleSeq->len, shortSeq->len, costMtx3d->lcm,        algn_mtxs3d);
    }
    if (DO_3D_AFF) {
        initializeNWMtx(longSeq->len, middleSeq->len, shortSeq->len, costMtx3d_affine->lcm, algn_mtxs3dAffine);
    }

    int algnCost;

    /**
    // Print TCM in pretty format
    const int n = costMtx2d->lcm;
    for (size_t i = 0; i < n; ++i) {
        for (size_t j = 0; j < n; ++j) {
            printf("%2d ", tcm[ n*i + j ]);
        }
        printf("\n");
    }
    **/

    // the following to compute deltawh, which increases the matrix height or width in algn_nw_2d
    // pulled from ML code
    // deltawh is for use in Ukonnen, it gives the current necessary width of the Ukk matrix
    int deltawh     = 0;
    int diff        = longSeq->len - shortSeq->len;
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

        algnCost = algn_nw_2d( shortSeq, longSeq, costMtx2d, algn_mtxs2d, deltawh );

        if (DEBUG_MAT) {
            printf("\n\nFinal alignment matrix: \n\n");
            algn_print_dynmtrx_2d( longSeq, shortSeq, algn_mtxs2d );
        }


        printf("Original 2d sequences:\n");
        seq_print(longSeq, 1);
        seq_print(shortSeq, 2);

        algn_backtrace_2d (shortSeq, longSeq, retShortSeq, retLongSeq, algn_mtxs2d, costMtx2d, 0, 0, 1);
        printf("\nAligned 2d sequences\n");
        seq_print(retLongSeq, 1);
        seq_print(retShortSeq, 2);

        printf("\nAlignment cost: %d\n", algnCost);

        /****  Now get alignments  ****/

        printf("\nAligned sequences:\n");
        //int *algnSeqVals = calloc(retLongSeq->len, sizeof(int));
        seq_p algnSeq = malloc( sizeof(struct seq) );;
        initializeSeq(algnSeq, SEQ_CAPACITY);
        //free (algnSeqVals);
        resetSeqValues(algnSeq);

        // union:
        algn_union (retShortSeq, retLongSeq, algnSeq);
        printf("  Unioned sequence\n  ");
        seq_print(algnSeq, 0);

        // ungapped:
        resetSeqValues(algnSeq);
        algn_get_median_2d_no_gaps (retShortSeq, retLongSeq, costMtx2d, algnSeq);
        printf("\n  Median without gaps\n  ");
        seq_print(algnSeq, 0);

        // gapped:
        resetSeqValues(algnSeq);
        algn_get_median_2d_with_gaps (retShortSeq, retLongSeq, costMtx2d, algnSeq);
        printf("\n  Median with gaps\n  ");
        seq_print(algnSeq, 0);

        free (algnSeq);
    }



/************************************************ Do 2d affine alignment *****************************************************/

    /*** must have gap at start of sequence!!! ***/

    if (DO_2D_AFF) {
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
        int *gap_open_prec;                 // precalculated gap opening value (top row of nw matrix)
        int *s_horizontal_gap_extension;    //
        int lenLongerSeq;                   //

        DIR_MTX_ARROW_t *direction_matrix;
        size_t lenLongSeq  = seq_get_len(longSeq);
        size_t lenShortSeq = seq_get_len(shortSeq);
        lenLongerSeq = (lenLongSeq > lenShortSeq) ? lenLongSeq : lenShortSeq;

        //    mat_setup_size (algn_mtxs2dAffine, lenLongerSeq, lenLongerSeq, 0, 0, cm_get_lcm (costMtx2d_affine));
        matrix_2d  = mat_get_2d_nwMtx (algn_mtxs2dAffine);
        precalcMtx = mat_get_2d_prec  (algn_mtxs2dAffine);

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



        // TODO: ungappedMedSeq might not be necessary, as it's unused in ml code:
        size_t medianSeqLen          = lenLongSeq + lenShortSeq + 2;  // 2 because that's how it is in ML code
        seq_p gappedMedSeq        = malloc( sizeof(struct seq) );
        gappedMedSeq->cap         = medianSeqLen;
        gappedMedSeq->array_head  = calloc( medianSeqLen, sizeof(SEQT));
        gappedMedSeq->len         = 0;
        gappedMedSeq->seq_begin   = gappedMedSeq->end = gappedMedSeq->array_head + medianSeqLen;

        seq_p ungappedMedSeq              = malloc( sizeof(struct seq) );
        ungappedMedSeq->cap               = medianSeqLen;
        ungappedMedSeq->array_head        = calloc( medianSeqLen, sizeof(SEQT));
        ungappedMedSeq->len               = 0;
        ungappedMedSeq->seq_begin         = ungappedMedSeq->end = ungappedMedSeq->array_head + medianSeqLen;

        direction_matrix             = mat_get_2d_direct (algn_mtxs2dAffine);

        printf("\n\n\n***************** Align 2 sequences affine ********************\n\n");

        printf("Original affine 2d sequences:\n");

        // seq_p longerSequence = lenLongSeq > lenShortSeq ? longSeq : shortSeq;
        // seq_p shorterSequence = lenLongSeq > lenShortSeq ? shortSeq : longSeq;

        seq_print(longSeq,  1);
        seq_print(shortSeq, 2);

        cm_precalc_4algn(costMtx2d_affine, algn_mtxs2dAffine, longSeq);

        // TODO: consider moving all of this into algn.
        //       the following three fns were initially not declared in algn.h
        algn_initialize_matrices_affine (costMtx2d_affine->gap_open,
                                         shortSeq,
                                         longSeq,
                                         costMtx2d_affine,
                                         close_block_diagonal,
                                         extend_block_diagonal,
                                         extend_vertical,
                                         extend_horizontal,
                                         final_cost_matrix,
                                         direction_matrix,
                                         precalcMtx);

        if (DEBUG_AFFINE) {
            printf("\n");
            printf("close_block_diagonal      : %d\n", *close_block_diagonal      );
            printf("extend_block_diagonal     : %d\n", *extend_block_diagonal     );
            printf("extend_vertical           : %d\n", *extend_vertical           );
            printf("extend_horizontal         : %d\n", *extend_horizontal         );
            printf("final_cost_matrix         : %d\n", *final_cost_matrix         );
            printf("gap_open_prec             : %d\n", *gap_open_prec             );
            printf("s_horizontal_gap_extension: %d\n", *s_horizontal_gap_extension);
            printf("\n");
        }

       // for (int *i = matrix_2d, j = 0; i < matrix_2d + algn_mtxs2dAffine->len; i++, j++) {
       //     printf("%d, ", *i);
       //     if (j % (lenLongerSeq ) == 0) {
       //         printf("\n");
       //     }
       // }


        // shorter first
        // TODO: why isn't this argument order consistent with next fn call?
        algnCost = algn_fill_plane_2d_affine (shortSeq,
                                             longSeq,
                                             shortSeq->len - 1,
                                             longSeq->len - 1,
                                             final_cost_matrix,
                                             direction_matrix,
                                             costMtx2d_affine,
                                             extend_horizontal,
                                             extend_vertical,
                                             close_block_diagonal,
                                             extend_block_diagonal,
                                             precalcMtx,
                                             gap_open_prec,
                                             s_horizontal_gap_extension);



        if (DEBUG_MAT) {
            printf("\n\nFinal alignment matrix, affine: \n\n");
            algn_print_dynmtrx_2d( shortSeq, longSeq, algn_mtxs2dAffine );
        }


        // shorter first
        // TODO: fix this to make it consistent
        algn_backtrace_affine (shortSeq,
                               longSeq,
                               direction_matrix,
                               ungappedMedSeq,
                               gappedMedSeq,
                               retShortSeq,
                               retLongSeq,
                               costMtx2d_affine);

        printf("\nAligned affine 2d sequences\n");
        if (lenLongSeq > lenShortSeq) {
          seq_print(retShortSeq, 1);
          seq_print(retLongSeq, 2);
        } else {
          seq_print(retLongSeq, 1);
          seq_print(retShortSeq, 2);
        }


        printf("\nAlignment cost: %d\n", algnCost);

        // ungapped:
        printf("\n  Median without gaps\n  ");
        seq_print(ungappedMedSeq, 0);

        // gapped:
        printf("\n  Median with gaps\n  ");
        seq_print(gappedMedSeq, 0);

        freeSeq(gappedMedSeq);
        freeSeq(ungappedMedSeq);

    }


/************************************************ Do 3d alignment *************************************************/

    if (DO_3D) {

        printf("\n\n\n******************** Align 3 sequences **********************\n\n");

        // must first reset values in retLongSeq and retShortSeq
        resetSeqValues(retLongSeq);
        resetSeqValues(retShortSeq);

        // algnCost = algn_nw_3d (longSeq, middleSeq, shortSeq, costMtx3d, algn_mtxs3d, deltawh);
        //printf("Final alignment matrix: \n");
        //algn_print_dynmtrx_2d_2d( longSeq, shortSeq, algn_mtxs3d );

        printf("Original 3d sequences:\n");
        seq_print(longSeq,   1);
        seq_print(middleSeq, 2);
        seq_print(shortSeq,  3);
        printf("\n");

        // short input, middle input, long input
        // short return, middle return, long return
        // sub, gap open, gap extend
        algnCost = powell_3D_align (shortSeq,    middleSeq,    longSeq,
                                    retLongSeq, retMiddleSeq, retShortSeq,
                                    1, 2, 1);

        // algn_backtrace_3d(shortSeq, middleSeq, longSeq,
        //                   retShortSeq, retMiddleSeq, retLongSeq,
        //                   costMtx3d, algn_nw_3d);

        //algn_backtrace_3d (longSeq, middleSeq, shortSeq, retLongSeq, retMiddleSeq, retShortSeq, costMtx3d, algn_mtxs3d);

        printf("\n\nAligned 3d sequences:\n");
        seq_print(retLongSeq,   1);
        seq_print(retMiddleSeq, 2);
        seq_print(retShortSeq,  3);

        printf("\nAlignment cost: %d\n", algnCost);

        printf("\n\n\n");

        // for (SEQT *base = retLongSeq->seq_begin; base != retLongSeq->end; base++) {
        //     printf("a: %c\n", *base);
        // }
        // for (SEQT *base = retShortSeq->seq_begin; base != retShortSeq->end; base++) {
        //     printf("b: %s\n", base);
        // }
    }

    // Next this: algn_get_median_3d (seq_p seq1, seq_p seq2, seq_p seq3,
    //                cost_matrices_3d_p m, seq_p sm)

    freeCostMtx(costMtx2d,        1);  // 1 is 2d
    freeCostMtx(costMtx2d_affine, 1);
    freeCostMtx(costMtx3d,        0);  // 0 is !2d

    freeNWMtx(algn_mtxs2d);
    freeNWMtx(algn_mtxs2dAffine);
    freeNWMtx(algn_mtxs3d);

    freeSeq(longSeq);
    freeSeq(shortSeq);
    freeSeq(middleSeq);
    freeSeq(retLongSeq);
    freeSeq(retShortSeq);
    freeSeq(retMiddleSeq);

    free(tcm);

    return 0;
}
