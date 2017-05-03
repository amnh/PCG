#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "algn.h"
#include "c_code_alloc_setup.h"
#include "debug_constants.h"
// #include "costMatrix.h"
#include "nwMatrices.h"
#include "ukkCheckp.h"
#include "ukkCommon.h"


/** Aligns two sequences using non-affine algorithm.
 *  Takes in two arrays of integer values, as well as two previously allocated
 *  sequences.
 *
 */
int align2d(int* seq1vals, size_t lenSeq1,
            int* seq2vals, size_t lenSeq2,
            dyn_char_p seq1, dyn_char_p seq2,
            cost_matrices_2d_p costMtx2d);

int align2dAffine(int* seq1vals, size_t lenSeq1,
                  int* seq2vals, size_t lenSeq2,
                  dyn_char_p seq1, dyn_char_p seq2,
                  cost_matrices_2d_p costMtx2d) {

    const size_t SEQ_CAPACITY = lenSeq1 * lenSeq2;
    int *longInputSeq, *shortInputSeq;
    int longSeqLen, shortSeqLen;
    dyn_char_p longSeq, shortSeq;
    int swapped = 0;
    if (lenSeq1 > lenSeq2) {
        longInputSeq  = seq1;
        shortInputSeq = seq2;
        longSeqLen    = lenSeq1;
        shortSeqLen   = lenSeq2;
        longSeq       = seq1;
        shortSeq      = seq2;
        swapped       = 1;
    } else {
        longInputSeq  = seq2;
        shortInputSeq = seq1;
        longSeqLen    = lenSeq2;
        shortSeqLen   = lenSeq1;
        longSeq       = seq2;
        shortSeq      = seq1;
    }

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

    DIRECTION_MATRIX *direction_matrix;

    initializeSeq(longSeq, SEQ_CAPACITY);
    setSeq(longInputSeq, longSeqLen, longSeq);
    initializeSeq(shortSeq, SEQ_CAPACITY);
    setSeq(shortInputSeq, shortSeqLen, shortSeq);

    dyn_char_p retLongSeq          = initializeSeq(SEQ_CAPACITY);
    dyn_char_p retShortSeq         = initializeSeq(SEQ_CAPACITY);

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

    nw_matrices_p algn_mtxs2d = initializeNWMtx(longSeq->len, shortSeq->len, 0, costMtx2d->lcm);

    // TODO: empty_medianSeq might not be necessary, as it's unused in ml code:
    size_t medianSeqLen          = lenLongSeq + lenShortSeq + 2;  // 2 because that's how it is in ML code
    dyn_char_p empty_medianSeq        = malloc( sizeof(struct seq) );
    empty_medianSeq->cap         = medianSeqLen;
    empty_medianSeq->array_head  = calloc( medianSeqLen, sizeof(elem_t));
    empty_medianSeq->len         = 0;
    empty_medianSeq->seq_begin   = empty_medianSeq->end = empty_medianSeq->array_head + medianSeqLen;

    dyn_char_p medianSeq              = malloc( sizeof(struct seq) );
    medianSeq->cap               = medianSeqLen;
    medianSeq->array_head        = calloc( medianSeqLen, sizeof(elem_t));
    medianSeq->len               = 0;
    medianSeq->seq_begin         = medianSeq->end = medianSeq->array_head + medianSeqLen;

    direction_matrix             = mat_get_2d_direct (algn_mtxs2d);


    cm_precalc_4algn(costMtx2d_affine, algn_mtxs2dAffine, longSeq);

    // TODO: consider moving all of this into algn.
    //       the following three fns were initially not declared in algn.h
    algn_initialize_matrices_affine (costMtx2d_affine->gap_open, shortSeq, longSeq,
                                     costMtx2d_affine,
                                     close_block_diagonal, extend_block_diagonal,
                                     extend_vertical, extend_horizontal,
                                     final_cost_matrix, direction_matrix, precalcMtx);

    int algnCost = algn_fill_plane_3_affine (shortSeq,
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

    algn_backtrace_affine (shortSeq,
                           longSeq,
                           direction_matrix,
                           medianSeq,
                           empty_medianSeq,
                           retShortSeq,
                           retLongSeq,
                           costMtx2d);

    freeSeq(empty_medianSeq);
    freeSeq(medianSeq);
}

int align3d(int* seq1vals, size_t lenSeq1,
            int* seq2vals, size_t lenSeq2,
            int* seq3vals, size_t lenSeq3,
            dyn_char_p seq1, dyn_char_p seq2, dyn_char_p seq3
            cost_matrices_3d_p costMtx3d) {

    nw_matrices_p algn_mtxs3d       = initializeNWMtx(longSeq->len, middleSeq->len, shortSeq->len, costMtx3d->lcm);

    // last three parameters are mismatchCost, gapOpenCost, gapExtendCost
    // They're here because of an assert statement in
    int algnCost = powell_3D_align (shortSeq,   middleSeq,    longSeq,
                                    retLongSeq, retMiddleSeq, retShortSeq,
                                    1, 0, 2);
}



/************************************************ Do 3d alignment *************************************************/

    if (DO_3D) {


        // short input, middle input, long input
        // short return, middle return, long return
        // sub, gap open, gap extend
        algnCost = powell_3D_align (shortSeq,    middleSeq,    longSeq,
                                    retLongSeq, retMiddleSeq, retShortSeq,
                                    1, 0, 2);

        //algn_backtrace_3d (longSeq, middleSeq, shortSeq, retLongSeq, retMiddleSeq, retShortSeq, costMtx3d, algn_mtxs3d);

        printf("\n\nAligned 3d sequences:\n");
        dyn_char_print(retLongSeq,   1);
        dyn_char_print(retMiddleSeq, 2);
        dyn_char_print(retShortSeq,  3);

        printf("\nAlignment cost: %d\n", algnCost);

        printf("\n\n\n");

        // for (elem_t *base = retLongSeq->seq_begin; base != retLongSeq->end; base++) {
        //     printf("a: %c\n", *base);
        // }
        // for (elem_t *base = retShortSeq->seq_begin; base != retShortSeq->end; base++) {
        //     printf("b: %s\n", base);
        // }
    }

    // Next this: algn_get_median_3d (dyn_char_p seq1, dyn_char_p seq2, dyn_char_p seq3,
    //                cost_matrices_3d_p m, dyn_char_p sm)

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
