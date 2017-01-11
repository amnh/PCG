#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "seqAlign.h"
#include "c_alignment_interface.h"
#include "c_code_alloc_setup.h"
#include "debug_constants.h"
// #include "costMatrix.h"
#include "nwMatrices.h"
//#include "ukkCheckp.h"
#include "ukkCommon.h"

void setSeq(alignIO_p input, seq_p retSeq) {
    // assign sequence into sequence struct
    retSeq->len = input->length;
    retSeq->seq_begin = retSeq->end - input->length;
    if (input->length > 0) {
        for(size_t i = 0; i < input->length; i++) {
            retSeq->seq_begin[i] = (int) input->sequence[i];
        }
    }
}

void seqToAlignIO(seq_p input, alignIO_p output) {
    output->length = input->len;
    for(int i = 0; i < output->length; i++) {
        output->sequence[i] = input->seq_begin[i];
    }
}

/** Returns true if sequence length is 0 or all characters are gaps. Otherwise returns false. */
int is_empty(seq_p seq, SEQT gap_char) {
    for (unsigned char *pos = seq->seq_begin; pos < seq->end; pos++) {
        if (gap_char != *pos) {
            return 0;
        }
    }
    return 1;
}

int compare (seq_p seq1, seq_p seq2) {
    size_t minLength = seq1->len < seq2->len ? seq1->len : seq2->len;
    size_t difference = seq1->seq_begin[0] - seq2->seq_begin[0];
    int retVal = 0;
    if (difference) {
        for (size_t count = 1; count < minLength; count++) {
            difference = seq1->seq_begin[count] - seq2->seq_begin[count];

            if( difference ) {
                retVal = difference;
                break;
            }
        }
    }
    return retVal ? retVal : seq1->len - seq2->len;
}

int align_3_powell(seq_p seq1, seq_p seq2, seq_p seq3,
                   seq_p retSeq1, seq_p retSeq2, seq_p retSeq3,
                   int mismatchCost, int gapOpenCost, int gapExtendCost) {
    size_t total_length = seq1->len + seq2->len + seq3->len;
    int cost = powell_3D_align(seq1, seq2, seq3, retSeq1, retSeq2, retSeq3, mismatchCost, gapOpenCost, gapExtendCost);
    return cost;

}

/** [align_3_powell_inter a b c cm cm3] generates the median and edit
    * cost between the three sequences seq1, seq2, and seq3, according to the cost
    * matrices specified by costMtx2d and costMtx3d. */
int align_3_powell_inter (seq_p seq1, seq_p seq2, seq_p seq3,
                          seq_p retSeq1, seq_p retSeq2, seq_p retSeq3,
                          seq_p median,
                          cost_matrices_3d_p costMtx3d) {
        SEQT gap_char = cm_get_gap_char_3d(costMtx3d);
        SEQT medianChar;
        int mismatchCost,
            gapOpenCost,
            gapExtendCost;
        switch (costMtx3d->cost_model_type) {
            case 1:
            case 2:
                gapOpenCost = 0;
                break;
            default:
                gapOpenCost = costMtx3d->gap_open;
                break;
        }
        mismatchCost  = cm_calc_cost(costMtx3d->cost, 1, 2,  costMtx3d->lcm);
        gapExtendCost = cm_calc_cost(costMtx3d->cost, 1, 16, costMtx3d->lcm);

        int cost = align_3_powell (seq1, seq2, seq3,
                                   retSeq1, retSeq2, retSeq3,
                                   mismatchCost, gapOpenCost, gapExtendCost);

        for( int pos = retSeq1->len - 1; pos >= 0; pos-- ) {
            medianChar = cm_get_median_3d(costMtx3d, seq1->seq_begin[pos], seq2->seq_begin[pos], seq3->seq_begin[pos]);
            if(medianChar != gap_char) {
                seq_prepend(median, medianChar);
            }
        }
        seq_prepend(median, gap_char);
        return cost;


}

/** [readjust_3d a b mine cm cm3 p] readjust [mine] as the median between
    * the sequences [a], [b] and [p]. The result is a triple [(ed, s, ch)],
    * where [ed] is the total edit cost of the median [s], which is in the
    * center of [a], [b], and [p], and [ch] is true iff [ch] is different from
    * [mine]. */

/** return cost, median and true/false value. isDifferent is true if the input median, curMedian is
 *  unchanged, false otherwise.
 *
 *  Initial call has first_gap = 0.
 */
int readjust_3d(int first_gap, int* isDifferent,
                seq_p seq1, seq_p seq2, seq_p seq3,
                seq_p retSeq1, seq_p retSeq2, seq_p retSeq3,
                seq_p curMedian,
                cost_matrices_3d_p costMtx3d) {
    int cost = 0;
    seq_p median = malloc(sizeof(struct seq));
    initializeSeq(median, seq1->len + seq2->len + seq3->len + 1);
    if ( seq1->len      == seq2->len &&
         curMedian->len == seq3->len &&
         seq1->len      == curMedian->len) {
        *isDifferent = 0;
        return cost; // cost == 0
    } else {
        SEQT gap_char = cm_get_gap_char_3d(costMtx3d);
        if (is_empty(seq1, gap_char) && !is_empty(seq2, gap_char)) {
            *isDifferent = 0 != compare (curMedian, seq2);
            //copy seq2 to median
            return cost; // cost == 0
        } else if (is_empty(seq2, gap_char) && !is_empty(seq1, gap_char)) {
            *isDifferent = 0 != compare(curMedian, seq1);
            //copy seq1 to median
            return cost; // cost == 0
        } else if (is_empty(seq3, gap_char)) {
            *isDifferent = 0 != compare(curMedian, seq3);
            //copy seq3 to median
            return cost; // cost == 0
        } else {
            if (first_gap) {
                cost = align_3_powell_inter(seq1, seq2, seq3,
                                            retSeq1, retSeq2, retSeq3,
                                            median,
                                            costMtx3d);
            } else {
                seq_prepend(seq1, gap_char);
                seq_prepend(seq2, gap_char);
                seq_prepend(seq3, gap_char);
                cost = align_3_powell_inter(seq1, seq2, seq3,
                                            retSeq1, retSeq2, retSeq3,
                                            median,
                                            costMtx3d);
            }
            *isDifferent = 0 != compare(curMedian, median);
            return cost;
        }
    }
}


int align2d(alignIO_p input1,
            alignIO_p input2,
            alignIO_p gappedOutputSeq,
            alignIO_p ungappedOutputSeq,
            alignIO_p unionOutputSeq,
            cost_matrices_2d_p costMtx2d,
            int doUnion,
            int doDOTraceback) {

    const size_t SEQ_CAPACITY = input1->length + input2->length + 1;

    alignIO_p longIO,
              middleIO,
              shortIO;

    seq_p longSeq     = malloc(sizeof(struct seq));
    seq_p shortSeq    = malloc(sizeof(struct seq));
    seq_p retShortSeq = malloc(sizeof(struct seq));
    seq_p retLongSeq  = malloc(sizeof(struct seq));

    initializeSeq(longSeq,     SEQ_CAPACITY);
    initializeSeq(shortSeq,    SEQ_CAPACITY);
    initializeSeq(retLongSeq,  SEQ_CAPACITY);
    initializeSeq(retShortSeq, SEQ_CAPACITY);


    int swapped = 0;

    if (input1->length > input2->length) {
        setSeq(input1, longSeq);
        longIO = input1;

        setSeq(input2, shortSeq);
        shortIO = input2;

        swapped        = 1;
    } else {
        setSeq(input2, longSeq);
        longIO = input2;

        setSeq(input1, shortSeq);
        shortIO = input1;
    }
    //initializeSeq(longSeq, SEQ_CAPACITY);
    setSeq(longIO, longSeq);
    // printf("%zu\n", longSeq->len);
    // printf("%zu\n", longSeq->cap);
    // printf("%d\n", *longSeq->array_head);
    // for(unsigned char *i = longSeq->seq_begin; i < longSeq->end; i++){
    //         printf("%d\n", *i);
    // }

    //initializeSeq(shortSeq, SEQ_CAPACITY);
    setSeq(shortIO, shortSeq);
    // printf("\n\n%zu\n", shortSeq->len);
    // printf("%zu\n", shortSeq->cap);
    // printf("%d\n", *shortSeq->array_head);
    // for(unsigned char *i = shortSeq->seq_begin; i < shortSeq->end; i++){
    //         printf("%d\n", *i);
    // }

    nw_matrices_p nw_mtxs2d = malloc(sizeof(struct nwMatrices));
    initializeNWMtx(longSeq->len, shortSeq->len, 0, costMtx2d->lcm, nw_mtxs2d);

    // deltawh is for use in Ukonnen, it gives the current necessary width of the Ukk matrix.
    // The following calculation to compute deltawh, which increases the matrix height or width in algn_nw_2d,
    // was pulled from POY ML code.
    int deltawh = 0;
    int diff = longSeq->len - shortSeq->len;
    int lower_limit = .1 * longSeq->len;
    if (deltawh) {
        deltawh = diff < lower_limit ? lower_limit : deltawh;
    } else {
        deltawh = diff < lower_limit ? lower_limit / 2 : 2;
    }
    //printf("%d, %zu, %d, %zu\n", shortSeqLen, shortSeq->len, longSeqLen, longSeq->len);
    int algnCost = algn_nw_2d( shortSeq, longSeq, costMtx2d, nw_mtxs2d, deltawh );
    if(doDOTraceback || doUnion) {
        algn_backtrace_2d (shortSeq, longSeq, retShortSeq, retLongSeq, nw_mtxs2d, costMtx2d, 0, 0, swapped);

        if(doDOTraceback) {
            seq_p ungappedMedianSeq = malloc(sizeof(struct seq));
            seq_p gappedMedianSeq   = malloc(sizeof(struct seq));
            initializeSeq(ungappedMedianSeq, SEQ_CAPACITY);
            initializeSeq(gappedMedianSeq,   SEQ_CAPACITY);

            algn_get_median_2d_no_gaps (retShortSeq, retLongSeq, costMtx2d, ungappedMedianSeq);
            algn_get_median_2d_with_gaps (retShortSeq, retLongSeq, costMtx2d, gappedMedianSeq);

            seqToAlignIO(ungappedMedianSeq, ungappedOutputSeq);
            seqToAlignIO(gappedMedianSeq,   gappedOutputSeq);

            freeSeq(ungappedMedianSeq);
            freeSeq(gappedMedianSeq);

        }
        if(doUnion) {
            seq_p unionSeq = malloc(sizeof(struct seq));
            initializeSeq(unionSeq, SEQ_CAPACITY);
            algn_union(retShortSeq, retLongSeq, unionSeq);

            seqToAlignIO(unionSeq, unionOutputSeq);
            freeSeq(unionSeq);
        }
    }


    seqToAlignIO(retLongSeq, longIO);
    seqToAlignIO(retShortSeq, shortIO);

    //freeCostMtx(costMtx2d, 1);  // 1 is 2d
    freeNWMtx(nw_mtxs2d);
    freeSeq(shortSeq);
    freeSeq(longSeq);
    freeSeq(retLongSeq);
    freeSeq(retShortSeq);

    return algnCost;

}

int align2dAffine(alignIO_p input1,
                  alignIO_p input2,
                  alignIO_p gappedOutputSeq,
                  alignIO_p ungappedOutputSeq,
                  alignIO_p unionOutputSeq,
                  cost_matrices_2d_p costMtx2d_affine,
                  int doUnion,
                  int doDOTraceback) {

    const size_t SEQ_CAPACITY = input1->length + input2->length + 2;

    alignIO_p longIO,
              middleIO,
              shortIO;


    seq_p longSeq     = malloc(sizeof(struct seq));
    seq_p shortSeq    = malloc(sizeof(struct seq));
    seq_p retShortSeq = malloc(sizeof(struct seq));
    seq_p retLongSeq  = malloc(sizeof(struct seq));

    printf("here!!\n");
    initializeSeq(longSeq,     SEQ_CAPACITY);
    initializeSeq(shortSeq,    SEQ_CAPACITY);
    initializeSeq(retLongSeq,  SEQ_CAPACITY);
    initializeSeq(retShortSeq, SEQ_CAPACITY);

    int swapped = 0;

    if (input1->length > input2->length) {
        setSeq(input1, longSeq);
        longIO = input1;

        setSeq(input2, shortSeq);
        shortIO = input2;

        swapped        = 1;
    } else {
        setSeq(input2, longSeq);
        longIO = input2;

        setSeq(input1, shortSeq);
        shortIO = input1;
    }

    //initializeSeq(longSeq, SEQ_CAPACITY);
    setSeq(longIO, longSeq);
    //initializeSeq(shortSeq, SEQ_CAPACITY);
    setSeq(shortIO, shortSeq);

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

    DIR_MTX_ARROW_t  *direction_matrix;

    nw_matrices_p nw_mtxs2dAffine = malloc(sizeof(struct nwMatrices));
    initializeNWMtx(longSeq->len, shortSeq->len, 0, costMtx2d_affine->lcm, nw_mtxs2dAffine);
    lenLongerSeq = longSeq->len;

    matrix_2d  = mat_get_2d_nwMtx (nw_mtxs2dAffine);
    precalcMtx = mat_get_2d_prec  (nw_mtxs2dAffine);

    // TODO: figure out what the following seven values do/are
    //       also note the int factors, which maybe have something to do with the unexplained 12
    //       that appears in matrices.c?
    // here and in algn.c, "block" refers to a block of gaps, so close_block_diagonal is the cost to
    // end a subsequence of gaps, presumably with a substitution, but maybe by simply switching directions:
    // there was a vertical gap, now there's a horizontal one.
    close_block_diagonal            = (int *)  matrix_2d;
    extend_block_diagonal           = (int *) (matrix_2d + ( 2 * lenLongerSeq));
    extend_vertical                 = (int *) (matrix_2d + ( 4 * lenLongerSeq));
    extend_horizontal               = (int *) (matrix_2d + ( 6 * lenLongerSeq));
    final_cost_matrix               = (int *) (matrix_2d + ( 8 * lenLongerSeq));
    gap_open_prec                   = (int *) (matrix_2d + (10 * lenLongerSeq));
    s_horizontal_gap_extension      = (int *) (matrix_2d + (11 * lenLongerSeq));


    // TODO: empty_medianSeq might not be necessary, as it's unused in ml code:
    size_t medianSeqLen             = longIO->length + shortIO->length + 2;  // 2 because that's how it is in ML code
    seq_p empty_medianSeq           = malloc( sizeof(struct seq) );
    empty_medianSeq->cap            = medianSeqLen;
    empty_medianSeq->array_head     = calloc( medianSeqLen, sizeof(SEQT));
    empty_medianSeq->len            = 0;
    empty_medianSeq->seq_begin      = empty_medianSeq->end = empty_medianSeq->array_head + medianSeqLen;



    direction_matrix                = mat_get_2d_direct (nw_mtxs2dAffine);

    cm_precalc_4algn(costMtx2d_affine, nw_mtxs2dAffine, longSeq);

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

    int algnCost = algn_fill_plane_2d_affine (shortSeq,
                                             longSeq,
                                             shortSeq->len - 1,
                                             longSeq->len  - 1,
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

    if(doDOTraceback) {
        seq_p ungappedMedianSeq = malloc(sizeof(struct seq));
        seq_p gappedMedianSeq   = malloc(sizeof(struct seq));
        initializeSeq(ungappedMedianSeq, SEQ_CAPACITY);
        initializeSeq(gappedMedianSeq,   SEQ_CAPACITY);

        algn_backtrace_affine (shortSeq,
                               longSeq,
                               direction_matrix,
                               ungappedMedianSeq,
                               gappedMedianSeq,
                               retShortSeq,
                               retLongSeq,
                               costMtx2d_affine);
        seqToAlignIO(ungappedMedianSeq, ungappedOutputSeq);
        seqToAlignIO(gappedMedianSeq,   gappedOutputSeq);

        freeSeq(ungappedMedianSeq);
        freeSeq(gappedMedianSeq);
    }


    seqToAlignIO(retLongSeq,  longIO);
    seqToAlignIO(retShortSeq, shortIO);


    freeNWMtx(nw_mtxs2dAffine);
    freeSeq(shortSeq);
    freeSeq(longSeq);
    freeSeq(retLongSeq);
    freeSeq(retShortSeq);

    return algnCost;
}

/*
int align3d(alignIO_p input1,
            alignIO_p input2,
            alignIO_p input3,
            alignIO_p outputMedian,
            cost_matrices_3d_p costMtx3d) {

    const size_t SEQ_CAPACITY = input1->length + input2->length + input3->length + 1;

    seq_p longSeq      = malloc(sizeof(struct seq));
    seq_p middleSeq    = malloc(sizeof(struct seq));
    seq_p shortSeq     = malloc(sizeof(struct seq));
    seq_p retShortSeq  = malloc(sizeof(struct seq));
    seq_p retMiddleSeq = malloc(sizeof(struct seq));
    seq_p retLongSeq   = malloc(sizeof(struct seq));
    seq_p medianSeq    = malloc(sizeof(struct seq));

    alignIO_p longIO,
              middleIO,
              shortIO;

    initializeSeq(longSeq,      SEQ_CAPACITY);
    initializeSeq(middleSeq,    SEQ_CAPACITY);
    initializeSeq(shortSeq,     SEQ_CAPACITY);
    initializeSeq(retLongSeq,   SEQ_CAPACITY);
    initializeSeq(retMiddleSeq, SEQ_CAPACITY);
    initializeSeq(retShortSeq,  SEQ_CAPACITY);
    initializeSeq(medianSeq,    SEQ_CAPACITY);


    if (input1->length > input2->length) {
        if (input2->length > input3->length) {
            //s1 > s2 > s3
            setSeq(input1, longSeq);
            longIO = input1;

            setSeq(input2, middleSeq);
            middleIO = input2;

            setSeq(input3, shortSeq);
            shortIO = input3;
        } else if (input3->length > input1->length) {
            //s3 > s1 > s2
            setSeq(input3, longSeq);
            longIO = input3;

            setSeq(input1, middleSeq);
            middleIO = input1;

            setSeq(input2, shortSeq);
            shortIO = input2;
        } else {
            // s1 > s3 > s2
            setSeq(input1, longSeq);
            longIO = input1;

            setSeq(input3, middleSeq);
            middleIO = input3;

            setSeq(input2, shortSeq);
            shortIO = input2;

        }
    } else { // s2 > s1
        if (input1->length > input3->length) {
            // s2 > s1 > s3
            setSeq(input2, longSeq);
            longIO = input2;

            setSeq(input1, middleSeq);
            middleIO = input1;

            setSeq(input3, shortSeq);
            shortIO = input3;

        } else if (input3->length > input2->length) {
            // s3 > s2 > s1
            setSeq(input3, longSeq);
            longIO = input3;

            setSeq(input2, middleSeq);
            middleIO = input2;

            setSeq(input1, shortSeq);
            shortIO = input1;
        } else {
            // s2 > s3 > s1
            setSeq(input2, longSeq);
            longIO = input2;

            setSeq(input3, middleSeq);
            middleIO = input3;

            setSeq(input1, shortSeq);
            shortIO = input1;
        }
    }

    //initializeSeq(longSeq, SEQ_CAPACITY);
    setSeq(longIO, longSeq);

    //initializeSeq(longSeq, SEQ_CAPACITY);
    setSeq(middleIO, middleSeq);

    //initializeSeq(shortSeq, SEQ_CAPACITY);
    setSeq(shortIO, shortSeq);

    int *isDifferent = 0;

    int algnCost = readjust_3d (0, isDifferent,
                                shortSeq,   middleSeq,    longSeq,
                                retLongSeq, retMiddleSeq, retShortSeq,
                                medianSeq,
                                costMtx3d);

    printf("here!!\n");

    seqToAlignIO(retLongSeq,   longIO);
    seqToAlignIO(retMiddleSeq, middleIO);
    seqToAlignIO(retShortSeq,  shortIO);
    seqToAlignIO(medianSeq,    outputMedian);

    freeSeq(longSeq);
    freeSeq(shortSeq);
    freeSeq(middleSeq);
    freeSeq(retLongSeq);
    freeSeq(retShortSeq);
    freeSeq(retMiddleSeq);
    freeSeq(medianSeq);

    //free(tcm);
    return algnCost;
}
*/