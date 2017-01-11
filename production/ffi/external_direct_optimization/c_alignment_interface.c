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

void setChar(alignIO_p input, seq_p retChar) {
    // assign character into character struct
    retChar->len = input->length;
    retChar->seq_begin = retChar->end - input->length;
    if (input->length > 0) {
        for(size_t i = 0; i < input->length; i++) {
            retChar->seq_begin[i] = (int) input->character[i];
        }
    }
}

void charToAlignIO(seq_p input, alignIO_p output) {
    output->length = input->len;
    for(int i = 0; i < output->length; i++) {
        output->character[i] = input->seq_begin[i];
    }
}

/** Returns true if character length is 0 or all characters are gaps. Otherwise returns false. */
int is_empty(seq_p charToTest, SEQT gap_char) {
    for (unsigned char *pos = charToTest->seq_begin; pos < charToTest->end; pos++) {
        if (gap_char != *pos) {
            return 0;
        }
    }
    return 1;
}

int compare (seq_p char1, seq_p char2) {
    size_t minLength = char1->len < char2->len ? char1->len : char2->len;
    size_t difference = char1->seq_begin[0] - char2->seq_begin[0];
    int retVal = 0;
    if (difference) {
        for (size_t count = 1; count < minLength; count++) {
            difference = char1->seq_begin[count] - char2->seq_begin[count];

            if( difference ) {
                retVal = difference;
                break;
            }
        }
    }
    return retVal ? retVal : char1->len - char2->len;
}

int align_3_powell(seq_p char1, seq_p char2, seq_p char3,
                   seq_p retChar1, seq_p retChar2, seq_p retChar3,
                   int mismatchCost, int gapOpenCost, int gapExtendCost) {
    size_t total_length = char1->len + char2->len + char3->len;
    int cost = powell_3D_align(char1, char2, char3, retChar1, retChar2, retChar3, mismatchCost, gapOpenCost, gapExtendCost);
    return cost;

}

/** [align_3_powell_inter a b c cm cm3] generates the median and edit
    * cost between the three characters char1, char2, and char3, according to the cost
    * matrices specified by costMtx2d and costMtx3d. */
int align_3_powell_inter (seq_p char1, seq_p char2, seq_p char3,
                          seq_p retChar1, seq_p retChar2, seq_p retChar3,
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

        int cost = align_3_powell (char1, char2, char3,
                                   retChar1, retChar2, retChar3,
                                   mismatchCost, gapOpenCost, gapExtendCost);

        for( int pos = retChar1->len - 1; pos >= 0; pos-- ) {
            medianChar = cm_get_median_3d(costMtx3d, char1->seq_begin[pos], char2->seq_begin[pos], char3->seq_begin[pos]);
            if(medianChar != gap_char) {
                seq_prepend(median, medianChar);
            }
        }
        seq_prepend(median, gap_char);
        return cost;


}

/** [readjust_3d a b mine cm cm3 p] readjust [mine] as the median between
    * the characters [a], [b] and [p]. The result is a triple [(ed, s, ch)],
    * where [ed] is the total edit cost of the median [s], which is in the
    * center of [a], [b], and [p], and [ch] is true iff [ch] is different from
    * [mine]. */

/** return cost, median and true/false value. isDifferent is true if the input median, curMedian is
 *  unchanged, false otherwise.
 *
 *  Initial call has first_gap = 0.
 */
int readjust_3d(int first_gap, int* isDifferent,
                seq_p char1, seq_p char2, seq_p char3,
                seq_p retChar1, seq_p retChar2, seq_p retChar3,
                seq_p curMedian,
                cost_matrices_3d_p costMtx3d) {
    int cost = 0;
    seq_p median = malloc(sizeof(struct seq));
    initializeChar(median, char1->len + char2->len + char3->len + 1);
    if ( char1->len      == char2->len &&
         curMedian->len == char3->len &&
         char1->len      == curMedian->len) {
        *isDifferent = 0;
        return cost; // cost == 0
    } else {
        SEQT gap_char = cm_get_gap_char_3d(costMtx3d);
        if (is_empty(char1, gap_char) && !is_empty(char2, gap_char)) {
            *isDifferent = 0 != compare (curMedian, char2);
            //copy char2 to median
            return cost; // cost == 0
        } else if (is_empty(char2, gap_char) && !is_empty(char1, gap_char)) {
            *isDifferent = 0 != compare(curMedian, char1);
            //copy char1 to median
            return cost; // cost == 0
        } else if (is_empty(char3, gap_char)) {
            *isDifferent = 0 != compare(curMedian, char3);
            //copy char3 to median
            return cost; // cost == 0
        } else {
            if (first_gap) {
                cost = align_3_powell_inter(char1, char2, char3,
                                            retChar1, retChar2, retChar3,
                                            median,
                                            costMtx3d);
            } else {
                seq_prepend(char1, gap_char);
                seq_prepend(char2, gap_char);
                seq_prepend(char3, gap_char);
                cost = align_3_powell_inter(char1, char2, char3,
                                            retChar1, retChar2, retChar3,
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
            alignIO_p gappedOutputChar,
            alignIO_p ungappedOutputChar,
            alignIO_p unionOutputChar,
            cost_matrices_2d_p costMtx2d,
            int doUnion,
            int doMedians) {

    const size_t CHAR_CAPACITY = input1->length + input2->length + 1;

    alignIO_p longIO,
              middleIO,
              shortIO;

    seq_p longChar     = malloc(sizeof(struct seq));
    seq_p shortChar    = malloc(sizeof(struct seq));
    seq_p retShortChar = malloc(sizeof(struct seq));
    seq_p retLongChar  = malloc(sizeof(struct seq));

    initializeChar(longChar,     CHAR_CAPACITY);
    initializeChar(shortChar,    CHAR_CAPACITY);
    initializeChar(retLongChar,  CHAR_CAPACITY);
    initializeChar(retShortChar, CHAR_CAPACITY);


    int swapped = 0;

    if (input1->length > input2->length) {
        setChar(input1, longChar);
        longIO = input1;

        setChar(input2, shortChar);
        shortIO = input2;

        swapped        = 1;
    } else {
        setChar(input2, longChar);
        longIO = input2;

        setChar(input1, shortChar);
        shortIO = input1;
    }
    //initializeChar(longChar, CHAR_CAPACITY);
    setChar(longIO, longChar);
    // printf("%zu\n", longChar->len);
    // printf("%zu\n", longChar->cap);
    // printf("%d\n", *longChar->array_head);
    // for(unsigned char *i = longChar->seq_begin; i < longChar->end; i++){
    //         printf("%d\n", *i);
    // }

    //initializeChar(shortChar, CHAR_CAPACITY);
    setChar(shortIO, shortChar);
    // printf("\n\n%zu\n", shortChar->len);
    // printf("%zu\n", shortChar->cap);
    // printf("%d\n", *shortChar->array_head);
    // for(unsigned char *i = shortChar->seq_begin; i < shortChar->end; i++){
    //         printf("%d\n", *i);
    // }

    nw_matrices_p nw_mtxs2d = malloc(sizeof(struct nwMatrices));
    initializeNWMtx(longChar->len, shortChar->len, 0, costMtx2d->lcm, nw_mtxs2d);

    // deltawh is for use in Ukonnen, it gives the current necessary width of the Ukk matrix.
    // The following calculation to compute deltawh, which increases the matrix height or width in algn_nw_2d,
    // was pulled from POY ML code.
    int deltawh = 0;
    int diff = longChar->len - shortChar->len;
    int lower_limit = .1 * longChar->len;
    if (deltawh) {
        deltawh = diff < lower_limit ? lower_limit : deltawh;
    } else {
        deltawh = diff < lower_limit ? lower_limit / 2 : 2;
    }
    //printf("%d, %zu, %d, %zu\n", shortCharLen, shortChar->len, longCharLen, longChar->len);
    int algnCost = algn_nw_2d( shortChar, longChar, costMtx2d, nw_mtxs2d, deltawh );
    if(doMedians || doUnion) {
        algn_backtrace_2d (shortChar, longChar, retShortChar, retLongChar, nw_mtxs2d, costMtx2d, 0, 0, swapped);

        if(doMedians) {
            seq_p ungappedMedianChar = malloc(sizeof(struct seq));
            seq_p gappedMedianChar   = malloc(sizeof(struct seq));
            initializeChar(ungappedMedianChar, CHAR_CAPACITY);
            initializeChar(gappedMedianChar,   CHAR_CAPACITY);

            algn_get_median_2d_no_gaps (retShortChar, retLongChar, costMtx2d, ungappedMedianChar);
            algn_get_median_2d_with_gaps (retShortChar, retLongChar, costMtx2d, gappedMedianChar);

            charToAlignIO(ungappedMedianChar, ungappedOutputChar);
            charToAlignIO(gappedMedianChar,   gappedOutputChar);

            freeChar(ungappedMedianChar);
            freeChar(gappedMedianChar);

        }
        if(doUnion) {
            seq_p unionChar = malloc(sizeof(struct seq));
            initializeChar(unionChar, CHAR_CAPACITY);
            algn_union(retShortChar, retLongChar, unionChar);

            charToAlignIO(unionChar, unionOutputChar);
            freeChar(unionChar);
        }
    }


    charToAlignIO(retLongChar, longIO);
    charToAlignIO(retShortChar, shortIO);

    //freeCostMtx(costMtx2d, 1);  // 1 is 2d
    freeNWMtx(nw_mtxs2d);
    freeChar(shortChar);
    freeChar(longChar);
    freeChar(retLongChar);
    freeChar(retShortChar);

    return algnCost;

}

int align2dAffine(alignIO_p input1,
                  alignIO_p input2,
                  alignIO_p gappedOutputChar,
                  alignIO_p ungappedOutputChar,
                  alignIO_p unionOutputChar,
                  cost_matrices_2d_p costMtx2d_affine,
                  int doUnion,
                  int doMedians) {

    const size_t CHAR_CAPACITY = input1->length + input2->length + 2;

    alignIO_p longIO,
              middleIO,
              shortIO;


    seq_p longChar     = malloc(sizeof(struct seq));
    seq_p shortChar    = malloc(sizeof(struct seq));
    seq_p retShortChar = malloc(sizeof(struct seq));
    seq_p retLongChar  = malloc(sizeof(struct seq));

    printf("here!!\n");
    initializeChar(longChar,     CHAR_CAPACITY);
    initializeChar(shortChar,    CHAR_CAPACITY);
    initializeChar(retLongChar,  CHAR_CAPACITY);
    initializeChar(retShortChar, CHAR_CAPACITY);

    int swapped = 0;

    if (input1->length > input2->length) {
        setChar(input1, longChar);
        longIO = input1;

        setChar(input2, shortChar);
        shortIO = input2;

        swapped        = 1;
    } else {
        setChar(input2, longChar);
        longIO = input2;

        setChar(input1, shortChar);
        shortIO = input1;
    }

    //initializeChar(longChar, CHAR_CAPACITY);
    setChar(longIO, longChar);
    //initializeChar(shortChar, CHAR_CAPACITY);
    setChar(shortIO, shortChar);

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
    int lenLongerChar;                   //

    DIR_MTX_ARROW_t  *direction_matrix;

    nw_matrices_p nw_mtxs2dAffine = malloc(sizeof(struct nwMatrices));
    initializeNWMtx(longChar->len, shortChar->len, 0, costMtx2d_affine->lcm, nw_mtxs2dAffine);
    lenLongerChar = longChar->len;

    matrix_2d  = mat_get_2d_nwMtx (nw_mtxs2dAffine);
    precalcMtx = mat_get_2d_prec  (nw_mtxs2dAffine);

    // TODO: figure out what the following seven values do/are
    //       also note the int factors, which maybe have something to do with the unexplained 12
    //       that appears in matrices.c?
    // here and in algn.c, "block" refers to a block of gaps, so close_block_diagonal is the cost to
    // end a subcharacter of gaps, presumably with a substitution, but maybe by simply switching directions:
    // there was a vertical gap, now there's a horizontal one.
    close_block_diagonal            = (int *)  matrix_2d;
    extend_block_diagonal           = (int *) (matrix_2d + ( 2 * lenLongerChar));
    extend_vertical                 = (int *) (matrix_2d + ( 4 * lenLongerChar));
    extend_horizontal               = (int *) (matrix_2d + ( 6 * lenLongerChar));
    final_cost_matrix               = (int *) (matrix_2d + ( 8 * lenLongerChar));
    gap_open_prec                   = (int *) (matrix_2d + (10 * lenLongerChar));
    s_horizontal_gap_extension      = (int *) (matrix_2d + (11 * lenLongerChar));


    // TODO: empty_medianChar might not be necessary, as it's unused in ml code:
    size_t medianCharLen             = longIO->length + shortIO->length + 2;  // 2 because that's how it is in ML code
    seq_p empty_medianChar           = malloc( sizeof(struct seq) );
    empty_medianChar->cap            = medianCharLen;
    empty_medianChar->array_head     = calloc( medianCharLen, sizeof(SEQT));
    empty_medianChar->len            = 0;
    empty_medianChar->seq_begin      = empty_medianChar->end = empty_medianChar->array_head + medianCharLen;



    direction_matrix                = mat_get_2d_direct (nw_mtxs2dAffine);

    cm_precalc_4algn(costMtx2d_affine, nw_mtxs2dAffine, longChar);

    // TODO: consider moving all of this into algn.
    //       the following three fns were initially not declared in algn.h
    algn_initialize_matrices_affine (costMtx2d_affine->gap_open,
                                     shortChar,
                                     longChar,
                                     costMtx2d_affine,
                                     close_block_diagonal,
                                     extend_block_diagonal,
                                     extend_vertical,
                                     extend_horizontal,
                                     final_cost_matrix,
                                     direction_matrix,
                                     precalcMtx);

    int algnCost = algn_fill_plane_2d_affine (shortChar,
                                             longChar,
                                             shortChar->len - 1,
                                             longChar->len  - 1,
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

    if(doMedians) {
        seq_p ungappedMedianChar = malloc(sizeof(struct seq));
        seq_p gappedMedianChar   = malloc(sizeof(struct seq));
        initializeChar(ungappedMedianChar, CHAR_CAPACITY);
        initializeChar(gappedMedianChar,   CHAR_CAPACITY);

        algn_backtrace_affine (shortChar,
                               longChar,
                               direction_matrix,
                               ungappedMedianChar,
                               gappedMedianChar,
                               retShortChar,
                               retLongChar,
                               costMtx2d_affine);
        charToAlignIO(ungappedMedianChar, ungappedOutputChar);
        charToAlignIO(gappedMedianChar,   gappedOutputChar);

        freeChar(ungappedMedianChar);
        freeChar(gappedMedianChar);
    }


    charToAlignIO(retLongChar,  longIO);
    charToAlignIO(retShortChar, shortIO);


    freeNWMtx(nw_mtxs2dAffine);
    freeChar(shortChar);
    freeChar(longChar);
    freeChar(retLongChar);
    freeChar(retShortChar);

    return algnCost;
}

/*
int align3d(alignIO_p input1,
            alignIO_p input2,
            alignIO_p input3,
            alignIO_p outputMedian,
            cost_matrices_3d_p costMtx3d) {

    const size_t CHAR_CAPACITY = input1->length + input2->length + input3->length + 1;

    seq_p longChar      = malloc(sizeof(struct seq));
    seq_p middleChar    = malloc(sizeof(struct seq));
    seq_p shortChar     = malloc(sizeof(struct seq));
    seq_p retShortChar  = malloc(sizeof(struct seq));
    seq_p retMiddleChar = malloc(sizeof(struct seq));
    seq_p retLongChar   = malloc(sizeof(struct seq));
    seq_p medianChar    = malloc(sizeof(struct seq));

    alignIO_p longIO,
              middleIO,
              shortIO;

    initializeChar(longChar,      CHAR_CAPACITY);
    initializeChar(middleChar,    CHAR_CAPACITY);
    initializeChar(shortChar,     CHAR_CAPACITY);
    initializeChar(retLongChar,   CHAR_CAPACITY);
    initializeChar(retMiddleChar, CHAR_CAPACITY);
    initializeChar(retShortChar,  CHAR_CAPACITY);
    initializeChar(medianChar,    CHAR_CAPACITY);


    if (input1->length > input2->length) {
        if (input2->length > input3->length) {
            //s1 > s2 > s3
            setChar(input1, longChar);
            longIO = input1;

            setChar(input2, middleChar);
            middleIO = input2;

            setChar(input3, shortChar);
            shortIO = input3;
        } else if (input3->length > input1->length) {
            //s3 > s1 > s2
            setChar(input3, longChar);
            longIO = input3;

            setChar(input1, middleChar);
            middleIO = input1;

            setChar(input2, shortChar);
            shortIO = input2;
        } else {
            // s1 > s3 > s2
            setChar(input1, longChar);
            longIO = input1;

            setChar(input3, middleChar);
            middleIO = input3;

            setChar(input2, shortChar);
            shortIO = input2;

        }
    } else { // s2 > s1
        if (input1->length > input3->length) {
            // s2 > s1 > s3
            setChar(input2, longChar);
            longIO = input2;

            setChar(input1, middleChar);
            middleIO = input1;

            setChar(input3, shortChar);
            shortIO = input3;

        } else if (input3->length > input2->length) {
            // s3 > s2 > s1
            setChar(input3, longChar);
            longIO = input3;

            setChar(input2, middleChar);
            middleIO = input2;

            setChar(input1, shortChar);
            shortIO = input1;
        } else {
            // s2 > s3 > s1
            setChar(input2, longChar);
            longIO = input2;

            setChar(input3, middleChar);
            middleIO = input3;

            setChar(input1, shortChar);
            shortIO = input1;
        }
    }

    //initializeChar(longChar, CHAR_CAPACITY);
    setChar(longIO, longChar);

    //initializeChar(longChar, CHAR_CAPACITY);
    setChar(middleIO, middleChar);

    //initializeChar(shortChar, CHAR_CAPACITY);
    setChar(shortIO, shortChar);

    int *isDifferent = 0;

    int algnCost = readjust_3d (0, isDifferent,
                                shortChar,   middleChar,    longChar,
                                retLongChar, retMiddleChar, retShortChar,
                                medianChar,
                                costMtx3d);

    printf("here!!\n");

    charToAlignIO(retLongChar,   longIO);
    charToAlignIO(retMiddleChar, middleIO);
    charToAlignIO(retShortChar,  shortIO);
    charToAlignIO(medianChar,    outputMedian);

    freeChar(longChar);
    freeChar(shortChar);
    freeChar(middleChar);
    freeChar(retLongChar);
    freeChar(retShortChar);
    freeChar(retMiddleChar);
    freeChar(medianChar);

    //free(tcm);
    return algnCost;
}
*/