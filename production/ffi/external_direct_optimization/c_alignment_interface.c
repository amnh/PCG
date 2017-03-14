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

void alignIO_print(alignIO_p character) {
    printf("\n");
    printf("Length:   %zu\n", character->length);
    printf("Capacity: %zu\n", character->capacity);
    for(int i = 0; i < character->capacity; i++) {
        if (character->character[i] == 0) continue;
        printf("%2d, ", character->character[i]);
    }
    printf("\n");
}

/**  */
void copyValsToAIO(alignIO_p outChar, SEQT *vals, size_t length, size_t capacity) {
    outChar->length   = length;
    outChar->capacity = capacity;
    // printf("here!\n");
    // outChar->character = calloc(outChar->capacity, capacity);
    // printf("here!!\n");
    size_t loopOffset = capacity - length;
    for(size_t i = 0; i < length; i++) {
        // printf("%d, ", vals[i]);
        outChar->character[i+loopOffset] = vals[i];
    }
    // printf("here!!!\n");
}

/** resets an alignIO struct. Note: does not realloc or change capacity, so can only be reused if not changing allocation size. */
void resetAlignIO(alignIO_p inChar) {
    for(size_t i = 0; i < inChar->capacity; i++) {
        inChar->character[i] = 0;
    }
    inChar->length = 0;
}

void allocAlignIO(alignIO_p toAlloc, size_t capacity) {
    toAlloc->length    = 0;
    toAlloc->capacity  = capacity;
    toAlloc->character = calloc(capacity, sizeof(SEQT));
}

/** takes in an alignIO struct and a seq struct. Copies values of alignIO to seq.
 *  Points seq->seq_begin, seq->end to respective points in alighIO->character.
 *  Adds a gap character at the front of the array, to deal with old OCaml-forced interface.
 */
void alignIOtoChar(alignIO_p input, seq_p retChar, size_t alphabetSize) {
    //printf("Input Length:     %2d\n", input->length  );
    //printf("Input Capacity:   %2d\n", input->capacity);
    
    // assign character into character struct
    retChar->len        = input->length;
    retChar->cap        = input->capacity;
    retChar->array_head = input->character;
    retChar->seq_begin  = retChar->array_head + retChar->cap - retChar->len;
    retChar->end        = retChar->seq_begin  + retChar->len - 1;
    // now add gap to beginning
    retChar->seq_begin--; // Add another cell, prepended to the array
    *retChar->seq_begin = 1 << (alphabetSize - 1); //Prepend a gap to the array.
    retChar->len++;
    /*
    printf("Sequence Length:     %2d\n", retChar->len);
    printf("Sequence Capacity:   %2d\n", retChar->cap);
    printf("Sequence Array Head: %2d <- %p\n", retChar->array_head[0], retChar->array_head);
    printf("Sequence Begin:      %2d <- %p\n", retChar->seq_begin[0] , retChar->seq_begin );
    printf("Sequence End:        %2d <- %p\n", retChar->end[0]       , retChar->end       );
    fflush(stdout);
    */
}

/** Takes in an alignIO and a seq. *Copies* values of character from end of seq to beginning of alignIO->character.
 *  Also eliminates extra gap needed by legacy code.
 */
void charToAlignIO(seq_p input, alignIO_p output) {
  /*
    printf("Length:   %zu\n", input->len);
    printf("Capacity: %zu\n", input->cap);
    fflush(stdout);
  */
    //TODO: The length is ZERO, why?

    input->seq_begin++;                // to start after unnecessary gap char at begining
    output->length   = input->len - 1; // (decrement because of the leading gap char?)
    output->capacity = input->cap;     // this shouldn't actually change

    for(size_t i = 0; i < output->length; i++) {
      //        printf("Before charToAlignIO[%d]\n", i);
      //  fflush(stdout);
        output->character[i] = input->seq_begin[i];
	//  printf("After  charToAlignIO[%d]\n", i);
        //fflush(stdout);
    }
    for(size_t i = output->length; i < input->cap; i++) {
        output->character[i] = 0;
    }
}

void freeAlignIO(alignIO_p toFree) {
    free(toFree->character);
    free(toFree);
}


/** Do a 2d alignment. Depending on the values of last two inputs,
 *  | (0,0) = return only a cost
 *  | (0,1) = calculate gapped and ungapped characters
 *  | (1,0) = calculate union
 *  | (1,1) = calculate both union and ungapped characters.
 *
 *  In the last two cases the union will replace the gapped character placeholder.
 */
int align2d(alignIO_p inputChar1_aio,
            alignIO_p inputChar2_aio,
            alignIO_p gappedOutput_aio,
            alignIO_p ungappedOutput_aio,
            // alignIO_p unionOutput_aio,
            cost_matrices_2d_p costMtx2d,
            int getUngapped,
            int getGapped,
            int getUnion) {

    if (DEBUG_ALGN) {
        printf("\n\nalign2d char1 input:\n");
        printf("\ninput char 1:");
        alignIO_print(inputChar1_aio);
        printf("input char 2:");
        alignIO_print(inputChar2_aio);
    }

    const size_t CHAR_CAPACITY = inputChar1_aio->length + inputChar2_aio->length;

    alignIO_p longIO,
              shortIO;

    seq_p longChar     = malloc(sizeof(struct seq));
    seq_p shortChar    = malloc(sizeof(struct seq));
    seq_p retShortChar = malloc(sizeof(struct seq));
    seq_p retLongChar  = malloc(sizeof(struct seq));

    /*** Most character allocation is now done on Haskell side, but these two are local. ***/
    /*** longChar and shortChar will both have pointers into the input characters, so don't need to be initialized separately ***/
    initializeChar(retLongChar,  CHAR_CAPACITY);
    initializeChar(retShortChar, CHAR_CAPACITY);


    int swapped = 0;

    // size_t alphabetSize = costMtx2d->alphSize;
    size_t alphabetSize = costMtx2d->costMtxDimension;

    if (inputChar1_aio->length >= inputChar2_aio->length) {
        alignIOtoChar(inputChar1_aio, longChar, alphabetSize);
        longIO = inputChar1_aio;

        alignIOtoChar(inputChar2_aio, shortChar, alphabetSize);
        shortIO = inputChar2_aio;

    } else {
        alignIOtoChar(inputChar2_aio, longChar, alphabetSize);
        longIO = inputChar2_aio;

        alignIOtoChar(inputChar1_aio, shortChar, alphabetSize);
        shortIO = inputChar1_aio;
        swapped = 1;
    }

    if (DEBUG_ALGN) {
        printf("\nafter copying, seq 1:\n");
        seq_print(longChar);
        printf("\nafter copying, seq 2:\n");
        seq_print(shortChar);
    }
    //printf("Before NW init.\n");
    //fflush(stdout);
    nw_matrices_p nw_mtxs2d = malloc(sizeof(struct nwMatrices));
    initializeNWMtx(longChar->len, shortChar->len, 0, costMtx2d->costMtxDimension, nw_mtxs2d);
    //printf("After  NW init.\n");
    //fflush(stdout);

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
    //printf("Before align cost.\n");
    //fflush(stdout);
    int algnCost = algn_nw_2d( shortChar, longChar, costMtx2d, nw_mtxs2d, deltawh );
    //printf("Ater align cost.\n");
    //fflush(stdout);
    if (getGapped || getUngapped || getUnion) {
        algn_print_dynmtrx_2d (shortChar, longChar, nw_mtxs2d);
        //printf("Before backtrace.\n"), fflush(stdout);
        algn_backtrace_2d (shortChar, longChar, retShortChar, retLongChar, nw_mtxs2d, costMtx2d, 0, 0, swapped);
        //printf("After  backtrace.\n"), fflush(stdout);

        if (getUngapped) {
            seq_p ungappedMedianChar = malloc(sizeof(struct seq));
            initializeChar(ungappedMedianChar, CHAR_CAPACITY);

            algn_get_median_2d_no_gaps (retShortChar, retLongChar, costMtx2d, ungappedMedianChar);

            charToAlignIO(ungappedMedianChar, ungappedOutput_aio);

            freeChar(ungappedMedianChar);

            charToAlignIO(retLongChar,  longIO);
            charToAlignIO(retShortChar, shortIO);

        }
        if (getGapped && !getUnion) {
	    //printf("In here!\n"), fflush(stdout);
            seq_p gappedMedianChar   = malloc(sizeof(struct seq));

	    //printf("Before initialize character!\n"), fflush(stdout);
            initializeChar(gappedMedianChar, CHAR_CAPACITY);
	    //printf("After  initialize character!\n"), fflush(stdout);

	    //printf("Before algn_get_median\n"), fflush(stdout);
            algn_get_median_2d_with_gaps (retShortChar, retLongChar, costMtx2d, gappedMedianChar);
	    //printf("After  algn_get_median\n"), fflush(stdout);

	    //printf("Before charToAlignIO\n"), fflush(stdout);
            charToAlignIO(gappedMedianChar, gappedOutput_aio);
	    //printf("After  charToAlignIO\n"),  fflush(stdout);

            freeChar(gappedMedianChar);

            charToAlignIO(retLongChar,  longIO);
            charToAlignIO(retShortChar, shortIO);

        }
        if (getUnion) {
            seq_p gappedMedianChar   = malloc(sizeof(struct seq));
            initializeChar(gappedMedianChar, CHAR_CAPACITY);

            algn_union (retShortChar, retLongChar, gappedMedianChar);

            charToAlignIO(gappedMedianChar, gappedOutput_aio);

            freeChar(gappedMedianChar);

            /*** following once union has its own output field again ***/
            // seq_p unionChar = malloc(sizeof(struct seq));
            // initializeChar(unionChar, CHAR_CAPACITY);
            // algn_union(retShortChar, retLongChar, gappedMedianChar);

            // charToAlignIO(unionChar, unionOutputChar);
            // freeChar(unionChar);

            charToAlignIO(retLongChar,  longIO);
            charToAlignIO(retShortChar, shortIO);
        }
    }


    //freeCostMtx(costMtx2d, 1);  // 1 is 2d
    freeNWMtx(nw_mtxs2d);

    freeChar(retLongChar);
    freeChar(retShortChar);

    return algnCost;

}

/** As align2d, but affine */
int align2dAffine(alignIO_p inputChar1_aio,
                  alignIO_p inputChar2_aio,
                  alignIO_p gappedOutput_aio,
                  alignIO_p ungappedOutput_aio,
                  // alignIO_p unionOutput_aio,
                  cost_matrices_2d_p costMtx2d_affine,
                  int getMedians) {

    const size_t CHAR_CAPACITY = inputChar1_aio->length + inputChar2_aio->length + 2;

    alignIO_p longIO,
              shortIO;


    seq_p longChar     = malloc(sizeof(struct seq));
    seq_p shortChar    = malloc(sizeof(struct seq));
    seq_p retShortChar = malloc(sizeof(struct seq));
    seq_p retLongChar  = malloc(sizeof(struct seq));

    /*** Most character allocation is now done on Haskell side, but these two are local. ***/
    /*** longChar and shortChar will both have pointers into the input characters, so don't need to be initialized separately ***/
    initializeChar(retLongChar,  CHAR_CAPACITY);
    initializeChar(retShortChar, CHAR_CAPACITY);

    int swapped = 0;

    size_t alphabetSize = costMtx2d_affine->alphSize;

    if (inputChar1_aio->length > inputChar2_aio->length) {
        alignIOtoChar(inputChar1_aio, longChar, alphabetSize);
        longIO = inputChar1_aio;

        alignIOtoChar(inputChar2_aio, shortChar, alphabetSize);
        shortIO = inputChar2_aio;

        swapped = 1;
    } else {
        alignIOtoChar(inputChar2_aio, longChar, alphabetSize);
        longIO = inputChar2_aio;

        alignIOtoChar(inputChar1_aio, shortChar, alphabetSize);
        shortIO = inputChar1_aio;
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
    int lenLongerChar;                   //

    DIR_MTX_ARROW_t  *direction_matrix;

    nw_matrices_p nw_mtxs2dAffine = malloc(sizeof(struct nwMatrices));
    initializeNWMtx(longChar->len, shortChar->len, 0, costMtx2d_affine->costMtxDimension, nw_mtxs2dAffine);
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

    if(getMedians) {
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
        charToAlignIO(ungappedMedianChar, ungappedOutput_aio);
        charToAlignIO(gappedMedianChar,   gappedOutput_aio);

        charToAlignIO(retLongChar,  longIO);
        charToAlignIO(retShortChar, shortIO);


        freeChar(ungappedMedianChar);
        freeChar(gappedMedianChar);
    }

    freeNWMtx(nw_mtxs2dAffine);
    freeChar(retLongChar);
    freeChar(retShortChar);

    return algnCost;
}


/** Returns true if character length is 0 or all characters are gaps. Otherwise returns false. */
// int is_empty(seq_p charToTest, SEQT gap_char) {
//     for (unsigned char *pos = charToTest->seq_begin; pos < charToTest->end; pos++) {
//         if (gap_char != *pos) {
//             return 0;
//         }
//     }
//     return 1;
// }

// /** Compares two seqs to see if they're equivalent. TODO: Check this, as it was taken from ML code. */
// int compare (seq_p char1, seq_p char2) {
//     size_t minLength = char1->len < char2->len ? char1->len : char2->len;
//     size_t difference = char1->seq_begin[0] - char2->seq_begin[0];
//     int retVal = 0;
//     if (difference) {
//         for (size_t count = 1; count < minLength; count++) {
//             difference = char1->seq_begin[count] - char2->seq_begin[count];

//             if( difference ) {
//                 retVal = difference;
//                 break;
//             }
//         }
//     }
//     return retVal ? retVal : char1->len - char2->len;
// }

// int align_3_powell(seq_p char1, seq_p char2, seq_p char3,
//                    seq_p retChar1, seq_p retChar2, seq_p retChar3,
//                    int mismatchCost, int gapOpenCost, int gapExtendCost) {
//     size_t total_length = char1->len + char2->len + char3->len;
//     int cost = powell_3D_align(char1, char2, char3, retChar1, retChar2, retChar3, mismatchCost, gapOpenCost, gapExtendCost);
//     return cost;

// }

// /** [align_3_powell_inter a b c cm cm3] generates the median and edit
//     * cost between the three characters char1, char2, and char3, according to the cost
//     * matrices specified by costMtx2d and costMtx3d. */
// int align_3_powell_inter (seq_p char1, seq_p char2, seq_p char3,
//                           seq_p retChar1, seq_p retChar2, seq_p retChar3,
//                           seq_p median,
//                           cost_matrices_3d_p costMtx3d) {
//         SEQT gap_char = cm_get_gap_char_3d(costMtx3d);
//         SEQT medianChar;
//         int mismatchCost,
//             gapOpenCost,
//             gapExtendCost;
//         switch (costMtx3d->cost_model_type) {
//             case 1:
//             case 2:
//                 gapOpenCost = 0;
//                 break;
//             default:
//                 gapOpenCost = costMtx3d->gap_open;
//                 break;
//         }
//         mismatchCost  = cm_calc_cost(costMtx3d->cost, 1, 2,  costMtx3d->lcm);
//         gapExtendCost = cm_calc_cost(costMtx3d->cost, 1, 16, costMtx3d->lcm);

//         int cost = align_3_powell (char1, char2, char3,
//                                    retChar1, retChar2, retChar3,
//                                    mismatchCost, gapOpenCost, gapExtendCost);

//         for( int pos = retChar1->len - 1; pos >= 0; pos-- ) {
//             medianChar = cm_get_median_3d(costMtx3d, char1->seq_begin[pos], char2->seq_begin[pos], char3->seq_begin[pos]);
//             if(medianChar != gap_char) {
//                 seq_prepend(median, medianChar);
//             }
//         }
//         seq_prepend(median, gap_char);
//         return cost;


// }

// /** [readjust_3d a b mine cm cm3 p] readjust [mine] as the median between
//     * the characters [a], [b] and [p]. The result is a triple [(ed, s, ch)],
//     * where [ed] is the total edit cost of the median [s], which is in the
//     * center of [a], [b], and [p], and [ch] is true iff [ch] is different from
//     * [mine]. */

// /** return cost, median and true/false value. isDifferent is true if the input median, curMedian is
//  *  unchanged, false otherwise.
//  *
//  *  Initial call has first_gap = 0.
//  */
// int readjust_3d(int first_gap, int* isDifferent,
//                 seq_p char1, seq_p char2, seq_p char3,
//                 seq_p retChar1, seq_p retChar2, seq_p retChar3,
//                 seq_p curMedian,
//                 cost_matrices_3d_p costMtx3d) {
//     int cost = 0;
//     seq_p median = malloc(sizeof(struct seq));
//     initializeChar(median, char1->len + char2->len + char3->len + 1);
//     if ( char1->len      == char2->len &&
//          curMedian->len == char3->len &&
//          char1->len      == curMedian->len) {
//         *isDifferent = 0;
//         return cost; // cost == 0
//     } else {
//         SEQT gap_char = cm_get_gap_char_3d(costMtx3d);
//         if (is_empty(char1, gap_char) && !is_empty(char2, gap_char)) {
//             *isDifferent = 0 != compare (curMedian, char2);
//             //copy char2 to median
//             return cost; // cost == 0
//         } else if (is_empty(char2, gap_char) && !is_empty(char1, gap_char)) {
//             *isDifferent = 0 != compare(curMedian, char1);
//             //copy char1 to median
//             return cost; // cost == 0
//         } else if (is_empty(char3, gap_char)) {
//             *isDifferent = 0 != compare(curMedian, char3);
//             //copy char3 to median
//             return cost; // cost == 0
//         } else {
//             if (first_gap) {
//                 cost = align_3_powell_inter(char1, char2, char3,
//                                             retChar1, retChar2, retChar3,
//                                             median,
//                                             costMtx3d);
//             } else {
//                 seq_prepend(char1, gap_char);
//                 seq_prepend(char2, gap_char);
//                 seq_prepend(char3, gap_char);
//                 cost = align_3_powell_inter(char1, char2, char3,
//                                             retChar1, retChar2, retChar3,
//                                             median,
//                                             costMtx3d);
//             }
//             *isDifferent = 0 != compare(curMedian, median);
//             return cost;
//         }
//     }
// }

/*
int align3d(alignIO_p inputChar1_aio,
            alignIO_p inputChar2_aio,
            alignIO_p input3,
            alignIO_p outputMedian,
            cost_matrices_3d_p costMtx3d) {

    const size_t CHAR_CAPACITY = inputChar1_aio->length + inputChar2_aio->length + input3->length + 1;

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

    size_t alphabetSize = costmtx3d->alphSize;

    if (inputChar1_aio->length > inputChar2_aio->length) {
        if (inputChar2_aio->length > input3->length) {
            //s1 > s2 > s3
            alignIOtoChar(inputChar1_aio, longChar, alphabetSize);
            longIO = inputChar1_aio;

            alignIOtoChar(inputChar2_aio, middleChar, alphabetSize);
            middleIO = inputChar2_aio;

            alignIOtoChar(input3, shortChar, alphabetSize);
            shortIO = input3;
        } else if (input3->length > inputChar1_aio->length) {
            //s3 > s1 > s2
            alignIOtoChar(input3, longChar, alphabetSize);
            longIO = input3;

            alignIOtoChar(inputChar1_aio, middleChar, alphabetSize);
            middleIO = inputChar1_aio;

            alignIOtoChar(inputChar2_aio, shortChar, alphabetSize);
            shortIO = inputChar2_aio;
        } else {
            // s1 > s3 > s2
            alignIOtoChar(inputChar1_aio, longChar, alphabetSize);
            longIO = inputChar1_aio;

            alignIOtoChar(input3, middleChar, alphabetSize);
            middleIO = input3;

            alignIOtoChar(inputChar2_aio, shortChar, alphabetSize);
            shortIO = inputChar2_aio;

        }
    } else { // s2 > s1
        if (inputChar1_aio->length > input3->length) {
            // s2 > s1 > s3
            alignIOtoChar(inputChar2_aio, longChar, alphabetSize);
            longIO = inputChar2_aio;

            alignIOtoChar(inputChar1_aio, middleChar, alphabetSize);
            middleIO = inputChar1_aio;

            alignIOtoChar(input3, shortChar, alphabetSize);
            shortIO = input3;

        } else if (input3->length > inputChar2_aio->length) {
            // s3 > s2 > s1
            alignIOtoChar(input3, longChar, alphabetSize);
            longIO = input3;

            alignIOtoChar(inputChar2_aio, middleChar, alphabetSize);
            middleIO = inputChar2_aio;

            alignIOtoChar(inputChar1_aio, shortChar, alphabetSize);
            shortIO = inputChar1_aio;
        } else {
            // s2 > s3 > s1
            alignIOtoChar(inputChar2_aio, longChar, alphabetSize);
            longIO = inputChar2_aio;

            alignIOtoChar(input3, middleChar, alphabetSize);
            middleIO = input3;

            alignIOtoChar(inputChar1_aio, shortChar, alphabetSize);
            shortIO = inputChar1_aio;
        }
    }

    //initializeChar(longChar, CHAR_CAPACITY);
    alignIOtoChar(longIO, longChar, alphabetSize);

    //initializeChar(longChar, CHAR_CAPACITY);
    alignIOtoChar(middleIO, middleChar, alphabetSize);

    //initializeChar(shortChar, CHAR_CAPACITY);
    alignIOtoChar(shortIO, shortChar, alphabetSize);

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
