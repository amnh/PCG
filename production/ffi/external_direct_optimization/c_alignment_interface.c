#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "alignSequences.h"
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
    size_t loopOffset = character->capacity - character->length;
    for(size_t i = 0; i < character->length; i++) {
        printf("%2zu, %d\n", i, character->character[i + loopOffset]);
        //if (character->character[i] == 0) continue;
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
    size_t offset = capacity - length;
    // printf("\n");
    memcpy(outChar->character + offset, vals, length * sizeof(SEQT));
    // for(size_t i = 0; i < length; i++) {
    //     //outChar->character[i + offset] = vals[i];
    //     printf("copy %zu: %d, %d\n", i, vals[i], outChar->character[i + offset]);
    // }
    // printf("here!!!\n");
}

/** resets an alignIO struct. Note: does not realloc or change capacity, so can only be reused if not changing allocation size. */
void resetAlignIO(alignIO_p inChar) {
    memset(inChar->character, 0, inChar->capacity * sizeof(SEQT));
    inChar->length = 0;
    // for(size_t i = 0; i < inChar->capacity; i++) {
    //     inChar->character[i] = 0;
    // }
}

void allocAlignIO(alignIO_p toAlloc, size_t capacity) {
    toAlloc->length    = 0;
    toAlloc->capacity  = capacity;
    toAlloc->character = calloc(capacity, sizeof(SEQT));
}


void alignIOtoChar(seq_p retChar, alignIO_p input, size_t alphabetSize) {
    // printf("\n\nInput Length:        %2zu\n", input->length  );
    // printf("Input Capacity:      %2zu\n", input->capacity);
    // printf("Input alphabetSize:  %2zu\n", alphabetSize);

    // assign character into character struct
    retChar->len        = input->length;
    retChar->cap        = input->capacity;
    retChar->array_head = input->character;
    retChar->end        = input->character + input->capacity - 1;
    retChar->seq_begin  = input->character + input->capacity - input->length;
    //retChar->end        = retChar->seq_begin  + retChar->len - 1;

    // printf("\nBefore duping struct:\n");
    // printf("Input Length:      %2zu\n", input->length);
    // printf("Input Capacity:    %2zu\n", input->capacity);
    // printf("Input Array Head:  %2d\n",  input->character[0]);
    // printf("Input First:       %2d\n",  input->character[input->capacity - input->length]);
    // printf("Input Last:        %2d\n",  input->character[input->capacity - 1]);
    // fflush(stdout);

    //memcpy(retChar->seq_begin, input->character, input->length * sizeof(SEQT));
    //printf("\nmemcpy completed\n");

    // now add gap to beginning
    retChar->seq_begin--; // Add another cell, prepended to the array
    *retChar->seq_begin = ((SEQT) 1) << (alphabetSize - 1); //Prepend a gap to the array.
    retChar->len++;

    // printf("\nAfter duping struct:\n");
    // printf("Output Length:     %2zu\n", retChar->len);
    // printf("Output Capacity:   %2zu\n", retChar->cap);
    // printf("Output Array Head: %2d\n",  retChar->array_head[0]);
    // printf("Output First:      %2d\n",  retChar->seq_begin[0] );
    // printf("Output Last:       %2d\n",  retChar->end[0]       );
    // //printf("Gap value:           %2d\n", ((SEQT) 1) << (alphabetSize - 1));
    // fflush(stdout);

}

/** Takes in an alignIO and a seq. *Copies* values of character from end of seq to beginning of alignIO->character.
 *  Also eliminates extra gap needed by legacy code.
 */
void charToAlignIO(alignIO_p output, seq_p input) {
  /*
    printf("Length:   %zu\n", input->len);
    printf("Capacity: %zu\n", input->cap);
    fflush(stdout);
  */
    // TODO: The length is ZERO, why?

    input->seq_begin++;                // to start after unnecessary gap char at begining
    output->length   = input->len - 1; // (decrement because of the leading gap char?)
    output->capacity = input->cap;     // this shouldn't actually change
    size_t offset    = output->capacity - output->length;

    // TODO: is this necessary? Is it calloc'ed, and if not do these values matter?
    memset(output->character, 0, input->cap * sizeof(SEQT));
    // for(size_t i = output->length; i < input->cap; i++) {
    //     output->character[i] = 0;
    // }

    // TODO: use copyValsToAIO here, somehow, so process is consistent?
    memcpy( output->character, input->seq_begin, output->length * sizeof(SEQT));
 //    for(size_t i = 0; i < output->length; i++) {
 //      //        printf("Before charToAlignIO[%d]\n", i);
 //      //  fflush(stdout);
 //        output->character[i] = input->seq_begin[i];
	// //  printf("After  charToAlignIO[%d]\n", i);
 //        //fflush(stdout);
 //    }

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
    initializeChar(longChar,     CHAR_CAPACITY);
    initializeChar(shortChar,    CHAR_CAPACITY);

    // NOTE: We do not set the swapped flag, regardless of whether we swap the inputs.
    //       Doing so causes the C algorithm to return inconsistent reult inputs
    //       which create an NW matrix that contains a cell with equally costly
    //       left-arrow (INSERT) and up-arrow (DELETE) directions but a more
    //       costly diagonal-arrow (ALIGN) direction. This is because internally
    //       the algn_backtrace_2d function will check the 'swapped' flag and
    //       conditionally change the bias preference between left-arrow (INSERT)
    //       and up-arrow (DELETE) directions. For our use of the C code, we do
    //       not require this conditional biasing. We handle all swapping in this
    //       C interface.
    //
    //       I believe that the swapped flag is superfluous for our interface and
    //       the swapped != 0 code branches in algn_backtrace_2d is all dead code.
    const int swapped = 0;

    // size_t alphabetSize = costMtx2d->alphSize;
    size_t alphabetSize = costMtx2d->costMatrixDimension;

    if (inputChar1_aio->length >= inputChar2_aio->length) {
        alignIOtoChar(longChar, inputChar1_aio, alphabetSize);
        longIO = inputChar1_aio;

        alignIOtoChar(shortChar, inputChar2_aio, alphabetSize);
        shortIO = inputChar2_aio;

    } else {
        alignIOtoChar(longChar, inputChar2_aio, alphabetSize);
        longIO = inputChar2_aio;

        alignIOtoChar(shortChar, inputChar1_aio, alphabetSize);
        shortIO = inputChar1_aio;
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
    initializeNWMtx(longChar->len, shortChar->len, 0, costMtx2d->costMatrixDimension, nw_mtxs2d);
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
        //printf("Before backtrace.\n"), fflush(stdout);
        algn_backtrace_2d (shortChar, longChar, retShortChar, retLongChar, nw_mtxs2d, costMtx2d, 0, 0, swapped);
        //printf("After  backtrace.\n"), fflush(stdout);

        if (getUngapped) {
            seq_p ungappedMedianChar = malloc(sizeof(struct seq));
            initializeChar(ungappedMedianChar, CHAR_CAPACITY);

            algn_get_median_2d_no_gaps (retShortChar, retLongChar, costMtx2d, ungappedMedianChar);

            charToAlignIO(ungappedOutput_aio, ungappedMedianChar);

            freeChar(ungappedMedianChar);

            charToAlignIO(longIO,  retLongChar);
            charToAlignIO(shortIO, retShortChar);

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
            charToAlignIO(gappedOutput_aio, gappedMedianChar);
	    //printf("After  charToAlignIO\n"),  fflush(stdout);

            freeChar(gappedMedianChar);

            charToAlignIO(longIO,  retLongChar);
            charToAlignIO(shortIO, retShortChar);

        }
        if (getUnion) {
            seq_p gappedMedianChar = malloc(sizeof(struct seq));

            initializeChar(gappedMedianChar, CHAR_CAPACITY);

            algn_union (retShortChar, retLongChar, gappedMedianChar);

            charToAlignIO(gappedOutput_aio, gappedMedianChar);

            freeChar(gappedMedianChar);

            /*** following once union has its own output field again ***/
            // seq_p unionChar = malloc(sizeof(struct seq));
            // initializeChar(unionChar, CHAR_CAPACITY);
            // algn_union(retShortChar, retLongChar, gappedMedianChar);

            // charToAlignIO(unionChar, unionOutputChar);
            // freeChar(unionChar);

            charToAlignIO(longIO,  retLongChar);
            charToAlignIO(shortIO, retShortChar);
        }
    }


    //freeCostMtx(costMtx2d, 1);  // 1 is 2d
    freeNWMtx(nw_mtxs2d);

    freeChar(retLongChar);
    freeChar(retShortChar);

    return algnCost;

}

/** As align2d, but affine */
int align2dAffine( alignIO_p inputChar1_aio
                 , alignIO_p inputChar2_aio
                 , alignIO_p gappedOutput_aio
                 , alignIO_p ungappedOutput_aio
//                 , alignIO_p unionOutput_aio
                 , cost_matrices_2d_p costMtx2d_affine
                 , int getMedians
                 )
{

    if (DEBUG_ALGN) {
        printf("\n\nalign2d char1 input:\n");
        printf("\ninput char 1:");
        alignIO_print(inputChar1_aio);
        printf("input char 2:");
        alignIO_print(inputChar2_aio);
    }

    const size_t CHAR_CAPACITY = inputChar1_aio->length + inputChar2_aio->length; // 2 because a gap will be added to front of each array
    printf("capacity%zu\n", CHAR_CAPACITY);
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
    initializeChar(longChar,     CHAR_CAPACITY);
    initializeChar(shortChar,    CHAR_CAPACITY);


    // const int swapped = 0; no longer used.

    size_t alphabetSize = costMtx2d_affine->costMatrixDimension;

    if (inputChar1_aio->length > inputChar2_aio->length) {
        alignIOtoChar(longChar, inputChar1_aio, alphabetSize);
        longIO = inputChar1_aio;

        alignIOtoChar(shortChar, inputChar2_aio, alphabetSize);
        shortIO = inputChar2_aio;

    } else {
        alignIOtoChar(longChar, inputChar2_aio, alphabetSize);
        longIO = inputChar2_aio;

        alignIOtoChar(shortChar, inputChar1_aio, alphabetSize);
        shortIO = inputChar1_aio;
    }

    if (DEBUG_ALGN) {
        printf("\nafter copying, seq 1:\n");
        seq_print(longChar);
        printf("\nafter copying, seq 2:\n");
        seq_print(shortChar);
    }

    // TODO: document these variables
    // int *matrix;                        //
    int *close_block_diagonal;          //
    int *extend_block_diagonal;         //
    int *extend_vertical;               //
    int *extend_horizontal;             //
    int *final_cost_matrix;             //
    int *precalcMtx;                    //
    int *matrix_2d;                     //
    int *gap_open_prec;                 // precalculated gap opening value (top row of nw matrix)
    int *s_horizontal_gap_extension;    //
    int lenLongerChar;                  //

    DIR_MTX_ARROW_t  *direction_matrix;

    nw_matrices_p nw_mtxs2dAffine = malloc(sizeof(struct nwMatrices));
    initializeNWMtx(longChar->len, shortChar->len, 0, costMtx2d_affine->costMatrixDimension, nw_mtxs2dAffine);
    lenLongerChar = longChar->len;

    matrix_2d  = nw_mtxs2dAffine->nw_costMtx;
    precalcMtx = nw_mtxs2dAffine->precalcMtx;

    // TODO: figure out what the following seven values do/are
    //       also note the int factors, which maybe have something to do with the unexplained 12
    //       that appears in matrices.c?
    // here and in algn.c, "block" refers to a block of gaps, so close_block_diagonal is the cost to
    // end a subcharacter of gaps, presumably with a substitution, but maybe by simply switching directions:
    // there was a vertical gap, now there's a horizontal one.

    /** 2 through 11 below are offsets into various "matrices" in the alignment matrices, of which there are four
        of length 2 * longer_sequence and two of longer_sequence */
    //TODO: do I need these casts? This is C, not C++.
    close_block_diagonal            = (int *)  matrix_2d;
    extend_block_diagonal           = (int *) (matrix_2d + ( 2 * lenLongerChar));
    extend_vertical                 = (int *) (matrix_2d + ( 4 * lenLongerChar));
    extend_horizontal               = (int *) (matrix_2d + ( 6 * lenLongerChar));
    final_cost_matrix               = (int *) (matrix_2d + ( 8 * lenLongerChar));
    gap_open_prec                   = (int *) (matrix_2d + (10 * lenLongerChar));
    s_horizontal_gap_extension      = (int *) (matrix_2d + (11 * lenLongerChar));


    // TODO: empty_medianChar might not be necessary, as it's unused in ml code:
/*    size_t medianCharLen             = CHAR_CAPACITY;
    seq_p empty_medianChar           = malloc( sizeof(struct seq) );
    empty_medianChar->cap            = CHAR_CAPACITY;
    empty_medianChar->array_head     = calloc( CHAR_CAPACITY, sizeof(SEQT));
    empty_medianChar->len            = 0;
    empty_medianChar->seq_begin      = empty_medianChar->end = empty_medianChar->array_head + CHAR_CAPACITY;
*/


    direction_matrix                = nw_mtxs2dAffine->nw_dirMtx;

    // printf("!!!!!  HERE !!!!!\n");

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
                                              shortChar->len,
                                              longChar->len,
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

        charToAlignIO(ungappedOutput_aio, ungappedMedianChar);
        charToAlignIO(gappedOutput_aio,   gappedMedianChar);

        charToAlignIO(longIO,  retLongChar);
        charToAlignIO(shortIO, retShortChar);
//    fflush(stdout);

        freeChar(ungappedMedianChar);
        freeChar(gappedMedianChar);
    }

    freeNWMtx(nw_mtxs2dAffine);
    freeChar(retLongChar);
    freeChar(retShortChar);

    return algnCost;
}
