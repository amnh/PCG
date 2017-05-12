#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "alignCharacters.h"
#include "c_alignment_interface.h"
#include "c_code_alloc_setup.h"
#include "debug_constants.h"
// #include "costMatrix.h"
#include "nwMatrices.h"
//#include "ukkCheckp.h"
#include "ukkCommon.h"

/** Print an alignIO char. Assume char exists at end of buffer. */
void alignIO_print(alignIO_p character) {
    printf("\n");
    printf("Length:   %zu\n", character->length);
    printf("Capacity: %zu\n", character->capacity);
    size_t charStart = character->capacity - character->length;
    for(size_t i = charStart; i < character->capacity; i++) {
        printf("%d, ", character->character[i]);
        //if (character->character[i] == 0) continue;
    }
    printf("\n\n");
    fflush(stdout);
}

/** Copy an array of elem_t into an input/output type. Array values should fill last `length` elements of character buffer. */
void copyValsToAIO(alignIO_p outChar, elem_t *vals, size_t length, size_t capacity) {
    outChar->length   = length;
    outChar->capacity = capacity;
    // printf("here!\n");
    // outChar->character = calloc(outChar->capacity, capacity);
    // printf("here!!\n");
    size_t offset = capacity - length;
    // printf("\n");
    memcpy(outChar->character + offset, vals, length * sizeof(elem_t));
    // for(size_t i = 0; i < length; i++) {
    //     //outChar->character[i + offset] = vals[i];
    //     printf("copy %zu: %d, %d\n", i, vals[i], outChar->character[i + offset]);
    // }
    // printf("here!!!\n");
}

/** Reset an alignIO struct. Note: does not realloc or change capacity, so can only be reused if not changing allocation size. */
void resetAlignIO(alignIO_p inChar) {
    memset(inChar->character, 0, inChar->capacity * sizeof(elem_t));
    inChar->length = 0;
    // for(size_t i = 0; i < inChar->capacity; i++) {
    //     inChar->character[i] = 0;
    // }
}

void allocAlignIO(alignIO_p toAlloc, size_t capacity) {
    toAlloc->length    = 0;
    toAlloc->capacity  = capacity;
    toAlloc->character = calloc(capacity, sizeof(elem_t));
}

void reallocAlignIO(alignIO_p toAlloc, size_t capacity) {
    toAlloc->length    = 0;
    toAlloc->capacity  = capacity;
    toAlloc->character = realloc(toAlloc->character, capacity * sizeof(elem_t));
}


/** Copy an input/output type into the char type needed by code ported forward from POY.
 *  The values in the last `length` elements in `input` get copied to the last `length` elements in the array in`retChar`.
 *  Ported code needs a gap character at beginning, so provide that.
 */
void alignIOtoDynChar(dyn_char_p retChar, alignIO_p input, size_t alphabetSize) {
    // printf("\n\nInput Length:        %2zu\n", input->length  );
    // printf("Input Capacity:      %2zu\n", input->capacity);
    // printf("Input alphabetSize:  %2zu\n", alphabetSize);

    // assign character into character struct
    size_t offset       = input->capacity - input->length;
    retChar->len        = input->length;
    retChar->cap        = input->capacity;
    retChar->array_head = input->character;
    retChar->end        = input->character + input->capacity - 1; // TODO: make sure this is correct.
    retChar->char_begin = input->character + offset;

    // printf("\nBefore duping struct:\n");
    // printf("Input Length:      %2zu\n", input->length);
    // printf("Input Capacity:    %2zu\n", input->capacity);
    // printf("Input Array Head:  %2d\n",  input->character[0]);
    // printf("Input First:       %2d\n",  input->character[input->capacity - input->length]);
    // printf("Input Last:        %2d\n",  input->character[input->capacity - 1]);
    // fflush(stdout);
    // memcpy(retChar->char_begin, input->character + offset, input->length * sizeof(elem_t));
    //printf("\nmemcpy completed\n");

    // now add gap to beginning
    retChar->char_begin--;   // Add another cell, prepended to the array
    *retChar->char_begin = ((elem_t) 1) << (alphabetSize - 1);   //Prepend a gap to the array.
    retChar->len++;

    // printf("\nAfter duping struct:\n");
    // printf("Output Length:     %2zu\n", retChar->len);
    // printf("Output Capacity:   %2zu\n", retChar->cap);
    // printf("Output Array Head: %2d\n",  retChar->array_head[0]);
    // printf("Output First:      %2d\n",  retChar->char_begin[0] );
    // printf("Output Last:       %2d\n",  retChar->end[0]       );
    // //printf("Gap value:           %2d\n", ((elem_t) 1) << (alphabetSize - 1));
    // fflush(stdout);

}

/** Takes in an alignIO and a char. *Copies* values of character from end of char to end of alignIO->character.
 *  Also eliminates extra gap needed by legacy code.
 */
void dynCharToAlignIO(alignIO_p output, dyn_char_p input) {
  /*
    printf("Length:   %zu\n", input->len);
    printf("Capacity: %zu\n", input->cap);
    fflush(stdout);
  */
    // TODO: The length is ZERO, why?
    size_t offset    = input->cap - input->len + 1; // How far in to output to start copying input character.
                                                    // Extra 1 because of extraneous gap.

    output->length   = input->len - 1; // (decrement because of the leading gap char?)
    output->capacity = input->cap;     // this shouldn't change

    // TODO: is this necessary? Is it calloc'ed, and if not do these values matter?
    memset(output->character, 0, input->cap * sizeof(elem_t));
    // for(size_t i = output->length; i < input->cap; i++) {
    //     output->character[i] = 0;
    // }

    // Start copy after unnecessary gap char in input.
    memcpy( output->character + offset, input->char_begin + 1, (input->len - 1) * sizeof(elem_t));
    // for(size_t i = 0; i < input->len; i++) {
    //     printf("Cur char: %zu %u\n", i, input->char_begin[i]);
    //     fflush(stdout);
    //     output->character[i] = input->char_begin[i];
    //     // printf("After  dynCharToAlignIO[%d]\n", i);
    //     // fflush(stdout);
    // }

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
int align2d( alignIO_p inputChar1_aio
           , alignIO_p inputChar2_aio
           , alignIO_p gappedOutput_aio
           , alignIO_p ungappedOutput_aio
         // , alignIO_p unionOutput_aio
           , cost_matrices_2d_p costMtx2d
           , int getUngapped
           , int getGapped
           , int getUnion
           )
{

    if (DEBUG_ALGN) {
        printf("\n\nalign2d char1 input:\n");
        printf("\ninput char 1:");
        alignIO_print(inputChar1_aio);
        printf("input char 2:");
        alignIO_print(inputChar2_aio);
    }

    const size_t CHAR_CAPACITY = inputChar1_aio->length + inputChar2_aio->length + 2; // 2 to account for gaps,
                                                                                      // which will be added in initializeChar()

    alignIO_p longIO,
              shortIO;

    dyn_char_p longChar     = malloc(sizeof(dyn_character_t)); // input to algn_nw_2d
    dyn_char_p shortChar    = malloc(sizeof(dyn_character_t)); // input to algn_nw_2d
    dyn_char_p retShortChar = malloc(sizeof(dyn_character_t)); // aligned character outputs from backtrace (not medians)
    dyn_char_p retLongChar  = malloc(sizeof(dyn_character_t)); // aligned character outputs from backtrace (not medians)

    /*** Most character allocation is now done on Haskell side, but these two are local. ***/
    /*** longChar and shortChar will both have pointers into the input characters, so don't need to be initialized separately ***/
    initializeChar(retLongChar,  CHAR_CAPACITY);
    initializeChar(retShortChar, CHAR_CAPACITY);

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
    //       TODO: I believe that the swapped flag is superfluous for our interface and
    //       the swapped != 0 code branches in algn_backtrace_2d is all dead code.
    const int swapped = 0;

    // size_t alphabetSize = costMtx2d->alphSize;
    size_t alphabetSize = costMtx2d->costMatrixDimension;

    if (inputChar1_aio->length >= inputChar2_aio->length) {
        alignIOtoDynChar(longChar, inputChar1_aio, alphabetSize);
        longIO = inputChar1_aio;

        alignIOtoDynChar(shortChar, inputChar2_aio, alphabetSize);
        shortIO = inputChar2_aio;

    } else {
        alignIOtoDynChar(longChar, inputChar2_aio, alphabetSize);
        longIO = inputChar2_aio;

        alignIOtoDynChar(shortChar, inputChar1_aio, alphabetSize);
        shortIO = inputChar1_aio;
    }

    if (DEBUG_ALGN) {
        printf("\nafter copying, char 1:\n");
        dyn_char_print(longChar);
        printf("\nafter copying, char 2:\n");
        dyn_char_print(shortChar);
    }
    //printf("Before NW init.\n");
    //fflush(stdout);
    nw_matrices_p nw_mtxs2d = malloc( sizeof(nwMatrices_t) );
    initializeNWMtx(nw_mtxs2d, longChar->len, shortChar->len, 0, costMtx2d->costMatrixDimension);
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

    //printf("After align cost.\n");
    //fflush(stdout);
    if (getGapped || getUngapped || getUnion) {
        //printf("Before backtrace.\n"), fflush(stdout);
        algn_backtrace_2d (shortChar, longChar, retShortChar, retLongChar, nw_mtxs2d, costMtx2d, 0, 0, swapped);
        // printf("After  backtrace.\n"), fflush(stdout);
        // dyn_char_print(retShortChar);
        // dyn_char_print(retLongChar);

        if (getUngapped) {
            dyn_char_p ungappedMedianChar = malloc(sizeof(dyn_character_t));
            initializeChar(ungappedMedianChar, CHAR_CAPACITY);

            algn_get_median_2d_no_gaps (retShortChar, retLongChar, costMtx2d, ungappedMedianChar);
            // printf("\n\nUngapped median:\n"), fflush(stdout);
            // dyn_char_print(ungappedMedianChar);

            dynCharToAlignIO(ungappedOutput_aio, ungappedMedianChar);
            // printf("\n\nUngapped median copy:\n"), fflush(stdout);
            // alignIO_print(ungappedOutput_aio);

            freeChar(ungappedMedianChar);


            dynCharToAlignIO(longIO,  retLongChar);
            dynCharToAlignIO(shortIO, retShortChar);

            // printf("\nAfter copy.\n"), fflush(stdout);
            // alignIO_print(longIO);
            // alignIO_print(shortIO);

        }
        if (getGapped && !getUnion) {
	    //printf("In here!\n"), fflush(stdout);
            dyn_char_p gappedMedianChar   = malloc(sizeof(dyn_character_t));

	    //printf("Before initialize character!\n"), fflush(stdout);
            initializeChar(gappedMedianChar, CHAR_CAPACITY);
	    //printf("After  initialize character!\n"), fflush(stdout);

	    //printf("Before algn_get_median\n"), fflush(stdout);
            algn_get_median_2d_with_gaps (retShortChar, retLongChar, costMtx2d, gappedMedianChar);
	    //printf("After  algn_get_median\n"), fflush(stdout);

	    //printf("Before dynCharToAlignIO\n"), fflush(stdout);
            dynCharToAlignIO(gappedOutput_aio, gappedMedianChar);
	    //printf("After  dynCharToAlignIO\n"),  fflush(stdout);

            freeChar(gappedMedianChar);

            dynCharToAlignIO(longIO,  retLongChar);
            dynCharToAlignIO(shortIO, retShortChar);

        }
        if (getUnion) {
            dyn_char_p gappedMedianChar = malloc(sizeof(dyn_character_t));

            initializeChar(gappedMedianChar, CHAR_CAPACITY);

            algn_union (retShortChar, retLongChar, gappedMedianChar);

            dynCharToAlignIO(gappedOutput_aio, gappedMedianChar);

            freeChar(gappedMedianChar);

            /*** following once union has its own output field again ***/
            // dyn_char_p unionChar = malloc(sizeof(dyn_character_t));
            // initializeChar(unionChar, CHAR_CAPACITY);
            // algn_union(retShortChar, retLongChar, gappedMedianChar);

            // dynCharToAlignIO(unionChar, unionOutputChar);
            // freeChar(unionChar);

            dynCharToAlignIO(longIO,  retLongChar);
            dynCharToAlignIO(shortIO, retShortChar);
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

    const size_t CHAR_CAPACITY = inputChar1_aio->length + inputChar2_aio->length + 2; // 2 to account for gaps,
                                                                                      // which will be added in initializeChar()
    // printf("capacity%zu\n", CHAR_CAPACITY);
    alignIO_p longIO,
              shortIO;


    dyn_char_p longChar     = malloc(sizeof(dyn_character_t)); // input to algn_nw_2d
    dyn_char_p shortChar    = malloc(sizeof(dyn_character_t)); // input to algn_nw_2d
    dyn_char_p retShortChar = malloc(sizeof(dyn_character_t)); // aligned character outputs from backtrace (not medians)
    dyn_char_p retLongChar  = malloc(sizeof(dyn_character_t)); // aligned character outputs from backtrace (not medians)

    /*** Most character allocation is now done on Haskell side, but these two are local. ***/
    /*** longChar and shortChar will both have pointers into the input characters, so don't need to be initialized separately ***/
    initializeChar(retLongChar,  CHAR_CAPACITY);
    initializeChar(retShortChar, CHAR_CAPACITY);

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
    //       TODO: I believe that the swapped flag is superfluous for our interface and
    //       the swapped != 0 code branches in algn_backtrace_2d is all dead code.
    // const int swapped = 0;

    size_t alphabetSize = costMtx2d_affine->costMatrixDimension;

    if (inputChar1_aio->length >= inputChar2_aio->length) {
        alignIOtoDynChar(longChar, inputChar1_aio, alphabetSize);
        longIO = inputChar1_aio;

        alignIOtoDynChar(shortChar, inputChar2_aio, alphabetSize);
        shortIO = inputChar2_aio;

    } else {
        alignIOtoDynChar(longChar, inputChar2_aio, alphabetSize);
        longIO = inputChar2_aio;

        alignIOtoDynChar(shortChar, inputChar1_aio, alphabetSize);
        shortIO = inputChar1_aio;
    }

    if (DEBUG_ALGN) {
        printf("\nafter copying, char 1:\n");
        dyn_char_print(longChar);
        printf("\nafter copying, char 2:\n");
        dyn_char_print(shortChar);
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
    int  lenLongerChar;                 //

    DIR_MTX_ARROW_t  *direction_matrix;

    nw_matrices_p nw_mtxs2dAffine = malloc( sizeof(nwMatrices_t) );
    initializeNWMtx(nw_mtxs2dAffine, longChar->len, shortChar->len, 0, costMtx2d_affine->costMatrixDimension);
    // printf("Jut initialized alignment matrices.\n");
    lenLongerChar = longChar->len;

    matrix_2d  = nw_mtxs2dAffine->nw_costMtx;
    precalcMtx = nw_mtxs2dAffine->precalcMtx;


    cm_precalc_4algn(costMtx2d_affine, nw_mtxs2dAffine, longChar);


    // here and in algn.c, "block" refers to a block of gaps, so close_block_diagonal is the cost to
    // end a subcharacter of gaps, presumably with a substitution, but maybe by simply switching directions:
    // there was a vertical gap, now there's a horizontal one.

    /** 2 through 11 below are offsets into various "matrices" in the alignment matrices, of which there are four
        of length 2 * longer_character and two of longer_character */
    close_block_diagonal            =  matrix_2d;
    extend_block_diagonal           = (matrix_2d + ( 2 * lenLongerChar));
    extend_vertical                 = (matrix_2d + ( 4 * lenLongerChar));
    extend_horizontal               = (matrix_2d + ( 6 * lenLongerChar));
    final_cost_matrix               = (matrix_2d + ( 8 * lenLongerChar));
    gap_open_prec                   = (matrix_2d + (10 * lenLongerChar));
    s_horizontal_gap_extension      = (matrix_2d + (11 * lenLongerChar));


    // TODO: empty_medianChar might not be necessary, as it's unused in ml code:
/*    size_t medianCharLen             = CHAR_CAPACITY;
    dyn_char_p empty_medianChar           = malloc( sizeof(dyn_character_t) );
    empty_medianChar->cap            = CHAR_CAPACITY;
    empty_medianChar->array_head     = calloc( CHAR_CAPACITY, sizeof(elem_t));
    empty_medianChar->len            = 0;
    empty_medianChar->char_begin      = empty_medianChar->end = empty_medianChar->array_head + CHAR_CAPACITY;
*/


    direction_matrix                = nw_mtxs2dAffine->nw_dirMtx;

    // printf("!!!!!  HERE !!!!!\n");

    // TODO: consider moving all of this into algn.
    //       the following three fns were initially not declared in algn.h
    algn_initialize_matrices_affine ( costMtx2d_affine->gap_open
                                    , shortChar
                                    , longChar
                                    , costMtx2d_affine
                                    , close_block_diagonal
                                    , extend_block_diagonal
                                    , extend_vertical
                                    , extend_horizontal
                                    , final_cost_matrix
                                    , direction_matrix
                                    , precalcMtx
                                    );

    int algnCost = algn_fill_plane_2d_affine ( shortChar
                                             , longChar
                                             , shortChar->len - 1  // -1 because of a loop condition in algn_fill_plane_2d_affine
                                             , longChar->len - 1   // -1 because of a loop condition in algn_fill_plane_2d_affine
                                             , final_cost_matrix
                                             , direction_matrix
                                             , costMtx2d_affine
                                             , extend_horizontal
                                             , extend_vertical
                                             , close_block_diagonal
                                             , extend_block_diagonal
                                             , precalcMtx
                                             , gap_open_prec
                                             , s_horizontal_gap_extension
                                             );

    if(getMedians) {
        dyn_char_p ungappedMedianChar = malloc(sizeof(dyn_character_t));
        dyn_char_p gappedMedianChar   = malloc(sizeof(dyn_character_t));
        initializeChar(ungappedMedianChar, CHAR_CAPACITY);
        initializeChar(gappedMedianChar,   CHAR_CAPACITY);

        algn_backtrace_affine( shortChar
                             , longChar
                             , direction_matrix
                             , ungappedMedianChar
                             , gappedMedianChar
                             , retShortChar
                             , retLongChar
                             , costMtx2d_affine
                             );

        dynCharToAlignIO(ungappedOutput_aio, ungappedMedianChar);
        dynCharToAlignIO(gappedOutput_aio,   gappedMedianChar);

        dynCharToAlignIO(longIO,  retLongChar);
        dynCharToAlignIO(shortIO, retShortChar);

        freeChar(ungappedMedianChar);
        freeChar(gappedMedianChar);
    }

    freeNWMtx(nw_mtxs2dAffine);
    freeChar(retLongChar);
    freeChar(retShortChar);

    return algnCost;
}


/** Do a 3d alignment. Depending on the values of last two inputs,
 *  | (0,0) = return only a cost
 *  | (0,1) = calculate gapped and ungapped characters
 *  | (1,0) = calculate union
 *  | (1,1) = calculate both union and ungapped characters.
 *
 *  In the last two cases the union will replace the gapped character placeholder.
 */
int align3d( const alignIO_p          inputChar1_aio
           , const alignIO_p          inputChar2_aio
           , const alignIO_p          inputChar3_aio
           , const alignIO_p          ungappedOutput_aio
           , const alignIO_p          gappedOutput_aio
           // , const nw_matrices_p      algn_mtxs3d
           , const cost_matrices_3d_p costMtx3d
           )
{

    if (DEBUG_ALGN) {
        printf("\n\nalign2d char1 input:\n");
        printf("\ninput char 1:");
        alignIO_print(inputChar1_aio);
        printf("input char 2:");
        alignIO_print(inputChar2_aio);
    }
    printf("gap char: %u\n", costMtx3d->gap_char);

    const size_t CHAR_CAPACITY = inputChar1_aio->length + inputChar2_aio->length + 2; // 2 to account for gaps,
                                                                                      // which will be added in initializeChar()
    int algnCost;

    alignIO_p longIO,
              middleIO,
              shortIO;

    dyn_char_p longChar      = malloc(sizeof(dyn_character_t)); // input to algn_nw_3d
    dyn_char_p middleChar    = malloc(sizeof(dyn_character_t)); // input to algn_nw_3d
    dyn_char_p shortChar     = malloc(sizeof(dyn_character_t)); // input to algn_nw_3d
    dyn_char_p retLongChar   = malloc(sizeof(dyn_character_t)); // aligned character outputs from backtrace (not medians)
    dyn_char_p retMiddleChar = malloc(sizeof(dyn_character_t)); // aligned character outputs from backtrace (not medians)
    dyn_char_p retShortChar  = malloc(sizeof(dyn_character_t)); // aligned character outputs from backtrace (not medians)

    /*** Most character allocation is now done on Haskell side, but these three are local. ***/
    /*** longChar, middleChar and shortChar will both have pointers into the input characters, so don't need to be initialized separately ***/
    initializeChar(retLongChar,   CHAR_CAPACITY);
    initializeChar(retMiddleChar, CHAR_CAPACITY);
    initializeChar(retShortChar,  CHAR_CAPACITY);


    size_t alphabetSize = costMtx3d->costMatrixDimension;

    // now sort inputs into appropriate structs by name
    if (inputChar1_aio->length >= inputChar2_aio->length) {

        if (inputChar3_aio->length >= inputChar1_aio->length) {        // input 3 is longest, 2 is shortest
            alignIOtoDynChar(longChar, inputChar3_aio, alphabetSize);
            longIO = inputChar3_aio;

            alignIOtoDynChar(middleChar, inputChar1_aio, alphabetSize);
            middleIO = inputChar1_aio;

            alignIOtoDynChar(shortChar, inputChar2_aio, alphabetSize);
            shortIO = inputChar2_aio;
        } else if (inputChar3_aio->length >= inputChar2_aio->length) { // input 1 is longest, 2 is shortest
            alignIOtoDynChar(longChar, inputChar1_aio, alphabetSize);
            longIO = inputChar1_aio;

            alignIOtoDynChar(middleChar, inputChar3_aio, alphabetSize);
            middleIO = inputChar3_aio;

            alignIOtoDynChar(shortChar, inputChar2_aio, alphabetSize);
            shortIO = inputChar2_aio;
        } else {                                                       // input 1 is longest, 3 is shortest
            alignIOtoDynChar(longChar, inputChar1_aio, alphabetSize);
            longIO = inputChar1_aio;

            alignIOtoDynChar(middleChar, inputChar2_aio, alphabetSize);
            middleIO = inputChar2_aio;

            alignIOtoDynChar(shortChar, inputChar3_aio, alphabetSize);
            shortIO = inputChar3_aio;
        }
    } else { // input 2 longer than input 1
        if (inputChar3_aio->length >= inputChar2_aio->length) {        // input 3 is longest, 1 is shortest
            alignIOtoDynChar(longChar, inputChar3_aio, alphabetSize);
            longIO = inputChar3_aio;

            alignIOtoDynChar(middleChar, inputChar2_aio, alphabetSize);
            middleIO = inputChar2_aio;

            alignIOtoDynChar(shortChar, inputChar1_aio, alphabetSize);
            shortIO = inputChar1_aio;
        } else if (inputChar3_aio->length >= inputChar1_aio->length) { // input 2 is longest, 1 is shortest
            alignIOtoDynChar(longChar, inputChar2_aio, alphabetSize);
            longIO = inputChar2_aio;

            alignIOtoDynChar(middleChar, inputChar3_aio, alphabetSize);
            middleIO = inputChar3_aio;

            alignIOtoDynChar(shortChar, inputChar1_aio, alphabetSize);
            shortIO = inputChar1_aio;
        } else {                                                       // input 2 is longest, 3 is shortest
            alignIOtoDynChar(longChar, inputChar2_aio, alphabetSize);
            longIO = inputChar2_aio;

            alignIOtoDynChar(middleChar, inputChar1_aio, alphabetSize);
            middleIO = inputChar1_aio;

            alignIOtoDynChar(shortChar, inputChar3_aio, alphabetSize);
            shortIO = inputChar3_aio;
        }
    }

    if (DEBUG_3D) {
        printf("\nafter copying, long (char 1):\n");
        dyn_char_print(longChar);
        printf("\nafter copying, middle (char 2):\n");
        dyn_char_print(middleChar);
        printf("\nafter copying, short (char 3):\n");
        dyn_char_print(shortChar);
    }

    longChar->array_head[0] = middleChar->array_head[0] = shortChar->array_head[0] = 8;

    algnCost = powell_3D_align ( longChar
                               , shortChar
                               , middleChar
                               , retLongChar
                               , retShortChar
                               , retMiddleChar
                               , 1    // mismatch cost, must be > 0
                               , 2    // gap open cost, must be >= 0
                               , 1    // gap extend cost, must be > 0
                               );

    dyn_char_p ungappedMedianChar = malloc(sizeof(dyn_character_t));
    dyn_char_p gappedMedianChar   = malloc(sizeof(dyn_character_t));
    initializeChar(ungappedMedianChar, CHAR_CAPACITY);
    initializeChar(gappedMedianChar,   CHAR_CAPACITY);

    algn_get_medians_3d ( retLongChar
                        , retMiddleChar
                        , retShortChar
                        , costMtx3d
                        , ungappedMedianChar
                        , gappedMedianChar
                        );

    dynCharToAlignIO(ungappedOutput_aio, ungappedMedianChar);
    dynCharToAlignIO(gappedOutput_aio,   gappedMedianChar);

    dynCharToAlignIO(longIO,   retLongChar);
    dynCharToAlignIO(middleIO, retMiddleChar);
    dynCharToAlignIO(shortIO,  retShortChar);

    freeChar(ungappedMedianChar);
    freeChar(gappedMedianChar);

    freeChar(retLongChar);
    freeChar(retMiddleChar);
    freeChar(retShortChar);

    return algnCost;
}
