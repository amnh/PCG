#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "alignCharacters.h"
#include "alignmentMatrices.h"
#include "c_alignment_interface.h"
#include "c_code_alloc_setup.h"
#include "debug_constants.h"
#include "dyn_character.h"
#include "ukkCommon.h"


int align2d( alignIO_t          *inputChar1_aio
           , alignIO_t          *inputChar2_aio
           , alignIO_t          *gappedOutput_aio
           , alignIO_t          *ungappedOutput_aio
           // , alignIO_t          *unionOutput_aio
           , cost_matrices_2d_t *costMtx2d
           , int                 getUngapped
           , int                 getGapped
           , int                 getUnion
           )
{

    if (DEBUG_ALGN) {
        printf("\n\nalign2d char1 input:\n");
        printf("\ninput char 1:");
        alignIO_print(inputChar1_aio);
        printf("input char 2:");
        alignIO_print(inputChar2_aio);
    }

    // TODO: I'd still ike to get rid of these stupid extra characters
    const size_t CHAR_CAPACITY = inputChar1_aio->length + inputChar2_aio->length + 2; // 2 to account for gaps,
                                                                                      // which will be added in dyn_char_initialize()

    alignIO_t *longIO,
              *shortIO;

    /*** Most character allocation is now done on Haskell side, but these two are local. ***/
    /*** longChar and shortChar will both have pointers into the input characters, so don't need to be initialized separately ***/
    dyn_character_t *retLongChar  = dyn_char_alloc(CHAR_CAPACITY);
    dyn_character_t *retShortChar = dyn_char_alloc(CHAR_CAPACITY);
    dyn_character_t *longChar     = dyn_char_alloc(0);
    dyn_character_t *shortChar    = dyn_char_alloc(0);
    size_t alphabetSize = costMtx2d->alphSize;

    int firstCharIsLongerOrBothAreIdenticalInValue = 1;

    if (inputChar1_aio->length < inputChar2_aio->length) {
        firstCharIsLongerOrBothAreIdenticalInValue = 0;
    }
    else if (inputChar1_aio->length > inputChar2_aio->length) {
        firstCharIsLongerOrBothAreIdenticalInValue = 1;
    }
    else { // Length is equal!
        // Use lexical ordering to determine "length" in the case of equal length dynamic characters
        // Two dynamic chanracters will *only* be equal in length if they contain the same sequence of elements.
        // By doing this we ensure that the alignment operation is commutative.
        elem_t *p1 = inputChar1_aio->character + (inputChar1_aio->capacity - inputChar1_aio->length);
        elem_t *p2 = inputChar2_aio->character + (inputChar2_aio->capacity - inputChar2_aio->length);
        for (size_t i = 0; i < inputChar1_aio->length; ++i) {
            if (p1[i] != p2[i]) {
                firstCharIsLongerOrBothAreIdenticalInValue = p1[i] > p2[i];
                break;
            }
        }
    }

    if (firstCharIsLongerOrBothAreIdenticalInValue) {
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
    alignment_matrices_t *algnMtxs2d = malloc( sizeof(alignment_matrices_t) );
    assert( algnMtxs2d != NULL && "2D alignment matrices could not be allocated." );

    initializeAlignmentMtx( algnMtxs2d, longChar->len, shortChar->len, alphabetSize );

    // deltawh is for use in Ukonnen, it gives the current necessary width of the Ukk matrix.
    // The following calculation to compute deltawh, which increases the matrix height or width in algn_nw_2d,
    // was pulled from POY ML code.
    int deltawh     = 0;
    int diff        = longChar->len - shortChar->len;
    int lower_limit = .1 * longChar->len;

    if (deltawh) {
        deltawh = diff < lower_limit ? lower_limit : deltawh;
    } else {
        deltawh = diff < lower_limit ? lower_limit / 2 : 2;
    }

    int algnCost = algn_nw_2d( shortChar, longChar, costMtx2d, algnMtxs2d, deltawh );

    if (getGapped || getUngapped || getUnion) {
        //printf("Before backtrace.\n"), fflush(stdout);
        algn_backtrace_2d( shortChar, longChar, retShortChar, retLongChar, algnMtxs2d, costMtx2d, 0, 0 );

        if (getUngapped) {
            dyn_character_t *ungappedMedianChar = dyn_char_alloc( CHAR_CAPACITY );

            algn_get_median_2d_no_gaps( retShortChar, retLongChar, costMtx2d, ungappedMedianChar );

            dynCharToAlignIO( ungappedOutput_aio, ungappedMedianChar, 1 );
            dynCharToAlignIO( longIO,  retLongChar, 1 );
            dynCharToAlignIO( shortIO, retShortChar, 1 );

            dyn_char_free( ungappedMedianChar );
            if (NULL != ungappedMedianChar) free(ungappedMedianChar);
        }
        if (getGapped && !getUnion) {
            dyn_character_t *gappedMedianChar = dyn_char_alloc( CHAR_CAPACITY );

            algn_get_median_2d_with_gaps( retShortChar, retLongChar, costMtx2d, gappedMedianChar );

            dynCharToAlignIO( gappedOutput_aio, gappedMedianChar, 1 );
            dynCharToAlignIO( longIO,  retLongChar, 1 );
            dynCharToAlignIO( shortIO, retShortChar, 1 );

            dyn_char_free( gappedMedianChar );
            if (NULL != gappedMedianChar) free(gappedMedianChar);
        }
        if (getUnion) {
            dyn_character_t *unionMedianChar = dyn_char_alloc(CHAR_CAPACITY);

            algn_union( retShortChar, retLongChar, unionMedianChar );

            dynCharToAlignIO( gappedOutput_aio, unionMedianChar, 1 );

            dyn_char_free( unionMedianChar );
            if (NULL != unionMedianChar) free(unionMedianChar);

            /*** following once union has its own output field again ***/
            // dyn_character_t *unionChar = malloc(sizeof(dyn_character_t));
            // dyn_char_alloc(unionChar, CHAR_CAPACITY);
            // algn_union(retShortChar, retLongChar, gappedMedianChar);

            // dynCharToAlignIO(unionChar, unionOutputChar);
            // dyn_char_free(unionChar);
            // if (NULL != retLongChar) free(unionChar);

            dynCharToAlignIO( longIO,  retLongChar,  1 );
            dynCharToAlignIO( shortIO, retShortChar, 1 );
        }
    }

    freeNWMtx( algnMtxs2d );
    dyn_char_free( retLongChar );
    if (NULL != retLongChar) free(retLongChar);
    dyn_char_free( retShortChar );
    if (NULL != retShortChar) free(retShortChar);

    // Shouldn't have to free the whole structs here because they just pointed into the retChars.
    if (NULL != longChar) free(longChar);
    if (NULL != shortChar) free(shortChar);

    return algnCost;

}


int align2dAffine( alignIO_t          *inputChar1_aio
                 , alignIO_t          *inputChar2_aio
                 , alignIO_t          *gappedOutput_aio
                 , alignIO_t          *ungappedOutput_aio
                 // , alignIO_t          *unionOutput_aio
                 , cost_matrices_2d_t *costMtx2d_affine
                 , int                 getMedians
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
                                                                                      // which will be added in dyn_char_initialize()
    alignIO_t *longIO,
              *shortIO;

    /*** Most character allocation is now done on Haskell side, but these two are local. ***/
    /*** longChar and shortChar will both have pointers into the input characters, so don't need to be initialized separately ***/
    dyn_character_t *longChar     = dyn_char_alloc(0);
    dyn_character_t *shortChar    = dyn_char_alloc(0);
    dyn_character_t *retLongChar  = dyn_char_alloc(CHAR_CAPACITY);
    dyn_character_t *retShortChar = dyn_char_alloc(CHAR_CAPACITY);

    size_t alphabetSize = costMtx2d_affine->alphSize;

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
    unsigned int *close_block_diagonal;       //
    unsigned int *extend_block_diagonal;      //
    unsigned int *extend_vertical;            //
    unsigned int *extend_horizontal;          //
    unsigned int *final_cost_matrix;          //
    unsigned int *precalcMtx;                 //
    unsigned int *matrix_2d;                  //
    unsigned int *precalc_gap_open_cost;      // precalculated gap opening value (top row of nw matrix)
    unsigned int *s_horizontal_gap_extension; //
    size_t        lenLongerChar;              //

    DIR_MTX_ARROW_t  *direction_matrix;

    alignment_matrices_t *algnMtxs2dAffine = malloc( sizeof(alignment_matrices_t) );
    assert( algnMtxs2dAffine != NULL && "Can't allocate 2D affine alignment matrices." );

    initializeAlignmentMtx(algnMtxs2dAffine, longChar->len, shortChar->len, alphabetSize );
    // printf("Jut initialized alignment matrices.\n");
    lenLongerChar = longChar->len;

    matrix_2d  = algnMtxs2dAffine->algn_costMtx;
    precalcMtx = algnMtxs2dAffine->algn_precalcMtx;


    algnMtx_precalc_4algn_2d( algnMtxs2dAffine, costMtx2d_affine, longChar );


    // here and in algn.c, "block" refers to a block of gaps, so close_block_diagonal is the cost to
    // end a subcharacter of gaps, presumably with a substitution, but maybe by simply switching directions:
    // there was a vertical gap, now there's a horizontal one.

    /** 2 through 11 below are offsets into various "matrices" in the alignment matrices, of which there are four
        of length 2 * longer_character and two of longer_character */
    close_block_diagonal       =  matrix_2d;
    extend_block_diagonal      = (matrix_2d + ( lenLongerChar *  2 ));
    extend_vertical            = (matrix_2d + ( lenLongerChar *  4 ));
    extend_horizontal          = (matrix_2d + ( lenLongerChar *  6 ));
    final_cost_matrix          = (matrix_2d + ( lenLongerChar *  8 ));
    precalc_gap_open_cost      = (matrix_2d + ( lenLongerChar * 10 ));
    s_horizontal_gap_extension = (matrix_2d + ( lenLongerChar * 11 ));

    direction_matrix           = algnMtxs2dAffine->algn_dirMtx;

    // TODO: consider moving all of this into algn.
    //       the following three fns were initially not declared in algn.h
    algn_initialize_matrices_affine ( costMtx2d_affine->gap_open_cost
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

    int algnCost = algn_fill_plane_2d_affine( shortChar
                                            , longChar
                                            , shortChar->len - 1  // -1 because of a loop condition in algn_fill_plane_2d_affine
                                            , longChar->len  - 1  // -1 because of a loop condition in algn_fill_plane_2d_affine
                                            , final_cost_matrix
                                            , direction_matrix
                                            , costMtx2d_affine
                                            , extend_horizontal
                                            , extend_vertical
                                            , close_block_diagonal
                                            , extend_block_diagonal
                                            , precalcMtx
                                            , precalc_gap_open_cost
                                            , s_horizontal_gap_extension
                                            );

    if(getMedians) {
        dyn_character_t *ungappedMedianChar = dyn_char_alloc(CHAR_CAPACITY);
        dyn_character_t *gappedMedianChar   = dyn_char_alloc(CHAR_CAPACITY);

        algn_backtrace_affine( shortChar
                             , longChar
                             , direction_matrix
                             , ungappedMedianChar
                             , gappedMedianChar
                             , retShortChar
                             , retLongChar
                             , costMtx2d_affine
                             );

        dynCharToAlignIO( ungappedOutput_aio, ungappedMedianChar, 1 );
        dynCharToAlignIO( gappedOutput_aio,   gappedMedianChar, 1 );

        dynCharToAlignIO( longIO,  retLongChar, 1 );
        dynCharToAlignIO( shortIO, retShortChar, 1 );

        dyn_char_free(ungappedMedianChar);
        if (NULL != ungappedMedianChar) free(ungappedMedianChar);
        dyn_char_free(gappedMedianChar);
        if (NULL != gappedMedianChar) free(gappedMedianChar);
    }

    freeNWMtx(algnMtxs2dAffine);
    /**** Can't free these structs internals because they're pointing into inputChar1_aio and inputChar2_aio ****/
    // dyn_char_free(longChar);
    // dyn_char_free(shortChar);
    if (NULL != longChar) free(longChar);
    if (NULL != shortChar) free(shortChar);
    dyn_char_free(retLongChar);
    if (NULL != retLongChar) free(retLongChar);
    dyn_char_free(retShortChar);
    if (NULL != retShortChar) free(retShortChar);

    return algnCost;
}


/** Set `gap_open_cost` == `gap_extension_cost` for non-affine. */
// TODO: double check this
int align3d( alignIO_t          *inputChar1_aio
           , alignIO_t          *inputChar2_aio
           , alignIO_t          *inputChar3_aio
           , alignIO_t          *outputChar1_aio
           , alignIO_t          *outputChar2_aio
           , alignIO_t          *outputChar3_aio
           , alignIO_t          *gappedOutput_aio
           , alignIO_t          *ungappedOutput_aio
           , cost_matrices_3d_t *costMtx3d
           , unsigned int        substitution_cost
           , unsigned int        gap_open_cost      // Set `gap_open_cost` == `gap_extension_cost` for non-affine. TODO: check this.
           , unsigned int        gap_extension_cost
           )
{

    if (DEBUG_3D) {
        printf("\n\nalign3d input:\n");
        printf("\ninput char 1:");
        alignIO_print( inputChar1_aio );

        printf("input char 2:");
        alignIO_print( inputChar2_aio );

        printf("input char 3:");
        alignIO_print( inputChar3_aio );

        printf("\n\nalph size: %zu,   matrix dimension: %zu,   gap char: %u\n", costMtx3d->alphSize, costMtx3d->costMatrixDimension, costMtx3d->gap_char);
    }

    const size_t CHAR_CAPACITY = inputChar1_aio->length + inputChar2_aio->length + inputChar3_aio->length;

    unsigned int algnCost;

    // powellInputs will be sent to Powell 3D alignment, powellOutputs will be returned.
    characters_t *powellInputs  = alloc_characters_t(inputChar1_aio->length, inputChar2_aio->length, inputChar3_aio->length);
    characters_t *powellOutputs = alloc_characters_t(CHAR_CAPACITY, CHAR_CAPACITY, CHAR_CAPACITY);

    alignIOtoCharacters_t( powellInputs, inputChar1_aio, inputChar2_aio, inputChar3_aio );

    if (DEBUG_CALL_ORDER) printf( "\n---Calling Powell\n\n" );

    // Powell aligns three sequences.
    algnCost = powell_3D_align ( powellInputs
                               , powellOutputs
                               , 4 // costMtx3d->alphSize
                               , substitution_cost   // mismatch cost, must be > 0
                               , gap_open_cost       // must be >= 0
                               , gap_extension_cost  // gap extension cost: must be > 0
                               );

    dyn_character_t *gappedMedianChar   = dyn_char_alloc( powellOutputs->idxSeq1 );
    dyn_character_t *ungappedMedianChar = dyn_char_alloc( powellOutputs->idxSeq1 );

    algnCost = algn_get_cost_medians_3d( powellOutputs
                                       , costMtx3d
                                       , ungappedMedianChar
                                       , gappedMedianChar
                                       );

    dynCharToAlignIO( gappedOutput_aio,   gappedMedianChar,   0 );
    dynCharToAlignIO( ungappedOutput_aio, ungappedMedianChar, 0 );

    copyValsToAIO( outputChar1_aio, powellOutputs->seq1, powellOutputs->lenSeq1, powellOutputs->lenSeq1 );
    copyValsToAIO( outputChar2_aio, powellOutputs->seq2, powellOutputs->lenSeq1, powellOutputs->lenSeq1 );
    copyValsToAIO( outputChar3_aio, powellOutputs->seq3, powellOutputs->lenSeq1, powellOutputs->lenSeq1 );

    reverseCharacterElements(outputChar1_aio);
    reverseCharacterElements(outputChar2_aio);
    reverseCharacterElements(outputChar3_aio);
    reverseCharacterElements(gappedOutput_aio);
    reverseCharacterElements(ungappedOutput_aio);

    dyn_char_free(gappedMedianChar);
    if (gappedMedianChar != NULL) free(gappedMedianChar);
    dyn_char_free(ungappedMedianChar);
    if (ungappedMedianChar != NULL) free(ungappedMedianChar);

    free_characters_t(powellInputs);
    if (powellInputs != NULL) free(powellInputs);
    free_characters_t(powellOutputs);
    if (powellOutputs != NULL) free(powellOutputs);

    return algnCost;
}


void reverseCharacterElements ( const alignIO_t *aio )
{
    size_t offset = aio->capacity - aio->length;
    revElem_tArray(aio->character, offset, aio->capacity - 1);
}

void alignIO_print( const alignIO_t *character )
{
    printf("\n");
    printf("Length:   %zu\n", character->length);
    printf("Capacity: %zu\n", character->capacity);
    size_t charStart = character->capacity - character->length;

    for(size_t i = charStart; i < character->capacity; i++) {
        printf("%3d,", character->character[i]);
        //if (character->character[i] == 0) continue;
    }
    printf("\n\n");
    fflush(stdout);
}


void alignIOtoDynChar(       dyn_character_t *retChar
                     , const alignIO_t       *input
                     ,       size_t           alphabetSize
                     )
{
    // Assign character into character struct. Note not copying, just creating pointers.
    size_t offset       = input->capacity - input->length;
    retChar->len        = input->length;
    retChar->cap        = input->capacity;
    retChar->array_head = input->character;
    retChar->end        = input->character + input->capacity - 1;
    retChar->char_begin = input->character + offset;

    // now add gap to beginning, Grrr.
    retChar->char_begin--;   // Add another cell, prepended to the array
    *retChar->char_begin = ((elem_t) 1) << (alphabetSize - 1);   //Prepend a gap to the array.
    retChar->len++;
}


void alignIOtoCharacters_t( characters_t *output
                          , alignIO_t    *inputChar1
                          , alignIO_t    *inputChar2
                          , alignIO_t    *inputChar3
                          )
{
    output->seq1 = realloc( output->seq1, inputChar1->length * sizeof(elem_t) );
    output->seq2 = realloc( output->seq2, inputChar2->length * sizeof(elem_t) );
    output->seq3 = realloc( output->seq3, inputChar3->length * sizeof(elem_t) );

    memcpy( output->seq1, inputChar1->character + inputChar1->capacity - inputChar1->length, inputChar1->length * sizeof(elem_t) );
    memcpy( output->seq2, inputChar2->character + inputChar2->capacity - inputChar2->length, inputChar2->length * sizeof(elem_t) );
    memcpy( output->seq3, inputChar3->character + inputChar3->capacity - inputChar3->length, inputChar3->length * sizeof(elem_t) );

    output->lenSeq1 = inputChar1->length;
    output->lenSeq2 = inputChar2->length;
    output->lenSeq3 = inputChar3->length;

    output->idxSeq1 = 0;
    output->idxSeq2 = 0;
    output->idxSeq3 = 0;
}



alignIO_t *allocAlignIO( size_t capacity )
{
    alignIO_t *toReturn = malloc( sizeof(alignIO_t) );
    assert( NULL != toReturn && "Out of memory: Can't allocate alignIO." );
    toReturn->length    = 0;
    toReturn->capacity  = capacity;
    toReturn->character = calloc(capacity, sizeof(elem_t));
    assert( NULL != toReturn->character && "Out of memory: Can't allocate alignIO character field." );
    return(toReturn);
}


void copyAioToVals( elem_t *vals, alignIO_t *inChar )
{
    unsigned int *character_begin = inChar->character + inChar->capacity - inChar->length;
    for (size_t i = 0; i < inChar->length; i++) {
        vals[i] = (char) character_begin[i];  // truncating from unsigned int to char
    }
}


void copyValsToAIO( alignIO_t *outChar, elem_t *vals, size_t length, size_t capacity )
{
    outChar->length   = length;
    outChar->capacity = capacity;
    outChar->character = realloc( outChar->character, outChar->capacity * sizeof(elem_t) );
    assert( NULL != outChar->character && "Can't allocate alignIO character field." );

    size_t offset = capacity - length;
    memcpy(outChar->character + offset, vals, length * sizeof(elem_t));
}


/** Takes in an alignIO and a char. *Copies* values of character from end of char to end of alignIO->character, so output must already
 *  be alloc'ed.
 *  Also eliminates extra gap at beginning of character that was needed by legacy code, as well as 0 that Powell appends to end of
 *  character.
 */
void dynCharToAlignIO( alignIO_t *output, dyn_character_t *input, int delete_initial_gap )
{

    if (DEBUG_ALGN) {
        printf("input:\n");
        printf("  Length:   %zu\n", input->len);
        printf("  Capacity: %zu\n", input->cap);
        printf("output:\n");
        printf("  Length:   %zu\n", output->length);
        printf("  Capacity: %zu\n", output->capacity);
        fflush(stdout);
    }

    size_t  copy_length;    // These two because ungapped characters will have their initial gaps removed, so may be length 0.
    elem_t *input_begin;

    // Now set length to copy and copy initial read location.
    // If input length > 0, Decrement because of the leading gap.
    // Ungapped character already has had initial gap removed.
    // Likewise, copy start has different setting for two conditions.
    if (input->len == 0) {
        copy_length = 0;
        input_begin = input->char_begin;
    } else if (delete_initial_gap) {
        copy_length = input->len - 1;
        input_begin = input->char_begin + 1;
    } else {
        copy_length = input->len;
        input_begin = input->char_begin;
    }

    output->length = copy_length;
    // output->capacity = input->cap;     // this shouldn't change
    size_t offset  = output->capacity - copy_length; // How far into output to start copying input character.

    output->character = realloc( output->character, output->capacity * sizeof(elem_t) );
    assert( NULL != output->character && "Can't allocate alignIO character field." );

    // Start copy after unnecessary gap char in input, if it exists.
    memcpy( output->character + offset, input_begin, copy_length * sizeof(elem_t) );

}


void freeAlignIO(alignIO_t *toFree )
{
    if (NULL != toFree->character) free(toFree->character);
    // free(toFree);
}


void reallocAlignIO(alignIO_t *toAlloc, size_t capacity )
{
    toAlloc->length    = 0;
    toAlloc->capacity  = capacity;
    toAlloc->character = realloc(toAlloc->character, capacity * sizeof(elem_t));
    assert( NULL != toAlloc->character && "OOM: can't allocate aligntIO_t character." );
}


void resetAlignIO(alignIO_t *inChar)
{
    memset(inChar->character, 0, inChar->capacity * sizeof(elem_t));
    inChar->length = 0;
}
