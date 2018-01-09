/** Tests external DO code under various conditions:
    1. with varying alphabet lengths
    2. with random-length characters
    3. with varying character patterns
 */

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "../../alignCharacters.h"
#include "../../alignmentMatrices.h"
#include "../../c_alignment_interface.h"
#include "../../c_code_alloc_setup.h"
#include "../../costMatrix.h"
#include "../../debug_constants.h"

#define CHAR_LENGTH 100


void do2D_nonAffine( unsigned int *tcm, size_t alphSize );
void do2D_affine( unsigned int *tcm, size_t alphSize );
void do3D( unsigned int *tcm, size_t alphSize );


int power_2 (int input) {
    return (__builtin_popcount(input) == 1);
}

void set_vals( elem_t *vals, size_t vals_length, size_t max_val) {
    elem_t curVal;
    for (size_t k = 0; k < vals_length; k++ ) {
        curVal = rand() % (max_val);

        while (curVal == 0) curVal = rand() % (max_val);

        vals[k] = curVal;
    }
}

int main() {

/******************************** set up and allocate all variables and structs ************************************/



    size_t i, j;                    // various loop indices

    // size_t maxLength;               // max length of an aligned character, given two chars to align

    srand( (unsigned) time(NULL) ); // initialize random number generator

    /************  Allocate cost matrices  **************/

    //TODO: this will have to be changed to account for the fact that you  might have 2D and 3D.
    size_t alphSize;
    if (DO_3D) alphSize = 5;
    else       alphSize = rand() % 6 + 3;     // includes gap, but no ambiguities; alphabet sizes between binary and 8 are accepted

    size_t tcm_total_len     = alphSize * alphSize; // the size of the input tcm

    /** TCM is only for non-ambiguous nucleotides, and it used to generate
     *  the entire cost matrix, which includes ambiguous elements.
     *  TCM is row-major, with each row being the left character element.
     *  It is therefore indexed not by powers of two, but by cardinal integer.
     *  This particular example is both metric and symmetric. All TCMs must be
     *  symmetric. Metricity is decided by PCG application.
     */
    unsigned int *tcm = calloc(tcm_total_len, sizeof(int)); // this is the input tcm, not the generated one
    assert( tcm != NULL && "Couldn't allocate simple tcm in interface test." );

    for (i = 0; i < tcm_total_len; i += alphSize) {
        //printf("i: %zu\n", i);
        for (j = 0; j < alphSize; j++) {
            //tcm[i + j] = 2 * i + 2 * j;
            if ( i == j * alphSize ) {
                tcm[i + j] = IDENTITY_COST;    // identity
                printf("i: %zu, j: %zu, cost: %d\n", i, j, IDENTITY_COST);
            }
            else if (i == (tcm_total_len - alphSize) || j == (alphSize - 1)) {
                tcm[i + j] = INDEL_COST;       // indel cost
                printf("i: %zu, j: %zu, cost: %d\n", i, j, INDEL_COST);
            }
            else {
                tcm[i + j] = SUB_COST;         // sub cost
                printf("i: %zu, j: %zu, cost: %d\n", i, j, SUB_COST);
            }
        }
    }
    if (DO_2D)     do2D_nonAffine( tcm, alphSize );

    if (DO_2D_AFF) do2D_affine( tcm, alphSize );

    if (DO_3D)     do3D( tcm, alphSize );

    // Next this: algn_get_median_3d (dyn_char_p inputChar1, dyn_char_p inputChar2, dyn_char_p char3,
    //                cost_matrices_3d_t *m, dyn_char_p sm)

    free(tcm);

    return 0;
}


// /**************************************************** Do 2d alignment ********************************************************/

void do2D_nonAffine( unsigned int * tcm, size_t alphSize )
{
    printf("\n\n\n******************** Align 2 characters ********************\n");

    cost_matrices_2d_t *costMtx2d = malloc(sizeof(struct cost_matrices_2d_t));
    assert( costMtx2d != NULL && "Couldn't allocate 2D cost matrix in interface test." );

    setUp2dCostMtx (costMtx2d, tcm, alphSize, 0);

    elem_t one      = 1;
    elem_t gap_char = one << (alphSize - 1);
    elem_t max_val  = one << alphSize;     // actually one more than max, because we're using it to mod

    int algnCost;

    elem_t *longer_vals = malloc(sizeof(elem_t)),
           *lesser_vals = malloc(sizeof(elem_t));
    assert(   longer_vals != NULL
           && lesser_vals != NULL
           && "Couldn't allocate input character values in 2D interface test." );

    size_t longerLen,
           lesserLen,
           maxLength;

    alignIO_t *lesserInputChar = malloc( sizeof(struct alignIO_t) );    // inputs to align2d fn.
    alignIO_t *longerInputChar = malloc( sizeof(struct alignIO_t) );    // inputs to align2d fn.

    alignIO_t *ungappedMedianChar = malloc( sizeof(struct alignIO_t) );
    alignIO_t *gappedMedianChar   = malloc( sizeof(struct alignIO_t) );
    alignIO_t *unionMedianChar    = malloc( sizeof(struct alignIO_t) );

    assert(   lesserInputChar    != NULL
           && longerInputChar    != NULL
           && ungappedMedianChar != NULL
           && gappedMedianChar   != NULL
           && unionMedianChar    != NULL
           && "Couldn't allocate input or median characters structs in 2D interface test." );

    // set to 1 so I can realloc later:
    allocAlignIO(lesserInputChar,    1);
    allocAlignIO(longerInputChar,    1);

    allocAlignIO(ungappedMedianChar, 1);
    allocAlignIO(gappedMedianChar,   1);


    for (size_t i = 1; i <= 30; i++) { // run 30 tests

        longerLen = rand() % CHAR_LENGTH + 1;
        lesserLen = rand() % CHAR_LENGTH + 1;
        maxLength = longerLen + lesserLen + 2; // 2 because there are two gaps added (1 on beginning of each character)

        // need to realloc each time through the loop
        longer_vals = realloc( longer_vals, longerLen * sizeof(elem_t) );
        lesser_vals = realloc( lesser_vals, lesserLen * sizeof(elem_t) );
        assert(   longer_vals    != NULL
               && lesser_vals    != NULL
               && "Couldn't reallocate input character vals in 2D interface test." );

        set_vals( longer_vals, longerLen, max_val );
        set_vals( lesser_vals, lesserLen, max_val );

        // need to allocate space for return alignIOs, as they're no longer alloc'ed in c_alignment_interface
        reallocAlignIO( lesserInputChar, maxLength );
        reallocAlignIO( longerInputChar, maxLength );

        reallocAlignIO ( ungappedMedianChar, maxLength );
        reallocAlignIO ( gappedMedianChar,   maxLength );

        copyValsToAIO( lesserInputChar, longer_vals, longerLen, maxLength );
        copyValsToAIO( longerInputChar, lesser_vals, lesserLen, maxLength );

        allocAlignIO(unionMedianChar, maxLength);


        printf("\n\n********** Cost only (all chars should be empty): **********\n");
        printf("  \n***************** Original 2d characters: ******************\n");
        printf("  \n******************** Alphabet Size: %zu ********************\n", alphSize);
        alignIO_print( lesserInputChar );
        alignIO_print( longerInputChar );

        algnCost = align2d( lesserInputChar
                          , longerInputChar
                          , gappedMedianChar
                          , ungappedMedianChar
                          // , unionMedianChar
                          , costMtx2d
                          , 0                    // do ungapped
                          , 0                    // do gapped
                          , 0                    // do union
                          );
        // if (DEBUG_MAT) {
        //     printf("\n\nFinal alignment matrix: \n\n");
        //     algn_print_dynmtrx_2d( longChar, shortChar, algn_mtxs2d );
        // }

        printf("Alignment cost: %d\n", algnCost);

        printf("\nAligned 2d characters (should be the same as inputs, as no backtrace has been performed)\n");
        alignIO_print(lesserInputChar);
        alignIO_print(longerInputChar);

        printf("\n  Gapped character  ");
        alignIO_print(gappedMedianChar);

        printf("\n  Ungapped character  ");
        alignIO_print(ungappedMedianChar);

        // union:
        // algn_union (retShortChar, retLongChar, algnChar);
        // printf("  Unioned character\n  ");
        // alignIO_print(unionMedianChar);
        // printf("here.\n");

        resetAlignIO( lesserInputChar );
        resetAlignIO( longerInputChar );
        resetAlignIO( ungappedMedianChar );
        resetAlignIO( gappedMedianChar );
        // resetAlignIO(unionMedianChar);



        // printf("\n\n********** Ungapped only (gapped should be empty): **********\n");

        printf("  \n****************** Original 2d characters: ******************\n");
        printf("Alphabet size: %zu\n", alphSize);
        printf("gap character: %u\n",  gap_char);

        alignIO_print(lesserInputChar);
        alignIO_print(longerInputChar);

        algnCost = align2d( lesserInputChar
                          , longerInputChar
                          , gappedMedianChar
                          , ungappedMedianChar
                          // , unionMedianChar
                          , costMtx2d
                          , 1                    // do ungapped
                          , 1                    // do gapped
                          , 0                    // do union
                          );

        printf("\nAligned 2d characters\n");
        alignIO_print(lesserInputChar);
        alignIO_print(longerInputChar);

        printf("Alignment cost: %d\n", algnCost);

        printf("\nGapped character  ");
        alignIO_print(gappedMedianChar);

        printf("\nUngapped character  ");
        alignIO_print(ungappedMedianChar);

        resetAlignIO(lesserInputChar);
        resetAlignIO(longerInputChar);
        resetAlignIO(ungappedMedianChar);
        resetAlignIO(gappedMedianChar);

        copyValsToAIO(lesserInputChar, longer_vals, longerLen, maxLength);
        copyValsToAIO(longerInputChar, lesser_vals, lesserLen, maxLength);

        printf("\n\n********** Gapped only (ungapped should be empty): **********\n");
        printf("  \n*******************Original 2d characters:*******************\n");
        printf("Alphabet size: %zu\n", alphSize);
        printf("gap character: %u\n", gap_char);

        alignIO_print(lesserInputChar);
        alignIO_print(longerInputChar);

        algnCost = align2d(lesserInputChar,
                           longerInputChar,
                           gappedMedianChar,
                           ungappedMedianChar,
                           // unionMedianChar,
                           costMtx2d,
                           0,                    // do ungapped
                           1,                    // do gapped
                           0);                   // do union

        printf("Alignment cost: %d\n", algnCost);

        printf("\nAligned 2d characters\n");
        alignIO_print(lesserInputChar);
        alignIO_print(longerInputChar);

        printf("\nGapped character  ");
        alignIO_print(gappedMedianChar);

        printf("\nUngapped character  ");
        alignIO_print(ungappedMedianChar);

        resetAlignIO(lesserInputChar);
        resetAlignIO(longerInputChar);
        resetAlignIO(ungappedMedianChar);
        resetAlignIO(gappedMedianChar);

        copyValsToAIO(lesserInputChar, longer_vals, longerLen, maxLength);
        copyValsToAIO(longerInputChar, lesser_vals, lesserLen, maxLength);

        resetAlignIO(lesserInputChar);
        resetAlignIO(longerInputChar);
        resetAlignIO(ungappedMedianChar);
        resetAlignIO(gappedMedianChar);

        copyValsToAIO(lesserInputChar, longer_vals, longerLen, maxLength);
        copyValsToAIO(longerInputChar, lesser_vals, lesserLen, maxLength);

        printf("\n\n******************** Gapped and ungapped: ******************\n");
        printf(  "\n****************** Original 2d characters: *****************\n");
        printf("Alphabet size: %zu\n", alphSize);
        printf("gap character: %u\n",  gap_char);

        alignIO_print(lesserInputChar);
        alignIO_print(longerInputChar);

        algnCost = align2d(lesserInputChar,
                           longerInputChar,
                           gappedMedianChar,
                           ungappedMedianChar,
                           // unionMedianChar,
                           costMtx2d,
                           1,                    // do ungapped
                           1,                    // do gapped
                           0);                   // do union

        printf("\nAligned 2d characters\n");
        alignIO_print(lesserInputChar);
        alignIO_print(longerInputChar);
        printf("Alignment cost: %d\n", algnCost);


        printf("\nGapped character  ");
        alignIO_print(gappedMedianChar);

        printf("alphSize: %zu\n", alphSize);
        printf("gap char: %u\n", gap_char);

        printf("\nUngapped character  ");
        alignIO_print(ungappedMedianChar);

        resetAlignIO(lesserInputChar);
        resetAlignIO(longerInputChar);
        resetAlignIO(ungappedMedianChar);
        resetAlignIO(gappedMedianChar);

        copyValsToAIO(lesserInputChar, longer_vals, longerLen, maxLength);
        copyValsToAIO(longerInputChar, lesser_vals, lesserLen, maxLength);

        printf("\n\n********** Gapped and union (ungapped should be empty, union should override ungapped): **********\n");
        printf(  "\n************************************ Original 2d characters: *************************************\n");
        printf("  \n*************************************** Alphabet Size: %zu ***************************************\n", alphSize);


        alignIO_print(lesserInputChar);
        alignIO_print(longerInputChar);


        algnCost = align2d(lesserInputChar,
                           longerInputChar,
                           gappedMedianChar,
                           ungappedMedianChar,
                           // unionMedianChar,
                           costMtx2d,
                           0,                    // do ungapped
                           1,                    // do gapped
                           0);                   // do union


        printf("\nAligned 2d characters\n");
        alignIO_print(lesserInputChar);
        alignIO_print(longerInputChar);
        printf("Alignment cost: %d\n", algnCost);


        printf("\n  Union character  ");
        alignIO_print(gappedMedianChar);

        printf("\n  Ungapped character  ");
        alignIO_print(ungappedMedianChar);
    }
    freeCostMtx(costMtx2d, 1);  // 1 is 2d
} // Do 2D



// /************************************************ Do 2d affine alignment *****************************************************/

//     /*** must have gap at start of character!!! ***/

void do2D_affine( unsigned int * tcm, size_t alphSize )
{

    cost_matrices_2d_t *costMtx2d_affine = malloc(sizeof(struct cost_matrices_2d_t));
    setUp2dCostMtx (costMtx2d_affine, tcm, alphSize, GAP_OPEN_COST);

    elem_t one      = 1;
    elem_t gap_char = one << (alphSize - 1);
    elem_t max_val  = one << alphSize;     // actually one more than max, because we're using it to mod

    int algnCost;

    elem_t *longer_vals = malloc( sizeof(elem_t) ),
           *lesser_vals = malloc( sizeof(elem_t) );
    assert(   longer_vals    != NULL
            && lesser_vals    != NULL
            && "Couldn't reallocate input character vals in 2D affine interface test." );

    size_t longerLen,
           lesserLen,
           maxLength;

    alignIO_t *lesserInputChar = malloc( sizeof(struct alignIO_t) );    // inputs to align2d fn.
    alignIO_t *longerInputChar = malloc( sizeof(struct alignIO_t) );    // inputs to align2d fn.

    alignIO_t *ungappedMedianChar = malloc( sizeof(struct alignIO_t) );
    alignIO_t *gappedMedianChar   = malloc( sizeof(struct alignIO_t) );

    assert(   lesserInputChar    != NULL
           && longerInputChar    != NULL
           && ungappedMedianChar != NULL
           && gappedMedianChar   != NULL
           && "Couldn't allocate input or median characters structs in 2D affine interface test." );

    // set to 1 so I can realloc later:
    allocAlignIO(lesserInputChar,    1);
    allocAlignIO(longerInputChar,    1);

    allocAlignIO(ungappedMedianChar, 1);
    allocAlignIO(gappedMedianChar,   1);


    printf("\n\n\n*************** Align 2 characters nonaffine ***************\n");
    printf("    \n******************** Alphabet Size: %zu ********************\n", alphSize);

    for (size_t i = 1; i <= 30; i++) { // run 30 tests
        // printf("\nloop idx: %zu\n", i);

        for (size_t j = 1; j < 15; j++) {
            longerLen = rand() % CHAR_LENGTH + 1;                // 1 because I don't want to mod 0
            lesserLen = rand() % CHAR_LENGTH + 1;                // to make sure that the alignment works with switched lengths
            maxLength = longerLen + lesserLen + 2; // 2 because there are two gaps added (1 on beginning of each character)

            longer_vals = realloc( longer_vals, longerLen * sizeof(elem_t) );
            lesser_vals = realloc( lesser_vals, lesserLen * sizeof(elem_t) );
            assert(   longer_vals    != NULL
                   && lesser_vals    != NULL
                   && "Couldn't reallocate input character vals in 2D affine interface test." );

            set_vals( longer_vals, longerLen, max_val);
            set_vals( lesser_vals, lesserLen, max_val);
        }
        // printf("long len: %2zu short len: %2zu\n", longerLen, lesserLen);

        // need to allocate space for return alignIOs, as they're no longer alloc'ed in c_alignment_interface
        reallocAlignIO(lesserInputChar, maxLength);
        reallocAlignIO(longerInputChar, maxLength);

        reallocAlignIO(ungappedMedianChar, maxLength);
        reallocAlignIO(gappedMedianChar,   maxLength);

        copyValsToAIO(lesserInputChar, longer_vals, longerLen, maxLength);
        copyValsToAIO(longerInputChar, lesser_vals, lesserLen, maxLength);

        printf("\n\n******* Cost only (all characters should be empty): ********\n");
        printf("  \n***************** Original 2d characters: ******************\n");
        printf("Alphabet size: %zu\n", alphSize);
        printf("gap character: %u\n",  gap_char);

        alignIO_print(lesserInputChar);
        alignIO_print(longerInputChar);

        algnCost = align2dAffine( lesserInputChar
                                , longerInputChar
                                , gappedMedianChar
                                , ungappedMedianChar
                                // , unionMedianChar
                                , costMtx2d_affine
                                , 0                 // compute medians
                                );

        printf("Alignment cost: %d\n", algnCost);

        printf("\nAligned 2d characters affine\n");
        alignIO_print(lesserInputChar);
        alignIO_print(longerInputChar);

        printf("  Gapped character\n  ");
        alignIO_print(gappedMedianChar);

        printf("  Ungapped character\n  ");
        alignIO_print(ungappedMedianChar);

        resetAlignIO(lesserInputChar);
        resetAlignIO(longerInputChar);
        resetAlignIO(ungappedMedianChar);
        resetAlignIO(gappedMedianChar);

        copyValsToAIO(lesserInputChar, longer_vals, longerLen, maxLength);
        copyValsToAIO(longerInputChar, lesser_vals, lesserLen, maxLength);

        printf("\n\n*********************** With medians: ***********************\n");
        printf("  \n****************** Original 2d characters: ******************\n");
        printf("Alphabet size: %zu\n", alphSize);
        printf("gap character: %u\n",  gap_char);

        alignIO_print(lesserInputChar);
        alignIO_print(longerInputChar);

        algnCost = align2dAffine( lesserInputChar
                                , longerInputChar
                                , gappedMedianChar
                                , ungappedMedianChar
                                , costMtx2d_affine
                                , 1                   // do medians
                                );

        printf("\nAligned 2d characters\n");
        alignIO_print(lesserInputChar);
        alignIO_print(longerInputChar);
        printf("Alignment cost: %d\n", algnCost);

        printf("\nGapped character\n  ");
        alignIO_print(gappedMedianChar);

        printf("alphSize: %zu\n", alphSize);
        printf("gap char: %u\n", gap_char);

        printf("\nUngapped character\n  ");
        alignIO_print(ungappedMedianChar);



    }
    freeAlignIO(lesserInputChar);
    freeAlignIO(longerInputChar);
    freeAlignIO(ungappedMedianChar);
    freeAlignIO(gappedMedianChar);
    // freeAlignIO(unionMedianChar);

    freeCostMtx(costMtx2d_affine, 1);  // 1 is 2d

}


/************************************************ Do 3d alignment *************************************************/

void do3D( unsigned int *tcm, size_t alphSize )
{
    elem_t one      = 1;
    elem_t gap_char = one << (alphSize - 1);

    cost_matrices_3d_t *costMtx3d = malloc( sizeof(struct cost_matrices_3d_t) );
    setUp3dCostMtx( costMtx3d, tcm, alphSize, 0 );

    alignIO_t *inputChar1 = malloc( sizeof(struct alignIO_t) );    // Inputs to 3d alignment
    alignIO_t *inputChar2 = malloc( sizeof(struct alignIO_t) );    //
    alignIO_t *inputChar3 = malloc( sizeof(struct alignIO_t) );    //

    alignIO_t *returnChar1 = malloc( sizeof(struct alignIO_t) );    // Outputs from 3d alignment
    alignIO_t *returnChar2 = malloc( sizeof(struct alignIO_t) );    //
    alignIO_t *returnChar3 = malloc( sizeof(struct alignIO_t) );    //

    assert(   inputChar1  != NULL
           && inputChar2  != NULL
           && inputChar3  != NULL
           && returnChar1 != NULL
           && returnChar2 != NULL
           && returnChar3 != NULL
           && "Couldn't allocate input or output structs in 3D interface test." );

    // set to 1 so I can realloc later:
    allocAlignIO( inputChar1, 1 );
    allocAlignIO( inputChar2, 1 );
    allocAlignIO( inputChar3, 1 );

    allocAlignIO( returnChar1, 1 );
    allocAlignIO( returnChar2, 1 );
    allocAlignIO( returnChar3, 1 );

    alignIO_t *ungappedMedianChar = malloc( sizeof(struct alignIO_t) );
    alignIO_t *gappedMedianChar   = malloc( sizeof(struct alignIO_t) );
    assert(   ungappedMedianChar != NULL
           && gappedMedianChar   != NULL
           && "Couldn't allocate input or median characters structs in 3D interface test." );

    allocAlignIO( ungappedMedianChar, 1 );
    allocAlignIO( gappedMedianChar,   1 );

    size_t inChar1Len,
           inChar2Len,
           inChar3Len,
           maxLength;

    int algnCost;

    elem_t inputVals1[1] = {30};
    inChar1Len = 1;
    elem_t inputVals2[1] = {1};
    inChar2Len = 1;
    elem_t inputVals3[1] = {1};
    inChar3Len = 1;

    maxLength  = inChar1Len + inChar2Len + inChar3Len;

    reallocAlignIO( inputChar1, maxLength );
    reallocAlignIO( inputChar2, maxLength );
    reallocAlignIO( inputChar3, maxLength );

    reallocAlignIO( returnChar1, maxLength );
    reallocAlignIO( returnChar2, maxLength );
    reallocAlignIO( returnChar3, maxLength );

    reallocAlignIO( ungappedMedianChar, maxLength );
    reallocAlignIO( gappedMedianChar,   maxLength );

    copyValsToAIO( inputChar1, inputVals1, inChar1Len, maxLength );
    copyValsToAIO( inputChar2, inputVals2, inChar2Len, maxLength );
    copyValsToAIO( inputChar3, inputVals3, inChar3Len, maxLength );

    printf("\n\n\n******************** Align 3 characters **********************\n\n");
    printf(      "*****************  Original 3d characters:  ******************\n");
    alignIO_print( inputChar1 );
    alignIO_print( inputChar2 );
    alignIO_print( inputChar3 );

    algnCost = align3d( inputChar1
                      , inputChar2
                      , inputChar3
                      , returnChar1
                      , returnChar2
                      , returnChar3
                      , ungappedMedianChar
                      , gappedMedianChar
                      , costMtx3d
                      , 1        // substitution_cost
                      , 1        // gap open cost
                      , 2        // gap extension cost
                      );
    // if (DEBUG_MAT) {
    //     printf("\n\nFinal alignment matrix: \n\n");
    //     algn_print_dynmtrx_2d( longChar, shortChar, algn_mtxs2d );
    // }

    printf("alphSize: %zu\n", alphSize);
    printf("gap char: %u\n",  gap_char);

    printf("\nAligned 3d characters\n");
    alignIO_print( returnChar1 );
    alignIO_print( returnChar2 );
    alignIO_print( returnChar3 );

    printf("Alignment cost: %d\n", algnCost);

    printf("\nGapped median\n  ");
    alignIO_print( gappedMedianChar );

    printf("\nUngapped median\n  ");
    alignIO_print( ungappedMedianChar );

    printf("\n\n\n");

    freeCostMtx( costMtx3d, 0 );  // 0 is !2d
}
