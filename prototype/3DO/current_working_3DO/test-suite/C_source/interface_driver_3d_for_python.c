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
#include "../../c_alignment_interface.h"
#include "../../c_code_alloc_setup.h"
#include "../../debug_constants.h"
#include "../../costMatrix.h"
#include "../../alignmentMatrices.h"
#include "../../ukkCheckPoint.h"
#include "../../ukkCommon.h"

// #define SEQ_CAPACITY 64


int power_2 (int input)
{
    return (__builtin_popcount(input) == 1);
}


void set_vals( elem_t *vals, size_t vals_length, size_t max_val)
{
    elem_t curVal;
    for (size_t k = 0; k < vals_length; k++ ) {
        curVal = rand() % (max_val);

        while (curVal == 0) curVal = rand() % (max_val);

        vals[k] = curVal;
    }
}


/** First input is longest sequence, second is shortest. **/
int wrapperFunction( elem_t *longer_vals
                   , size_t longCharLen
                   , elem_t *lesser_vals
                   , size_t shortCharLen
                   , elem_t *middle_vals
                   , size_t middleCharLen
                   )
{


/******************************** set up and allocate all variables and structs ************************************/

    size_t i, j;   // various loop indices

    const size_t MAXLENGTH = longCharLen + shortCharLen + middleCharLen + 3; // Max length of an aligned character,
                                                                             // given three chars to align. Extra 3 for stupid gaps.


    // elem_t *longer_vals  = malloc(sizeof(elem_t)),
    //        *middle_vals   = malloc(sizeof(elem_t)),
    //        *lesser_vals = malloc(sizeof(elem_t));

    // dyn_char_p shortChar     = malloc( sizeof(dyn_character_t) );
    // dyn_char_p middleChar    = malloc( sizeof(dyn_character_t) );
    // dyn_char_p longChar      = malloc( sizeof(dyn_character_t) );

    // need these next three for union
    // dyn_char_p retLongChar   = malloc( sizeof(dyn_character_t) );
    // dyn_char_p retMiddleChar = malloc( sizeof(dyn_character_t) );
    // dyn_char_p retShortChar  = malloc( sizeof(dyn_character_t) );

    alignIO_t *inputLonger = malloc( sizeof(struct alignIO_t) );    // inputs to align2d fn.
    alignIO_t *inputMiddle = malloc( sizeof(struct alignIO_t) );    // inputs to align2d fn.
    alignIO_t *inputLesser = malloc( sizeof(struct alignIO_t) );    // additional input to align3d fn.

    // various median outputs
    alignIO_t *ungappedMedianChar = malloc( sizeof(struct alignIO_t) );
    alignIO_t *gappedMedianChar   = malloc( sizeof(struct alignIO_t) );
    alignIO_t *unionMedianChar    = malloc( sizeof(struct alignIO_t) );

    // set to 1 so I can realloc later
    allocAlignIO(inputLonger,        longCharLen);
    allocAlignIO(inputLesser,        shortCharLen);
    allocAlignIO(inputMiddle,        middleCharLen);
    allocAlignIO(ungappedMedianChar, MAXLENGTH);
    allocAlignIO(gappedMedianChar,   MAXLENGTH);


    /************  Allocate cost matrices  **************/

    size_t alphSize = 5;   // includes gap

    elem_t one      = 1;
    elem_t gap_char = one << (alphSize - 1);
    elem_t max_val  = one << alphSize;     // actually one more than max, because we're using it to mod
                                                    // the size of the input tcm
    size_t tcm_total_len     = alphSize * alphSize;
    const size_t CHAR_LENGTH = 100;

    int algnCost;

    /** TCM is only for non-ambiguous nucleotides, and it used to generate
     *  the entire cost matrix, which includes ambiguous elements.
     *  TCM is row-major, with each row being the left character element.
     *  It is therefore indexed not by powers of two, but by cardinal integer.
     *  This particular example is both metric and symmetric. All TCMs must be
     *  symmetric. Metricity is decided by PCG application.
     */
    unsigned int *tcm = calloc(tcm_total_len, sizeof(int)); // this is the input tcm, not the generated one
    for (i = 0; i < tcm_total_len; i += alphSize) {
        //printf("i: %zu\n", i);
        for (j = 0; j < alphSize; j++) {
            //tcm[i + j] = 2 * i + 2 * j;
            if ( i == j * alphSize ) {
                tcm[i + j] = IDENTITY_COST;    // identity
                printf("i: %zu, j: %zu, cost: %d\n", i, j, IDENTITY_COST);
            } else if (i == (tcm_total_len - alphSize) || j == (alphSize - 1)) {
                tcm[i + j] = INDEL_COST;       // indel cost
                printf("i: %zu, j: %zu, cost: %d\n", i, j, INDEL_COST);
            } else {
                tcm[i + j] = SUB_COST;         // sub cost
                printf("i: %zu, j: %zu, cost: %d\n", i, j, SUB_COST);
            }
         }
    }



/************************************************ Do 3d alignment *************************************************/


    cost_matrices_3d_t *costMtx3d = malloc(sizeof(struct cost_matrices_3d_t));
    setUp3dCostMtx (costMtx3d, tcm, alphSize, 0);


    // need to realloc each time through the loop
    // elem_t longer_vals[6] = {3, 1, 9, 1, 8, 1}; // realloc( longer_vals,  longCharLen   * sizeof(elem_t) );
    // elem_t middle_vals[5] = {3, 9, 9, 7, 5}; // realloc( middle_vals,   middleCharLen * sizeof(elem_t) );
    // elem_t lesser_vals[4] = {7, 9, 9, 4};    // realloc( lesser_vals, shortCharLen  * sizeof(elem_t) );

    // set_vals( longer_vals,  longCharLen,   max_val);
    // set_vals( middle_vals,   middleCharLen, max_val);
    // set_vals( lesser_vals, shortCharLen,  max_val);

    // reallocAlignIO(inputLonger, MAXLENGTH);
    // reallocAlignIO(inputMiddle, MAXLENGTH);
    // reallocAlignIO(inputLesser, MAXLENGTH);

    // reallocAlignIO(ungappedMedianChar, MAXLENGTH);
    // reallocAlignIO(gappedMedianChar,   MAXLENGTH);

    copyValsToAIO(inputLonger, longer_vals, longCharLen,   MAXLENGTH);
    copyValsToAIO(inputMiddle, middle_vals, middleCharLen, MAXLENGTH);
    copyValsToAIO(inputLesser, lesser_vals, shortCharLen,  MAXLENGTH);

    printf("\n\n\n******************** Align 3 characters **********************\n\n");
    printf(      "*****************  Original 3d characters:  ******************\n");
    alignIO_print(inputLonger);
    alignIO_print(inputMiddle);
    alignIO_print(inputLesser);

    algnCost = align3d( inputLonger
                      , inputMiddle
                      , inputLesser
                      , ungappedMedianChar
                      , gappedMedianChar
                      , costMtx3d
                      , 2        // gap open cost
                      // , 1        // gap extension cost
                      );
    // if (DEBUG_MAT) {
    //     printf("\n\nFinal alignment matrix: \n\n");
    //     algn_print_dynmtrx_2d( longChar, shortChar, algn_mtxs2d );
    // }

    printf("\nAligned 3d characters\n");
    alignIO_print(inputLonger);
    alignIO_print(inputMiddle);
    alignIO_print(inputLesser);

    printf("Alignment cost: %d\n", algnCost);

    printf("\nGapped median\n  ");
    alignIO_print(gappedMedianChar);

    printf("alphSize: %zu\n", alphSize);
    printf("gap char: %u\n",  gap_char);

    printf("\nUngapped median\n  ");
    alignIO_print(ungappedMedianChar);

    printf("\n\n\n");

    freeCostMtx(costMtx3d, 0);  // 0 is !2d


/*
    if (DO_3D_AFF) {

        printf("\n\n\n******************** Align 3 characters affine **********************\n\n");


        alignIO_p inputLonger      = malloc(sizeof(struct alignIO));
        alignIO_p inputMiddle      = malloc(sizeof(struct alignIO));
        alignIO_p char3      = malloc(sizeof(struct alignIO));
        alignIO_p medianChar = malloc(sizeof(struct alignIO));

        const size_t MAXLENGTH = longCharLen + shortCharLen + 1;

        medianChar->character  = malloc(MAXLENGTH * sizeof(int));
        medianChar->length    = 0;
        medianChar->capacity  = MAXLENGTH;

        copyValsToAIO(inputLonger, longer_vals,  longCharLen,   MAXLENGTH);
        copyValsToAIO(inputMiddle, middle_vals,   middleCharLen, MAXLENGTH);
        copyValsToAIO(char3, lesser_vals, shortCharLen,  MAXLENGTH);

        // printf("Original alignment matrix before algn_nw_2d: \n");
        // algn_print_dynmtrx_2d( longChar, shortChar, algn_mtxs2d );

        // resetCharValues(retLongChar);
        // resetCharValues(retShortChar);
        printf("Original 3d character:\n");
        alignIO_print(inputLonger);
        alignIO_print(inputMiddle);
        alignIO_print(char3);

        algnCost = align3d(inputLonger,
                           inputMiddle,
                           char3,
                           medianChar,
                           costMtx3d_affine);
        // if (DEBUG_MAT) {
        //     printf("\n\nFinal alignment matrix: \n\n");
        //     algn_print_dynmtrx_2d( longChar, shortChar, algn_mtxs2d );
        // }

        printf("\nAligned 3d character\n");
        alignIO_print(inputLonger);
        alignIO_print(inputMiddle);
        alignIO_print(char3);

        printf("Alignment cost: %d\n", algnCost);

        // union:
        //algn_union (retShortChar, retLongChar, algnChar);
        printf("  Unioned character\n  ");
        alignIO_print(medianChar);
        // must first reset values in retLongChar and retShortChar

        printf("\n\nAlignment cost: %d\n", algnCost);

        printf("\n\n\n");

        // for (elem_t *base = retLongChar->char_begin; base != retLongChar->end; base++) {
        //     printf("a: %c\n", *base);
        // }
        // for (elem_t *base = retShortChar->char_begin; base != retShortChar->end; base++) {
        //     printf("b: %s\n", base);
        // }
    }
*/

    // Next this: algn_get_median_3d (dyn_char_p inputLonger, dyn_char_p inputMiddle, dyn_char_p char3,
    //                cost_matrices_3d_t *m, dyn_char_p sm)

    free(tcm);

    return 0;
}

