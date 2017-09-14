/** Tests external DO code under various conditions:
    1. with varying alphabet lengths
    2. with random-length characters
    3. with varying character patterns
 */

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "../alignCharacters.h"
#include "../c_alignment_interface.h"
#include "../c_code_alloc_setup.h"
#include "../debug_constants.h"
#include "../costMatrix.h"
#include "../alignmentMatrices.h"
#include "../ukkCheckPoint.h"
#include "../ukkCommon.h"

// #define SEQ_CAPACITY 64


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

    size_t maxLength;               // max length of an aligned character, given two chars to align

    srand( (unsigned) time(NULL) ); // initialize random number generator

    // Lengths and values will be set randomly in loops below
    size_t longCharLen,
           middleCharLen,
           shortCharLen;

    // elem_t *longer_vals  = malloc(sizeof(elem_t)),
    //        *middle_vals   = malloc(sizeof(elem_t)),
    //        *lesser_vals = malloc(sizeof(elem_t));

    // Likewise, internal arrays will be alloced and realloced below.
    // dyn_char_p shortChar     = malloc( sizeof(dyn_character_t) );
    // dyn_char_p middleChar    = malloc( sizeof(dyn_character_t) );
    // dyn_char_p longChar      = malloc( sizeof(dyn_character_t) );

    // need these next three for union
    // dyn_char_p retLongChar   = malloc( sizeof(dyn_character_t) );
    // dyn_char_p retMiddleChar = malloc( sizeof(dyn_character_t) );
    // dyn_char_p retShortChar  = malloc( sizeof(dyn_character_t) );

    alignIO_t *inputChar1 = malloc( sizeof(struct alignIO_t) );    // inputs to align2d fn.
    alignIO_t *inputChar2 = malloc( sizeof(struct alignIO_t) );    // inputs to align2d fn.
    alignIO_t *inputChar3 = malloc( sizeof(struct alignIO_t) );    // additional input to align3d fn.

    // various median outputs
    alignIO_t *ungappedMedianChar = malloc( sizeof(struct alignIO_t) );
    alignIO_t *gappedMedianChar   = malloc( sizeof(struct alignIO_t) );
    alignIO_t *unionMedianChar    = malloc( sizeof(struct alignIO_t) );

    // set to 1 so I can realloc later:
    allocAlignIO(inputChar1,         1);
    allocAlignIO(inputChar2,         1);
    allocAlignIO(inputChar3,         1);
    allocAlignIO(ungappedMedianChar, 1);
    allocAlignIO(gappedMedianChar,   1);


    /************  Allocate cost matrices  **************/

    //TODO: this will have to be changed to account for the fact that you  might have 2D and 3D.
    size_t alphSize;
    if (DO_3D) alphSize = 5;
    else       alphSize = rand() % 6 + 3;     // includes gap, but no ambiguities; alphabet sizes between binary and 8 are accepted

    elem_t one = 1;
    elem_t gap_char          = one << (alphSize - 1);
    elem_t max_val           = one << alphSize;     // actually one more than max, because we're using it to mod
    size_t tcm_total_len     = alphSize * alphSize; // the size of the input tcm
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

    if(DO_2D) {
        cost_matrices_2d_t *costMtx2d        = malloc(sizeof(struct cost_matrices_2d_t));
        setUp2dCostMtx (costMtx2d, tcm, alphSize, 0);
    }

    if(DO_2D_AFF) {
        cost_matrices_2d_t *costMtx2d_affine = malloc(sizeof(struct cost_matrices_2d_t));
        setUp2dCostMtx (costMtx2d_affine, tcm, alphSize, GAP_OPEN_COST);
    }

    if(DO_3D) {
        setUp3dCostMtx (costMtx3d, tcm, alphSize, 0);
        cost_matrices_3d_t *costMtx3d        = malloc(sizeof(struct cost_matrices_3d_t));
    }


// /**************************************************** Do 2d alignment ********************************************************/

//     if (DO_2D) {
//         printf("\n\n\n******************** Align 2 characters ********************\n");




//         for (i = 1; i <= 5; i++) { // run 30 tests

//             longCharLen  = rand() % CHAR_LENGTH + 1;
//             shortCharLen = rand() % CHAR_LENGTH + 1;
//             maxLength    = longCharLen + shortCharLen + 2; // 2 because there are two gaps added (1 on beginning of each character)

//             // need to realloc each time through the loop
//             longer_vals  = realloc( longer_vals,   longCharLen  * sizeof(elem_t) );
//             lesser_vals = realloc( lesser_vals,  shortCharLen * sizeof(elem_t) );

//             set_vals( longer_vals,  longCharLen,  max_val);
//             set_vals( lesser_vals, shortCharLen, max_val);

//             // need to allocate space for return alignIOs, as they're no longer alloc'ed in c_alignment_interface
//             reallocAlignIO(inputChar1, maxLength);
//             reallocAlignIO(inputChar2, maxLength);

//             reallocAlignIO(ungappedMedianChar, maxLength);
//             reallocAlignIO(gappedMedianChar,   maxLength);

//             copyValsToAIO(inputChar1, longer_vals,  longCharLen,  maxLength);
//             copyValsToAIO(inputChar2, lesser_vals, shortCharLen, maxLength);

//             allocAlignIO(unionMedianChar,    maxLength);


//             printf("\n\n********** Cost only (all chars should be empty): **********\n");
//             printf("  \n***************** Original 2d characters: ******************\n");
//             printf("  \n******************** Alphabet Size: %zu ********************\n", alphSize);
//             alignIO_print(inputChar1);
//             alignIO_print(inputChar2);

//             algnCost = align2d( inputChar1
//                               , inputChar2
//                               , gappedMedianChar
//                               , ungappedMedianChar
//                               // , unionMedianChar
//                               , costMtx2d
//                               , 0                    // do ungapped
//                               , 0                    // do gapped
//                               , 0                    // do union
//                               );
//             // if (DEBUG_MAT) {
//             //     printf("\n\nFinal alignment matrix: \n\n");
//             //     algn_print_dynmtrx_2d( longChar, shortChar, algn_mtxs2d );
//             // }

//             printf("Alignment cost: %d\n", algnCost);

//             printf("\nAligned 2d characters (should be the same as inputs, as no backtrace has been performed)\n");
//             alignIO_print(inputChar1);
//             alignIO_print(inputChar2);

//             printf("\n  Gapped character  ");
//             alignIO_print(gappedMedianChar);

//             printf("\n  Ungapped character  ");
//             alignIO_print(ungappedMedianChar);

//             // union:
//             // algn_union (retShortChar, retLongChar, algnChar);
//             // printf("  Unioned character\n  ");
//             // alignIO_print(unionMedianChar);
//             // printf("here.\n");

//             resetAlignIO(inputChar1);
//             resetAlignIO(inputChar2);
//             resetAlignIO(ungappedMedianChar);
//             resetAlignIO(gappedMedianChar);
//             // resetAlignIO(unionMedianChar);



//             // printf("\n\n********** Ungapped only (gapped should be empty): **********\n");

//             printf("  \n****************** Original 2d characters: ******************\n");
//             printf("Alphabet size: %zu\n", alphSize);
//             printf("gap character: %u\n", gap_char);

//             alignIO_print(inputChar1);
//             alignIO_print(inputChar2);

//             algnCost = align2d( inputChar1
//                               , inputChar2
//                               , gappedMedianChar
//                               , ungappedMedianChar
//                               // , unionMedianChar
//                               , costMtx2d
//                               , 1                    // do ungapped
//                               , 1                    // do gapped
//                               , 0                   // do union
//                               );

//             printf("\nAligned 2d characters\n");
//             alignIO_print(inputChar1);
//             alignIO_print(inputChar2);

//             printf("Alignment cost: %d\n", algnCost);

//             printf("\nGapped character  ");
//             alignIO_print(gappedMedianChar);

//             printf("\nUngapped character  ");
//             alignIO_print(ungappedMedianChar);

//             resetAlignIO(inputChar1);
//             resetAlignIO(inputChar2);
//             resetAlignIO(ungappedMedianChar);
//             resetAlignIO(gappedMedianChar);

//             copyValsToAIO(inputChar1, longer_vals, longCharLen, maxLength);
//             copyValsToAIO(inputChar2, lesser_vals, shortCharLen, maxLength);

//             printf("\n\n********** Gapped only (ungapped should be empty): **********\n");
//             printf("  \n*******************Original 2d characters:*******************\n");
//             printf("Alphabet size: %zu\n", alphSize);
//             printf("gap character: %u\n", gap_char);

//             alignIO_print(inputChar1);
//             alignIO_print(inputChar2);

//             algnCost = align2d(inputChar1,
//                                inputChar2,
//                                gappedMedianChar,
//                                ungappedMedianChar,
//                                // unionMedianChar,
//                                costMtx2d,
//                                0,                    // do ungapped
//                                1,                    // do gapped
//                                0);                   // do union

//             printf("Alignment cost: %d\n", algnCost);

//             printf("\nAligned 2d characters\n");
//             alignIO_print(inputChar1);
//             alignIO_print(inputChar2);

//             printf("\nGapped character  ");
//             alignIO_print(gappedMedianChar);

//             printf("\nUngapped character  ");
//             alignIO_print(ungappedMedianChar);

//             resetAlignIO(inputChar1);
//             resetAlignIO(inputChar2);
//             resetAlignIO(ungappedMedianChar);
//             resetAlignIO(gappedMedianChar);

//             copyValsToAIO(inputChar1, longer_vals,  longCharLen,  maxLength);
//             copyValsToAIO(inputChar2, lesser_vals, shortCharLen, maxLength);

//             resetAlignIO(inputChar1);
//             resetAlignIO(inputChar2);
//             resetAlignIO(ungappedMedianChar);
//             resetAlignIO(gappedMedianChar);

//             copyValsToAIO(inputChar1, longer_vals,  longCharLen,  maxLength);
//             copyValsToAIO(inputChar2, lesser_vals, shortCharLen, maxLength);

//             printf("\n\n******************** Gapped and ungapped: ******************\n");
//             printf(  "\n****************** Original 2d characters: *****************\n");
//             printf("Alphabet size: %zu\n", alphSize);
//             printf("gap character: %u\n",  gap_char);

//             alignIO_print(inputChar1);
//             alignIO_print(inputChar2);

//             algnCost = align2d(inputChar1,
//                                inputChar2,
//                                gappedMedianChar,
//                                ungappedMedianChar,
//                                // unionMedianChar,
//                                costMtx2d,
//                                1,                    // do ungapped
//                                1,                    // do gapped
//                                0);                   // do union

//             printf("\nAligned 2d characters\n");
//             alignIO_print(inputChar1);
//             alignIO_print(inputChar2);
//             printf("Alignment cost: %d\n", algnCost);


//             printf("\nGapped character  ");
//             alignIO_print(gappedMedianChar);

//             printf("alphSize: %zu\n", alphSize);
//             printf("gap char: %u\n", gap_char);

//             printf("\nUngapped character  ");
//             alignIO_print(ungappedMedianChar);

//             resetAlignIO(inputChar1);
//             resetAlignIO(inputChar2);
//             resetAlignIO(ungappedMedianChar);
//             resetAlignIO(gappedMedianChar);

//             copyValsToAIO(inputChar1, longer_vals, longCharLen, maxLength);
//             copyValsToAIO(inputChar2, lesser_vals, shortCharLen, maxLength);

//             printf("\n\n********** Gapped and union (ungapped should be empty, union should override ungapped): **********\n");
//             printf(  "\n************************************ Original 2d characters: *************************************\n");
//             printf("  \n*************************************** Alphabet Size: %zu ***************************************\n", alphSize);


//             alignIO_print(inputChar1);
//             alignIO_print(inputChar2);


//             algnCost = align2d(inputChar1,
//                                inputChar2,
//                                gappedMedianChar,
//                                ungappedMedianChar,
//                                // unionMedianChar,
//                                costMtx2d,
//                                0,                    // do ungapped
//                                1,                    // do gapped
//                                0);                   // do union


//             printf("\nAligned 2d characters\n");
//             alignIO_print(inputChar1);
//             alignIO_print(inputChar2);
//             printf("Alignment cost: %d\n", algnCost);


//             printf("\n  Union character  ");
//             alignIO_print(gappedMedianChar);

//             printf("\n  Ungapped character  ");
//             alignIO_print(ungappedMedianChar);


//         }
//     } // Do 2D



// /************************************************ Do 2d affine alignment *****************************************************/

//     /*** must have gap at start of character!!! ***/

//     if (DO_2D_AFF) {
//         printf("\n\n\n***************** Align 2 characters affine ****************\n");
//         printf("    \n******************** Alphabet Size: %zu ********************\n", alphSize);

//         for (i = 1; i <= 30; i++){ // run 30 tests
//             // printf("\nloop idx: %zu\n", i);

//             for (j = 1; j < 15; j++) {
//                 longCharLen  = rand() % CHAR_LENGTH + 1;                // 1 because I don't want to mod 0
//                 shortCharLen = rand() % CHAR_LENGTH + 1;                // to make sure that the alignment works with switched lengths
//                 maxLength    = longCharLen + shortCharLen + 2; // 2 because there are two gaps added (1 on beginning of each character)

//                 longer_vals  = realloc( longer_vals,   longCharLen  * sizeof(elem_t) );
//                 lesser_vals = realloc( lesser_vals,  shortCharLen * sizeof(elem_t) );

//                 set_vals( longer_vals,  longCharLen,  max_val);
//                 set_vals( lesser_vals, shortCharLen, max_val);
//             }
//             // printf("long len: %2zu short len: %2zu\n", longCharLen, shortCharLen);

//             // need to allocate space for return alignIOs, as they're no longer alloc'ed in c_alignment_interface
//             reallocAlignIO(inputChar1, maxLength);
//             reallocAlignIO(inputChar2, maxLength);

//             reallocAlignIO(ungappedMedianChar, maxLength);
//             reallocAlignIO(gappedMedianChar,   maxLength);

//             copyValsToAIO(inputChar1, longer_vals,  longCharLen,  maxLength);
//             copyValsToAIO(inputChar2, lesser_vals, shortCharLen, maxLength);

//             printf("\n\n******* Cost only (all characters should be empty): ********\n");
//             printf("  \n***************** Original 2d characters: ******************\n");
//             printf("Alphabet size: %zu\n", alphSize);
//             printf("gap character: %u\n",  gap_char);

//             alignIO_print(inputChar1);
//             alignIO_print(inputChar2);

//             algnCost = align2dAffine( inputChar1
//                                     , inputChar2
//                                     , gappedMedianChar
//                                     , ungappedMedianChar
//                                     // , unionMedianChar
//                                     , costMtx2d_affine
//                                     , 0                 // compute medians
//                                     );

//             printf("Alignment cost: %d\n", algnCost);

//             printf("\nAligned 2d characters affine\n");
//             alignIO_print(inputChar1);
//             alignIO_print(inputChar2);

//             printf("  Gapped character\n  ");
//             alignIO_print(gappedMedianChar);

//             printf("  Ungapped character\n  ");
//             alignIO_print(ungappedMedianChar);

//             resetAlignIO(inputChar1);
//             resetAlignIO(inputChar2);
//             resetAlignIO(ungappedMedianChar);
//             resetAlignIO(gappedMedianChar);

//             copyValsToAIO(inputChar1, longer_vals,  longCharLen,  maxLength);
//             copyValsToAIO(inputChar2, lesser_vals, shortCharLen, maxLength);

//             printf("\n\n*********************** With medians: ***********************\n");
//             printf("  \n****************** Original 2d characters: ******************\n");
//             printf("Alphabet size: %zu\n", alphSize);
//             printf("gap character: %u\n",  gap_char);

//             alignIO_print(inputChar1);
//             alignIO_print(inputChar2);

//             algnCost = align2dAffine( inputChar1
//                                     , inputChar2
//                                     , gappedMedianChar
//                                     , ungappedMedianChar
//                                     , costMtx2d_affine
//                                     , 1                   // do medians
//                                     );

//             printf("\nAligned 2d characters\n");
//             alignIO_print(inputChar1);
//             alignIO_print(inputChar2);
//             printf("Alignment cost: %d\n", algnCost);

//             printf("\nGapped character\n  ");
//             alignIO_print(gappedMedianChar);

//             printf("alphSize: %zu\n", alphSize);
//             printf("gap char: %u\n", gap_char);

//             printf("\nUngapped character\n  ");
//             alignIO_print(ungappedMedianChar);



//         }
//         freeAlignIO(inputChar1);
//         freeAlignIO(inputChar2);
//         freeAlignIO(ungappedMedianChar);
//         freeAlignIO(gappedMedianChar);
//         // freeAlignIO(unionMedianChar);

//         // printf("LOOKS LIKE WE MADE IT!\n");

//     }


/************************************************ Do 3d alignment *************************************************/

    if (DO_3D) {

        for (i = 0; i < 1; i++) { // run 30 tests

            longCharLen   = 6; // rand() % CHAR_LENGTH + 1;
            middleCharLen = 6; // rand() % CHAR_LENGTH + 1;
            shortCharLen  = 5; // rand() % CHAR_LENGTH + 1;
            maxLength     = longCharLen + middleCharLen + shortCharLen + 3; // extra 3 because there are three gaps added
                                                                            // (1 on beginning of each character)

            // need to realloc each time through the loop
            elem_t longer_vals[6] = {3, 1, 9, 1, 8, 1}; // realloc( longer_vals,  longCharLen   * sizeof(elem_t) );
            elem_t middle_vals[6] = {3, 9, 9, 7, 5, 8}; // realloc( middle_vals,   middleCharLen * sizeof(elem_t) );
            elem_t lesser_vals[5] = {7, 9, 9, 4, 5};    // realloc( lesser_vals, shortCharLen  * sizeof(elem_t) );

            // set_vals( longer_vals,  longCharLen,   max_val);
            // set_vals( middle_vals,   middleCharLen, max_val);
            // set_vals( lesser_vals, shortCharLen,  max_val);

            reallocAlignIO(inputChar1, maxLength);
            reallocAlignIO(inputChar2, maxLength);
            reallocAlignIO(inputChar3, maxLength);

            reallocAlignIO(ungappedMedianChar, maxLength);
            reallocAlignIO(gappedMedianChar,   maxLength);

            copyValsToAIO(inputChar1, longer_vals, longCharLen,   maxLength);
            copyValsToAIO(inputChar2, middle_vals, middleCharLen, maxLength);
            copyValsToAIO(inputChar3, lesser_vals, shortCharLen,  maxLength);

            printf("\n\n\n******************** Align 3 characters **********************\n\n");
            printf(      "*****************  Original 3d characters:  ******************\n");
            alignIO_print(inputChar1);
            alignIO_print(inputChar2);
            alignIO_print(inputChar3);

            algnCost = align3d( inputChar1
                              , inputChar2
                              , inputChar3
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
            alignIO_print(inputChar1);
            alignIO_print(inputChar2);
            alignIO_print(inputChar3);

            printf("Alignment cost: %d\n", algnCost);

            printf("\nGapped median\n  ");
            alignIO_print(gappedMedianChar);

            printf("alphSize: %zu\n", alphSize);
            printf("gap char: %u\n",  gap_char);

            printf("\nUngapped median\n  ");
            alignIO_print(ungappedMedianChar);

            printf("\n\n\n");
        }
    }

/*
    if (DO_3D_AFF) {

        printf("\n\n\n******************** Align 3 characters affine **********************\n\n");


        alignIO_p inputChar1      = malloc(sizeof(struct alignIO));
        alignIO_p inputChar2      = malloc(sizeof(struct alignIO));
        alignIO_p char3      = malloc(sizeof(struct alignIO));
        alignIO_p medianChar = malloc(sizeof(struct alignIO));

        const size_t maxLength = longCharLen + shortCharLen + 1;

        medianChar->character  = malloc(maxLength * sizeof(int));
        medianChar->length    = 0;
        medianChar->capacity  = maxLength;

        copyValsToAIO(inputChar1, longer_vals,  longCharLen,   maxLength);
        copyValsToAIO(inputChar2, middle_vals,   middleCharLen, maxLength);
        copyValsToAIO(char3, lesser_vals, shortCharLen,  maxLength);

        // printf("Original alignment matrix before algn_nw_2d: \n");
        // algn_print_dynmtrx_2d( longChar, shortChar, algn_mtxs2d );

        // resetCharValues(retLongChar);
        // resetCharValues(retShortChar);
        printf("Original 3d character:\n");
        alignIO_print(inputChar1);
        alignIO_print(inputChar2);
        alignIO_print(char3);

        algnCost = align3d(inputChar1,
                           inputChar2,
                           char3,
                           medianChar,
                           costMtx3d_affine);
        // if (DEBUG_MAT) {
        //     printf("\n\nFinal alignment matrix: \n\n");
        //     algn_print_dynmtrx_2d( longChar, shortChar, algn_mtxs2d );
        // }

        printf("\nAligned 3d character\n");
        alignIO_print(inputChar1);
        alignIO_print(inputChar2);
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

    // Next this: algn_get_median_3d (dyn_char_p inputChar1, dyn_char_p inputChar2, dyn_char_p char3,
    //                cost_matrices_3d_t *m, dyn_char_p sm)

    if(DO_2D)     freeCostMtx(costMtx2d,        1);
    if(DO_2D_AFF) freeCostMtx(costMtx2d_affine, 1);  // 1 is 2d
    if(DO_3D)     freeCostMtx(costMtx3d,        0);  // 0 is !2d

    free(tcm);

    return 0;
}

