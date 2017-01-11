#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "seqAlign.h"
#include "c_alignment_interface.h"
#include "c_code_alloc_setup.h"
#include "debug_constants.h"
#include "costMatrix.h"
#include "nwMatrices.h"
#include "ukkCheckp.h"
#include "ukkCommon.h"

// #define SEQ_CAPACITY 64

int power_2 (int input) {
    if (input == 1)  return 1;
    if (input == 2)  return 1;
    if (input == 4)  return 1;
    if (input == 8)  return 1;
    if (input == 16) return 1;
    return 0;
}

void copyVals(alignIO_p character, int *vals, size_t length, size_t capacity) {
    character->length   = length;
    character->capacity = capacity;
    character->character = malloc(capacity * sizeof(int));
    for(int i = 0; i < length; i++) {
        character->character[i] = vals[i];
    }
}

void alignIO_print(alignIO_p character) {
    printf("\n");
    printf("Capacity: %zu\n", character->capacity);
    printf("Length:   %zu\n", character->length);
    for(int i = 0; i < character->length; i++) {
        printf("%2d, ", character->character[i]);
    }
    printf("\n");
}


int main() {


/******************************** set up and allocate all variables and structs ************************************/


/****************  Allocate characters  ****************/

    /************  Allocate cost matrices  **************/

    int alphSize         = 5; // includes gap, but no ambiguities
    size_t tcm_total_len = alphSize * alphSize; // the size of the input tcm


    // !!!!! if modifying this code, also make sure to change is_metric !!!!!!
    /** TCM is only for non-ambiguous nucleotides, and it used to generate
     *  the entire cost matrix, which includes ambiguous elements.
     *  TCM is row-major, with each row being the left character element.
     *  It is therefore indexed not by powers of two, but by cardinal integer.
     *  This particular example is both metric and symmetric. All TCMs must be
     *  symmetric. Metricity is decided by PCG application.
     */
    int *tcm = calloc(tcm_total_len, sizeof(int)); // this is the input tcm, not the generated one
    for (size_t i = 0; i < tcm_total_len; i += alphSize) {
        //printf("i: %zu\n", i);
        for (size_t j = 0; j < alphSize; j++) {
            //printf("i: %zu, j: %zu, cost: %lu\n", i, j, 2 * i + 2 * j);
            //tcm[i + j] = 2 * i + 2 * j;
            if ( i == j * alphSize ) {
                tcm[i + j] = IDENTITY_COST;    // identity
            } else if (i == (tcm_total_len - alphSize) || j == (alphSize - 1)) {
                tcm[i + j] = INDEL_COST;       // indel cost
            } else {
                tcm[i + j] = SUB_COST;         // sub cost
            }
         }
    }


        //***** for following characters, affine requires gap at start of character!!! *****/

    const size_t longSeqLen   = 22;
    const size_t middleSeqLen = 18;
    const size_t shortSeqLen  = 17;

    const size_t SEQ_CAPACITY = longSeqLen + shortSeqLen + middleSeqLen;


    int longest_vals[longSeqLen]   = {16, 2, 1, 8, 4, 2, 1, 8, 4, 1, 1, 1, 1, 1, 2, 1, 8, 4, 2, 1, 8, 4}; // don't forget to change lengths!!!
    int middle_vals[middleSeqLen]  = {16, 8, 8, 2, 1, 8, 4, 2, 1, 8, 4, 1, 1, 2, 1, 8, 4, 1};             // don't forget to change lengths!!!
    int shortest_vals[shortSeqLen] = {16, 2, 1, 8, 4, 2, 1, 8, 4, 2, 1, 8, 4, 2, 1, 8, 4};                // don't forget to change lengths!!!

    seq_p shortSeq = malloc( sizeof(struct seq) );
    // initializeSeq(shortSeq, SEQ_CAPACITY);

    seq_p middleSeq = malloc( sizeof(struct seq) );
    // initializeSeq(middleSeq, SEQ_CAPACITY);

    seq_p longSeq = malloc( sizeof(struct seq) );
    // initializeSeq(longSeq, SEQ_CAPACITY);

    seq_p retLongSeq = malloc( sizeof(struct seq) );
    // initializeSeq(retLongSeq, SEQ_CAPACITY);

    seq_p retMiddleSeq = malloc( sizeof(struct seq) );
//    initializeSeq(retMiddleSeq, SEQ_CAPACITY);

    seq_p retShortSeq = malloc( sizeof(struct seq) );
//    initializeSeq(retShortSeq,  SEQ_CAPACITY);

    cost_matrices_2d_p costMtx2d        = malloc(sizeof(struct cost_matrices_2d));
    cost_matrices_2d_p costMtx2d_affine = malloc(sizeof(struct cost_matrices_2d));
    cost_matrices_3d_p costMtx3d        = malloc(sizeof(struct cost_matrices_3d));


    if(DO_2D) {
        setup2dCostMtx (tcm, alphSize, 0, costMtx2d);
    }

    if(DO_2D_AFF) {
        setup2dCostMtx (tcm, alphSize, GAP_OPEN_COST, costMtx2d_affine);
    }

    if(DO_3D) {
        setup3dCostMtx (tcm, alphSize, 0, costMtx3d);
    }
    // cost_matrices_3d_p costMtx3d_affine = malloc(sizeof(struct cost_matrices_3d));
    // setup3dCostMtx (tcm, alphSize, GAP_OPEN_COST, costMtx3d);



    int algnCost;


/**************************************************** Do 2d alignment ********************************************************/

    if (DO_2D) {
        printf("\n\n\n******************** Align 2 characters **********************\n");
        alignIO_p char1              = malloc(sizeof(struct alignIO));
        alignIO_p char2              = malloc(sizeof(struct alignIO));
        alignIO_p ungappedMedianSeq = malloc(sizeof(struct alignIO));
        alignIO_p gappedMedianSeq   = malloc(sizeof(struct alignIO));
        alignIO_p unionMedianSeq    = malloc(sizeof(struct alignIO));

        const size_t MAX_LENGTH = longSeqLen + shortSeqLen + 1;

        ungappedMedianSeq->character = malloc(MAX_LENGTH * sizeof(int));
        ungappedMedianSeq->length   = 0;
        ungappedMedianSeq->capacity = MAX_LENGTH;

        gappedMedianSeq->character   = malloc(MAX_LENGTH * sizeof(int));
        gappedMedianSeq->length     = 0;
        gappedMedianSeq->capacity   = MAX_LENGTH;

        unionMedianSeq->character    = malloc(MAX_LENGTH * sizeof(int));
        unionMedianSeq->length      = 0;
        unionMedianSeq->capacity    = MAX_LENGTH;

        copyVals(char1, longest_vals, longSeqLen, MAX_LENGTH);
        copyVals(char2, shortest_vals, shortSeqLen, MAX_LENGTH);

        // printf("Original alignment matrix before algn_nw_2d: \n");
        // algn_print_dynmtrx_2d( longSeq, shortSeq, algn_mtxs2d );

        // resetSeqValues(retLongSeq);
        // resetSeqValues(retShortSeq);
        printf("Original 2d characters:\n");
        alignIO_print(char1);
        alignIO_print(char2);

        algnCost = align2d(char1,
                           char2,
                           gappedMedianSeq,
                           ungappedMedianSeq,
                           unionMedianSeq,
                           costMtx2d,
                           1,                    // do traceback
                           1);                   // do union
        // if (DEBUG_MAT) {
        //     printf("\n\nFinal alignment matrix: \n\n");
        //     algn_print_dynmtrx_2d( longSeq, shortSeq, algn_mtxs2d );
        // }




        //algn_backtrace_2d (shortSeq, longSeq, retShortSeq, retLongSeq, algn_mtxs2d, costMtx2d, 0, 0, 1);
        printf("\nAligned 2d characters\n");
        alignIO_print(char1);
        alignIO_print(char2);

        printf("Alignment cost: %d\n", algnCost);

        printf("  Gapped character\n  ");
        alignIO_print(gappedMedianSeq);

        printf("  Ungapped character\n  ");
        alignIO_print(ungappedMedianSeq);

        // union:
        //algn_union (retShortSeq, retLongSeq, algnSeq);
        printf("  Unioned character\n  ");
        alignIO_print(unionMedianSeq);
    }



/************************************************ Do 2d affine alignment *****************************************************/

    /*** must have gap at start of character!!! ***/

    if (DO_2D_AFF) {
        printf("\n\n\n******************** Align 2 character affine **********************\n");

        alignIO_p char1      = malloc(sizeof(struct alignIO));
        alignIO_p char2      = malloc(sizeof(struct alignIO));
        alignIO_p ungappedMedianSeq = malloc(sizeof(struct alignIO));
        alignIO_p gappedMedianSeq   = malloc(sizeof(struct alignIO));
        alignIO_p unionMedianSeq    = malloc(sizeof(struct alignIO));

        const size_t MAX_LENGTH = longSeqLen + shortSeqLen + 1;

        ungappedMedianSeq->character = malloc(MAX_LENGTH * sizeof(int));
        ungappedMedianSeq->length   = 0;
        ungappedMedianSeq->capacity = MAX_LENGTH;

        gappedMedianSeq->character   = malloc(MAX_LENGTH * sizeof(int));
        gappedMedianSeq->length     = 0;
        gappedMedianSeq->capacity   = MAX_LENGTH;

        unionMedianSeq->character    = malloc(MAX_LENGTH * sizeof(int));
        unionMedianSeq->length      = 0;
        unionMedianSeq->capacity    = MAX_LENGTH;

        copyVals(char1, longest_vals, longSeqLen, MAX_LENGTH);
        copyVals(char2, shortest_vals, shortSeqLen, MAX_LENGTH);

        printf("Original 2d affine characters\n");
        alignIO_print(char1);
        alignIO_print(char2);


        // shorter first
        algnCost = align2dAffine(char1,
                                 char2,
                                 gappedMedianSeq,
                                 ungappedMedianSeq,
                                 unionMedianSeq,
                                 costMtx2d_affine,
                                 1,                    // do traceback
                                 1);                   // do union



        printf("\nAligned 2d affine characters\n");
        alignIO_print(char1);
        alignIO_print(char2);

        printf("\n\nAlignment cost: %d\n", algnCost);


        printf("  Gapped character\n  ");
        alignIO_print(gappedMedianSeq);

        printf("  Ungapped character\n  ");
        alignIO_print(ungappedMedianSeq);

        // union:
        //algn_union (retShortSeq, retLongSeq, algnSeq);
        printf("  Unioned character\n  ");
        alignIO_print(unionMedianSeq);

    }


/************************************************ Do 3d alignment *************************************************/
/*
    if (DO_3D) {



        printf("\n\n\n******************** Align 3 characters **********************\n\n");


        alignIO_p char1      = malloc(sizeof(struct alignIO));
        alignIO_p char2      = malloc(sizeof(struct alignIO));
        alignIO_p char3      = malloc(sizeof(struct alignIO));
        alignIO_p medianSeq = malloc(sizeof(struct alignIO));

        const size_t MAX_LENGTH = longSeqLen + shortSeqLen + 1;

        medianSeq->character  = malloc(MAX_LENGTH * sizeof(int));
        medianSeq->length    = 0;
        medianSeq->capacity  = MAX_LENGTH;

        copyVals(char1, longest_vals,  longSeqLen,   MAX_LENGTH);
        copyVals(char2, middle_vals,   middleSeqLen, MAX_LENGTH);
        copyVals(char3, shortest_vals, shortSeqLen,  MAX_LENGTH);

        // printf("Original alignment matrix before algn_nw_2d: \n");
        // algn_print_dynmtrx_2d( longSeq, shortSeq, algn_mtxs2d );

        // resetSeqValues(retLongSeq);
        // resetSeqValues(retShortSeq);
        printf("Original 3d characters:\n");
        alignIO_print(char1);
        alignIO_print(char2);
        alignIO_print(char3);

        algnCost = align3d(char1,
                           char2,
                           char3,
                           medianSeq,
                           costMtx3d);
        // if (DEBUG_MAT) {
        //     printf("\n\nFinal alignment matrix: \n\n");
        //     algn_print_dynmtrx_2d( longSeq, shortSeq, algn_mtxs2d );
        // }

        printf("\nAligned 3d characters\n");
        alignIO_print(char1);
        alignIO_print(char2);
        alignIO_print(char3);

        printf("Alignment cost: %d\n", algnCost);

        // union:
        //algn_union (retShortSeq, retLongSeq, algnSeq);
        printf("  Unioned character\n  ");
        alignIO_print(medianSeq);
        // must first reset values in retLongSeq and retShortSeq

        printf("\n\nAlignment cost: %d\n", algnCost);

        printf("\n\n\n");
    }
*/
/*
    if (DO_3D_AFF) {

        printf("\n\n\n******************** Align 3 characters affine **********************\n\n");


        alignIO_p char1      = malloc(sizeof(struct alignIO));
        alignIO_p char2      = malloc(sizeof(struct alignIO));
        alignIO_p char3      = malloc(sizeof(struct alignIO));
        alignIO_p medianSeq = malloc(sizeof(struct alignIO));

        const size_t MAX_LENGTH = longSeqLen + shortSeqLen + 1;

        medianSeq->character  = malloc(MAX_LENGTH * sizeof(int));
        medianSeq->length    = 0;
        medianSeq->capacity  = MAX_LENGTH;

        copyVals(char1, longest_vals,  longSeqLen,   MAX_LENGTH);
        copyVals(char2, middle_vals,   middleSeqLen, MAX_LENGTH);
        copyVals(char3, shortest_vals, shortSeqLen,  MAX_LENGTH);

        // printf("Original alignment matrix before algn_nw_2d: \n");
        // algn_print_dynmtrx_2d( longSeq, shortSeq, algn_mtxs2d );

        // resetSeqValues(retLongSeq);
        // resetSeqValues(retShortSeq);
        printf("Original 3d character:\n");
        alignIO_print(char1);
        alignIO_print(char2);
        alignIO_print(char3);

        algnCost = align3d(char1,
                           char2,
                           char3,
                           medianSeq,
                           costMtx3d_affine);
        // if (DEBUG_MAT) {
        //     printf("\n\nFinal alignment matrix: \n\n");
        //     algn_print_dynmtrx_2d( longSeq, shortSeq, algn_mtxs2d );
        // }

        printf("\nAligned 3d character\n");
        alignIO_print(char1);
        alignIO_print(char2);
        alignIO_print(char3);

        printf("Alignment cost: %d\n", algnCost);

        // union:
        //algn_union (retShortSeq, retLongSeq, algnSeq);
        printf("  Unioned character\n  ");
        alignIO_print(medianSeq);
        // must first reset values in retLongSeq and retShortSeq

        printf("\n\nAlignment cost: %d\n", algnCost);

        printf("\n\n\n");

        // for (SEQT *base = retLongSeq->seq_begin; base != retLongSeq->end; base++) {
        //     printf("a: %c\n", *base);
        // }
        // for (SEQT *base = retShortSeq->seq_begin; base != retShortSeq->end; base++) {
        //     printf("b: %s\n", base);
        // }
    }
*/

    // Next this: algn_get_median_3d (seq_p char1, seq_p char2, seq_p char3,
    //                cost_matrices_3d_p m, seq_p sm)

    if(DO_2D) freeCostMtx(costMtx2d, 1);
    if(DO_2D_AFF) freeCostMtx(costMtx2d_affine, 1);  // 0 is !2d
    if(DO_3D) freeCostMtx(costMtx3d, 0);  // 0 is !2d

    free(tcm);

    return 0;
}

