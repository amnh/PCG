#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "../alignSequences.h"
#include "../c_alignment_interface.h"
#include "../c_code_alloc_setup.h"
#include "../debug_constants.h"
#include "../costMatrix.h"
#include "../nwMatrices.h"
#include "../ukkCheckp.h"
#include "../ukkCommon.h"

// #define SEQ_CAPACITY 64


int power_2 (int input) {
    if (input == 1)  return 1;
    if (input == 2)  return 1;
    if (input == 4)  return 1;
    if (input == 8)  return 1;
    if (input == 16) return 1;
    return 0;
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

    const size_t longSeqLen   = 6;
    const size_t middleSeqLen = 6;
    const size_t shortSeqLen  = 4;

    const size_t SEQ_CAPACITY_2D = longSeqLen + shortSeqLen + 2; // extra 2 for gaps? TODO: Think about this

    SEQT longest_vals [6] = {4, 1, 8, 3, 1, 15};  // don't forget to change lengths!!!
    SEQT middle_vals  [6] = {4, 1, 8, 1, 2, 3};  // don't forget to change lengths!!!
    SEQT shortest_vals[4] = {4, 1, 8, 1};  // don't forget to change lengths!!!

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
        alignIO_p char1             = malloc(sizeof(struct alignIO));
        alignIO_p char2             = malloc(sizeof(struct alignIO));
        alignIO_p ungappedMedianSeq = malloc(sizeof(struct alignIO));
        alignIO_p gappedMedianSeq   = malloc(sizeof(struct alignIO));
        // alignIO_p unionMedianSeq    = malloc(sizeof(struct alignIO));

        // TODO: When should I add gaps at beginning of characters? Probably on this side of FFI, so work out math on this correctly.
        const size_t MAX_LENGTH      = longSeqLen + shortSeqLen + 3; // 3 because there are two gaps added (1 on beginning of each)

        // need to allocate space for return alignIOs, as they're no long alloced in c_alignment_interface
        allocAlignIO(char1,             MAX_LENGTH);
        allocAlignIO(char2,             MAX_LENGTH);
        allocAlignIO(ungappedMedianSeq, MAX_LENGTH);
        allocAlignIO(gappedMedianSeq,   MAX_LENGTH);
        //allocAlignIO(unionMedianSeq,    MAX_LENGTH);

        copyValsToAIO(char1, longest_vals,  longSeqLen,  MAX_LENGTH);
        copyValsToAIO(char2, shortest_vals, shortSeqLen, MAX_LENGTH);

        printf("\n\n********** Cost only (all chars should be empty): **********\n");
        printf("\n********** Original 2d characters: **********\n");
        alignIO_print(char1);
        alignIO_print(char2);

        algnCost = align2d(char1,
                           char2,
                           gappedMedianSeq,
                           ungappedMedianSeq,
                           // unionMedianSeq,
                           costMtx2d,
                           0,                    // do ungapped
                           0,                    // do gapped
                           0);                   // do union
        // if (DEBUG_MAT) {
        //     printf("\n\nFinal alignment matrix: \n\n");
        //     algn_print_dynmtrx_2d( longSeq, shortSeq, algn_mtxs2d );
        // }

        printf("Alignment cost: %d\n", algnCost);

        printf("\nAligned 2d characters\n");
        alignIO_print(char1);
        alignIO_print(char2);

        printf("\n  Gapped character  ");
        alignIO_print(gappedMedianSeq);

        printf("\n  Ungapped character  ");
        alignIO_print(ungappedMedianSeq);

        // union:
        //algn_union (retShortSeq, retLongSeq, algnSeq);
        // printf("  Unioned character\n  ");
        // alignIO_print(unionMedianSeq);
        //printf("here.\n");

        resetAlignIO(char1);
        resetAlignIO(char2);
        resetAlignIO(ungappedMedianSeq);
        resetAlignIO(gappedMedianSeq);
        // resetAlignIO(unionMedSeq);

        copyValsToAIO(char1, longest_vals,  longSeqLen,  MAX_LENGTH);
        copyValsToAIO(char2, shortest_vals, shortSeqLen, MAX_LENGTH);

        printf("\n\n********** Ungapped only (gapped should be empty): **********\n");
        printf("\n**********Original 2d characters:**********\n");
        alignIO_print(char1);
        alignIO_print(char2);

        algnCost = align2d(char1,
                           char2,
                           gappedMedianSeq,
                           ungappedMedianSeq,
                           // unionMedianSeq,
                           costMtx2d,
                           1,                    // do ungapped
                           0,                    // do gapped
                           0);                   // do union


        printf("\nAligned 2d characters\n");
        alignIO_print(char1);
        alignIO_print(char2);

        printf("Alignment cost: %d\n", algnCost);

        printf("\nGapped character  ");
        alignIO_print(gappedMedianSeq);

        printf("\nUngapped character  ");
        alignIO_print(ungappedMedianSeq);

        resetAlignIO(char1);
        resetAlignIO(char2);
        resetAlignIO(ungappedMedianSeq);
        resetAlignIO(gappedMedianSeq);
        printf("copy longer\n");
        copyValsToAIO(char1, longest_vals, longSeqLen, MAX_LENGTH);
        printf("copy shorter\n");
        copyValsToAIO(char2, shortest_vals, shortSeqLen, MAX_LENGTH);

        printf("\n\n********** Gapped only (ungapped should be empty): **********\n");
        printf("\n**********Original 2d characters:**********\n");
        alignIO_print(char1);
        alignIO_print(char2);

        algnCost = align2d(char1,
                           char2,
                           gappedMedianSeq,
                           ungappedMedianSeq,
                           // unionMedianSeq,
                           costMtx2d,
                           0,                    // do ungapped
                           1,                    // do gapped
                           0);                   // do union

        printf("Alignment cost: %d\n", algnCost);

        printf("\nAligned 2d characters\n");
        alignIO_print(char1);
        alignIO_print(char2);

        printf("\nGapped character  ");
        alignIO_print(gappedMedianSeq);

        printf("\nUngapped character  ");
        alignIO_print(ungappedMedianSeq);

        resetAlignIO(char1);
        resetAlignIO(char2);
        resetAlignIO(ungappedMedianSeq);
        resetAlignIO(gappedMedianSeq);

        copyValsToAIO(char1, longest_vals, longSeqLen, MAX_LENGTH);
        copyValsToAIO(char2, shortest_vals, shortSeqLen, MAX_LENGTH);

        resetAlignIO(char1);
        resetAlignIO(char2);
        resetAlignIO(ungappedMedianSeq);
        resetAlignIO(gappedMedianSeq);

        copyValsToAIO(char1, longest_vals, longSeqLen, MAX_LENGTH);
        copyValsToAIO(char2, shortest_vals, shortSeqLen, MAX_LENGTH);

        printf("\n\n********** Gapped and ungapped: **********\n");
        printf("\n**********Original 2d characters:**********\n");
        alignIO_print(char1);
        alignIO_print(char2);

        algnCost = align2d(char1,
                           char2,
                           gappedMedianSeq,
                           ungappedMedianSeq,
                           // unionMedianSeq,
                           costMtx2d,
                           1,                    // do ungapped
                           1,                    // do gapped
                           0);                   // do union

        printf("\nAligned 2d characters\n");
        alignIO_print(char1);
        alignIO_print(char2);
        printf("Alignment cost: %d\n", algnCost);


        printf("\nGapped character  ");
        alignIO_print(gappedMedianSeq);

        printf("\nUngapped character  ");
        alignIO_print(ungappedMedianSeq);

        resetAlignIO(char1);
        resetAlignIO(char2);
        resetAlignIO(ungappedMedianSeq);
        resetAlignIO(gappedMedianSeq);

        copyValsToAIO(char1, longest_vals, longSeqLen, MAX_LENGTH);
        copyValsToAIO(char2, shortest_vals, shortSeqLen, MAX_LENGTH);

        printf("\n\n********** Gapped and union (ungapped should be empty, union should override ungapped): **********\n");
        printf("\n**********Original 2d characters:**********\n");


        alignIO_print(char1);
        alignIO_print(char2);


        algnCost = align2d(char1,
                           char2,
                           gappedMedianSeq,
                           ungappedMedianSeq,
                           // unionMedianSeq,
                           costMtx2d,
                           0,                    // do ungapped
                           1,                    // do gapped
                           1);                   // do union


        printf("\nAligned 2d characters\n");
        alignIO_print(char1);
        alignIO_print(char2);
        printf("Alignment cost: %d\n", algnCost);


        printf("\n  Union character  ");
        alignIO_print(gappedMedianSeq);

        printf("\n  Ungapped character  ");
        alignIO_print(ungappedMedianSeq);


        freeAlignIO(char1);
        freeAlignIO(char2);
        freeAlignIO(ungappedMedianSeq);
        freeAlignIO(gappedMedianSeq);
        // freeAlignIO(unionMedianSeq);

    } // Do 2D



/************************************************ Do 2d affine alignment *****************************************************/

    /*** must have gap at start of character!!! ***/

    if (DO_2D_AFF) {
        printf("\n\n\n******************** Align 2 characters affine **********************\n");

        alignIO_p char1             = malloc(sizeof(struct alignIO));
        alignIO_p char2             = malloc(sizeof(struct alignIO));
        alignIO_p ungappedMedianSeq = malloc(sizeof(struct alignIO));
        alignIO_p gappedMedianSeq   = malloc(sizeof(struct alignIO));

        const size_t MAX_LENGTH = longSeqLen + shortSeqLen + 2;

        allocAlignIO(char1,             MAX_LENGTH);
        allocAlignIO(char2,             MAX_LENGTH);
        allocAlignIO(ungappedMedianSeq, MAX_LENGTH);
        allocAlignIO(gappedMedianSeq,   MAX_LENGTH);

        printf("copy longer\n");
        copyValsToAIO(char1, longest_vals,  longSeqLen,  MAX_LENGTH);
        printf("\n\ncopy shorter\n");
        copyValsToAIO(char2, shortest_vals, shortSeqLen, MAX_LENGTH);

        printf("\n\n********** Cost only (all chars should be empty): **********\n");
        printf("\n******************* Original 2d characters: *******************\n");
        alignIO_print(char1);
        alignIO_print(char2);

        algnCost = align2dAffine(char1,
                                 char2,
                                 gappedMedianSeq,
                                 ungappedMedianSeq,
                                 // unionMedianSeq,
                                 costMtx2d_affine,
                                 0);                    // do medians

        printf("Alignment cost: %d\n", algnCost);

        printf("\nAligned 2d characters affine\n");
        alignIO_print(char1);
        alignIO_print(char2);

        printf("  Gapped character\n  ");
        alignIO_print(gappedMedianSeq);

        printf("  Ungapped character\n  ");
        alignIO_print(ungappedMedianSeq);

        resetAlignIO(char1);
        resetAlignIO(char2);
        resetAlignIO(ungappedMedianSeq);
        resetAlignIO(gappedMedianSeq);

        copyValsToAIO(char1, longest_vals, longSeqLen, MAX_LENGTH);
        copyValsToAIO(char2, shortest_vals, shortSeqLen, MAX_LENGTH);

        printf("\n\n********** With medians: **********\n");
        printf("\n**********Original 2d characters:**********\n");

        alignIO_print(char1);
        alignIO_print(char2);

        algnCost = align2dAffine(char1,
                                 char2,
                                 gappedMedianSeq,
                                 ungappedMedianSeq,
                                 costMtx2d_affine,
                                 1);                   // do medians

        printf("\nAligned 2d characters\n");
        alignIO_print(char1);
        alignIO_print(char2);
        printf("Alignment cost: %d\n", algnCost);

        printf("\nGapped character\n  ");
        alignIO_print(gappedMedianSeq);

        printf("\nUngapped character\n  ");
        alignIO_print(ungappedMedianSeq);

        freeAlignIO(char1);
        freeAlignIO(char2);
        freeAlignIO(ungappedMedianSeq);
        freeAlignIO(gappedMedianSeq);

        printf("LOOKS LIKE WE MADE IT!\n");


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

        copyValsToAIO(char1, longest_vals,  longSeqLen,   MAX_LENGTH);
        copyValsToAIO(char2, middle_vals,   middleSeqLen, MAX_LENGTH);
        copyValsToAIO(char3, shortest_vals, shortSeqLen,  MAX_LENGTH);

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

        copyValsToAIO(char1, longest_vals,  longSeqLen,   MAX_LENGTH);
        copyValsToAIO(char2, middle_vals,   middleSeqLen, MAX_LENGTH);
        copyValsToAIO(char3, shortest_vals, shortSeqLen,  MAX_LENGTH);

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

