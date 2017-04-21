#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "../alignSequences.h"
#include "../c_code_alloc_setup.h"
#include "../debug_constants.h"
// #include "../costMatrix.h"
#include "../nwMatrices.h"
#include "../ukkCheckp.h"
#include "../ukkCommon.h"

// #define CHAR_CAPACITY 64

int power_2 (int input) {
    return __builtin_popcount(input) == 1;
}



void setChar(const int *vals, size_t length, seq_p retChar) {
    // assign sequence into sequence struct
    retChar->len = length;
    retChar->seq_begin = retChar->end - length;
    if (length > 0) {
        for(size_t i = 0; i < length; i++) {
            retChar->seq_begin[i] = (int) vals[i];
        }
    }
}



int main() {


/******************************** set up and allocate all variables and structs ************************************/


/****************  Allocate sequences  ****************/

        //***** for following seqs, affine requires gap at start of sequence!!! *****/

    const int longCharLen   = 22;
    const int middleCharLen = 18;
    const int shortCharLen  = 17;

    const size_t CHAR_CAPACITY = longCharLen + shortCharLen + middleCharLen;


    int alphSize = 5; // includes gap, but no ambiguities
    int longest_vals [22] = {16, 2, 1, 8, 4, 2, 1, 8, 4, 1, 1, 1, 1, 1, 2, 1, 8, 4, 2, 1, 8, 4}; // don't forget to change lengths!!!
    int middle_vals  [18] = {16, 8, 8, 2, 1, 8, 4, 2, 1, 8, 4, 1, 1, 2, 1, 8, 4, 1};             // don't forget to change lengths!!!
    int shortest_vals[17] = {16, 2, 1, 8, 4, 2, 1, 8, 4, 2, 1, 8, 4, 2, 1, 8, 4};                // don't forget to change lengths!!!



    seq_p shortChar     = malloc(sizeof(struct seq));
    initializeChar(shortChar, CHAR_CAPACITY);
    setChar(shortest_vals, shortCharLen, shortChar);

    seq_p middleChar    = malloc(sizeof(struct seq));
    initializeChar(middleChar, CHAR_CAPACITY);
    setChar(middle_vals, middleCharLen, middleChar);

    seq_p longChar      = malloc(sizeof(struct seq));
    initializeChar(longChar, CHAR_CAPACITY);
    setChar(longest_vals, longCharLen, longChar);

    seq_p retShortChar  = malloc( sizeof(struct seq) );
    initializeChar(retShortChar,  CHAR_CAPACITY);

    seq_p retMiddleChar = malloc( sizeof(struct seq) );
    initializeChar(retMiddleChar, CHAR_CAPACITY);

    seq_p retLongChar   = malloc( sizeof(struct seq) );
    initializeChar(retLongChar,   CHAR_CAPACITY);






/************  Allocate cost matrices  **************/

    size_t tcm_total_len = alphSize * alphSize; // the size of the input tcm


    // !!!!! if modifying this code, also make sure to change is_metric !!!!!!
    /** TCM is only for non-ambiguous nucleotides, and it used to generate
     *  the entire cost matrix, which includes ambiguous elements.
     *  TCM is row-major, with each row being the left sequence element.
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

    // Print TCM in pretty format
    const int n = alphSize;
    for (size_t i = 0; i < n; ++i) {
        for (size_t j = 0; j < n; ++j) {
            printf("%2d ", tcm[ n*i + j ]);
        }
        printf("\n");
    }


    cost_matrices_2d_p costMtx2d        = malloc(sizeof(struct cost_matrices_2d));
    cost_matrices_2d_p costMtx2d_affine = malloc(sizeof(struct cost_matrices_2d));
    cost_matrices_3d_p costMtx3d        = malloc(sizeof(struct cost_matrices_3d));
    cost_matrices_3d_p costMtx3d_affine = malloc(sizeof(struct cost_matrices_3d));

    // tcm is tcm; alphSize includes gap; third param is gap opening cost
    setup2dCostMtx (tcm, alphSize, 0,             costMtx2d);
    setup2dCostMtx (tcm, alphSize, GAP_OPEN_COST, costMtx2d_affine);
    setup3dCostMtx (tcm, alphSize, 0,             costMtx3d);
    setup3dCostMtx (tcm, alphSize, GAP_OPEN_COST, costMtx3d_affine);
    //cm_print_2d (costMtx2d);

    /****************  Allocate NW matrices  ****************/
    // in following, penultimate parameter was ukk flag, used only to set up 3d matrices.
    nw_matrices_p algn_mtxs2d       = malloc(sizeof(struct nwMatrices));
    nw_matrices_p algn_mtxs2dAffine = malloc(sizeof(struct nwMatrices));
    nw_matrices_p algn_mtxs3d       = malloc(sizeof(struct nwMatrices));
    nw_matrices_p algn_mtxs3dAffine = malloc(sizeof(struct nwMatrices));

    if (DO_2D) {
        initializeNWMtx(longChar->len, shortChar->len,  0,             costMtx2d->costMatrixDimension,        algn_mtxs2d);
    }
    if (DO_2D_AFF) {
        initializeNWMtx(longChar->len, shortChar->len,  0,             costMtx2d_affine->costMatrixDimension, algn_mtxs2dAffine);
    }
    if (DO_3D) {
        initializeNWMtx(longChar->len, middleChar->len, shortChar->len, costMtx3d->costMatrixDimension,        algn_mtxs3d);
    }
    if (DO_3D_AFF) {
        initializeNWMtx(longChar->len, middleChar->len, shortChar->len, costMtx3d_affine->costMatrixDimension, algn_mtxs3dAffine);
    }

    int algnCost;

    /**
    // Print TCM in pretty format
    const int n = costMtx2d->costMatrixDimension;
    for (size_t i = 0; i < n; ++i) {
        for (size_t j = 0; j < n; ++j) {
            printf("%2d ", tcm[ n*i + j ]);
        }
        printf("\n");
    }
    **/

    // the following to compute deltawh, which increases the matrix height or width in algn_nw_2d
    // pulled from ML code
    // deltawh is for use in Ukonnen, it gives the current necessary width of the Ukk matrix
    int deltawh     = 0;
    int diff        = longChar->len - shortChar->len;
    int lower_limit = .1 * longChar->len;
    if (deltawh) {
        deltawh = diff < lower_limit ? lower_limit : deltawh;
    } else {
        deltawh = diff < lower_limit ? lower_limit / 2 : 2;
    }


    // cm_print_3d (costMtx3d);

/**************************************************** Do 2d alignment ********************************************************/

    if (DO_2D) {
        printf("\n\n\n******************** Align 2 sequences **********************\n");

        // printf("Original alignment matrix before algn_nw_2d: \n");
        // algn_print_dynmtrx_2d( longChar, shortChar, algn_mtxs2d );

        algnCost = algn_nw_2d( shortChar, longChar, costMtx2d, algn_mtxs2d, deltawh );

        if (DEBUG_MAT) {
            printf("\n\nFinal alignment matrix: \n\n");
            algn_print_dynmtrx_2d( longChar, shortChar, algn_mtxs2d );
        }


        printf("Original 2d sequences:\n");
        seq_print(longChar);
        seq_print(shortChar);

        algn_backtrace_2d (shortChar, longChar, retShortChar, retLongChar, algn_mtxs2d, costMtx2d, 0, 0, 1);
        printf("\nAligned 2d sequences\n");
        seq_print(retLongChar);
        seq_print(retShortChar);

        printf("\nAlignment cost: %d\n", algnCost);

        /****  Now get alignments  ****/

        printf("\nAligned sequences:\n");
        //int *algnCharVals = calloc(retLongChar->len, sizeof(int));
        seq_p algnChar = malloc( sizeof(struct seq) );;
        initializeChar(algnChar, CHAR_CAPACITY);
        //free (algnCharVals);
        resetCharValues(algnChar);

        // union:
        algn_union (retShortChar, retLongChar, algnChar);
        printf("  Unioned sequence\n  ");
        seq_print(algnChar);

        // ungapped:
        resetCharValues(algnChar);
        algn_get_median_2d_no_gaps (retShortChar, retLongChar, costMtx2d, algnChar);
        printf("\n  Median without gaps\n  ");
        seq_print(algnChar);

        // gapped:
        resetCharValues(algnChar);
        algn_get_median_2d_with_gaps (retShortChar, retLongChar, costMtx2d, algnChar);
        printf("\n  Median with gaps\n  ");
        seq_print(algnChar);

        free (algnChar);
    }



/************************************************ Do 2d affine alignment *****************************************************/

    /*** must have gap at start of sequence!!! ***/

    if (DO_2D_AFF) {
        resetCharValues(retLongChar);
        resetCharValues(retShortChar);

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

        DIR_MTX_ARROW_t *direction_matrix;
        size_t lenLongChar  = longChar->len;
        size_t lenShortChar = shortChar->len;
        lenLongerChar = (lenLongChar > lenShortChar) ? lenLongChar : lenShortChar;

        //    mat_setup_size (algn_mtxs2dAffine, lenLongerChar, lenLongerChar, 0, 0, cm_get_costMatrixDimension (costMtx2d_affine));
        matrix_2d  = algn_mtxs2dAffine->nw_costMtx;
        precalcMtx = algn_mtxs2dAffine->precalcMtx;

        // TODO: figure out what the following seven values do/are
        //       also note the int factors, which maybe have something to do with the unexplained 12
        //       that appears in matrices.c?
        // here and in algn.c, "block" refers to a block of gaps, so close_block_diagonal is the cost to
        // end a subsequence of gaps, presumably with a substitution, but maybe by simply switching directions:
        // there was a vertical gap, now there's a horizontal one.
        close_block_diagonal       = matrix_2d;
        extend_block_diagonal      = matrix_2d + ( 2 * lenLongerChar);
        extend_vertical            = matrix_2d + ( 4 * lenLongerChar);
        extend_horizontal          = matrix_2d + ( 6 * lenLongerChar);
        final_cost_matrix          = matrix_2d + ( 8 * lenLongerChar);
        gap_open_prec              = matrix_2d + (10 * lenLongerChar);
        s_horizontal_gap_extension = matrix_2d + (11 * lenLongerChar);



        // TODO: ungappedMedChar might not be necessary, as it's unused in ml code:
        size_t medianCharLen       = lenLongChar + lenShortChar + 2;  // 2 because that's how it is in ML code
        seq_p gappedMedChar        = malloc( sizeof(struct seq) );
        gappedMedChar->cap         = medianCharLen;
        gappedMedChar->array_head  = calloc( medianCharLen, sizeof(SEQT));
        gappedMedChar->len         = 0;
        gappedMedChar->seq_begin   = gappedMedChar->end = gappedMedChar->array_head + medianCharLen;

        seq_p ungappedMedChar       = malloc( sizeof(struct seq) );
        ungappedMedChar->cap        = medianCharLen;
        ungappedMedChar->array_head = calloc( medianCharLen, sizeof(SEQT));
        ungappedMedChar->len        = 0;
        ungappedMedChar->seq_begin  = ungappedMedChar->end = ungappedMedChar->array_head + medianCharLen;

        direction_matrix            = algn_mtxs2dAffine->nw_dirMtx;

        printf("\n\n\n***************** Align 2 sequences affine ********************\n\n");

        printf("Original affine 2d sequences:\n");

        // seq_p longerCharuence = lenLongChar > lenShortChar ? longChar : shortChar;
        // seq_p shorterCharuence = lenLongChar > lenShortChar ? shortChar : longChar;

        seq_print(longChar);
        seq_print(shortChar);

        cm_precalc_4algn(costMtx2d_affine, algn_mtxs2dAffine, longChar);

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

        if (DEBUG_AFFINE) {
            printf("\n");
            printf("close_block_diagonal      : %d\n", *close_block_diagonal      );
            printf("extend_block_diagonal     : %d\n", *extend_block_diagonal     );
            printf("extend_vertical           : %d\n", *extend_vertical           );
            printf("extend_horizontal         : %d\n", *extend_horizontal         );
            printf("final_cost_matrix         : %d\n", *final_cost_matrix         );
            printf("gap_open_prec             : %d\n", *gap_open_prec             );
            printf("s_horizontal_gap_extension: %d\n", *s_horizontal_gap_extension);
            printf("\n");
        }

       // for (int *i = matrix_2d, j = 0; i < matrix_2d + algn_mtxs2dAffine->len; i++, j++) {
       //     printf("%d, ", *i);
       //     if (j % (lenLongerChar ) == 0) {
       //         printf("\n");
       //     }
       // }


        // shorter first
        // TODO: why isn't this argument order consistent with next fn call?
        algnCost = algn_fill_plane_2d_affine (shortChar,
                                              longChar,
                                              shortChar->len - 1,
                                              longChar->len - 1,
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



        if (DEBUG_MAT) {
            printf("\n\nFinal alignment matrix, affine: \n\n");
            algn_print_dynmtrx_2d( shortChar, longChar, algn_mtxs2dAffine );
        }


        // shorter first
        // TODO: fix this to make it consistent
        algn_backtrace_affine (shortChar,
                               longChar,
                               direction_matrix,
                               ungappedMedChar,
                               gappedMedChar,
                               retShortChar,
                               retLongChar,
                               costMtx2d_affine);

        printf("\nAligned affine 2d sequences\n");
        if (lenLongChar > lenShortChar) {
          seq_print(retShortChar);
          seq_print(retLongChar);
        } else {
          seq_print(retLongChar);
          seq_print(retShortChar);
        }


        printf("\nAlignment cost: %d\n", algnCost);

        // ungapped:
        printf("\n  Median without gaps\n  ");
        seq_print(ungappedMedChar);

        // gapped:
        printf("\n  Median with gaps\n  ");
        seq_print(gappedMedChar);

        freeChar(gappedMedChar);
        freeChar(ungappedMedChar);

    }


/************************************************ Do 3d alignment *************************************************/

    if (DO_3D) {

        printf("\n\n\n******************** Align 3 sequences **********************\n\n");

        // must first reset values in retLongChar and retShortChar
        resetCharValues(retLongChar);
        resetCharValues(retShortChar);

        // algnCost = algn_nw_3d (longChar, middleChar, shortChar, costMtx3d, algn_mtxs3d, deltawh);
        //printf("Final alignment matrix: \n");
        //algn_print_dynmtrx_2d_2d( longChar, shortChar, algn_mtxs3d );

        printf("Original 3d sequences:\n");
        seq_print(longChar);
        seq_print(middleChar);
        seq_print(shortChar);
        printf("\n");

        // short input, middle input, long input
        // short return, middle return, long return
        // sub, gap open, gap extend
        algnCost = powell_3D_align (shortChar,    middleChar,    longChar,
                                    retLongChar, retMiddleChar, retShortChar,
                                    1, 2, 1);

        // algn_backtrace_3d(shortChar, middleChar, longChar,
        //                   retShortChar, retMiddleChar, retLongChar,
        //                   costMtx3d, algn_nw_3d);

        //algn_backtrace_3d (longChar, middleChar, shortChar, retLongChar, retMiddleChar, retShortChar, costMtx3d, algn_mtxs3d);

        printf("\n\nAligned 3d sequences:\n");
        seq_print(retLongChar);
        seq_print(retMiddleChar);
        seq_print(retShortChar);

        printf("\nAlignment cost: %d\n", algnCost);

        printf("\n\n\n");

        // for (SEQT *base = retLongChar->seq_begin; base != retLongChar->end; base++) {
        //     printf("a: %c\n", *base);
        // }
        // for (SEQT *base = retShortChar->seq_begin; base != retShortChar->end; base++) {
        //     printf("b: %s\n", base);
        // }
    }

    // Next this: algn_get_median_3d (seq_p seq1, seq_p seq2, seq_p seq3,
    //                cost_matrices_3d_p m, seq_p sm)

    freeCostMtx(costMtx2d,        1);  // 1 is 2d
    freeCostMtx(costMtx2d_affine, 1);
    freeCostMtx(costMtx3d,        0);  // 0 is !2d

    freeNWMtx(algn_mtxs2d);
    freeNWMtx(algn_mtxs2dAffine);
    freeNWMtx(algn_mtxs3d);

    freeChar(longChar);
    freeChar(shortChar);
    freeChar(middleChar);
    freeChar(retLongChar);
    freeChar(retShortChar);
    freeChar(retMiddleChar);

    free(tcm);

    return 0;
}
