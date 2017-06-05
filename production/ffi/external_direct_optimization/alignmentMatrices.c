/* POY 4.0 Beta. A phylogenetic analysis program using Dynamic Homologies.    */
/* Copyright (C) 2007  Andrés Varón, Le Sy Vinh, Illya Bomash, Ward Wheeler,  */
/* and the American Museum of Natural History.                                */
/*                                                                            */
/* This program is free software; you can redistribute it and/or modify       */
/* it under the terms of the GNU General Public License as published by       */
/* the Free Software Foundation; either version 2 of the License, or          */
/* (at your option) any later version.                                        */
/*                                                                            */
/* This program is distributed in the hope that it will be useful,            */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of             */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              */
/* GNU General Public License for more details.                               */
/*                                                                            */
/* You should have received a copy of the GNU General Public License          */
/* along with this program; if not, write to the Free Software                */
/* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301   */
/* USA                                                                        */

#include <stdio.h>
#include <stdlib.h>

#include "debug_constants.h"
#include "alignmentMatrices.h"
#include "costMatrix.h"


/*
 * For memory management efficiency, I will keep all the matrices in one big
 * chunk of memory, that I can reallocate as a whole, and reduce fragmentation
 * a lot if possible, all the alignment calculations and all the matrices that
 * are precomputed to speedup the alignments are held here.
 */
inline size_t
algnMat_size_of_3d_matrix (size_t w, size_t d, size_t h) { // originally had a fourth parameter, k for ukkunonen
    /* Not sure what this was for, as it was commented out, but kept for posterity's sake
       int basic_cube;
       int prism_1, prism_2, pyramid;
       basic_cube = k * k * k;
       prism_1 = (d - k) * (d - k) * k;
       prism_2 = (w - k) * (w - k) * k;
       pyramid = (w - k) * (w - k + 1) * (2 * (w - k) + 1) / 6;
       if (h > 2 * k) basic_cube += (h - (2 * k)) * w * d;
       return (basic_cube + prism_2 + prism_1 + pyramid);
    */
    return (w * d * h);
}

void algnMat_print(alignment_matrices_t *alignMtx, size_t alphSize) {
    printf("\nMatrices:\n");
    printf("    NW Matrix cap:         %zu\n", alignMtx->cap_nw);
    printf("    Efficiency mtx cap:    %zu\n", alignMtx->cap_eff);
    printf("    precalcMtx mtx cap:    %zu\n", alignMtx->cap_pre);

    printf("\n    precalcMtxulated nw matrix:\n");
    for( size_t i = 0; i < alignMtx->cap_pre; i += alphSize) {
        printf("    ");
        for( size_t j = 0; j < alphSize; j++) {
            printf("%4d", alignMtx->algn_precalcMtx[i + j]);
        }
        printf("\n");
    }

}

// The 12 is because we only use 2 rows of the matrix at a time on the alignment matrix,
// and we have four alignment matrices plus two shorter ones for the gap costs.
inline size_t
algnMat_size_of_2d_matrix (size_t w, size_t h) {
    if (w > h) return (w * 12); //TODO: do I need a cast here?
    else       return (h * 12);
}

void
algnMat_clean_direction_matrix (alignment_matrices_t *alignMtx) {
    size_t cap = alignMtx->cap_nw;
    size_t i;
    for (i = 0; i < cap; i++) {
        alignMtx->algn_dirMtx[i] = (DIR_MTX_ARROW_t) 0;
    }
}

/** Allocate or reallocate space for the six matrices, for both 2d and 3d alignments.
 *  @param slphabetSize is length of original alphabet including gap.
 *  Checks current allocation size and increases size if necessary.
 */
inline void
algnMat_setup_size (alignment_matrices_t *alignMtx, size_t len_char1, size_t len_char2, size_t len_char3, size_t alphabetSize) {
    if(DEBUG_MAT) {
        printf("\n---algnMat_setup_size\n");
        printf("capacity: %zu\nefficiency: %zu\nprecalc: %zu\n", alignMtx->cap_nw, alignMtx->cap_eff, alignMtx->cap_pre);
    }
    int    realloc_error_3d = 0;
    size_t cap;
    size_t cap_2d = 0,
           cap_precalcMtx,
           cap_dir;
    //cap_dir     = (len_char1 + 1) * (len_char2 + 1);

    if (len_char3 == 0) {           /* If the size setup is only for 2d */
        cap            = algnMat_size_of_2d_matrix (len_char1, len_char2);
        cap_precalcMtx = (1 << alphabetSize) * len_char1;
        cap_dir        = (len_char1 + 1) * (len_char2 + 1);
    } else {                       /* If the size setup is for 3d */
        cap            = algnMat_size_of_3d_matrix (len_char1, len_char2, len_char3);
        cap_precalcMtx = (1 << alphabetSize) * (1 << alphabetSize) * len_char2;  // TODO: why character 2?
        cap_2d         = len_char1 * len_char2;
        cap_dir        = cap_2d * len_char3;
    }
    if (DEBUG_MAT) {
        printf("cap_eff: %zu, \ncap_nw: %zu\n", alignMtx->cap_eff, cap);
    }
    if (alignMtx->cap_eff < cap) {         /* If the current 2d or 3d matrix is not large enough */
        if (DEBUG_MAT) {
            printf("cap_eff too small. New allocation: %zu\n", cap);
        }
        alignMtx->algn_costMtx3d =
        alignMtx->algn_costMtx =
            realloc (alignMtx->algn_costMtx, (cap * sizeof(int)));
        alignMtx->cap_eff = cap;
        realloc_error_3d = alignMtx->algn_costMtx3d == NULL;
    }
    if (alignMtx->cap_nw < cap_dir) {         /* If the other matrices are not large enough */
        if (DEBUG_MAT) {
            printf("cap nw cost mtx too small. New allocation: %zu\n", cap_dir);
        }
        alignMtx->algn_dirMtx3d =
        alignMtx->algn_dirMtx =
                realloc (alignMtx->algn_dirMtx, cap_dir * sizeof(DIR_MTX_ARROW_t) );

        if (cap_2d) {
            if (DEBUG_MAT) {
                printf("\n3d alignment. cap_2d: %zu\n", cap_2d);
            }
        }
        alignMtx->cap_nw = cap_dir;
    }
    if (alignMtx->cap_pre < cap_precalcMtx) {
        if (DEBUG_MAT) {
            printf("precalc matrix too small. New allocation: %zu\n", cap_precalcMtx);
        }
        alignMtx->algn_precalcMtx = realloc (alignMtx->algn_precalcMtx, cap_precalcMtx * sizeof(int));
        alignMtx->cap_pre         = cap_precalcMtx;
    }
    /* Check if there is an allocation error then abort program */
    if ((cap > 0) && realloc_error_3d) {
        printf("capacity: %zu:\n", cap);
        printf("Memory allocation problem in cost matrix.\n");
        exit(1);
        // failwith ("Memory allocation problem in algn_costMtx3d_d.");
    }
    if ((cap_dir > 0) && (alignMtx->algn_dirMtx == NULL)) {
        printf("Memory allocation problem in direction matrix\n");
        exit(1);
        // failwith ("Memory allocation problem in algn_dirMtx");
    }
    if ((cap_precalcMtx > 0) && (alignMtx->algn_precalcMtx == NULL)) {
        printf("Memory allocation problem in precalc matrix\n");
        exit(1);
        // failwith ("Memory allocation problem in precalcMtx");
    }
    if (DEBUG_MAT) {
        printf("\nFinal allocated size of matrices:\n" );
        printf("    efficiency: %zu\n", alignMtx->cap_eff);
        printf("    nw matrix:  %zu\n", alignMtx->cap_nw);
        printf("    precalcMtx: %zu\n", alignMtx->cap_pre);
    }
}


static inline unsigned int *
algnMtx_get_ptr_to_precalc_3d ( unsigned int *outPrecalcMtx
                              , size_t        char3Len
                              , size_t        alphSize
                              , size_t        char1idx
                              , size_t        char2idx
                              , size_t        char3idx
                              )
{
    alphSize++;
    // TODO: rewrite this to use bitwise algebra.
    return outPrecalcMtx + ((char1idx * (alphSize * char3Len)) + (char3Len * char2idx) + char3idx);
}


unsigned int *
algnMtx_get_precal_row ( unsigned int *p
                       , elem_t        item
                       , size_t        len
                       )
{
    return p + (len * item);
}


unsigned int *
algnMtx_get_row_precalc_3d ( unsigned int *outPrecalcMtx
                           , size_t        char3Len
                           , size_t        alphSize
                           , size_t        char1idx
                           , size_t        char2idx
                           )
{
    return (algnMtx_get_ptr_to_precalc_3d (outPrecalcMtx, char3Len, alphSize, char1idx, char2idx, 0));
}


/** Sets first row of 2d nw matrix, where @param inChar is column headers */
// Pretty sure we don't want the version where there is an 'm' rather than an 'n'
// in the 'align' portion of the name.
/*
void
algmMtx_precalc_4algn_2d (       alignment_matrices_t *alignmentMatrices
                         , const cost_matrices_2d_t   *costMatrix
                         , const dyn_character_t      *inChar) {
    if(DEBUG_MAT) {
        printf("\n---cm_precalc_4algn_2d\n");
    }
    size_t i,
           j,
           charLen = inChar->len;

    int innerIndex,
        valueDatum;

    unsigned int *tmpCost,
                 *precalcMtx = alignmentMatrices->algn_precalcMtx,
                 *charTcm    = costMatrix->cost,
                 *tmpPrecMtx = precalcMtx + charLen,
                 *prepend    = costMatrix->prepend_cost,
                 *tailCosts  = costMatrix->tail_cost;

    elem_t *char_begin = inChar->char_begin;

    if (DEBUG_MAT) {
        printf ("Precalculated transformation cost matrix.\n");
    }

    if (DEBUG_MAT) {
        for (j = 0; j < charLen; j++) {
            printf ("char_begin_t[%zu]: %d\n", j, char_begin[j]), fflush(stdout);
        }
    }

    // We will put the cost of the prepend in the 0th row of the precalc matrix.
    for (j = 0; j < charLen; j++) {

      //printf ("Before innerIndex (j = %d)\n", j), fflush(stdout);
        innerIndex = char_begin[j];
        // printf ("After  innerIndex: {%d}\n", innerIndex), fflush(stdout);

        // printf ("Before valueDatum\n");
        //fflush(stdout);
        valueDatum = prepend[innerIndex];
        //printf ("After  valueDatum\n"), fflush(stdout);

        //printf ("Before Assignment\n"), fflush(stdout);
        precalcMtx[j] = valueDatum;
        //printf ("After  Assignment\n"), fflush(stdout);

        if (DEBUG_COST_M) {
            printf ("%7d", precalcMtx[j]);
            fflush(stdout);
        }
    }
    if (DEBUG_MAT) {
        printf("\n");
        fflush(stdout);
    }
    for (j = 1; j <= costMatrix->alphSize; j++, tmpPrecMtx += charLen) {
        // if (DEBUG_CM) {
        //     printf("%zu\t", j);
        // }
        tmpCost = cm_get_row (charTcm, j, costMatrix->costMatrixDimension);
        // We fill almost the complete row. Only the first (aligning with the
        // gap), is filled using the tail cost
        tmpPrecMtx[0] = tailCosts[j];
        if (DEBUG_MAT) {
            printf ("%7d", tmpPrecMtx[0]);
            fflush(stdout);

        }
        for (i = 1; i < charLen; i++) {
            tmpPrecMtx[i] = tmpCost[char_begin[i]];
            if (DEBUG_MAT) {
                printf ("%7d", tmpPrecMtx[i]);
                fflush(stdout);
            }
        }
        if (DEBUG_MAT) {
            printf ("\n");
        }
    }
    if (DEBUG_MAT) {
        printf ("Finished printing transformation cost matrix\n");
        fflush(stdout);

    }
}
*/

/** Sets first row of nw cost matrix, where @param inChar is column headers */
void
algnMtx_precalc_4algn_2d(       alignment_matrices_t *alignmentMatrices
                        , const cost_matrices_2d_t   *costMatrix
                        , const dyn_character_t      *inChar)
{
    if(DEBUG_MAT) {
        printf("\n---algnMtx_precalc_4algn\n");
    }
    size_t i,
           j,
           charLen = inChar->len;

    unsigned int *tmpCost,
                 *precalcMtx = alignmentMatrices->algn_precalcMtx,
                 *charTcm    = costMatrix->cost,
                 *tmpPrecMtx = precalcMtx + charLen,
                 *prepend    = costMatrix->prepend_cost,
                 *tailCosts  = costMatrix->tail_cost;

    elem_t *char_begin = inChar->char_begin;

    if (DEBUG_MAT) {
        printf ("Precalculated transformation cost matrix.\n");
    }

    if (DEBUG_MAT) {
        for (j = 0; j < charLen; j++) {
            printf ("char_begin_t[%zu]: %d\n", j, char_begin[j]), fflush(stdout);
        }
    }

    // We will put the cost of the prepend in the 0th row of the precalc matrix.
    for (j = 0; j < charLen; j++) {

      //printf ("Before innerIndex (j = %d)\n", j), fflush(stdout);
        int innerIndex = char_begin[j];
        // printf ("After  innerIndex: {%d}\n", innerIndex), fflush(stdout);

        // printf ("Before valueDatum\n");
        //fflush(stdout);
        int valueDatum = prepend[innerIndex];
        //printf ("After  valueDatum\n"), fflush(stdout);

        //printf ("Before Assignment\n"), fflush(stdout);
        precalcMtx[j] = valueDatum;
        //printf ("After  Assignment\n"), fflush(stdout);

        if (DEBUG_COST_M) {
            printf ("%7d", precalcMtx[j]);
            fflush(stdout);
        }
    }
    if (DEBUG_MAT) {
        printf("\n");
        fflush(stdout);
    }
    for (j = 1; j <= costMatrix->alphSize; j++, tmpPrecMtx += charLen) {
        // if (DEBUG_CM) {
        //     printf("%zu\t", j);
        // }
        tmpCost = cm_get_row (charTcm, j, costMatrix->costMatrixDimension);
        /* We fill almost the complete row. Only the first (aligning with the
         * gap), is filled using the tail cost */
        tmpPrecMtx[0] = tailCosts[j];
        if (DEBUG_MAT) {
            printf ("%7d", tmpPrecMtx[0]);
            fflush(stdout);

        }
        for (i = 1; i < charLen; i++) {
            tmpPrecMtx[i] = tmpCost[char_begin[i]];
            if (DEBUG_MAT) {
                printf ("%7d", tmpPrecMtx[i]);
                fflush(stdout);
            }
        }
        if (DEBUG_MAT) {
            printf ("\n");
        }
    }
    if (DEBUG_MAT) {
        printf ("Finished printing transformation cost matrix\n");
        fflush(stdout);

    }
}


void
algnMtx_precalc_4algn_3d(       unsigned int       *outPrecalcMtx
                        , const cost_matrices_3d_t *costMtx
                        , const dyn_character_t    *char3)
{
    size_t char3idx,
           char1idx,
           char2idx,
           char3Len;

    unsigned int *tmp_cost,
                 *tcm,
                 *precalc_ptr;

    elem_t character;

    char3Len = char3->len;
    tcm      = costMtx->cost;

    for (char1idx = 1; char1idx < costMtx->alphSize + 1; char1idx++) {
        for (char2idx = 1; char2idx < costMtx->alphSize + 1; char2idx++) {
            tmp_cost = cm_get_row_3d( tcm
                                    , char1idx
                                    , char2idx
                                    , costMtx->costMatrixDimension
                                    );

            //printf("char1: %d,    char2: %d,    cost: %d\n", char1idx, char2idx, *(tmp_cost+1));
            for (char3idx = 0; char3idx < char3Len; char3idx++) {

                character   = char3->char_begin[char3idx];
                precalc_ptr = algnMtx_get_ptr_to_precalc_3d( outPrecalcMtx
                                                           , char3Len
                                                           , costMtx->alphSize
                                                           , char1idx
                                                           , char2idx
                                                           , char3idx
                                                           );
                *precalc_ptr = tmp_cost[character];
                // printf("char1: %2d,    char2: %2d,    character: %2d,    cost: %2d\n", char1idx, char2idx, character, *(precalc_pos));
            }
        }
    }
}


void
algnMat_print_algn_2d (alignment_matrices_t *alignMtx, size_t w, size_t h) {
    unsigned int *nwCostMatrix = alignMtx->algn_costMtx;

    size_t i, j;

    for (i = 0; i < h; i++) {
        for (j = 0; j < w; j++)
            fprintf (stdout, "%d\t", *(nwCostMatrix + (w * i) + j));
        fprintf (stdout, "\n");
    }
    fprintf (stdout, "\n");
}


void
algnMat_print_algn_3d (alignment_matrices_t *alignmentMatrices, size_t w, size_t h, size_t d) {
    unsigned int *costs;

    size_t i, j, k, pos;

    costs = alignmentMatrices->algn_costMtx3d;
    for (i = 0; i < h; i++) {
        for (j = 0; j < d; j++) {
            for (k = 0; k < w; k++) {
                pos = (i * d * w) + (d * j) + k;
                fprintf (stdout, "%d\t", *(costs + pos));
            }
            fprintf (stdout, "\n");
        }
        fprintf (stdout, "\n");
    }
    fprintf (stdout, "\n");
}
