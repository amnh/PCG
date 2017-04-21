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

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "costMatrix.h"

static inline int
ceil_log_2 (int v) {
    int i = 0;
    while (v != 0) {
        i++;
        v = v >> 1;
    }
    return (i + 1);
}

static inline int
cm_combinations_of_alphabet (const int alphSize) {
    assert (alphSize >= 0);
    return ((1 << alphSize) - 1); // ignore empty set
}

void cm_print_2d (cost_matrices_2d_p costMatrix) {
    printf("\nCost matrix fields:\n");
    printf("  alphabet size: %d\n", costMatrix->alphSize);
    printf("  costMatrixDimension:           %zu\n", costMatrix->costMatrixDimension);
    printf("  gap_char:      %d\n", costMatrix->gap_char);
    printf("  cost model:    %d\n", costMatrix->cost_model_type);
    printf("  combinations:  %d\n", costMatrix->combinations);
    printf("  gap open:      %d\n", costMatrix->gap_open);
    printf("  is metric:     %d\n", costMatrix->is_metric);
    printf("  all elements:  %d\n", costMatrix->all_elements);

    printf("\n  Cost matrix:\n    ");
    cm_print_matrix(costMatrix->cost,         costMatrix->alphSize + 1, costMatrix->alphSize + 1);
    printf("  Prepend cost\n    ");
    cm_print_matrix(costMatrix->prepend_cost, 1,               costMatrix->alphSize + 1);
 //   printf("  Worst\n    ");
 //   cm_print_matrix(c->worst,        costMatrix->alphSize + 1, costMatrix->alphSize + 1);
    printf("  Tail cost:\n    ");
    cm_print_matrix(costMatrix->tail_cost,    1,               costMatrix->alphSize);
    printf("  Median costs:\n    ");
    cm_print_median(costMatrix->median,        costMatrix->alphSize + 1, costMatrix->alphSize + 1);
}

void cm_print_3d (cost_matrices_3d_p costMatrix) {
    printf("\nCost matrix fields:\n");
    printf("  alphabet size: %zu\n",                 costMatrix->alphSize);
    printf("  costMatrixDimension:           %zu\n", costMatrix->costMatrixDimension);
    printf("  gap_char:      %u\n",                  costMatrix->gap_char);
    printf("  cost model:    %d\n",                  costMatrix->cost_model_type);
    printf("  combinations:  %d\n",                  costMatrix->combinations);
    printf("  gap open:      %d\n",                  costMatrix->gap_open);
    printf("  all elements:  %d\n",                  costMatrix->all_elements);

    printf("\n  Cost matrix:\n    ");
    cm_print_matrix(costMatrix->cost,   costMatrix->alphSize + 1, costMatrix->alphSize + 1);
    printf("  Median costs:\n    ");
    cm_print_median(costMatrix->median, costMatrix->alphSize + 1, costMatrix->alphSize + 1);
}

void
cm_print_matrix (int *costMatrix, size_t w, size_t h) {
    size_t i, j;
    for (i = 0; i < h; i++) {
        //fprintf(stdout,"%zu: ", i);
        for (j = 0; j < w; j++)
            printf ("%4d", *(costMatrix + (w * i) + j));
        printf ("\n    ");
    }
}

void
cm_print_median (SEQT *costMatrix, size_t w, size_t h) {
    size_t i, j;
    for (i = 0; i < h; i++) {
        //fprintf(stdout,"%zu: ", i);
        for (j = 0; j < w; j++)
            printf ("%d", *(costMatrix + (w * i) + j));
        printf ("\n    ");
    }
}

void
cm_free (cost_matrices_2d_p costMatrix) {
    free (costMatrix->cost);
    free (costMatrix->median);
    free (costMatrix->worst);
    free (costMatrix->prepend_cost);
    free (costMatrix->tail_cost);
    free (costMatrix);
}

void
cm_3d_free (cost_matrices_2d_p costMatrix) {
    free (costMatrix->cost);
    free (costMatrix->median);
    free (costMatrix);
}

static inline void
cm_set_affine (cost_matrices_2d_p costMatrix, int do_aff, int gapOpenCost) {
    assert(costMatrix != NULL);
    costMatrix->cost_model_type = do_aff;
    costMatrix->gap_open        = gapOpenCost;
}

static inline void
cm_set_affine_3d (cost_matrices_3d_p costMatrix, int do_aff, int gapOpenCost) {
    assert(costMatrix != NULL);
    costMatrix->cost_model_type = do_aff;
    costMatrix->gap_open        = gapOpenCost;
}

/*
 * Creates a cost matrix with memory allocated for an alphabet of size alphSize
 * (not including the gap representation which is internally chosen), and whose
 * size must consider all possible combinations of characters in the alphabeet
 * iff combinations != 0. Set the affine gap model parameters to the values
 * stored in do_aff, gap_open, in the cost matrix res.
 * In case of error the function fails with the message "Memory error.".
 */
void
cm_alloc_set_costs_2d ( int alphSize
                      , int combinations
                      , int do_aff
                      , int gap_open
                      , int is_metric
                      , int all_elements
                      , cost_matrices_2d_p res)
{
    if(DEBUG_CM) {
        printf("\n---cm_alloc_set_costs_2d\n");
    }
#ifndef USE_LARGE_ALPHABETS
    if (alphSize > 255) {
        // TODO: update this error message from POY
        printf("Apparently you are analyzing large alphabets. This version \
                of PCG was compiled without the --enable-large-alphabets option. \
                To run this analysis you need to enable that option at compile time. \
                Either recompile the program yourself or request a version suited \
                for your needs in the POY mailing list (poy4@googlegroups.com). Thanks!");
        exit(1);
    }
#endif
    if (combinations) {
        res->gap_char = 1 << (alphSize - 1);
        res->alphSize = cm_combinations_of_alphabet (alphSize); // 2 ^ alphSize - 1 is |power set of alphSize|
        res->costMatrixDimension = alphSize;
        res->combinations = 1;
    } else {
        res->gap_char = alphSize;
        res->alphSize = alphSize;
        res->costMatrixDimension = ceil_log_2 (alphSize + 1);
        res->combinations = 0;
    }
    res->all_elements = all_elements;
    res->is_metric    = is_metric;
    cm_set_affine (res, do_aff, gap_open);

    size_t size = 2
                * (1 << (res->costMatrixDimension))
                * (1 << (res->costMatrixDimension))
                * sizeof(int); // size for cost matrices

    if (size == 0) {
        printf("Your cost matrix is too large to fit in memory. I can't continue with your data loading.\n");
        exit(1);
    }
    res->cost         = calloc (size, 1);
    res->worst        = calloc (size, 1);
    res->prepend_cost = calloc (size, 1);
    res->tail_cost    = calloc (size, 1);

    size = 2
         * (1 << (res->costMatrixDimension))
         * (1 << (res->costMatrixDimension))
         * sizeof(SEQT); // size for median matrix

    if (size == 0) {
        printf("Your cost matrix is too large to fit in your memory. I can't continue with your data loading.\n");
        exit(1);
    }
    res->median = (SEQT *) calloc (size, 1);

    if ((res->cost == NULL) || (res->median == NULL)) {
        free (res->cost);
        free (res->median);
        printf("Memory error during cost matrix allocation.\n");
        exit(1);
    }

    //printf("cm_get%d\n", );
}

/*
 * Creates a cost matrix with memory allocated for an alphabet of size alphSize
 * (not including the gap representation which is internally chosen), and whose
 * size must consider all possible combinations of characters in the alphabeet
 * iff combinations != 0. Set the affine gap model paramters to the values
 * stored in do_aff, gap_open, in the cost matrix res.
 * In case of error the function fails with the message "Memory error.".
 */
void
cm_alloc_set_costs_3d ( int alphSize
                      , int combinations
                      , int do_aff
                      , int gap_open
                      , int all_elements
                      , cost_matrices_3d_p res
                      )
{
    int size;
    if (DEBUG_CM) {
        printf ("Allocating a three dimensional matrix:\n");
        printf ("alphabet size: %d \n", alphSize);
        printf ("combinations:  %d \n", combinations);
        printf ("cost model:    %d \n", do_aff);
        printf ("gap open cost: %d \n", gap_open);
    }
    // TODO: check the following code. Does combinations need to be reset? Are the dimensions set right?
    if (combinations != 0) {
        res->gap_char            = 1 << (alphSize - 1);
        res->alphSize            = cm_combinations_of_alphabet (alphSize); // 2 ^ alphSize - 1 is |power set of alphSize|
        res->costMatrixDimension = alphSize;
        res->combinations = 1;
    } else {
        res->gap_char            = alphSize;
        res->alphSize            = alphSize;
        res->costMatrixDimension = ceil_log_2 (alphSize + 1);
        res->combinations = 0;
    }

    res->all_elements = all_elements;
    cm_set_affine_3d (res, do_aff, gap_open);
    size              = (1 << (res->costMatrixDimension + 1))
                      * (1 << (res->costMatrixDimension + 1))
                      * (1 << (res->costMatrixDimension + 1));

    res->cost         = calloc (size * sizeof(int),  1);
    res->median       = calloc (size * sizeof(SEQT), 1);

    if ((res->cost == NULL) || (res->median == NULL)) {
        free (res->cost);
        free (res->median);
        printf("Memory error during cost matrix allocation.\n");
        exit(1);
    }
}


inline int
cm_get_gap_opening_parameter_3d (const cost_matrices_3d_p c) {
    assert(c != NULL);
    return c->gap_open;
}

int
cm_calc_cost_position (int a, int b, int alphSize) {
    assert(alphSize >= 0);
    return ((a << alphSize) + b);
}

static inline int
cm_calc_cost_position_seq_p (SEQT a, SEQT b, int alphSize) {
    assert(alphSize >= 0);
    return ((((int) a) << alphSize) + ((int) b));
}

int
cm_calc_cost_position_3d_seq_p (SEQT a, SEQT b, SEQT c, int alphSize) {
    assert(alphSize >= 0);
    return ((((((int) a) << alphSize) + ((int) b)) << alphSize) + ((int) c));
}

static inline int
cm_calc_cost_position_3d (int a, int b, int c, int alphSize) {
    assert(alphSize >= 0);
    return (((a << alphSize) + b) << alphSize) + c;
}

static inline SEQT
cm_calc_median (SEQT *tcm, SEQT a, SEQT b, int alphSize) {
    SEQT *res;
    assert (alphSize >= 0);
    assert ((1 << alphSize) > a);
    assert ((1 << alphSize) > b);
    res = tcm + cm_calc_cost_position_seq_p (a, b, alphSize);
    return (*res);
}

int
cm_calc_cost (int *tcm, SEQT a, SEQT b, int alphSize) {
    int *res;
    /*
    printf ("(1 << alphSize) = %d\n", (1 << alphSize));
    printf ("a = %d\n", a);
    printf ("b = %d\n", b);
    fflush(stdout);
    */
    assert (alphSize >= 0);
    assert ((1 << alphSize) > a);
    assert ((1 << alphSize) > b);
    res = tcm + cm_calc_cost_position_seq_p (a, b, alphSize);
    return (*res);
}

static inline SEQT
cm_calc_median_3d (SEQT *tcm, SEQT a, SEQT b, SEQT c, int alphSize) {
    if (alphSize <= 0) {
        printf("Alphabet size = 2");
        exit(1);
    }
    if ((1 << alphSize) <= a) {
        printf("2a is bigger than alphabet size\n");
        exit(1);
    }
    if ((1 << alphSize) <= b) {
        printf("b is bigger than alphabet size\n");
        exit(1);
    }
    return (*(tcm + cm_calc_cost_position_3d (a, b, c, alphSize)));
}

/* TODO: dead code?
static inline int
cm_calc_cost_3d (int *tcm, SEQT a, SEQT b, SEQT c, int alphSize) {
    if (alphSize <= 0) {
        printf("Alphabet size = 2\n");
        exit(1);
    }
    if ((1 << alphSize) <= a) {
        printf("2a is bigger than alphabet size\n");
        exit(1);
    }
    if ((1 << alphSize) <= b) {
        printf("b is bigger than alphabet size\n");
        exit(1);
    }
    return (*(tcm + cm_calc_cost_position_3d (a, b, c, alphSize)));
}

static inline SEQT
cm_calc_cost_3d_seq_p (SEQT *tcm, SEQT a, SEQT b, SEQT c, int alphSize) {
    if (alphSize <= 0) {
        printf("Alphabet size = 2\n");
        exit(1);
    }
    if ((1 << alphSize) <= a) {
        printf("2a is bigger than alphabet size\n");
        exit(1);
    }
    if ((1 << alphSize) <= b) {
        printf("b is bigger than alphabet size\n");
        exit(1);
    }
    return (*(tcm + cm_calc_cost_position_3d (a, b, c, alphSize)));
}


static inline int
cm_calc_tmm (int *tmm, int a, int b, int alphSize) {
    return (cm_calc_cost (tmm, a, b, alphSize));
}

inline int
cm_calc_median_position (SEQT a, SEQT b, int alphSize) {
    return (cm_calc_cost_position (a, b, alphSize));
}
*/

static inline int *
cm_get_row (int *tcm, SEQT a, int alphSize) {
    if (alphSize <= 0) {
        printf("Alphabet size = 3\n");
        exit(1);
    }
    if ((1 << alphSize) <= a) {
        printf("3a is bigger than alphabet size\n");
        exit(1);
    }
    return (tcm + (a << alphSize));
}

static inline int *
cm_get_row_3d (int *tcm, SEQT seq1, SEQT seq2, int alphSize) {
    if (alphSize <= 0) {
        printf("Alphabet size = 4\n");
        exit(1);
    }
    if ((1 << alphSize) <= seq1) {
        printf("%u is bigger than alphabet size\n", seq1);
        exit(1);
    }
    if ((1 << alphSize) <= seq2) {
        printf("%u is bigger than alphabet size\n", seq2);
        exit(1);
    }
    return (tcm + (((seq1 << alphSize) + seq2) << alphSize));
}

void
cm_set_value_2d_seq_p (SEQT a, SEQT b, SEQT v, SEQT *p, int alphSize) {
    *(p + (cm_calc_cost_position_seq_p (a, b, alphSize))) = v;
}

void
cm_set_value (int a, int b, int v, int *p, int alphSize) {
    *(p + (cm_calc_cost_position (a, b, alphSize))) = v;
}

int
cm_get_value (int a, int b, int *p, int alphSize) {
    return *(p + (cm_calc_cost_position (a, b, alphSize)));
}

/** Sets first row of nw cost matrix, where @param seq is column headers */
void
cm_precalc_4algn (const cost_matrices_2d_p costMatrix, nw_matrices_p alignmentMatrices, const seq_p seq) {
    if(DEBUG_MAT) {
        printf("\n---cm_precalc_4algn\n");
    }
    size_t i, j,
           seqLen = seq->len;

    int *tmpCost_t,
        *precalcMtx_t = alignmentMatrices->precalcMtx,
        *seqTcm_t     = costMatrix->cost,
        *tmpPrecMtx_t = precalcMtx_t + seqLen,
        *prepend_t    = costMatrix->prepend_cost,
        *tailCosts_t  = costMatrix->tail_cost;

    SEQT *seq_begin_t = seq->seq_begin;

    if (DEBUG_MAT) {
        printf ("Precalculated transformation cost matrix.\n");
    }

    if (DEBUG_MAT) {
        for (j = 0; j < seqLen; j++) {
            printf ("seq_begin_t[%zu]: %d\n", j, seq_begin_t[j]), fflush(stdout);
        }
    }

    // We will put the cost of the prepend in the 0th row of the precalc matrix.
    for (j = 1; j < seqLen; j++) {

      //printf ("Before innerIndex (j = %d)\n", j), fflush(stdout);
        int innerIndex = seq_begin_t[j];
        // printf ("After  innerIndex: {%d}\n", innerIndex), fflush(stdout);

        // printf ("Before valueDatum\n");
        //fflush(stdout);
        int valueDatum = prepend_t[innerIndex];
        //printf ("After  valueDatum\n"), fflush(stdout);

        //printf ("Before Assignment\n"), fflush(stdout);
        precalcMtx_t[j] = valueDatum;
        //printf ("After  Assignment\n"), fflush(stdout);

        if (DEBUG_CM) {
            printf ("%7d", precalcMtx_t[j]);
            fflush(stdout);
        }
    }
    if (DEBUG_MAT) {
        printf("\n");
        fflush(stdout);
    }
    for (j = 1; j <= costMatrix->alphSize; j++, tmpPrecMtx_t += seqLen) {
        // if (DEBUG_CM) {
        //     printf("%zu\t", j);
        // }
        tmpCost_t = cm_get_row (seqTcm_t, j, costMatrix->costMatrixDimension);
        /* We fill almost the complete row. Only the first (aligning with the
         * gap), is filled using the tail cost */
        tmpPrecMtx_t[0] = tailCosts_t[j];
        if (DEBUG_MAT) {
            printf ("%7d", tmpPrecMtx_t[0]);
            fflush(stdout);

        }
        for (i = 1; i < seqLen; i++) {
            tmpPrecMtx_t[i] = tmpCost_t[seq_begin_t[i]];
            if (DEBUG_MAT) {
                printf ("%7d", tmpPrecMtx_t[i]);
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

const int *
cm_get_precal_row (const int *p, SEQT item, int len) {
    return (p + (len * item));
}

static inline int *
cm_get_pos_in_precalc (const int *outPrecalcMtx, int seq3Len, int alphSize,
                       int seq1idx, int seq2idx, int seq3idx) {
    int *result;
    alphSize++;
    result = (int *) outPrecalcMtx + ((seq1idx * (alphSize * seq3Len)) + (seq3Len * seq2idx) + seq3idx);
    return (result);
}

int *
cm_get_row_precalc_3d (const int *outPrecalcMtx, int seq3Len, int alphSize, int seq1idx, int seq2idx) {
    return (cm_get_pos_in_precalc (outPrecalcMtx, seq3Len, alphSize, seq1idx, seq2idx, 0));
}

void
cm_precalc_4algn_3d (const cost_matrices_3d_p costMtx, int *outPrecalcMtx, const seq_p seq3) {
    size_t seq3idx,
           seq1idx,
           seq2idx,
           seq3Len;

    int *tmp_cost,
        *tcm,
         sequence,
        *precalc_pos;

    seq3Len = seq3->len;
    tcm     = costMtx->cost;
    for (seq1idx = 1; seq1idx < costMtx->alphSize + 1; seq1idx++) {
        for (seq2idx = 1; seq2idx < costMtx->alphSize + 1; seq2idx++) {
            tmp_cost = cm_get_row_3d ( tcm
                                     , seq1idx
                                     , seq2idx
                                     , costMtx->costMatrixDimension
                                     );

            //printf("seq1: %d,    seq2: %d,    cost: %d\n", seq1idx, seq2idx, *(tmp_cost+1));
            for (seq3idx = 0; seq3idx < seq3Len; seq3idx++) {
                sequence     = seq3->seq_begin[seq3idx];

                precalc_pos  = (int *) cm_get_pos_in_precalc ( outPrecalcMtx
                                                             , seq3Len
                                                             , costMtx->alphSize
                                                             , seq1idx
                                                             , seq2idx
                                                             , seq3idx
                                                             );
                *precalc_pos = *(tmp_cost + sequence);
                // printf("seq1: %2d,    seq2: %2d,    sequence: %2d,    cost: %2d\n", seq1idx, seq2idx, sequence, *(precalc_pos));
            }
        }
    }
}

void
cm_set_value_3d_seq_p (SEQT a, SEQT b, SEQT c, SEQT v, SEQT *p, int alphSize) {
    *(p + (cm_calc_cost_position_3d_seq_p (a, b, c, alphSize))) = v;
}

void
cm_set_value_3d (int a, int b, int c, int v, int *p, int alphSize) {
    *(p + (cm_calc_cost_position_3d (a, b, c, alphSize))) = v;
}

void
cm_set_cost_2d (int a, int b, int v, cost_matrices_2d_p c) {
    cm_set_value (a, b, v, c->cost, c->costMatrixDimension);
}

int
cm_get_cost (int a, int b, cost_matrices_2d_p c) {
    return cm_get_value (a, b, c->cost, c->costMatrixDimension);
}

// TODO: This seems never to be used
void
cm_set_worst (int a, int b, int v, cost_matrices_2d_p c) {
    cm_set_value (a, b, v, c->worst, c->costMatrixDimension);
}

void
cm_set_cost_3d (int a, int b, int cp, int v, cost_matrices_3d_p c) {
    cm_set_value_3d (a, b, cp, v, c->cost, c->costMatrixDimension);
}

void
cm_set_prepend_2d (int a, int b, cost_matrices_2d_p c) {
    c->prepend_cost[a] = b;
}

void
cm_set_tail_2d (int a, int b, cost_matrices_2d_p c) {
    c->tail_cost[a] = b;
}

void
cm_set_median_2d (SEQT a, SEQT b, SEQT v, cost_matrices_2d_p c) {
    cm_set_value_2d_seq_p (a, b, v, c->median, c->costMatrixDimension);
}

void
cm_set_median_3d (SEQT a, SEQT b, SEQT cp, SEQT v, cost_matrices_3d_p c) {
    cm_set_value_3d_seq_p (a, b, cp, v, c->median, c->costMatrixDimension);
}

int
cm_compare (cost_matrices_2d_p a, cost_matrices_2d_p b) {
    int cmp, len_g;
    size_t len, len1;
    if (a->alphSize != b->alphSize) {
        return (a->alphSize - b->alphSize);
    }
    else if (a->combinations != b->combinations) {
        return (a->combinations - b->combinations);
    }
    else if (a->cost_model_type != b->cost_model_type) {
        return (a->cost_model_type - b->cost_model_type);
    }
    else if (a->gap_open != b->gap_open) {
        return (a->gap_open - b->gap_open);
    }
    else if (a->is_metric != b->is_metric) {
        return (a->is_metric - b->is_metric);
    }
    else {
        len_g = 2 * (1 << (a->costMatrixDimension)) * (1 << (a->costMatrixDimension));
        len   = len_g * sizeof(int);
        len1  = len_g * sizeof(SEQT);
        cmp   = memcmp (a->cost, b->cost, len);
        if (cmp != 0) return (cmp);
        cmp   = memcmp (a->median, b->median, len1);
        if (cmp != 0) return (cmp);
        cmp   = memcmp (a->worst, b->worst, len);
        return (cmp);
    }
}

void
cm_copy_contents (int *src, int *tgt, int len) {
    int i;
    for (i = 0; i < len; i++)
        *(tgt + i) = *(src + i);
}


void
cm_copy_contents_seq_p (SEQT *src, SEQT *tgt, int len) {
    int i;
    for (i = 0; i < len; i++)
        *(tgt + i) = *(src + i);
}

SEQT
cm_get_median (const cost_matrices_2d_p tmp, SEQT a, SEQT b) {
    return (cm_calc_median((tmp->median), a, b, tmp->costMatrixDimension));
}

SEQT
cm_get_median_3d (const cost_matrices_3d_p t, SEQT a, SEQT b, SEQT c) {
    return (cm_calc_median_3d((t->median), a, b, c, t->costMatrixDimension));
}
