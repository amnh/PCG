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

void cm_print_2d (cost_matrices_2d_p c) {
    printf("\nCost matrix fields:\n");
    printf("  alphabet size: %d\n", c->alphSize);
    printf("  costMtxDimension:           %d\n", c->costMtxDimension);
    printf("  gap_char:      %d\n", c->gap_char);
    printf("  cost model:    %d\n", c->cost_model_type);
    printf("  combinations:  %d\n", c->combinations);
    printf("  gap open:      %d\n", c->gap_open);
    printf("  is metric:     %d\n", c->is_metric);
    printf("  all elements:  %d\n", c->all_elements);

    printf("\n  Cost matrix:\n    ");
    cm_print_matrix(c->cost, c->alphSize + 1, c->alphSize + 1);
    printf("  Prepend cost\n    ");
    cm_print_matrix(c->prepend_cost, 1, c->alphSize + 1);
    printf("  Worst\n    ");
    cm_print_matrix(c->worst, c->alphSize + 1, c->alphSize + 1);
    printf("  Tail cost:\n    ");
    cm_print_matrix(c->tail_cost, 1, c->alphSize);
    printf("  Median costs:\n    ");
    cm_print_median(c->median, c->alphSize + 1, c->alphSize + 1);
}

void cm_print_3d (cost_matrices_3d_p c) {
    printf("\nCost matrix fields:\n");
    printf("  alphabet size: %d\n", c->alphSize);
    printf("  costMtxDimension:           %d\n", c->costMtxDimension);
    printf("  gap_char:      %d\n", c->gap_char);
    printf("  cost model:    %d\n", c->cost_model_type);
    printf("  combinations:  %d\n", c->combinations);
    printf("  gap open:      %d\n", c->gap_open);
    printf("  all elements:  %d\n", c->all_elements);

    printf("\n  Cost matrix:\n    ");
    cm_print_matrix(c->cost, c->alphSize + 1, c->alphSize + 1);
    printf("  Median costs:\n    ");
    cm_print_median(c->median, c->alphSize + 1, c->alphSize + 1);
}

void
cm_print_matrix (int* m, int w, int h) {
    size_t i, j;
    for (i = 0; i < h; i++) {
        //fprintf(stdout,"%zu: ", i);
        for (j = 0; j < w; j++)
            printf ("%4d", *(m + (w * i) + j));
        printf ("\n    ");
    }
    return;
}

void
cm_print_median (SEQT* m, int w, int h) {
    size_t i, j;
    for (i = 0; i < h; i++) {
        //fprintf(stdout,"%zu: ", i);
        for (j = 0; j < w; j++)
            printf ("%4d", *(m + (w * i) + j));
        printf ("\n    ");
    }
    return;
}

void
cm_free (cost_matrices_2d_p c) {
    free (c->cost);
    free (c->median);
    free (c->worst);
    free (c->prepend_cost);
    free (c->tail_cost);
    free (c);
    return;
}

void
cm_3d_free (cost_matrices_2d_p c) {
    free (c->cost);
    free (c->median);
    free (c);
    return;
}

static inline void
cm_set_alphSize (cost_matrices_2d_p c, int v) {
    assert(c != NULL);
    c->alphSize = v;
    return;
}

static inline void
cm_set_alphSize_3d (cost_matrices_3d_p c, int v) {
    assert(c != NULL);
    c->alphSize = v;
    return;
}

static inline void
cm_set_gap_char (cost_matrices_2d_p c, int v) {
    assert(c != NULL);
    c->gap_char = v;
    return;
}

static inline void
cm_set_gap_char_3d (cost_matrices_3d_p c, int v) {
    assert(c != NULL);
    c->gap_char = v;
    return;
}

static inline void
cm_set_affine (cost_matrices_2d_p c, int do_aff, int go) {
    assert(c != NULL);
    c->cost_model_type = do_aff;
    c->gap_open = go;
    return;
}

static inline void
cm_set_affine_3d (cost_matrices_3d_p c, int do_aff, int go) {
    assert(c != NULL);
    c->cost_model_type = do_aff;
    c->gap_open = go;
    return;
}

int
cm_get_costMtxDimension (cost_matrices_2d_p c) {
    assert(c != NULL);
    return (c->costMtxDimension);
}

inline int
cm_get_costMtxDimension_3d (cost_matrices_3d_p c) {
    assert(c != NULL);
    return (c->costMtxDimension);
}

static inline void
cm_set_costMtxDimension (cost_matrices_2d_p c, int v) {
    assert(c != NULL);
    c->costMtxDimension = v;
    return;
}

static inline void
cm_set_costMtxDimension_3d (cost_matrices_3d_p c, int v) {
    assert(c != NULL);
    c->costMtxDimension = v;
    return;
}

static inline void
cm_set_combinations (cost_matrices_2d_p c) {
    assert(c != NULL);
    c->combinations = 1;
    return;
}

static inline void
cm_set_combinations_3d (cost_matrices_3d_p c) {
    assert(c != NULL);
    c->combinations = 1;
    return;
}

static inline void
cm_unset_combinations (cost_matrices_2d_p c) {
    assert(c != NULL);
    c->combinations = 0;
    return;
}

static inline void
cm_unset_combinations_3d (cost_matrices_3d_p c) {
    assert(c != NULL);
    c->combinations = 0;
    return;
}

inline int
cm_get_combinations (cost_matrices_2d_p c) {
    assert(c != NULL);
    return (c->combinations);
}

inline int
cm_get_combinations_3d (cost_matrices_3d_p c) {
    assert(c != NULL);
    return (c->combinations);
}

void
cm_set_all_elements (cost_matrices_2d_p c, int v) {
    assert (c != NULL);
    c->all_elements = v;
    return;
}

void
cm_set_all_elements_3d (cost_matrices_3d_p c, int v) {
    assert (c != NULL);
    c->all_elements = v;
    return;
}

int
cm_get_all_elements (cost_matrices_2d_p c) {
    return (c->all_elements);
}

int
cm_get_all_elements_3d (cost_matrices_3d_p c) {
    return (c->all_elements);
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
        cm_set_gap_char (res, 1 << (alphSize - 1));
        cm_set_alphSize (res, cm_combinations_of_alphabet (alphSize)); // 2 ^ alphSize - 1 is |power set of alphSize|
        cm_set_costMtxDimension (res, alphSize);
        cm_set_combinations (res);
    } else {
        cm_set_gap_char (res, alphSize);
        cm_set_alphSize (res, alphSize);
        cm_set_costMtxDimension (res, ceil_log_2 (alphSize + 1));
        cm_unset_combinations (res);
    }
    res->all_elements = all_elements;
    res->is_metric    = is_metric;
    cm_set_affine (res, do_aff, gap_open);

    size_t size = 2 * (1 << (res->costMtxDimension)) * (1 << (res->costMtxDimension)) * sizeof(int); // size for cost matrices
    if (size == 0) {
        printf("Your cost matrix is too large to fit in memory. I can't continue with your data loading.\n");
        exit(1);
    }
    res->cost         = calloc (size, 1);
    res->worst        = calloc (size, 1);
    res->prepend_cost = calloc (size, 1);
    res->tail_cost    = calloc (size, 1);

    size = 2 * (1 << (res->costMtxDimension)) * (1 << (res->costMtxDimension)) * sizeof(SEQT); // size for median matrix
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
cm_alloc_set_costs_3d (int alphSize, int combinations, int do_aff, int gap_open,
                       int all_elements, cost_matrices_3d_p res) {
    int size;
    if (DEBUG_CM) {
        printf ("Allocating a three dimensional matrix:\n");
        printf ("alphabet size: %d \n", alphSize);
        printf ("combinations:  %d \n", combinations);
        printf ("cost model:    %d \n", do_aff);
        printf ("gap open cost: %d \n", gap_open);
    }
    if (combinations != 0) {
        cm_set_gap_char_3d (res, 1 << (alphSize - 1));
        cm_set_alphSize_3d (res, cm_combinations_of_alphabet (alphSize)); // 2 ^ alphSize - 1 is |power set of alphSize|
        cm_set_costMtxDimension_3d (res, alphSize);
        cm_set_combinations_3d (res);
    } else {
        cm_set_gap_char_3d (res, alphSize);
        cm_set_alphSize_3d (res, alphSize);
        cm_set_costMtxDimension_3d (res, ceil_log_2 (alphSize + 1));
        cm_unset_combinations_3d (res);
    }
    cm_set_all_elements_3d (res, all_elements);
    cm_set_affine_3d (res, do_aff, gap_open);
    size = (1 << (res->costMtxDimension + 1)) * (1 << (res->costMtxDimension + 1)) * (1 << (res->costMtxDimension + 1));
    res->cost = (int *) calloc (size * sizeof(int), 1);
    res->median = (SEQT *) calloc (size * sizeof(SEQT), 1);
    if ((res->cost == NULL) || (res->median == NULL)) {
        free (res->cost);
        free (res->median);
        printf("Memory error during cost matrix allocation.\n");
        exit(1);
    }
    return;
}

inline int
cm_get_alphabet_size (cost_matrices_2d_p c) {
    assert(c != NULL);
    return c->alphSize;
}

inline int
cm_get_alphabet_size_3d (cost_matrices_3d_p c) {
    assert(c != NULL);
    return c->alphSize;
}

SEQT
cm_get_gap_char_2d (const cost_matrices_2d_p c) {
    assert(c != NULL);
    return c->gap_char;
}

SEQT
cm_get_gap_char_3d (const cost_matrices_3d_p c) {
    assert(c != NULL);
    return c->gap_char;
}

int
cm_get_affine_flag (cost_matrices_2d_p c) {
    assert(c != NULL);
    return c->cost_model_type;
}

int
cm_get_affine_flag_3d (cost_matrices_3d_p c) {
    assert(c != NULL);
    return c->cost_model_type;
}

int
cm_get_gap_opening_parameter (cost_matrices_2d_p c) {
    assert(c != NULL);
    return c->gap_open;
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

inline int
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

inline SEQT
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

inline int
cm_calc_tmm (int *tmm, int a, int b, int alphSize) {
    return (cm_calc_cost (tmm, a, b, alphSize));
}

inline int
cm_calc_median_position (SEQT a, SEQT b, int alphSize) {
    return (cm_calc_cost_position (a, b, alphSize));
}

/*
 * Position of the first memory location of the transformation cost matrix given
 * a bigarray from ocaml.
 */
inline int *
cm_get_transformation_cost_matrix (const cost_matrices_2d_p a) {
    return (a->cost);
}

int *
cm_get_tail_cost (const cost_matrices_2d_p a) {
    return (a->tail_cost);
}

static inline int *
cm_get_prepend_cost (const cost_matrices_2d_p a) {
    return (a->prepend_cost);
}

static inline int *
cm_get_transformation_cost_matrix_3d (const cost_matrices_3d_p a) {
    return (a->cost);
}

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
cm_get_row_3d (int *tcm, SEQT a, SEQT b, int alphSize) {
    if (alphSize <= 0) {
        printf("Alphabet size = 4\n");
        exit(1);
    }
    if ((1 << alphSize) <= a) {
        printf("%u is bigger than alphabet size\n", a);
        exit(1);
    }
    if ((1 << alphSize) <= b) {
        printf("%u is bigger than alphabet size\n", b);
        exit(1);
    }
    return (tcm + (((a << alphSize) + b) << alphSize));
}

void
cm_set_value_2d_seq_p (SEQT a, SEQT b, SEQT v, SEQT *p, int alphSize) {
    *(p + (cm_calc_cost_position_seq_p (a, b, alphSize))) = v;
    return;
}

void
cm_set_value (int a, int b, int v, int *p, int alphSize) {
    *(p + (cm_calc_cost_position (a, b, alphSize))) = v;
    return;
}

int
cm_get_value (int a, int b, int *p, int alphSize) {
    return *(p + (cm_calc_cost_position (a, b, alphSize)));
}

/** Sets first row of nw cost matrix, where @param seq is column headers */
void
cm_precalc_4algn (const cost_matrices_2d_p costMtx_t, nw_matrices_p nwMtxs, const seq_p seq) {
    if(DEBUG_MAT) {
        printf("\n---cm_precalc_4algn\n");
    }
    size_t i, j, seqLen;
    int *tmpCost_t, *seqTcm_t, *tmpPrecMtx_t, *prepend_t, *tailCosts_t, *precalcMtx_t;
    SEQT *seq_begin_t;

    seqLen       = seq_get_len (seq);
    precalcMtx_t = mat_get_2d_prec (nwMtxs);
    seqTcm_t     = cm_get_transformation_cost_matrix (costMtx_t);
    prepend_t    = cm_get_prepend_cost (costMtx_t);
    tailCosts_t  = cm_get_tail_cost (costMtx_t);
    tmpPrecMtx_t = precalcMtx_t + seqLen;
    seq_begin_t  = seq_get_seq_begin (seq);         // Inlined seq_get for speed purposes
    if (DEBUG_MAT) {
        printf ("Precalculated transformation cost matrix.\n");
    }

    /*
    printf ("sequence length: %d\n", seqLen);
    fflush(stdout);
    for (j = 0; j <= seqLen; j++) {
      printf (" %d,", seq_begin_t[j]);
    }
    printf ("\n");
    fflush(stdout);
    */

    // We will put the cost of the prepend in the 0th row of the precalc matrix.
    for (j = 0; j < seqLen; j++) {

        //printf ("Before innerIndex\n"), fflush(stdout);
	int innerIndex = seq_begin_t[j];
        //printf ("After  innerIndex: {%d}\n", innerIndex), fflush(stdout);

        //printf ("Before valueDatum\n"), fflush(stdout);
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
    for (j = 1; j <= costMtx_t->alphSize; j++, tmpPrecMtx_t += seqLen) {
        // if (DEBUG_CM) {
        //     printf("%zu\t", j);
        // }
        tmpCost_t = cm_get_row (seqTcm_t, j, costMtx_t->costMtxDimension);
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
    return;
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
    int seq3idx, seq1idx, seq2idx, seq3Len, *tmp_cost, *tcm;
    int sequence, *precalc_pos;
    seq3Len = seq_get_len (seq3);
    tcm    = cm_get_transformation_cost_matrix_3d (costMtx);
    for (seq1idx = 1; seq1idx < costMtx->alphSize + 1; seq1idx++)
        for (seq2idx = 1; seq2idx < costMtx->alphSize + 1; seq2idx++) {
            tmp_cost = cm_get_row_3d (tcm, seq1idx, seq2idx, costMtx->costMtxDimension);
            //printf("seq1: %d,    seq2: %d,    cost: %d\n", seq1idx, seq2idx, *(tmp_cost+1));
            for (seq3idx = 0; seq3idx < seq3Len; seq3idx++) {
                sequence     = seq_get_element (seq3, seq3idx);
                precalc_pos  = (int *) cm_get_pos_in_precalc (outPrecalcMtx, seq3Len, costMtx->alphSize,
                                                              seq1idx, seq2idx, seq3idx);
                *precalc_pos = *(tmp_cost + sequence);
                // printf("seq1: %2d,    seq2: %2d,    sequence: %2d,    cost: %2d\n", seq1idx, seq2idx, sequence, *(precalc_pos));
            }
        }
    return;
}

void
cm_set_value_3d_seq_p (SEQT a, SEQT b, SEQT c, SEQT v, SEQT *p, int alphSize) {
    *(p + (cm_calc_cost_position_3d_seq_p (a, b, c, alphSize))) = v;
    return;
}

void
cm_set_value_3d (int a, int b, int c, int v, int *p, int alphSize) {
    *(p + (cm_calc_cost_position_3d (a, b, c, alphSize))) = v;
    return;
}

 void
cm_set_cost_2d (int a, int b, int v, cost_matrices_2d_p c) {
    cm_set_value (a, b, v, c->cost, c->costMtxDimension);
    return;
}

int
cm_get_cost (int a, int b, cost_matrices_2d_p c) {
    return cm_get_value (a, b, c->cost, c->costMtxDimension);
}

// TODO: This seems never to be used
void
cm_set_worst (int a, int b, int v, cost_matrices_2d_p c) {
    cm_set_value (a, b, v, c->worst, c->costMtxDimension);
    return;
}

void
cm_set_cost_3d (int a, int b, int cp, int v, cost_matrices_3d_p c) {
    cm_set_value_3d (a, b, cp, v, c->cost, c->costMtxDimension);
    return;
}

void
cm_set_prepend_2d (int a, int b, cost_matrices_2d_p c) {
    c->prepend_cost[a] = b;
    return;
}

void
cm_set_tail_2d (int a, int b, cost_matrices_2d_p c) {
    c->tail_cost[a] = b;
    return;
}

void
cm_set_median_2d (SEQT a, SEQT b, SEQT v, cost_matrices_2d_p c) {
    cm_set_value_2d_seq_p (a, b, v, c->median, c->costMtxDimension);
    return;
}

void
cm_set_median_3d (SEQT a, SEQT b, SEQT cp, SEQT v, cost_matrices_3d_p c) {
    cm_set_value_3d_seq_p (a, b, cp, v, c->median, c->costMtxDimension);
    return;
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
        len_g = 2 * (1 << (a->costMtxDimension)) * (1 << (a->costMtxDimension));
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
    return;
}


void
cm_copy_contents_seq_p (SEQT *src, SEQT *tgt, int len) {
    int i;
    for (i = 0; i < len; i++)
        *(tgt + i) = *(src + i);
    return;
}

SEQT
cm_get_median (const cost_matrices_2d_p tmp, SEQT a, SEQT b) {
    return (cm_calc_median((tmp->median), a, b, tmp->costMtxDimension));
}

SEQT
cm_get_median_3d (const cost_matrices_3d_p t, SEQT a, SEQT b, SEQT c) {
    return (cm_calc_median_3d((t->median), a, b, c, t->costMtxDimension));
}
