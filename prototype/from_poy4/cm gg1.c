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
    printf("  lcm:           %d\n", c->lcm);
    printf("  gap:           %d\n", c->gap);
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
    printf("  lcm:           %d\n", c->lcm);
    printf("  gap:           %d\n", c->gap);
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
cm_set_gap (cost_matrices_2d_p c, int v) {
    assert(c != NULL);
    c->gap = v;
    return;
}

static inline void
cm_set_gap_3d (cost_matrices_3d_p c, int v) {
    assert(c != NULL);
    c->gap = v;
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
cm_get_lcm (cost_matrices_2d_p c) {
    assert(c != NULL);
    return (c->lcm);
}

inline int
cm_get_lcm_3d (cost_matrices_3d_p c) {
    assert(c != NULL);
    return (c->lcm);
}

static inline void
cm_set_lcm (cost_matrices_2d_p c, int v) {
    assert(c != NULL);
    c->lcm = v;
    return;
}

static inline void
cm_set_lcm_3d (cost_matrices_3d_p c, int v) {
    assert(c != NULL);
    c->lcm = v;
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
value
cm_CAML_set_all_elements (value cm, value v) {
    CAMLparam2(cm, v);
    cost_matrices_2d_p c;
    int i;
    c = Cost_matrix_struct(cm);
    i = Int_val(v);
    cm_set_all_elements (c, i);
    CAMLreturn(Val_unit);
}

value
cm_CAML_set_all_elements_3d (value cm, value v) {
    CAMLparam2(cm, v);
    cost_matrices_3d_p c;
    int i;
    c = Cost_matrix_struct_3d(cm);
    i = Int_val(v);
    cm_set_all_elements_3d (c, i);
    CAMLreturn(Val_unit);
}

value
cm_CAML_get_all_elements_3d (value cm) {
    CAMLparam1(cm);
    cost_matrices_3d_p c;
    c = Cost_matrix_struct_3d(cm);
    CAMLreturn(Val_int(cm_get_all_elements_3d(c)));
}

value
cm_CAML_get_all_elements (value cm) {
    CAMLparam1(cm);
    cost_matrices_2d_p c;
    c = Cost_matrix_struct(cm);
    CAMLreturn(Val_int(cm_get_all_elements(c)));
}
*/

/*
 * Creates a cost matrix with memory allocated for an alphabet of size alphSize
 * (not including the gap representation which is internally chosen), and whose
 * size must consider all possible combinations of characters in the alphabeet
 * iff combinations != 0. Set the affine gap model parameters to the values
 * stored in do_aff, gap_open, in the cost matrix res.
 * In case of error the function fails with the message "Memory error.".
 */
void
cm_alloc_set_costs_2d (int alphSize, int combinations, int do_aff, int gap_open, \
        int is_metric, int all_elements, cost_matrices_2d_p res) {
    if(DEBUG_CM) {
        printf("\n---cm_alloc_set_costs_2d\n");
    }
    size_t size;
#ifndef USE_LARGE_ALPHABETS
    if (alphSize > 255) {
        // TODO: update this error message from POY
        printf("Apparently you are analyzing large alphabets. This version \
                of PCG was compiled without the --enable-large-alphabets option. \
                To run this analysis you need to enable that option at compile time. \
                Either compile yourself the program, or request a version suited \
                for your needs in the POY mailing list (poy4@googlegroups.com).");
        exit(1);
        // failwith ("Apparently you are analyzing large alphabets. This version \
        //         of POY was compiled without the --enable-large-alphabets option. \n
        //         To run this analysis you need to enable that option at compile time. \n
        //         Either compile yourself the program, or request a version suited \n
        //         for your needs in the POY mailing list (poy4@googlegroups.com).");
    }
#endif
    if (combinations != 0) {
        cm_set_gap (res, 1 << (alphSize - 1));
        cm_set_alphSize (res, cm_combinations_of_alphabet (alphSize)); // 2 ^ alphSize - 1 is |power set of alphSize|
        cm_set_lcm (res, alphSize);
        cm_set_combinations (res);
    } else {
        cm_set_gap (res, alphSize);
        cm_set_alphSize (res, alphSize);
        cm_set_lcm (res, ceil_log_2 (alphSize + 1));
        cm_unset_combinations (res);
    }
    cm_set_all_elements (res, all_elements);
    cm_set_affine (res, do_aff, gap_open);
    res->is_metric = is_metric;
    size = 2 * (1 << (res->lcm)) * (1 << (res->lcm)) * sizeof(int); // size for cost matrices
    if (0 == size) {
        printf("Your cost matrix is too large to fit in your memory. I can't continue with your data loading.");
        exit(1);
        // failwith ("Your cost matrix is too large to fit in your memory. I can't continue with your data loading.");
    }
    res->cost         = calloc (size, 1);
    res->worst        = calloc (size, 1);
    res->prepend_cost = calloc (size, 1);
    res->tail_cost    = calloc (size, 1);

    size = 2 * (1 << (res->lcm)) * (1 << (res->lcm)) * sizeof(SEQT); // size for median matrix
    if (0 == size) {
        printf("Your cost matrix is too large to fit in your memory. I can't continue with your data loading.");
        exit(1);
        // failwith ("Your cost matrix is too large to fit in your memory. I can't continue with your data loading.");
    }
    res->median = (SEQT *) calloc (size, 1);
    if ((res->cost == NULL) || (res->median == NULL)) {
        free (res->cost);
        free (res->median);
        printf("Memory error during cost matrix allocation.\n");
        exit(1);
        // failwith ("Memory error during cost matrix allocation.");
    }

    //printf("cm_get%d\n", );
    return;
}

/*
 * Creates a cost matrix with memory allocated for an alphabet of size alphSize
 * (not including the gap representation which is internally chosen), and whose
 * size must consider all possible combinations of characters in the alphabeet
 * iff combinations != 0. Set the affine gap model paramters to the values
 * stored in do_aff, gap_open, in the cost matrix res.
 * In case of error the function fails with the message "Memory error.".
 */
// TODO: remove do_aff
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
        cm_set_gap_3d (res, 1 << (alphSize - 1));
        cm_set_alphSize_3d (res, cm_combinations_of_alphabet (alphSize)); // 2 ^ alphSize - 1 is |power set of alphSize|
        cm_set_lcm_3d (res, alphSize);
        cm_set_combinations_3d (res);
    } else {
        cm_set_gap_3d (res, alphSize);
        cm_set_alphSize_3d (res, alphSize);
        cm_set_lcm_3d (res, ceil_log_2 (alphSize + 1));
        cm_unset_combinations_3d (res);
    }
    cm_set_all_elements_3d (res, all_elements);
    cm_set_affine_3d (res, do_aff, gap_open);
    size = (1 << (res->lcm + 1)) * (1 << (res->lcm + 1)) * (1 << (res->lcm + 1));
    res->cost = (int *) calloc (size * sizeof(int), 1);
    res->median = (SEQT *) calloc (size * sizeof(SEQT), 1);
    if ((res->cost == NULL) || (res->median == NULL)) {
        free (res->cost);
        free (res->median);
        printf("Memory error during cost matrix allocation.\n");
        exit(1);
        // failwith ("Memory error during cost matrix allocation.");
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
cm_get_gap_2d (const cost_matrices_2d_p c) {
    assert(c != NULL);
    return c->gap;
}

SEQT
cm_get_gap_3d (const cost_matrices_3d_p c) {
    assert(c != NULL);
    return c->gap;
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
        // failwith ("Alphabet size = 2");
    }
    if ((1 << alphSize) <= a) {
        printf("2a is bigger than alphabet size\n");
        exit(1);
        // failwith ("2a is bigger than alphabet size");
    }
    if ((1 << alphSize) <= b) {
        printf("b is bigger than alphabet size\n");
        exit(1);
        // failwith ("b is bigger than alphabet size");
    }
    return (*(tcm + cm_calc_cost_position_3d (a, b, c, alphSize)));
}

inline int
cm_calc_cost_3d (int *tcm, SEQT a, SEQT b, SEQT c, int alphSize) {
    if (alphSize <= 0) {
        printf("Alphabet size = 2\n");
        exit(1);
        // failwith ("Alphabet size = 2");
    }
    if ((1 << alphSize) <= a) {
        printf("2a is bigger than alphabet size\n");
        exit(1);
        // failwith ("2a is bigger than alphabet size");
    }
    if ((1 << alphSize) <= b) {
        printf("b is bigger than alphabet size\n");
        exit(1);
        // failwith ("b is bigger than alphabet size");
    }
    return (*(tcm + cm_calc_cost_position_3d (a, b, c, alphSize)));
}

inline SEQT
cm_calc_cost_3d_seq_p (SEQT *tcm, SEQT a, SEQT b, SEQT c, int alphSize) {
    if (alphSize <= 0) {
        printf("Alphabet size = 2\n");
        exit(1);
        // failwith ("Alphabet size = 2");
    }
    if ((1 << alphSize) <= a) {
        printf("2a is bigger than alphabet size\n");
        exit(1);
        // failwith ("2a is bigger than alphabet size");
    }
    if ((1 << alphSize) <= b) {
        printf("b is bigger than alphabet size\n");
        exit(1);
        // failwith ("b is bigger than alphabet size");
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
        // failwith ("Alphabet size = 3");
    }
    if ((1 << alphSize) <= a) {
        printf("3a is bigger than alphabet size\n");
        exit(1);
        // failwith ("3a is bigger than alphabet size");
    }
    return (tcm + (a << alphSize));
}

static inline int *
cm_get_row_3d (int *tcm, SEQT a, SEQT b, int alphSize) {
    if (alphSize <= 0) {
        printf("Alphabet size = 4\n");
        exit(1);
        // failwith ("Alphabet size = 4");
    }
    if ((1 << alphSize) <= a) {
        printf("%hhu is bigger than alphabet size\n", a);
        exit(1);
        // failwith ("4a is bigger than alphabet size");
    }
    if ((1 << alphSize) <= b) {
        printf("%hhu is bigger than alphabet size\n", b);
        exit(1);
        // failwith ("b is bigger than alphabet size");
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
    SEQT *begin_t;

    seqLen       = seq_get_len (seq);
    precalcMtx_t = mat_get_2d_prec (nwMtxs);
    seqTcm_t     = cm_get_transformation_cost_matrix (costMtx_t);
    prepend_t    = cm_get_prepend_cost (costMtx_t);
    tailCosts_t  = cm_get_tail_cost (costMtx_t);
    tmpPrecMtx_t = precalcMtx_t + seqLen;
    begin_t      = seq_get_begin (seq);         // Inlined seq_get for speed purposes
    if (DEBUG_MAT) {
        printf ("Precalculated transformation cost matrix.\n");
    }
    // We will put the cost of the prepend in the 0th row of the precalc matrix.
    for (j = 0; j < seqLen; j++) {
        precalcMtx_t[j] = prepend_t[begin_t[j]];
        if (DEBUG_CM) {
            printf ("%7d", precalcMtx_t[j]);
        }
    }
    if (DEBUG_MAT) {
        printf("\n");
    }
    for (j = 1; j <= costMtx_t->alphSize; j++, tmpPrecMtx_t += seqLen) {
        // if (DEBUG_CM) {
        //     printf("%zu\t", j);
        // }
        tmpCost_t = cm_get_row (seqTcm_t, j, costMtx_t->lcm);
        /* We fill almost the complete row. Only the first (aligning with the
         * gap), is filled using the tail cost */
        tmpPrecMtx_t[0] = tailCosts_t[j];
        if (DEBUG_MAT) {
            printf ("%7d", tmpPrecMtx_t[0]);
        }
        for (i = 1; i < seqLen; i++) {
            tmpPrecMtx_t[i] = tmpCost_t[begin_t[i]];
            if (DEBUG_MAT) {
                printf ("%7d", tmpPrecMtx_t[i]);
            }
        }
        if (DEBUG_MAT) {
            printf ("\n");
        }
    }
    if (DEBUG_MAT) {
        printf ("Finished printing transformation cost matrix\n");
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
            tmp_cost = cm_get_row_3d (tcm, seq1idx, seq2idx, costMtx->lcm);
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
    cm_set_value (a, b, v, c->cost, c->lcm);
    return;
}

int
cm_get_cost (int a, int b, cost_matrices_2d_p c) {
    return cm_get_value (a, b, c->cost, c->lcm);
}

// TODO: This seems never to be used
void
cm_set_worst (int a, int b, int v, cost_matrices_2d_p c) {
    cm_set_value (a, b, v, c->worst, c->lcm);
    return;
}

void
cm_set_cost_3d (int a, int b, int cp, int v, cost_matrices_3d_p c) {
    cm_set_value_3d (a, b, cp, v, c->cost, c->lcm);
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
    cm_set_value_2d_seq_p (a, b, v, c->median, c->lcm);
    return;
}

void
cm_set_median_3d (SEQT a, SEQT b, SEQT cp, SEQT v, cost_matrices_3d_p c) {
    cm_set_value_3d_seq_p (a, b, cp, v, c->median, c->lcm);
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
        len_g = 2 * (1 << (a->lcm)) * (1 << (a->lcm));
        len = len_g * sizeof(int);
        len1 = len_g * sizeof(SEQT);
        cmp = memcmp (a->cost, b->cost, len);
        if (cmp != 0) return (cmp);
        cmp = memcmp (a->median, b->median, len1);
        if (cmp != 0) return (cmp);
        cmp = memcmp (a->worst, b->worst, len);
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
    return (cm_calc_median((tmp->median), a, b, tmp->lcm));
}

SEQT
cm_get_median_3d (const cost_matrices_3d_p t, SEQT a, SEQT b, SEQT c) {
    return (cm_calc_median_3d((t->median), a, b, c, t->lcm));
}


/*
unsigned long
cm_CAML_deserialize (void *v) {
    cost_matrices_2d_p n;
    int len;
    n = (cost_matrices_2d_p) v;
    n->alphSize = deserialize_sint_4();
    n->lcm = deserialize_sint_4();
    n->gap = deserialize_sint_4();
    n->cost_model_type = deserialize_sint_4();
    n->combinations = deserialize_sint_4();
    n->gap_open = deserialize_sint_4();
    n->is_metric = deserialize_sint_4();
    n->all_elements = deserialize_sint_4();
    len = 2 * (1 << (n->lcm)) * (1 << n->lcm) ;
    n->cost = (int *) calloc (len * sizeof(int), 1);
    n->median = (SEQT *) calloc (len * sizeof(SEQT), 1);
    n->worst = (int *) calloc (len * sizeof(int), 1);
    n->prepend_cost = (int *) calloc (len * sizeof(int), 1);
    n->tail_cost = (int *) calloc (len * sizeof(int), 1);
    if ((n->cost == NULL) || (n->median == NULL)) failwith ("Memory error.");
    deserialize_block_4(n->cost, len);
    DESERIALIZE_SEQT(n->median, len);
    deserialize_block_4(n->worst, len);
    deserialize_block_4(n->prepend_cost, len);
    deserialize_block_4(n->tail_cost, len);
    return (sizeof(struct cm));
}

unsigned long
cm_CAML_deserialize_3d (void *v) {
    cost_matrices_3d_p n;
    int len;
    n = (cost_matrices_3d_p) v;
    n->alphSize = deserialize_sint_4();
    n->lcm = deserialize_sint_4();
    n->gap = deserialize_sint_4();
    n->cost_model_type = deserialize_sint_4();
    n->combinations = deserialize_sint_4();
    n->gap_open = deserialize_sint_4();
    n->all_elements = deserialize_sint_4();
    len = (1 << (n->lcm + 1)) * (1 << (n->lcm + 1)) * (1 << (n->lcm + 1));
    n->cost = (int *) calloc (len * sizeof(int), 1);
    n->median = (SEQT *) calloc (len * sizeof(SEQT), 1);
    if ((n->cost == NULL) || (n->median == NULL)) failwith ("Memory error.");
    deserialize_block_4(n->cost, len);
    DESERIALIZE_SEQT(n->median, len);
    return (sizeof(struct cm_3d));
}

void
cm_CAML_serialize (value vcm, unsigned long *wsize_32, \
        unsigned long *wsize_64) {
    cost_matrices_2d_p c;
    int len;
    if (DEBUG_CM) {
        printf ("I will serialize cm!\n");
        fflush (stdout);
    }
    c = Cost_matrix_struct(vcm);
    serialize_int_4(c->alphSize);
    serialize_int_4(c->lcm);
    serialize_int_4(c->gap);
    serialize_int_4(c->cost_model_type);
    serialize_int_4(c->combinations);
    serialize_int_4(c->gap_open);
    serialize_int_4(c->is_metric);
    serialize_int_4(c->all_elements);
    *wsize_64 = *wsize_32 = sizeof(struct cm);
    len = 2 * (1 << (c->lcm)) * (1 << (c->lcm));
    serialize_block_4(c->cost, len);
    SERIALIZE_SEQT(c->median, len);
    serialize_block_4(c->worst, len);
    serialize_block_4(c->prepend_cost, len);
    serialize_block_4(c->tail_cost, len);
    return;
}

void
cm_CAML_serialize_3d (value vcm, unsigned long *wsize_32, \
        unsigned long *wsize_64) {
    cost_matrices_3d_p c;
    int len;
    c = Cost_matrix_struct_3d(vcm);
    serialize_int_4(c->alphSize);
    serialize_int_4(c->lcm);
    serialize_int_4(c->gap);
    serialize_int_4(c->cost_model_type);
    serialize_int_4(c->combinations);
    serialize_int_4(c->gap_open);
    serialize_int_4(c->all_elements);
    *wsize_64 = *wsize_32 = sizeof(struct cm_3d);
    len = (1 << (c->lcm + 1)) * (1 << (c->lcm + 1)) * (1 << (c->lcm + 1));
    serialize_block_4(c->cost, len);
    SERIALIZE_SEQT(c->median, len);
    return;
}

void
cm_CAML_free (value v) {
    cost_matrices_2d_p c;
    c = Cost_matrix_struct(v);
    free (c->cost);
    free (c->median);
    free (c->worst);
    free (c->prepend_cost);
    free (c->tail_cost);
    return;
}

void
cm_CAML_free_3d (value v) {
    cost_matrices_3d_p c;
    c = Cost_matrix_struct_3d(v);
    free (c->cost);
    free (c->median);
    return;
}
*/



/*
int
cm_CAML_compare (value a, value b) {
    int res;
    res = cm_compare (Cost_matrix_struct(a), Cost_matrix_struct(b));
    return (res);
}


int
cm_compare_3d (value a, value b) {
    return(0);
}
*/

/*
static struct custom_operations cost_matrix = {
    "http://www.amnh.org/poy/cost_matrix/two_dimensional.0.1",
    &cm_CAML_free,
    &cm_CAML_compare,
    custom_hash_default,
    cm_CAML_serialize,
    cm_CAML_deserialize
};

static struct custom_operations cost_matrix_3d = {
    "http://www.amnh.org/poy/cost_matrix/three_dimensional.0.1",
    &cm_CAML_free_3d,
    &cm_compare_3d,
    custom_hash_default,
    cm_CAML_serialize_3d,
    cm_CAML_deserialize_3d
};
*/


/*
value
cm_CAML_clone (value v) {
    CAMLparam1(v);
    value clone;
    cost_matrices_2d_p clone2;
    cost_matrices_2d_p c;
    int len;
    clone = alloc_custom (&cost_matrix, sizeof(struct cm), 1, 1000000);
    clone2 = Cost_matrix_struct(clone);
    c = Cost_matrix_struct(v);
    if (c->combinations)
        cm_alloc_set_costs_2d(c->lcm, c->combinations, c->cost_model_type, \
                c->gap_open, c->is_metric, c->all_elements, clone2);
    else
        cm_alloc_set_costs_2d(c->alphSize, c->combinations, c->cost_model_type, \
                c->gap_open, c->is_metric, c->all_elements, clone2);
    len = 2 *(1 << (c->lcm)) * (1 << (c->lcm));
    cm_copy_contents (c->cost, clone2->cost, len);
    cm_copy_contents_seq_p (c->median, clone2->median, len);
    cm_copy_contents (c->worst, clone2->worst, len);
    cm_copy_contents (c->prepend_cost, clone2->prepend_cost, len);
    cm_copy_contents (c->tail_cost, clone2->tail_cost, len);
    CAMLreturn(clone);
}

value
cm_CAML_clone_3d (value v) {
    CAMLparam1(v);
    CAMLlocal1(clone);
    cost_matrices_3d_p clone2;
    cost_matrices_3d_p c;
    int len;
    clone = alloc_custom (&cost_matrix_3d, sizeof(struct cm), 1, 1000000);
    clone2 = Cost_matrix_struct_3d(clone);
    c = Cost_matrix_struct_3d(v);
    cm_alloc_set_costs_3d (c->lcm, c->combinations, c->cost_model_type, \
            c->gap_open, c->all_elements, clone2);
    len = (c->alphSize + 1) * (c->alphSize + 1) * (c->alphSize + 1);
    cm_copy_contents (c->cost, clone2->cost, len);
    cm_copy_contents_seq_p (c->median, clone2->median, len);
    CAMLreturn(clone);
}

value
cm_CAML_set_gap_3d (value c, value v) {
    CAMLparam2 (c, v);
    cm_set_gap_3d (Cost_matrix_struct_3d(c), Int_val(v));
    CAMLreturn (Val_unit);
}

value
cm_CAML_set_gap (value c, value v) {
    CAMLparam2 (c, v);
    cm_set_gap (Cost_matrix_struct(c), Int_val(v));
    CAMLreturn (Val_unit);
}

value
cm_CAML_set_affine_3d (value c, value do_aff, value go) {
    CAMLparam3(c, do_aff, go);
    cm_set_affine_3d (Cost_matrix_struct_3d(c), Int_val(do_aff), Int_val(go));
    CAMLreturn(Val_unit);
}

value
cm_CAML_set_affine (value c, value do_aff, value go) {
    CAMLparam3(c, do_aff, go);
    cm_set_affine (Cost_matrix_struct(c), Int_val(do_aff), Int_val(go));
    CAMLreturn(Val_unit);
}

value
cm_CAML_get_alphSize_3d (value cm) {
    CAMLparam1 (cm);
    CAMLreturn (Val_int((Cost_matrix_struct_3d(cm))->alphSize));
}

value
cm_CAML_get_alphSize (value cm) {
    CAMLparam1 (cm);
    CAMLreturn (Val_int((Cost_matrix_struct(cm))->alphSize));
}

value
cm_CAML_set_alphSize_3d (value v, value cm) {
    CAMLparam2 (cm, v);
    cm_set_alphSize_3d (Cost_matrix_struct_3d(cm), Int_val(v));
    CAMLreturn(Val_unit);
}


value
cm_CAML_set_alphSize (value cm, value v) {
    CAMLparam2 (cm, v);
    cm_set_alphSize (Cost_matrix_struct(cm), Int_val(v));
    CAMLreturn(Val_unit);
}


value
cm_CAML_get_gap_3d (value c) {
    CAMLparam1 (c);
    CAMLreturn (Val_int((cm_get_gap_3d (Cost_matrix_struct_3d(c)))));
}

value
cm_CAML_get_gap (value c) {
    CAMLparam1 (c);
    CAMLreturn (Val_int((cm_get_gap (Cost_matrix_struct(c)))));
}

value
cm_CAML_get_affine_3d (value c) {
    CAMLparam1(c);
    CAMLreturn(Val_int(cm_get_affine_flag_3d (Cost_matrix_struct_3d(c))));
}

value
cm_CAML_get_affine (value c) {
    CAMLparam1(c);
    CAMLreturn(Val_int(cm_get_affine_flag (Cost_matrix_struct(c))));
}

value
cm_CAML_get_gap_opening_3d (value c) {
    CAMLparam1(c);
    int i;
    i = cm_get_gap_opening_parameter_3d(Cost_matrix_struct_3d(c));
    CAMLreturn(Val_int(i));
}

value
cm_CAML_get_gap_opening (value c) {
    CAMLparam1(c);
    int i;
    i = cm_get_gap_opening_parameter(Cost_matrix_struct(c));
    CAMLreturn(Val_int(i));
}

value
cm_CAML_get_combinations (value c) {
    CAMLparam1(c);
    int i;
    i = cm_get_combinations (Cost_matrix_struct(c));
    CAMLreturn(Val_int(i));
}

value
cm_CAML_get_combinations_3d (value c) {
    CAMLparam1(c);
    int i;
    i = cm_get_combinations_3d (Cost_matrix_struct_3d(c));
    CAMLreturn(Val_int(i));
}

value
cm_CAML_get_cost_3d (value a, value b, value c, value cm) {
    CAMLparam3(a, b, c);
    int *tcm;
    cost_matrices_3d_p tmp;
    tmp = Cost_matrix_struct_3d(cm);
    tcm = tmp->cost;
    CAMLreturn(Val_int(cm_calc_cost_3d(tcm, Int_val(a), Int_val(b), \
                    Int_val(c), tmp->lcm)));
}

value
cm_CAML_get_cost (value a, value b, value c) {
    CAMLparam3(a, b, c);
    int *tcm;
    cost_matrices_2d_p tmp;
    tmp = Cost_matrix_struct(c);
    tcm = tmp->cost;
    CAMLreturn(Val_int(cm_calc_cost(tcm, Int_val(a), Int_val(b), tmp->lcm)));
}

value
cm_CAML_get_worst (value a, value b, value c) {
    CAMLparam3(a, b, c);
    int *tcm;
    cost_matrices_2d_p tmp;
    tmp = Cost_matrix_struct(c);
    tcm = tmp->worst;
    CAMLreturn(Val_int(cm_calc_cost(tcm, Int_val(a), Int_val(b), tmp->lcm)));
}


value
cm_CAML_get_median_3d (value a, value b, value c, value cm) {
    CAMLparam3(a, b, c);
    SEQT *tcm;
    cost_matrices_3d_p tmp;
    tmp = Cost_matrix_struct_3d(cm);
    tcm = tmp->median;
    CAMLreturn(Val_int(cm_calc_cost_3d_seq_p(tcm, Int_val(a), Int_val(b), \
                    Int_val(c), tmp->lcm)));
}
*/

/*
value
cm_CAML_get_median (value a, value b, value c) {
    CAMLparam3(a, b, c);
    SEQT *tcm;
    cost_matrices_2d_p tmp;
    tmp = Cost_matrix_struct(c);
    tcm = tmp->median;
    CAMLreturn(Val_int(cm_calc_median(tcm, Int_val(a), Int_val(b), tmp->lcm)));
}

value
cm_CAML_set_cost_3d (value a, value b, value c, value cc, value v) {
    CAMLparam5(a, b, c, cc, v);
    cost_matrices_3d_p tmp;
    tmp = Cost_matrix_struct_3d(cc);
    cm_set_cost_3d (Int_val(a), Int_val(b), Int_val(c), Int_val(v), tmp);
    CAMLreturn(Val_unit);
}

value
cm_CAML_set_cost (value a, value b, value c, value v) {
    CAMLparam4(a, b, c, v);
    cost_matrices_2d_p tmp;
    tmp = Cost_matrix_struct(c);
    cm_set_cost (Int_val(a), Int_val(b), Int_val(v), tmp);
    CAMLreturn(Val_unit);
}

value
cm_CAML_set_worst (value a, value b, value c, value v) {
    CAMLparam4(a, b, c, v);
    cost_matrices_2d_p tmp;
    tmp = Cost_matrix_struct(c);
    cm_set_worst (Int_val(a), Int_val(b), Int_val(v), tmp);
    CAMLreturn(Val_unit);
}

value
cm_CAML_set_median_3d (value a, value b, value c, value cp, value v) {
    CAMLparam4(a, b, c, v);
    cost_matrices_3d_p tmp;
    tmp = Cost_matrix_struct_3d(cp);
    cm_set_median_3d (Int_val(a), Int_val(b), Int_val(c), Int_val(v), tmp);
    CAMLreturn(Val_unit);
}

value
cm_CAML_set_median (value a, value b, value c, value v) {
    CAMLparam4(a, b, c, v);
    cost_matrices_2d_p tmp;
    tmp = Cost_matrix_struct(c);
    cm_set_median (Int_val(a), Int_val(b), Int_val(v), tmp);
    CAMLreturn(Val_unit);
}

value
cm_CAML_set_prepend (value a, value b, value v) {
    CAMLparam3(a, b, v);
    cost_matrices_2d_p tmp;
    tmp = Cost_matrix_struct(v);
    cm_set_prepend_2d (Int_val(a), Int_val(b), tmp);
    CAMLreturn(Val_unit);
}

value
cm_CAML_set_tail (value a, value b, value v) {
    CAMLparam3(a, b, v);
    cost_matrices_2d_p tmp;
    tmp = Cost_matrix_struct(v);
    cm_set_tail_2d (Int_val(a), Int_val(b), tmp);
    CAMLreturn(Val_unit);
}

value
cm_CAML_get_prepend (value a, value v) {
    CAMLparam2(a, v);
    int r;
    cost_matrices_2d_p tmp;
    tmp = Cost_matrix_struct(v);
    assert ((1 + tmp->alphSize) > Int_val(a));
    r = tmp->prepend_cost[Int_val(a)];
    CAMLreturn(Val_int(r));
}

value
cm_CAML_get_tail (value a, value v) {
    CAMLparam2(a, v);
    int r;
    cost_matrices_2d_p tmp;
    tmp = Cost_matrix_struct(v);
    assert ((1 + tmp->alphSize) > Int_val(a));
    r = tmp->tail_cost[Int_val(a)];
    CAMLreturn(Val_int(r));
}

value
cm_CAML_create_3d (value alphSize, value combine, value aff, value go, value d, value all) {
    CAMLparam5(alphSize, combine, aff, go, d);
    CAMLxparam1(all);
    value tmp;
    cost_matrices_3d_p tmp2;
    tmp = alloc_custom (&cost_matrix_3d, sizeof(struct cm_3d), 1, 1000000);
    tmp2 = Cost_matrix_struct_3d(tmp);
    cm_alloc_set_costs_3d (Int_val(alphSize), Bool_val(combine), Int_val(aff), \
            Int_val(go), Int_val(all), tmp2);
    CAMLreturn(tmp);
}

value
cm_CAML_create_3d_bc (value *argv, int argn) {
    return (cm_CAML_create_3d (argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]));
}

value
cm_CAML_create (value alphSize, value combine, value aff, value go, value all) {
    CAMLparam5(alphSize, combine, aff, go, all);
    value tmp;
    cost_matrices_2d_p tmp2;
    tmp = alloc_custom (&cost_matrix, sizeof(struct cm), 1, 1000000);
    tmp2 = Cost_matrix_struct(tmp);
    cm_alloc_set_costs_2d(Int_val(alphSize), Bool_val(combine), Int_val(aff), Int_val(go), \
            0, Int_val(all), tmp2);
    CAMLreturn(tmp);
}

value
cm_CAML_set_lcm (value c, value v) {
    CAMLparam2(c, v);
    cost_matrices_2d_p tmp;
    tmp = Cost_matrix_struct(c);
    cm_set_lcm (tmp, Int_val(v));
    CAMLreturn(Val_unit);
}

value
cm_CAML_set_lcm_3d (value c, value v) {
    CAMLparam2(c, v);
    cost_matrices_3d_p tmp;
    tmp = Cost_matrix_struct_3d(c);
    cm_set_lcm_3d (tmp, Int_val(v));
    CAMLreturn(Val_unit);
}

value
cm_CAML_get_lcm (value c) {
    CAMLparam1(c);
    cost_matrices_2d_p tmp;
    tmp = Cost_matrix_struct(c);
    CAMLreturn(Val_int(cm_get_lcm(tmp)));
}

value
cm_CAML_get_lcm_3d (value c) {
    CAMLparam1(c);
    cost_matrices_3d_p tmp;
    tmp = Cost_matrix_struct_3d(c);
    CAMLreturn(Val_int(cm_get_lcm_3d(tmp)));
}

value
cm_CAML_clone_to_3d (value c) {
    CAMLparam1(c);
    CAMLlocal1(res);
    cost_matrices_2d_p init;
    cost_matrices_3d_p final;
    init = Cost_matrix_struct(c);
    res = alloc_custom (&cost_matrix_3d, sizeof(struct cm_3d), 1, 1000000);
    final = Cost_matrix_struct_3d(res);
    cm_alloc_set_costs_3d (init->lcm, init->combinations, init->cost_model_type, \
            init->gap_open, init->all_elements, final);
    CAMLreturn(res);
}

value
cm_CAML_initialize (value unit) {
    CAMLparam1(unit);
    caml_register_custom_operations (&cost_matrix);
    caml_register_custom_operations (&cost_matrix_3d);
    CAMLreturn(Val_unit);
}

value
cm_CAML_set_is_metric (value c) {
    CAMLparam1(c);
    cost_matrices_2d_p init;
    init = Cost_matrix_struct(c);
    init->is_metric = 1;
    CAMLreturn(Val_unit);
}

value
cm_CAML_get_is_metric (value c) {
    CAMLparam1(c);
    cost_matrices_2d_p init;
    init = Cost_matrix_struct(c);
    CAMLreturn(Val_int(init->is_metric));
}
*/