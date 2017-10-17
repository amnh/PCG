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
#include <string.h>

#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
#include <caml/intext.h>
#include <caml/fail.h>
#include "cm.h"

#ifdef _WIN32
__inline int
#else
inline int
#endif
ceil_log_2 (int v) {
    int i = 0;
    while (v != 0) {
        i++;
        v = v >> 1;
    }
    return (i + 1);
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_combinations_of_alphabet (const int a_sz) {
    assert (a_sz >= 0);
    return ((1 << a_sz) - 1);
}

void
cm_free (cmt c) {
    free (c->cost);
    free (c->median);
    free (c->worst);
    free (c->prepend_cost);
    free (c->tail_cost);
    free (c);
    return;
}

void
cm_3d_free (cmt c) {
    free (c->cost);
    free (c->median);
    free (c);
    return;
}

#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_a_sz (cmt c, int v) {
    assert(c != NULL);
    c->a_sz = v;
    return;
}

#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_a_sz_3d (cm_3dt c, int v) {
    assert(c != NULL);
    c->a_sz = v;
    return;
}

#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_gap (cmt c, int v) {
    assert(c != NULL);
    c->gap = v;
    return;
}

#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_gap_3d (cm_3dt c, int v) {
    assert(c != NULL);
    c->gap = v;
    return;
}

#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_affine (cmt c, int do_aff, int go) {
    assert(c != NULL);
    c->cost_model_type = do_aff;
    c->gap_open = go;
    return;
}

#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_affine_3d (cm_3dt c, int do_aff, int go) {
    assert(c != NULL);
    c->cost_model_type = do_aff;
    c->gap_open = go;
    return;
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_lcm (cmt c) {
    assert(c != NULL);
    return (c->lcm);
}

#ifdef _WIN32
__inline int 
#else
inline int 
#endif
cm_get_lcm_3d (cm_3dt c) {
    assert(c != NULL);
    return (c->lcm);
}

#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_lcm (cmt c, int v) {
    assert(c != NULL);
    c->lcm = v;
    return;
}

#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_lcm_3d (cm_3dt c, int v) {
    assert(c != NULL);
    c->lcm = v;
    return;
}

#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_combinations (cmt c) {
    assert(c != NULL);
    c->combinations = 1;
    return;
}

#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_combinations_3d (cm_3dt c) {
    assert(c != NULL);
    c->combinations = 1;
    return;
}

#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_unset_combinations (cmt c) {
    assert(c != NULL);
    c->combinations = 0;
    return;
}

#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_unset_combinations_3d (cm_3dt c) {
    assert(c != NULL);
    c->combinations = 0;
    return;
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_combinations (cmt c) {
    assert(c != NULL);
    return (c->combinations);
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_combinations_3d (cm_3dt c) {
    assert(c != NULL);
    return (c->combinations);
}

void
cm_set_all_elements (cmt c, int v) {
    assert (c != NULL);
    c->all_elements = v;
    return;
}

void
cm_set_all_elements_3d (cm_3dt c, int v) {
    assert (c != NULL);
    c->all_elements = v;
    return;
}

int
cm_get_all_elements (cmt c) {
    return (c->all_elements);
}

int
cm_get_all_elements_3d (cm_3dt c) {
    return (c->all_elements);
}

value
cm_CAML_set_all_elements (value cm, value v) {
    CAMLparam2(cm, v);
    cmt c;
    int i;
    c = Cost_matrix_struct(cm);
    i = Int_val(v);
    cm_set_all_elements (c, i);
    CAMLreturn(Val_unit);
}

value
cm_CAML_set_all_elements_3d (value cm, value v) {
    CAMLparam2(cm, v);
    cm_3dt c;
    int i;
    c = Cost_matrix_struct_3d(cm);
    i = Int_val(v);
    cm_set_all_elements_3d (c, i);
    CAMLreturn(Val_unit);
}

value
cm_CAML_get_all_elements_3d (value cm) {
    CAMLparam1(cm);
    cm_3dt c;
    c = Cost_matrix_struct_3d(cm);
    CAMLreturn(Val_int(cm_get_all_elements_3d(c)));
}

value
cm_CAML_get_all_elements (value cm) {
    CAMLparam1(cm);
    cmt c;
    c = Cost_matrix_struct(cm);
    CAMLreturn(Val_int(cm_get_all_elements(c)));
}

/* 
 * Creates a cost matrix with memory allocated for an alphabet of size a_sz
 * (not including the gap representation which is internally chosen), and whose
 * size must consider all possible combinations of characters in the alphabeet
 * iff combinations != 0. Set the affine gap model paramters to the values
 * stored in do_aff, gap_open, in the cost matrix res. 
 * In case of error the function fails with the message "Memory error.".
 */
cmt 
cm_set_val (int a_sz, int combinations, int do_aff, int gap_open, \
        int is_metric, int all_elements, cmt res) {
    size_t size;
#ifndef USE_LARGE_ALPHABETS
    if (a_sz > 255) 
        failwith ("Apparently you are analyzing large alphabets. This version \
                of POY was compiled without the --enable-large-alphabets option. \
                To run this analysis you need to enable that option at compile time. \
                Either compile yourself the program, or request a version suited \
                for your needs in the POY mailing list (poy4@googlegroups.com).");
#endif
    if (combinations != 0) {
        cm_set_gap (res, 1 << (a_sz - 1));
        cm_set_a_sz (res, cm_combinations_of_alphabet (a_sz));
        cm_set_lcm (res, a_sz);
        cm_set_combinations (res);
    } else {
        cm_set_gap (res, a_sz);
        cm_set_a_sz (res, a_sz);
        cm_set_lcm (res, ceil_log_2 (a_sz + 1));
        cm_unset_combinations (res);
    }
    cm_set_all_elements (res, all_elements);
    cm_set_affine (res, do_aff, gap_open);
    res->is_metric = is_metric;
    size = 2 * (1 << (res->lcm)) * (1 << (res->lcm)) * sizeof(int);
    if (0 == size)
        failwith ("Your cost matrix is too large to fit in your memory. I can't continue with your data loading.");
    res->cost = (int *) calloc (size, 1);
    res->worst = (int *) calloc (size, 1);
    res->prepend_cost = (int *) calloc (size, 1);
    res->tail_cost = (int *) calloc (size, 1);
    size = 2 * (1 << (res->lcm)) * (1 << (res->lcm)) * sizeof(SEQT);
    if (0 == size)
        failwith ("Your cost matrix is too large to fit in your memory. I can't continue with your data loading.");
    res->median = (SEQT *) calloc (size, 1);
    if ((res->cost == NULL) || (res->median == NULL)) {
        free (res->cost);
        free (res->median);
        failwith ("Memory error during cost matrix allocation.");
    }
    return res;
}

/* 
 * Creates a cost matrix with memory allocated for an alphabet of size a_sz
 * (not including the gap representation which is internally chosen), and whose
 * size must consider all possible combinations of characters in the alphabeet
 * iff combinations != 0. Set the affine gap model paramters to the values
 * stored in do_aff, gap_open, in the cost matrix res. 
 * In case of error the function fails with the message "Memory error.".
 */
cm_3dt 
cm_set_val_3d (int a_sz, int combinations, int do_aff, int gap_open, \
        int all_elements, cm_3dt res) {
    int size;
    if (!NDEBUG) {
        printf ("Allocating a three dimensional matrix:\n");
        printf ("alphabet size: %d \n", a_sz);
        printf ("combinations: %d \n", combinations);
        printf ("cost model: %d \n", do_aff);
        printf ("gap open cost: %d \n", gap_open);
    }
    if (combinations != 0) {
        cm_set_gap_3d (res, 1 << (a_sz - 1));
        cm_set_a_sz_3d (res, cm_combinations_of_alphabet (a_sz));
        cm_set_lcm_3d (res, a_sz);
        cm_set_combinations_3d (res);
    } else {
        cm_set_gap_3d (res, a_sz);
        cm_set_a_sz_3d (res, a_sz);
        cm_set_lcm_3d (res, ceil_log_2 (a_sz + 1));
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
        failwith ("Memory error during cost matrix allocation.");
    }
    return res;
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_alphabet_size (cmt c) {
    assert(c != NULL);
    return c->a_sz;
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_alphabet_size_3d (cm_3dt c) {
    assert(c != NULL);
    return c->a_sz;
}

#ifdef _WIN32
__inline SEQT
#else
inline SEQT
#endif
cm_get_gap (const cmt c) {
    assert(c != NULL);
    return c->gap;
}

#ifdef _WIN32
__inline SEQT
#else
inline SEQT
#endif
cm_get_gap_3d (const cm_3dt c) {
    assert(c != NULL);
    return c->gap;
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_affine_flag (cmt c) {
    assert(c != NULL);
    return c->cost_model_type;
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_affine_flag_3d (cm_3dt c) {
    assert(c != NULL);
    return c->cost_model_type;
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_gap_opening_parameter (cmt c) {
    assert(c != NULL);
    return c->gap_open;
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_gap_opening_parameter_3d (const cm_3dt c) {
    assert(c != NULL);
    return c->gap_open;
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_calc_cost_position (int a, int b, int a_sz) {
    assert(a_sz >= 0);
    return ((a << a_sz) + b);
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_calc_cost_position_seqt (SEQT a, SEQT b, int a_sz) {
    assert(a_sz >= 0);
    return ((((int) a) << a_sz) + ((int) b));
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_calc_cost_position_3d_seqt (SEQT a, SEQT b, SEQT c, int a_sz) {
    assert(a_sz >= 0);
    return ((((((int) a) << a_sz) + ((int) b)) << a_sz) + ((int) c));
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_calc_cost_position_3d (int a, int b, int c, int a_sz) {
    assert(a_sz >= 0);
    return ((((a << a_sz) + b) << a_sz) + c);
}

#ifdef _WIN32
__inline SEQT
#else
inline SEQT
#endif
cm_calc_median (SEQT *tcm, SEQT a, SEQT b, int a_sz) {
    SEQT *res;
    assert (a_sz >= 0);
    assert ((1 << a_sz) > a);
    assert ((1 << a_sz) > b);
    res = tcm + cm_calc_cost_position_seqt (a, b, a_sz);
    return (*res);
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_calc_cost (int *tcm, SEQT a, SEQT b, int a_sz) {
    int *res;
    assert (a_sz >= 0);
    assert ((1 << a_sz) > a);
    assert ((1 << a_sz) > b);
    res = tcm + cm_calc_cost_position_seqt (a, b, a_sz);
    return (*res);
}

#ifdef _WIN32
__inline SEQT
#else
inline SEQT
#endif
cm_calc_median_3d (SEQT *tcm, SEQT a, SEQT b, SEQT c, int a_sz) {
    if (a_sz <= 0) failwith ("Alphabet size = 2");
    if ((1 << a_sz) <= a) failwith ("2a is bigger than alphabet size");
    if ((1 << a_sz) <= b) failwith ("b is bigger than alphabet size");
    return (*(tcm + cm_calc_cost_position_3d (a, b, c, a_sz)));
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_calc_cost_3d (int *tcm, SEQT a, SEQT b, SEQT c, int a_sz) {
    if (a_sz <= 0) failwith ("Alphabet size = 2");
    if ((1 << a_sz) <= a) failwith ("2a is bigger than alphabet size");
    if ((1 << a_sz) <= b) failwith ("b is bigger than alphabet size");
    return (*(tcm + cm_calc_cost_position_3d (a, b, c, a_sz)));
}

#ifdef _WIN32
__inline SEQT
#else
inline SEQT
#endif
cm_calc_cost_3d_seqt (SEQT *tcm, SEQT a, SEQT b, SEQT c, int a_sz) {
    if (a_sz <= 0) failwith ("Alphabet size = 2");
    if ((1 << a_sz) <= a) failwith ("2a is bigger than alphabet size");
    if ((1 << a_sz) <= b) failwith ("b is bigger than alphabet size");
    return (*(tcm + cm_calc_cost_position_3d (a, b, c, a_sz)));
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_calc_tmm (int *tmm, int a, int b, int a_sz) {
    return (cm_calc_cost (tmm, a, b, a_sz));
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_calc_median_position (SEQT a, SEQT b, int a_sz) {
    return (cm_calc_cost_position (a, b, a_sz));
}

/* 
 * Position of the first memory location of the transformation cost matrix given
 * a bigarray from ocaml.
 */
#ifdef _WIN32
__inline int *
#else
inline int *
#endif
cm_get_transformation_cost_matrix (const cmt a) {
    return (a->cost);
}

#ifdef _WIN32
__inline int *
#else
inline int *
#endif
cm_get_tail_cost (const cmt a) {
    return (a->tail_cost);
}

#ifdef _WIN32
__inline int *
#else
inline int *
#endif
cm_get_prepend_cost (const cmt a) {
    return (a->prepend_cost);
}

#ifdef _WIN32
__inline int *
#else
inline int *
#endif
cm_get_transformation_cost_matrix_3d (const cm_3dt a) {
    return (a->cost);
}

#ifdef _WIN32
__inline int *
#else
inline int *
#endif
cm_get_row (int *tcm, SEQT a, int a_sz) {
    if (a_sz <= 0) failwith ("Alphabet size = 3");
    if ((1 << a_sz) <= a) failwith ("3a is bigger than alphabet size");
    return (tcm + (a << a_sz));
}

#ifdef _WIN32
__inline int *
#else
inline int *
#endif
cm_get_row_3d (int *tcm, SEQT a, SEQT b, int a_sz) {
    if (a_sz <= 0) failwith ("Alphabet size = 4");
    if ((1 << a_sz) <= a) failwith ("4a is bigger than alphabet size");
    if ((1 << a_sz) <= b) failwith ("b is bigger than alphabet size");
    return (tcm + (((a << a_sz) + b) << a_sz));
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_value_seqt (SEQT a, SEQT b, SEQT v, SEQT *p, int a_sz) {
    *(p + (cm_calc_cost_position_seqt (a, b, a_sz))) = v;
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_value (int a, int b, int v, int *p, int a_sz) {
    *(p + (cm_calc_cost_position (a, b, a_sz))) = v;
    return;
}

void
cm_precalc_4algn (const cmt c, matricest matrix, const seqt s) {
    int i, j, l, m, *tmp_cost, *tcm, *tmp_to, *prepend, *tail, *to;
    SEQT *begin;
    l = seq_get_len (s);
    to = mat_get_2d_prec (matrix);
    tcm = cm_get_transformation_cost_matrix (c);
    prepend = cm_get_prepend_cost (c);
    tail = cm_get_tail_cost (c);
    tmp_to = to + l;
    begin = seq_get_begin (s);         /* Inlined seq_get for speed purposes */
    if (!NDEBUG) 
        printf ("Precalculated transformation cost matrix.\n");
    /* We will use the 0'th row to store the cost of the prepend */
    for (m = 0; m < l; m++) 
        to[m] = prepend[begin[m]];
    for (j = 1; j <= c->a_sz; j++, tmp_to += l) {
        tmp_cost = cm_get_row (tcm, j, c->lcm);
        /* We fill almost the complete row, only the first (aligning with the
         * gap), is filled using the tail cost */
        tmp_to[0] = tail[j];
        for (i = 1; i < l; i++) {
            tmp_to[i] = tmp_cost[begin[i]];
            if (!NDEBUG) 
                printf ("%d\t", tmp_to[i]);
        }
        if (!NDEBUG) 
            printf ("\n");
    }
    if (!NDEBUG)
        printf ("Finished printing transforamtion cost matrix\n");
    return;
}

const int *
cm_get_precal_row (const int *p, SEQT item, int len) {
    return (p + (len * item));
}

#ifdef _WIN32
__inline const int *
#else
inline const int *
#endif
cm_get_pos_in_precalc (const int *to, int s3l, int a_sz, int s1c, int s2c, \
        int s3p) {
    int *res;
    a_sz++;
    res = (int *) to + ((s1c * (a_sz * s3l)) + (s3l * s2c) + s3p);
    return (res);
}

const inline int *
cm_get_row_precalc_3d (const int *to, int s3l, int a_sz, int s1c, int s2c) {
    return (cm_get_pos_in_precalc (to, s3l, a_sz, s1c, s2c, 0));
}

void
cm_precalc_4algn_3d (const cm_3dt c, int *to, const seqt s) {
    int i, j, k, l, *tmp_cost, *tcm;
    int sequen, *precalc_pos;
    l = seq_get_len (s);
    tcm = cm_get_transformation_cost_matrix_3d (c);
    for (j = 1; j < c->a_sz + 1; j++) 
        for (k = 1; k < c->a_sz + 1; k++) {
            tmp_cost = cm_get_row_3d (tcm, j, k, c->lcm);
            for (i = 0; i < l; i++) {
                sequen = seq_get (s, i);
                precalc_pos = (int *) cm_get_pos_in_precalc (to, l, c->a_sz, j, k, i);
                *precalc_pos = *(tmp_cost + sequen); 
            }
        }
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_value_3d_seqt (SEQT a, SEQT b, SEQT c, SEQT v, SEQT *p, int a_sz) {
    *(p + (cm_calc_cost_position_3d_seqt (a, b, c, a_sz))) = v;
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_value_3d (int a, int b, int c, int v, int *p, int a_sz) {
    *(p + (cm_calc_cost_position_3d (a, b, c, a_sz))) = v;
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_cost (int a, int b, int v, cmt c) {
    cm_set_value (a, b, v, c->cost, c->lcm);
    return;
}


#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_worst (int a, int b, int v, cmt c) {
    cm_set_value (a, b, v, c->worst, c->lcm);
    return;
}




#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_cost_3d (int a, int b, int cp, int v, cm_3dt c) {
    cm_set_value_3d (a, b, cp, v, c->cost, c->lcm);
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_prepend (int a, int b, cmt c) {
    c->prepend_cost[a] = b;
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_tail (int a, int b, cmt c) {
    c->tail_cost[a] = b;
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_median (SEQT a, SEQT b, SEQT v, cmt c) {
    cm_set_value_seqt (a, b, v, c->median, c->lcm);
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_median_3d (SEQT a, SEQT b, SEQT cp, SEQT v, cm_3dt c) {
    cm_set_value_3d_seqt (a, b, cp, v, c->median, c->lcm);
    return;
}

unsigned long
cm_CAML_deserialize (void *v) {
    cmt n;
    int len;
    n = (cmt) v;
    n->a_sz = deserialize_sint_4();
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
    cm_3dt n;
    int len;
    n = (cm_3dt) v;
    n->a_sz = deserialize_sint_4();
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
    cmt c;
    int len;
    if (!NDEBUG) {
        printf ("I will serialize cm!\n");
        fflush (stdout);
    }
    c = Cost_matrix_struct(vcm);
    serialize_int_4(c->a_sz);
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
    cm_3dt c;
    int len;
    c = Cost_matrix_struct_3d(vcm);
    serialize_int_4(c->a_sz);
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
    cmt c;
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
    cm_3dt c;
    c = Cost_matrix_struct_3d(v);
    free (c->cost);
    free (c->median);
    return;
}

int
cm_compare (cmt a, cmt b) {
    int cmp, len_g;
    size_t len, len1;
    if (a->a_sz != b->a_sz) {
        return (a->a_sz - b->a_sz);
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

void
cm_copy_contents (int *src, int *tgt, int len) {
    int i;
    for (i = 0; i < len; i++)
        *(tgt + i) = *(src + i);
    return;
}


void
cm_copy_contents_seqt (SEQT *src, SEQT *tgt, int len) {
    int i;
    for (i = 0; i < len; i++)
        *(tgt + i) = *(src + i);
    return;
}

value
cm_CAML_clone (value v) {
    CAMLparam1(v);
    value clone;
    cmt clone2;
    cmt c;
    int len;
    clone = alloc_custom (&cost_matrix, sizeof(struct cm), 1, 1000000);
    clone2 = Cost_matrix_struct(clone);
    c = Cost_matrix_struct(v);
    if (c->combinations)
        cm_set_val (c->lcm, c->combinations, c->cost_model_type, \
                c->gap_open, c->is_metric, c->all_elements, clone2);
    else
        cm_set_val (c->a_sz, c->combinations, c->cost_model_type, \
                c->gap_open, c->is_metric, c->all_elements, clone2);
    len = 2 *(1 << (c->lcm)) * (1 << (c->lcm));
    cm_copy_contents (c->cost, clone2->cost, len);
    cm_copy_contents_seqt (c->median, clone2->median, len);
    cm_copy_contents (c->worst, clone2->worst, len);
    cm_copy_contents (c->prepend_cost, clone2->prepend_cost, len);
    cm_copy_contents (c->tail_cost, clone2->tail_cost, len);
    CAMLreturn(clone);
}

value
cm_CAML_clone_3d (value v) {
    CAMLparam1(v);
    CAMLlocal1(clone);
    cm_3dt clone2;
    cm_3dt c;
    int len;
    clone = alloc_custom (&cost_matrix_3d, sizeof(struct cm), 1, 1000000);
    clone2 = Cost_matrix_struct_3d(clone);
    c = Cost_matrix_struct_3d(v);
    cm_set_val_3d (c->lcm, c->combinations, c->cost_model_type, \
            c->gap_open, c->all_elements, clone2);
    len = (c->a_sz + 1) * (c->a_sz + 1) * (c->a_sz + 1);
    cm_copy_contents (c->cost, clone2->cost, len);
    cm_copy_contents_seqt (c->median, clone2->median, len);
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
cm_CAML_get_a_sz_3d (value cm) {
    CAMLparam1 (cm);
    CAMLreturn (Val_int((Cost_matrix_struct_3d(cm))->a_sz));
}

value 
cm_CAML_get_a_sz (value cm) {
    CAMLparam1 (cm);
    CAMLreturn (Val_int((Cost_matrix_struct(cm))->a_sz));
}

value
cm_CAML_set_a_sz_3d (value v, value cm) {
    CAMLparam2 (cm, v);
    cm_set_a_sz_3d (Cost_matrix_struct_3d(cm), Int_val(v));
    CAMLreturn(Val_unit);
}


value
cm_CAML_set_a_sz (value cm, value v) {
    CAMLparam2 (cm, v);
    cm_set_a_sz (Cost_matrix_struct(cm), Int_val(v));
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
    cm_3dt tmp;
    tmp = Cost_matrix_struct_3d(cm);
    tcm = tmp->cost;
    CAMLreturn(Val_int(cm_calc_cost_3d(tcm, Int_val(a), Int_val(b), \
                    Int_val(c), tmp->lcm)));
}

value
cm_CAML_get_cost (value a, value b, value c) {
    CAMLparam3(a, b, c);
    int *tcm;
    cmt tmp;
    tmp = Cost_matrix_struct(c);
    tcm = tmp->cost;
    CAMLreturn(Val_int(cm_calc_cost(tcm, Int_val(a), Int_val(b), tmp->lcm)));
}

value
cm_CAML_get_worst (value a, value b, value c) {
    CAMLparam3(a, b, c);
    int *tcm;
    cmt tmp;
    tmp = Cost_matrix_struct(c);
    tcm = tmp->worst;
    CAMLreturn(Val_int(cm_calc_cost(tcm, Int_val(a), Int_val(b), tmp->lcm)));
}


value
cm_CAML_get_median_3d (value a, value b, value c, value cm) {
    CAMLparam3(a, b, c);
    SEQT *tcm;
    cm_3dt tmp;
    tmp = Cost_matrix_struct_3d(cm);
    tcm = tmp->median;
    CAMLreturn(Val_int(cm_calc_cost_3d_seqt(tcm, Int_val(a), Int_val(b), \
                    Int_val(c), tmp->lcm)));
}

#ifdef _WIN32
__inline SEQT
#else
inline SEQT
#endif
cm_get_median (const cmt tmp, SEQT a, SEQT b) {
    return (cm_calc_median((tmp->median), a, b, tmp->lcm));
}

#ifdef _WIN32
__inline SEQT
#else
inline SEQT
#endif
cm_get_median_3d (const cm_3dt t, SEQT a, SEQT b, SEQT c) {
    return (cm_calc_median_3d((t->median), a, b, c, t->lcm));
}

value
cm_CAML_get_median (value a, value b, value c) {
    CAMLparam3(a, b, c);
    SEQT *tcm;
    cmt tmp;
    tmp = Cost_matrix_struct(c);
    tcm = tmp->median;
    CAMLreturn(Val_int(cm_calc_median(tcm, Int_val(a), Int_val(b), tmp->lcm)));
}

value
cm_CAML_set_cost_3d (value a, value b, value c, value cc, value v) {
    CAMLparam5(a, b, c, cc, v);
    cm_3dt tmp;
    tmp = Cost_matrix_struct_3d(cc);
    cm_set_cost_3d (Int_val(a), Int_val(b), Int_val(c), Int_val(v), tmp);
    CAMLreturn(Val_unit);
}

value
cm_CAML_set_cost (value a, value b, value c, value v) {
    CAMLparam4(a, b, c, v);
    cmt tmp;
    tmp = Cost_matrix_struct(c);
    cm_set_cost (Int_val(a), Int_val(b), Int_val(v), tmp);
    CAMLreturn(Val_unit);
}

value 
cm_CAML_set_worst (value a, value b, value c, value v) {
    CAMLparam4(a, b, c, v);
    cmt tmp;
    tmp = Cost_matrix_struct(c);
    cm_set_worst (Int_val(a), Int_val(b), Int_val(v), tmp);
    CAMLreturn(Val_unit);
}

value
cm_CAML_set_median_3d (value a, value b, value c, value cp, value v) {
    CAMLparam4(a, b, c, v);
    cm_3dt tmp;
    tmp = Cost_matrix_struct_3d(cp);
    cm_set_median_3d (Int_val(a), Int_val(b), Int_val(c), Int_val(v), tmp);
    CAMLreturn(Val_unit);
}

value
cm_CAML_set_median (value a, value b, value c, value v) {
    CAMLparam4(a, b, c, v);
    cmt tmp;
    tmp = Cost_matrix_struct(c);
    cm_set_median (Int_val(a), Int_val(b), Int_val(v), tmp);
    CAMLreturn(Val_unit);
}

value
cm_CAML_set_prepend (value a, value b, value v) {
    CAMLparam3(a, b, v);
    cmt tmp;
    tmp = Cost_matrix_struct(v);
    cm_set_prepend (Int_val(a), Int_val(b), tmp);
    CAMLreturn(Val_unit);
}

value
cm_CAML_set_tail (value a, value b, value v) {
    CAMLparam3(a, b, v);
    cmt tmp;
    tmp = Cost_matrix_struct(v);
    cm_set_tail (Int_val(a), Int_val(b), tmp);
    CAMLreturn(Val_unit);
}

value 
cm_CAML_get_prepend (value a, value v) {
    CAMLparam2(a, v);
    int r;
    cmt tmp;
    tmp = Cost_matrix_struct(v);
    assert ((1 + tmp->a_sz) > Int_val(a));
    r = tmp->prepend_cost[Int_val(a)];
    CAMLreturn(Val_int(r));
}

value 
cm_CAML_get_tail (value a, value v) {
    CAMLparam2(a, v);
    int r;
    cmt tmp;
    tmp = Cost_matrix_struct(v);
    assert ((1 + tmp->a_sz) > Int_val(a));
    r = tmp->tail_cost[Int_val(a)];
    CAMLreturn(Val_int(r));
}

value
cm_CAML_create_3d (value a_sz, value combine, value aff, value go, value d, value all) {
    CAMLparam5(a_sz, combine, aff, go, d);
    CAMLxparam1(all);
    value tmp;
    cm_3dt tmp2;
    tmp = alloc_custom (&cost_matrix_3d, sizeof(struct cm_3d), 1, 1000000);
    tmp2 = Cost_matrix_struct_3d(tmp);
    cm_set_val_3d (Int_val(a_sz), Bool_val(combine), Int_val(aff), \
            Int_val(go), Int_val(all), tmp2);
    CAMLreturn(tmp);
}

value 
cm_CAML_create_3d_bc (value *argv, int argn) {
    return (cm_CAML_create_3d (argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]));
}

value
cm_CAML_create (value a_sz, value combine, value aff, value go, value all) {
    CAMLparam5(a_sz, combine, aff, go, all);
    value tmp;
    cmt tmp2;
    tmp = alloc_custom (&cost_matrix, sizeof(struct cm), 1, 1000000);
    tmp2 = Cost_matrix_struct(tmp);
    cm_set_val (Int_val(a_sz), Bool_val(combine), Int_val(aff), Int_val(go), \
            0, Int_val(all), tmp2);
    CAMLreturn(tmp);
}

value 
cm_CAML_set_lcm (value c, value v) {
    CAMLparam2(c, v);
    cmt tmp;
    tmp = Cost_matrix_struct(c);
    cm_set_lcm (tmp, Int_val(v));
    CAMLreturn(Val_unit);
}

value 
cm_CAML_set_lcm_3d (value c, value v) {
    CAMLparam2(c, v);
    cm_3dt tmp;
    tmp = Cost_matrix_struct_3d(c);
    cm_set_lcm_3d (tmp, Int_val(v));
    CAMLreturn(Val_unit);
}

value 
cm_CAML_get_lcm (value c) {
    CAMLparam1(c);
    cmt tmp;
    tmp = Cost_matrix_struct(c);
    CAMLreturn(Val_int(cm_get_lcm(tmp)));
}

value 
cm_CAML_get_lcm_3d (value c) {
    CAMLparam1(c);
    cm_3dt tmp;
    tmp = Cost_matrix_struct_3d(c);
    CAMLreturn(Val_int(cm_get_lcm_3d(tmp)));
}

value 
cm_CAML_clone_to_3d (value c) {
    CAMLparam1(c);
    CAMLlocal1(res);
    cmt init;
    cm_3dt final;
    init = Cost_matrix_struct(c);
    res = alloc_custom (&cost_matrix_3d, sizeof(struct cm_3d), 1, 1000000);
    final = Cost_matrix_struct_3d(res);
    cm_set_val_3d (init->lcm, init->combinations, init->cost_model_type, \
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
    cmt init;
    init = Cost_matrix_struct(c);
    init->is_metric = 1;
    CAMLreturn(Val_unit);
}

value
cm_CAML_get_is_metric (value c) {
    CAMLparam1(c);
    cmt init;
    init = Cost_matrix_struct(c);
    CAMLreturn(Val_int(init->is_metric));
}
