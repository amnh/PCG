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
#include <string.h>

#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/bigarray.h"
#include "caml/custom.h"
#include "caml/intext.h"
#include "caml/fail.h"
#include "cm.h"

#define DEBUG_CM 0

 int
ceil_log_2 (int v) {
    int i = 0;
    while (v != 0) {
        i++;
        v = v >> 1;
    }
    return (i + 1);
}

 int
cm_combinations_of_alphabet (const int alphSize) {
    assert (alphSize >= 0);
    return ((1 << alphSize) - 1); // ignore empty set
}

void cm_print (cmt c) {
    printf("\nCost matrix fields:\n");
    printf("  alphabet size: %d\n", c->alphSize);
    printf("  lcm:           %d\n", c->lcm);
    printf("  gap:           %d\n", c->gap);
    printf("  cost model:    %d\n", c->cost_model_type);
    printf("  combinations:  %d\n", c->combinations);
    printf("  gap open:      %d\n", c->gap_open);
    printf("  is metric:     %d\n", c->is_metric);
    printf("  all elements:  %d\n", c->all_elements);

    // printf("\n  Cost matrix:\n    ");
    // cm_print_matrix(c->cost, c->alphSize + 1, c->alphSize + 1);
    // printf("  Prepend costs:\n    ");
    // cm_print_matrix(c->prepend_cost, 1, c->alphSize);
    // printf("  Worst costs:\n    ");
    // cm_print_matrix(c->worst, c->alphSize + 1, c->alphSize + 1);
    // printf("  Tail costs:\n    ");
    // cm_print_matrix(c->tail_cost, 1, c->alphSize);
    // printf("  Median costs:\n    ");
    // cm_print_matrix(c->median, c->alphSize + 1, c->alphSize + 1);
}

void
cm_print_matrix (int* m, int w, int h) {
    size_t i, j;
    for (i = 0; i < h; i++) {
        //fprintf(stdout,"%zu: ", i);
        for (j = 0; j < w; j++)
            fprintf (stdout, "%4d", *(m + (w * i) + j));
        fprintf (stdout, "\n    ");
    }
    return;
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

static  void
cm_set_alphSize (cmt c, int v) {
    assert(c != NULL);
    c->alphSize = v;
    return;
}

static  void
cm_set_alphSize_3d (cm_3dt c, int v) {
    assert(c != NULL);
    c->alphSize = v;
    return;
}

static  void
cm_set_gap (cmt c, int v) {
    assert(c != NULL);
    c->gap = v;
    return;
}

static  void
cm_set_gap_3d (cm_3dt c, int v) {
    assert(c != NULL);
    c->gap = v;
    return;
}

static  void
cm_set_affine (cmt c, int do_aff, int go) {
    assert(c != NULL);
    c->cost_model_type = do_aff;
    c->gap_open = go;
    return;
}

static  void
cm_set_affine_3d (cm_3dt c, int do_aff, int go) {
    assert(c != NULL);
    c->cost_model_type = do_aff;
    c->gap_open = go;
    return;
}

 int
cm_get_lcm (cmt c) {
    assert(c != NULL);
    return (c->lcm);
}

 int
cm_get_lcm_3d (cm_3dt c) {
    assert(c != NULL);
    return (c->lcm);
}

static  void
cm_set_lcm (cmt c, int v) {
    assert(c != NULL);
    c->lcm = v;
    return;
}

static  void
cm_set_lcm_3d (cm_3dt c, int v) {
    assert(c != NULL);
    c->lcm = v;
    return;
}

static  void
cm_set_combinations (cmt c) {
    assert(c != NULL);
    c->combinations = 1;
    return;
}

static  void
cm_set_combinations_3d (cm_3dt c) {
    assert(c != NULL);
    c->combinations = 1;
    return;
}

static  void
cm_unset_combinations (cmt c) {
    assert(c != NULL);
    c->combinations = 0;
    return;
}

static  void
cm_unset_combinations_3d (cm_3dt c) {
    assert(c != NULL);
    c->combinations = 0;
    return;
}

 int
cm_get_combinations (cmt c) {
    assert(c != NULL);
    return (c->combinations);
}

 int
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
    // CAMLparam2(cm, v);
    cmt c;
    int i;
    c = Cost_matrix_struct(cm);
    i = Int_val(v);
    cm_set_all_elements (c, i);
    return 0; //CAMLreturn(Val_unit);
}

value
cm_CAML_set_all_elements_3d (value cm, value v) {
    // CAMLparam2(cm, v);
    cm_3dt c;
    int i;
    c = Cost_matrix_struct_3d(cm);
    i = Int_val(v);
    cm_set_all_elements_3d (c, i);
    return 0; //CAMLreturn(Val_unit);
}

value
cm_CAML_get_all_elements_3d (value cm) {
    // CAMLparam1(cm);
    cm_3dt c;
    c = Cost_matrix_struct_3d(cm);
    return 0; //CAMLreturn(Val_int(cm_get_all_elements_3d(c)));
}

value
cm_CAML_get_all_elements (value cm) {
    // CAMLparam1(cm);
    cmt c;
    c = Cost_matrix_struct(cm);
    return 0; //CAMLreturn(Val_int(cm_get_all_elements(c)));
}

/*
 * Creates a cost matrix with memory allocated for an alphabet of size alphSize
 * (not including the gap representation which is internally chosen), and whose
 * size must consider all possible combinations of characters in the alphabeet
 * iff combinations != 0. Set the affine gap model paramters to the values
 * stored in do_aff, gap_open, in the cost matrix res.
 * In case of error the function fails with the message "Memory error.".
 */
cmt
cm_set_val (int alphSize, int combinations, int do_aff, int gap_open, \
        int is_metric, int all_elements, cmt res) {
    if(DEBUG_CM) {
        printf("\n---cm_set_val\n");
        printf("alphabet size: %d\n", alphSize);
    }
    size_t size;
#ifndef USE_LARGE_ALPHABETS
    // if (alphSize > 255)
    //     failwith ("Apparently you are analyzing large alphabets. This version \
                of POY was compiled without the --enable-large-alphabets option. \
                To run this analysis you need to enable that option at compile time. \
                Either compile yourself the program, or request a version suited \
                for your needs in the POY mailing list (poy4@googlegroups.com).");
#endif
    if (combinations != 0) {
        cm_set_gap (res, 1 << (alphSize - 1));
        cm_set_alphSize (res, cm_combinations_of_alphabet (alphSize));
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
    size = 2 * (1 << (res->lcm)) * (1 << (res->lcm)) * sizeof(int);
    // if (0 == size)
    //     failwith ("Your cost matrix is too large to fit in your memory. I can't continue with your data loading.");
    res->cost = (int *) calloc (size, 1);
    res->worst = (int *) calloc (size, 1);
    res->prepend_cost = (int *) calloc (size, 1);
    res->tail_cost = (int *) calloc (size, 1);
    size = 2 * (1 << (res->lcm)) * (1 << (res->lcm)) * sizeof(SEQT);
    // if (0 == size)
    //     failwith ("Your cost matrix is too large to fit in your memory. I can't continue with your data loading.");
    res->median = (SEQT *) calloc (size, 1);
    if ((res->cost == NULL) || (res->median == NULL)) {
        free (res->cost);
        free (res->median);
        // failwith ("Memory error during cost matrix allocation.");
    }
    //printf("cm_get%d\n", );
    return res;
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
cm_set_val_3d (int alphSize, int combinations, int do_aff, int gap_open, \
        int all_elements, cm_3dt res) {
    int size;
    if (DEBUG_CM) {
        printf ("Allocating a three dimensional matrix:\n");
        printf ("alphabet size: %d \n", alphSize);
        printf ("combinations: %d \n", combinations);
        printf ("cost model: %d \n", do_aff);
        printf ("gap open cost: %d \n", gap_open);
    }
    if (combinations != 0) {
        cm_set_gap_3d (res, 1 << (alphSize - 1));
        cm_set_alphSize_3d (res, cm_combinations_of_alphabet (alphSize));
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
        // failwith ("Memory error during cost matrix allocation.");
    }
  //  return res;
}

 int
cm_get_alphabet_size (cmt c) {
    assert(c != NULL);
    return c->alphSize;
}

//  int
// cm_get_alphabet_size_3d (cm_3dt c) {
//     assert(c != NULL);
//     return c->alphSize;
// }

//  SEQT
// cm_get_gap (const cmt c) {
//     assert(c != NULL);
//     return c->gap;
// }

//  SEQT
// cm_get_gap_3d (const cm_3dt c) {
//     assert(c != NULL);
//     return c->gap;
// }

//  int
// cm_get_affine_flag (cmt c) {
//     assert(c != NULL);
//     return c->cost_model_type;
// }

//  int
// cm_get_affine_flag_3d (cm_3dt c) {
//     assert(c != NULL);
//     return c->cost_model_type;
// }

//  int
// cm_get_gap_opening_parameter (cmt c) {
//     assert(c != NULL);
//     return c->gap_open;
// }

//  int
// cm_get_gap_opening_parameter_3d (const cm_3dt c) {
//     assert(c != NULL);
//     return c->gap_open;
// }

//  int
// cm_calc_cost_position (int a, int b, int alphSize) {
//     assert(alphSize >= 0);
//     return ((a << alphSize) + b);
// }

//  int
// cm_calc_cost_position_seqt (SEQT a, SEQT b, int alphSize) {
//     assert(alphSize >= 0);
//     return ((((int) a) << alphSize) + ((int) b));
// }

//  int
// cm_calc_cost_position_3d_seqt (SEQT a, SEQT b, SEQT c, int alphSize) {
//     assert(alphSize >= 0);
//     return ((((((int) a) << alphSize) + ((int) b)) << alphSize) + ((int) c));
// }

//  int
// cm_calc_cost_position_3d (int a, int b, int c, int alphSize) {
//     assert(alphSize >= 0);
//     return ((((a << alphSize) + b) << alphSize) + c);
// }

//  SEQT
// cm_calc_median (SEQT *tcm, SEQT a, SEQT b, int alphSize) {
//     SEQT *res;
//     assert (alphSize >= 0);
//     assert ((1 << alphSize) > a);
//     assert ((1 << alphSize) > b);
//     res = tcm + cm_calc_cost_position_seqt (a, b, alphSize);
//     return (*res);
// }

//  int
// cm_calc_cost (int *tcm, SEQT a, SEQT b, int alphSize) {
//     int *res;
//     assert (alphSize >= 0);
//     assert ((1 << alphSize) > a);
//     assert ((1 << alphSize) > b);
//     res = tcm + cm_calc_cost_position_seqt (a, b, alphSize);
//     return (*res);
// }

//  SEQT
// cm_calc_median_3d (SEQT *tcm, SEQT a, SEQT b, SEQT c, int alphSize) {
//     if (alphSize <= 0) failwith ("Alphabet size = 2");
//     if ((1 << alphSize) <= a) failwith ("2a is bigger than alphabet size");
//     if ((1 << alphSize) <= b) failwith ("b is bigger than alphabet size");
//     return (*(tcm + cm_calc_cost_position_3d (a, b, c, alphSize)));
// }

//  int
// cm_calc_cost_3d (int *tcm, SEQT a, SEQT b, SEQT c, int alphSize) {
//     if (alphSize <= 0) failwith ("Alphabet size = 2");
//     if ((1 << alphSize) <= a) failwith ("2a is bigger than alphabet size");
//     if ((1 << alphSize) <= b) failwith ("b is bigger than alphabet size");
//     return (*(tcm + cm_calc_cost_position_3d (a, b, c, alphSize)));
// }

//  SEQT
// cm_calc_cost_3d_seqt (SEQT *tcm, SEQT a, SEQT b, SEQT c, int alphSize) {
//     if (alphSize <= 0) failwith ("Alphabet size = 2");
//     if ((1 << alphSize) <= a) failwith ("2a is bigger than alphabet size");
//     if ((1 << alphSize) <= b) failwith ("b is bigger than alphabet size");
//     return (*(tcm + cm_calc_cost_position_3d (a, b, c, alphSize)));
// }

//  int
// cm_calc_tmm (int *tmm, int a, int b, int alphSize) {
//     return (cm_calc_cost (tmm, a, b, alphSize));
// }

//  int
// cm_calc_median_position (SEQT a, SEQT b, int alphSize) {
//     return (cm_calc_cost_position (a, b, alphSize));
// }

// /*
//  * Position of the first memory location of the transformation cost matrix given
//  * a bigarray from ocaml.
//  */
//  int *
// cm_get_transformation_cost_matrix (const cmt a) {
//     return (a->cost);
// }

//  int *
// cm_get_tail_cost (const cmt a) {
//     return (a->tail_cost);
// }

//  int *
// cm_get_prepend_cost (const cmt a) {
//     return (a->prepend_cost);
// }

//  int *
// cm_get_transformation_cost_matrix_3d (const cm_3dt a) {
//     return (a->cost);
// }

//  int *
// cm_get_row (int *tcm, SEQT a, int alphSize) {
//     if (alphSize <= 0) failwith ("Alphabet size = 3");
//     if ((1 << alphSize) <= a) failwith ("3a is bigger than alphabet size");
//     return (tcm + (a << alphSize));
// }

//  int *
// cm_get_row_3d (int *tcm, SEQT a, SEQT b, int alphSize) {
//     if (alphSize <= 0) failwith ("Alphabet size = 4");
//     if ((1 << alphSize) <= a) failwith ("4a is bigger than alphabet size");
//     if ((1 << alphSize) <= b) failwith ("b is bigger than alphabet size");
//     return (tcm + (((a << alphSize) + b) << alphSize));
// }

//  void
// cm_set_value_seqt (SEQT a, SEQT b, SEQT v, SEQT *p, int alphSize) {
//     *(p + (cm_calc_cost_position_seqt (a, b, alphSize))) = v;
//     return;
// }

//  void
// cm_set_value (int a, int b, int v, int *p, int alphSize) {
//     *(p + (cm_calc_cost_position (a, b, alphSize))) = v;
//     return;
// }

void
cm_precalc_4algn (const cmt c, matricest matrix, const seqt s) {
    if(DEBUG_CM) {
        printf("\n---cm_precalc_4algn\n");
    }
    int i, j, l, m, *tmp_cost, *tcm, *tmp_to, *prepend, *tail, *to;
    SEQT *begin;
    l = seq_get_len (s);
    to = mat_get_2d_prec (matrix);
    tcm = cm_get_transformation_cost_matrix (c);
    prepend = cm_get_prepend_cost (c);
    tail = cm_get_tail_cost (c);
    tmp_to = to + l;
    begin = seq_get_begin (s);         /* Inlined seq_get for speed purposes */
    if (DEBUG_CM) {
        printf ("Precalculated transformation cost matrix.\n");
    }
    /* We will use the 0'th row to store the cost of the prepend */
    for (m = 0; m < l; m++) {
        to[m] = prepend[begin[m]];
        if (DEBUG_CM) {
            printf ("%d\t", to[m]);
        }
    }
    // printf("\n");
    for (j = 1; j <= c->alphSize; j++, tmp_to += l) {
        // if (DEBUG_CM) {
        //     printf("%d\t", j);
        // }
        tmp_cost = cm_get_row (tcm, j, c->lcm);
        /* We fill almost the complete row. Only the first (aligning with the
         * gap), is filled using the tail cost */
        tmp_to[0] = tail[j];
        if (DEBUG_CM) {
            printf ("%d\t", tmp_to[0]);
        }
        for (i = 1; i < l; i++) {
            tmp_to[i] = tmp_cost[begin[i]];
            if (DEBUG_CM) {
                printf ("%d\t", tmp_to[i]);
            }
        }
        if (DEBUG_CM) {
            printf ("\n");
        }
    }
    if (DEBUG_CM) {
        printf ("Finished printing transforamtion cost matrix\n");
    }
    return;
}

const int *
cm_get_precal_row (const int *p, SEQT item, int len) {
    return (p + (len * item));
}

 const int *
cm_get_pos_in_precalc (const int *to, int s3l, int alphSize, int s1c, int s2c, \
        int s3p) {
    int *res;
    alphSize++;
    res = (int *) to + ((s1c * (alphSize * s3l)) + (s3l * s2c) + s3p);
    return (res);
}

const  int *
cm_get_row_precalc_3d (const int *to, int s3l, int alphSize, int s1c, int s2c) {
    return (cm_get_pos_in_precalc (to, s3l, alphSize, s1c, s2c, 0));
}

void
cm_precalc_4algn_3d (const cm_3dt c, int *to, const seqt s) {
    int i, j, k, l, *tmp_cost, *tcm;
    int sequen, *precalc_pos;
    l = seq_get_len (s);
    tcm = cm_get_transformation_cost_matrix_3d (c);
    for (j = 1; j < c->alphSize + 1; j++)
        for (k = 1; k < c->alphSize + 1; k++) {
            tmp_cost = cm_get_row_3d (tcm, j, k, c->lcm);
            for (i = 0; i < l; i++) {
                sequen = seq_get (s, i);
                precalc_pos = (int *) cm_get_pos_in_precalc (to, l, c->alphSize, j, k, i);
                *precalc_pos = *(tmp_cost + sequen);
            }
        }
    return;
}

//  void
// cm_set_value_3d_seqt (SEQT a, SEQT b, SEQT c, SEQT v, SEQT *p, int alphSize) {
//     *(p + (cm_calc_cost_position_3d_seqt (a, b, c, alphSize))) = v;
//     return;
// }

//  void
// cm_set_value_3d (int a, int b, int c, int v, int *p, int alphSize) {
//     *(p + (cm_calc_cost_position_3d (a, b, c, alphSize))) = v;
//     return;
// }

//  void
// cm_set_cost (int a, int b, int v, cmt c) {
//     cm_set_value (a, b, v, c->cost, c->lcm);
//     return;
// }


//  void
// cm_set_worst (int a, int b, int v, cmt c) {
//     cm_set_value (a, b, v, c->worst, c->lcm);
//     return;
// }




//  void
// cm_set_cost_3d (int a, int b, int cp, int v, cm_3dt c) {
//     cm_set_value_3d (a, b, cp, v, c->cost, c->lcm);
//     return;
// }

//  void
// cm_set_prepend (int a, int b, cmt c) {
//     c->prepend_cost[a] = b;
//     return;
// }

//  void
// cm_set_tail (int a, int b, cmt c) {
//     c->tail_cost[a] = b;
//     return;
// }

//  void
// cm_set_median (SEQT a, SEQT b, SEQT v, cmt c) {
//     cm_set_value_seqt (a, b, v, c->median, c->lcm);
//     return;
// }

//  void
// cm_set_median_3d (SEQT a, SEQT b, SEQT cp, SEQT v, cm_3dt c) {
//     cm_set_value_3d_seqt (a, b, cp, v, c->median, c->lcm);
//     return;
// }

unsigned long
cm_CAML_deserialize (void *v) {
    cmt n;
    int len;
    n = (cmt) v;
    // n->alphSize = deserialize_sint_4();
    // n->lcm = deserialize_sint_4();
    // n->gap = deserialize_sint_4();
    // n->cost_model_type = deserialize_sint_4();
    // n->combinations = deserialize_sint_4();
    // n->gap_open = deserialize_sint_4();
    // n->is_metric = deserialize_sint_4();
    // n->all_elements = deserialize_sint_4();
    len = 2 * (1 << (n->lcm)) * (1 << n->lcm) ;
    n->cost = (int *) calloc (len * sizeof(int), 1);
    n->median = (SEQT *) calloc (len * sizeof(SEQT), 1);
    n->worst = (int *) calloc (len * sizeof(int), 1);
    n->prepend_cost = (int *) calloc (len * sizeof(int), 1);
    n->tail_cost = (int *) calloc (len * sizeof(int), 1);
    // if ((n->cost == NULL) || (n->median == NULL)) failwith ("Memory error.");
    // deserialize_block_4(n->cost, len);
    // DESERIALIZE_SEQT(n->median, len);
    // deserialize_block_4(n->worst, len);
    // deserialize_block_4(n->prepend_cost, len);
    // deserialize_block_4(n->tail_cost, len);
    return (sizeof(struct cm));
}

unsigned long
cm_CAML_deserialize_3d (void *v) {
    cm_3dt n;
    int len;
    n = (cm_3dt) v;
    // n->alphSize = deserialize_sint_4();
    // n->lcm = deserialize_sint_4();
    // n->gap = deserialize_sint_4();
    // n->cost_model_type = deserialize_sint_4();
    // n->combinations = deserialize_sint_4();
    // n->gap_open = deserialize_sint_4();
    // n->all_elements = deserialize_sint_4();
    len = (1 << (n->lcm + 1)) * (1 << (n->lcm + 1)) * (1 << (n->lcm + 1));
    n->cost = (int *) calloc (len * sizeof(int), 1);
    n->median = (SEQT *) calloc (len * sizeof(SEQT), 1);
//    if ((n->cost == NULL) || (n->median == NULL)) failwith ("Memory error.");
    // deserialize_block_4(n->cost, len);
    // DESERIALIZE_SEQT(n->median, len);
    return (sizeof(struct cm_3d));
}

void
cm_CAML_serialize (value vcm, unsigned long *wsize_32, \
        unsigned long *wsize_64) {
    cmt c;
    int len;
    if (DEBUG_CM) {
        printf ("I will serialize cm!\n");
        fflush (stdout);
    }
    c = Cost_matrix_struct(vcm);
    // serialize_int_4(c->alphSize);
    // serialize_int_4(c->lcm);
    // serialize_int_4(c->gap);
    // serialize_int_4(c->cost_model_type);
    // serialize_int_4(c->combinations);
    // serialize_int_4(c->gap_open);
    // serialize_int_4(c->is_metric);
    // serialize_int_4(c->all_elements);
    *wsize_64 = *wsize_32 = sizeof(struct cm);
    len = 2 * (1 << (c->lcm)) * (1 << (c->lcm));
    // serialize_block_4(c->cost, len);
    // // SERIALIZE_SEQT(c->median, len);
    // serialize_block_4(c->worst, len);
    // serialize_block_4(c->prepend_cost, len);
    // serialize_block_4(c->tail_cost, len);
    return;
}

void
cm_CAML_serialize_3d (value vcm, unsigned long *wsize_32, \
        unsigned long *wsize_64) {
    cm_3dt c;
    int len;
    c = Cost_matrix_struct_3d(vcm);
    // serialize_int_4(c->alphSize);
    // serialize_int_4(c->lcm);
    // serialize_int_4(c->gap);
    // serialize_int_4(c->cost_model_type);
    // serialize_int_4(c->combinations);
    // serialize_int_4(c->gap_open);
    // serialize_int_4(c->all_elements);
    *wsize_64 = *wsize_32 = sizeof(struct cm_3d);
    len = (1 << (c->lcm + 1)) * (1 << (c->lcm + 1)) * (1 << (c->lcm + 1));
    // serialize_block_4(c->cost, len);
    // SERIALIZE_SEQT(c->median, len);
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
    // cm_CAML_serialize,
    // cm_CAML_deserialize
};

static struct custom_operations cost_matrix_3d = {
    "http://www.amnh.org/poy/cost_matrix/three_dimensional.0.1",
    &cm_CAML_free_3d,
    &cm_compare_3d,
    custom_hash_default,
    // cm_CAML_serialize_3d,
    // cm_CAML_deserialize_3d
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
    // CAMLparam1(v);
    value clone;
    cmt clone2;
    cmt c;
    int len;
    // clone = alloc_custom (&cost_matrix, sizeof(struct cm), 1, 1000000);
    clone2 = Cost_matrix_struct(clone);
    c = Cost_matrix_struct(v);
    if (c->combinations)
        cm_set_val (c->lcm, c->combinations, c->cost_model_type, \
                c->gap_open, c->is_metric, c->all_elements, clone2);
    else
        cm_set_val (c->alphSize, c->combinations, c->cost_model_type, \
                c->gap_open, c->is_metric, c->all_elements, clone2);
    len = 2 *(1 << (c->lcm)) * (1 << (c->lcm));
    cm_copy_contents (c->cost, clone2->cost, len);
    cm_copy_contents_seqt (c->median, clone2->median, len);
    cm_copy_contents (c->worst, clone2->worst, len);
    cm_copy_contents (c->prepend_cost, clone2->prepend_cost, len);
    cm_copy_contents (c->tail_cost, clone2->tail_cost, len);
    return 0; //CAMLreturn(clone);
}

value
cm_CAML_clone_3d (value v) {
    // CAMLparam1(v);
    // CAMLlocal1(clone);
    cm_3dt clone2;
    cm_3dt c;
    int len;
    // clone = alloc_custom (&cost_matrix_3d, sizeof(struct cm), 1, 1000000);
    // clone2 = Cost_matrix_struct_3d(clone);
    c = Cost_matrix_struct_3d(v);
    cm_set_val_3d (c->lcm, c->combinations, c->cost_model_type, \
            c->gap_open, c->all_elements, clone2);
    len = (c->alphSize + 1) * (c->alphSize + 1) * (c->alphSize + 1);
    cm_copy_contents (c->cost, clone2->cost, len);
    cm_copy_contents_seqt (c->median, clone2->median, len);
    return 0; //CAMLreturn(clone);
}

value
cm_CAML_set_gap_3d (value c, value v) {
    // CAMLparam2 (c, v);
    cm_set_gap_3d (Cost_matrix_struct_3d(c), Int_val(v));
    return 0; //CAMLreturn (Val_unit);
}

value
cm_CAML_set_gap (value c, value v) {
    // CAMLparam2 (c, v);
    cm_set_gap (Cost_matrix_struct(c), Int_val(v));
    return 0; //CAMLreturn (Val_unit);
}

value
cm_CAML_set_affine_3d (value c, value do_aff, value go) {
    // CAMLparam3(c, do_aff, go);
    cm_set_affine_3d (Cost_matrix_struct_3d(c), Int_val(do_aff), Int_val(go));
    return 0; //CAMLreturn(Val_unit);
}

value
cm_CAML_set_affine (value c, value do_aff, value go) {
    // CAMLparam3(c, do_aff, go);
    cm_set_affine (Cost_matrix_struct(c), Int_val(do_aff), Int_val(go));
    return 0; //CAMLreturn(Val_unit);
}

value
cm_CAML_get_alphSize_3d (value cm) {
    // CAMLparam1 (cm);
    return 0; //CAMLreturn (Val_int((Cost_matrix_struct_3d(cm))->alphSize));
}

value
cm_CAML_get_alphSize (value cm) {
    // CAMLparam1 (cm);
    return 0; //CAMLreturn (Val_int((Cost_matrix_struct(cm))->alphSize));
}

value
cm_CAML_set_alphSize_3d (value v, value cm) {
    // CAMLparam2 (cm, v);
    cm_set_alphSize_3d (Cost_matrix_struct_3d(cm), Int_val(v));
    return 0; //CAMLreturn(Val_unit);
}


value
cm_CAML_set_alphSize (value cm, value v) {
    // CAMLparam2 (cm, v);
    cm_set_alphSize (Cost_matrix_struct(cm), Int_val(v));
    return 0; //CAMLreturn(Val_unit);
}


value
cm_CAML_get_gap_3d (value c) {
    // CAMLparam1 (c);
    return 0; //CAMLreturn (Val_int((cm_get_gap_3d (Cost_matrix_struct_3d(c)))));
}

value
cm_CAML_get_gap (value c) {
    // CAMLparam1 (c);
    return 0; //CAMLreturn (Val_int((cm_get_gap (Cost_matrix_struct(c)))));
}

value
cm_CAML_get_affine_3d (value c) {
    // CAMLparam1(c);
    return 0; //CAMLreturn(Val_int(cm_get_affine_flag_3d (Cost_matrix_struct_3d(c))));
}

value
cm_CAML_get_affine (value c) {
    // CAMLparam1(c);
    return 0; //CAMLreturn(Val_int(cm_get_affine_flag (Cost_matrix_struct(c))));
}

value
cm_CAML_get_gap_opening_3d (value c) {
    // CAMLparam1(c);
    int i;
    i = cm_get_gap_opening_parameter_3d(Cost_matrix_struct_3d(c));
    return 0; //CAMLreturn(Val_int(i));
}

value
cm_CAML_get_gap_opening (value c) {
    // CAMLparam1(c);
    int i;
    i = cm_get_gap_opening_parameter(Cost_matrix_struct(c));
    return 0; //CAMLreturn(Val_int(i));
}

value
cm_CAML_get_combinations (value c) {
    // CAMLparam1(c);
    int i;
    i = cm_get_combinations (Cost_matrix_struct(c));
    return 0; //CAMLreturn(Val_int(i));
}

value
cm_CAML_get_combinations_3d (value c) {
    // CAMLparam1(c);
    int i;
    i = cm_get_combinations_3d (Cost_matrix_struct_3d(c));
    return 0; //CAMLreturn(Val_int(i));
}

value
cm_CAML_get_cost_3d (value a, value b, value c, value cm) {
    // CAMLparam3(a, b, c);
    int *tcm;
    cm_3dt tmp;
    tmp = Cost_matrix_struct_3d(cm);
    tcm = tmp->cost;
    return 0; //CAMLreturn(Val_int(cm_calc_cost_3d(tcm, Int_val(a), Int_val(b), \
                    Int_val(c), tmp->lcm)));
}

value
cm_CAML_get_cost (value a, value b, value c) {
    // CAMLparam3(a, b, c);
    int *tcm;
    cmt tmp;
    tmp = Cost_matrix_struct(c);
    tcm = tmp->cost;
    return 0; //CAMLreturn(Val_int(cm_calc_cost(tcm, Int_val(a), Int_val(b), tmp->lcm)));
}

value
cm_CAML_get_worst (value a, value b, value c) {
    // CAMLparam3(a, b, c);
    int *tcm;
    cmt tmp;
    tmp = Cost_matrix_struct(c);
    tcm = tmp->worst;
    return 0; //CAMLreturn(Val_int(cm_calc_cost(tcm, Int_val(a), Int_val(b), tmp->lcm)));
}


value
cm_CAML_get_median_3d (value a, value b, value c, value cm) {
    // CAMLparam3(a, b, c);
    SEQT *tcm;
    cm_3dt tmp;
    tmp = Cost_matrix_struct_3d(cm);
    tcm = tmp->median;
    return 0; //CAMLreturn(Val_int(cm_calc_cost_3d_seqt(tcm, Int_val(a), Int_val(b), \
                    Int_val(c), tmp->lcm)));
}

 SEQT
cm_get_median (const cmt tmp, SEQT a, SEQT b) {
    return (cm_calc_median((tmp->median), a, b, tmp->lcm));
}

 SEQT
cm_get_median_3d (const cm_3dt t, SEQT a, SEQT b, SEQT c) {
    return (cm_calc_median_3d((t->median), a, b, c, t->lcm));
}

value
cm_CAML_get_median (value a, value b, value c) {
    // CAMLparam3(a, b, c);
    SEQT *tcm;
    cmt tmp;
    tmp = Cost_matrix_struct(c);
    tcm = tmp->median;
    return 0; //CAMLreturn(Val_int(cm_calc_median(tcm, Int_val(a), Int_val(b), tmp->lcm)));
}

value
cm_CAML_set_cost_3d (value a, value b, value c, value cc, value v) {
    // CAMLparam5(a, b, c, cc, v);
    cm_3dt tmp;
    tmp = Cost_matrix_struct_3d(cc);
    cm_set_cost_3d (Int_val(a), Int_val(b), Int_val(c), Int_val(v), tmp);
    return 0; //CAMLreturn(Val_unit);
}

value
cm_CAML_set_cost (value a, value b, value c, value v) {
    // CAMLparam4(a, b, c, v);
    cmt tmp;
    tmp = Cost_matrix_struct(c);
    cm_set_cost (Int_val(a), Int_val(b), Int_val(v), tmp);
    return 0; //CAMLreturn(Val_unit);
}

value
cm_CAML_set_worst (value a, value b, value c, value v) {
    // CAMLparam4(a, b, c, v);
    cmt tmp;
    tmp = Cost_matrix_struct(c);
    cm_set_worst (Int_val(a), Int_val(b), Int_val(v), tmp);
    return 0; //CAMLreturn(Val_unit);
}

value
cm_CAML_set_median_3d (value a, value b, value c, value cp, value v) {
    // CAMLparam4(a, b, c, v);
    cm_3dt tmp;
    tmp = Cost_matrix_struct_3d(cp);
    cm_set_median_3d (Int_val(a), Int_val(b), Int_val(c), Int_val(v), tmp);
    return 0; //CAMLreturn(Val_unit);
}

value
cm_CAML_set_median (value a, value b, value c, value v) {
    // CAMLparam4(a, b, c, v);
    cmt tmp;
    tmp = Cost_matrix_struct(c);
    cm_set_median (Int_val(a), Int_val(b), Int_val(v), tmp);
    return 0; //CAMLreturn(Val_unit);
}

value
cm_CAML_set_prepend (value a, value b, value v) {
    // CAMLparam3(a, b, v);
    cmt tmp;
    tmp = Cost_matrix_struct(v);
    cm_set_prepend (Int_val(a), Int_val(b), tmp);
    return 0; //CAMLreturn(Val_unit);
}

value
cm_CAML_set_tail (value a, value b, value v) {
    // CAMLparam3(a, b, v);
    cmt tmp;
    tmp = Cost_matrix_struct(v);
    cm_set_tail (Int_val(a), Int_val(b), tmp);
    return 0; //CAMLreturn(Val_unit);
}

value
cm_CAML_get_prepend (value a, value v) {
    // CAMLparam2(a, v);
    int r;
    cmt tmp;
    tmp = Cost_matrix_struct(v);
    assert ((1 + tmp->alphSize) > Int_val(a));
    r = tmp->prepend_cost[Int_val(a)];
    return 0; //CAMLreturn(Val_int(r));
}

value
cm_CAML_get_tail (value a, value v) {
    // CAMLparam2(a, v);
    int r;
    cmt tmp;
    tmp = Cost_matrix_struct(v);
    assert ((1 + tmp->alphSize) > Int_val(a));
    r = tmp->tail_cost[Int_val(a)];
    return 0; //CAMLreturn(Val_int(r));
}

value
cm_CAML_create_3d (value alphSize, value combine, value aff, value go, value d, value all) {
    // CAMLparam5(alphSize, combine, aff, go, d);
    // CAMLcparam1(all);
    value tmp;
    cm_3dt tmp2;
    // tmp = alloc_custom (&cost_matrix_3d, sizeof(struct cm_3d), 1, 1000000);
    tmp2 = Cost_matrix_struct_3d(tmp);
    cm_set_val_3d (Int_val(alphSize), Bool_val(combine), Int_val(aff), \
            Int_val(go), Int_val(all), tmp2);
    return 0; //CAMLreturn(tmp);
}

value
cm_CAML_create_3d_bc (value *argv, int argn) {
    return (cm_CAML_create_3d (argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]));
}

value
cm_CAML_create (value alphSize, value combine, value aff, value go, value all) {
    // CAMLparam5(alphSize, combine, aff, go, all);
    value tmp;
    cmt tmp2;
    // tmp = alloc_custom (&cost_matrix, sizeof(struct cm), 1, 1000000);
    tmp2 = Cost_matrix_struct(tmp);
    cm_set_val (Int_val(alphSize), Bool_val(combine), Int_val(aff), Int_val(go), \
            0, Int_val(all), tmp2);
    return 0; //CAMLreturn(tmp);
}

value
cm_CAML_set_lcm (value c, value v) {
    // CAMLparam2(c, v);
    cmt tmp;
    tmp = Cost_matrix_struct(c);
    cm_set_lcm (tmp, Int_val(v));
    return 0; //CAMLreturn(Val_unit);
}

value
cm_CAML_set_lcm_3d (value c, value v) {
    // CAMLparam2(c, v);
    cm_3dt tmp;
    tmp = Cost_matrix_struct_3d(c);
    cm_set_lcm_3d (tmp, Int_val(v));
    return 0; //CAMLreturn(Val_unit);
}

value
cm_CAML_get_lcm (value c) {
    // CAMLparam1(c);
    cmt tmp;
    tmp = Cost_matrix_struct(c);
    return 0; //CAMLreturn(Val_int(cm_get_lcm(tmp)));
}

value
cm_CAML_get_lcm_3d (value c) {
    // CAMLparam1(c);
    cm_3dt tmp;
    tmp = Cost_matrix_struct_3d(c);
    return 0; //CAMLreturn(Val_int(cm_get_lcm_3d(tmp)));
}

value
cm_CAML_clone_to_3d (value c) {
    // CAMLparam1(c);
    // CAMLlocal1(res);
    cmt init;
    cm_3dt final;
    init = Cost_matrix_struct(c);
    // res = alloc_custom (&cost_matrix_3d, sizeof(struct cm_3d), 1, 1000000);
    // final = Cost_matrix_struct_3d(res);
    cm_set_val_3d (init->lcm, init->combinations, init->cost_model_type, \
            init->gap_open, init->all_elements, final);
    return 0; //CAMLreturn(res);
}

value
cm_CAML_initialize (value unit) {
    // CAMLparam1(unit);
    // caml_register_custom_operations (&cost_matrix);
    // caml_register_custom_operations (&cost_matrix_3d);
    return 0; //CAMLreturn(Val_unit);
}

value
cm_CAML_set_is_metric (value c) {
    // CAMLparam1(c);
    cmt init;
    init = Cost_matrix_struct(c);
    init->is_metric = 1;
    return 0; //CAMLreturn(Val_unit);
}

value
cm_CAML_get_is_metric (value c) {
    // CAMLparam1(c);
    cmt init;
    init = Cost_matrix_struct(c);
    return 0; //CAMLreturn(Val_int(init->is_metric));
}


 int
cm_calc_cost_position_3d (int a, int b, int c, int alphSize) {
    assert(alphSize >= 0);
    return ((((a << alphSize) + b) << alphSize) + c);
}


void
cm_set_value_3d (int a, int b, int c, int v, int *p, int alphSize) {
    *(p + (cm_calc_cost_position_3d (a, b, c, alphSize))) = v;
    return;
}

 void
cm_set_cost_3d (int a, int b, int cp, int v, cm_3dt c) {
    cm_set_value_3d (a, b, cp, v, c->cost, c->lcm);
    return;
}

 void
cm_set_prepend (int a, int b, cmt c) {
    c->prepend_cost[a] = b;
    return;
}

 void
cm_set_tail (int a, int b, cmt c) {
    c->tail_cost[a] = b;
    return;
}

 int
cm_calc_cost_position_seqt (SEQT a, SEQT b, int alphSize) {
    assert(alphSize >= 0);
    return ((((int) a) << alphSize) + ((int) b));
}

 void
cm_set_value_seqt (SEQT a, SEQT b, SEQT v, SEQT *p, int alphSize) {
    *(p + (cm_calc_cost_position_seqt (a, b, alphSize))) = v;
    return;
}

 void
cm_set_median (SEQT a, SEQT b, SEQT v, cmt c) {
    cm_set_value_seqt (a, b, v, c->median, c->lcm);
    return;
}

 int
cm_calc_cost_position_3d_seqt (SEQT a, SEQT b, SEQT c, int alphSize) {
    assert(alphSize >= 0);
    return ((((((int) a) << alphSize) + ((int) b)) << alphSize) + ((int) c));
}

 void
cm_set_value_3d_seqt (SEQT a, SEQT b, SEQT c, SEQT v, SEQT *p, int alphSize) {
    *(p + (cm_calc_cost_position_3d_seqt (a, b, c, alphSize))) = v;
    return;
}

 void
cm_set_median_3d (SEQT a, SEQT b, SEQT cp, SEQT v, cm_3dt c) {
    cm_set_value_3d_seqt (a, b, cp, v, c->median, c->lcm);
    return;
}

 int
cm_calc_cost_position (int a, int b, int alphSize) {
    assert(alphSize >= 0);
    return ((a << alphSize) + b);
}

 void
cm_set_value (int a, int b, int v, int *p, int alphSize) {
    *(p + (cm_calc_cost_position (a, b, alphSize))) = v;
    return;
}

 void
cm_set_cost (int a, int b, int v, cmt c) {
    cm_set_value (a, b, v, c->cost, c->lcm);
    return;
}


 void
cm_set_worst (int a, int b, int v, cmt c) {
    cm_set_value (a, b, v, c->worst, c->lcm);
    return;
}

 int
cm_get_alphabet_size_3d (cm_3dt c) {
    assert(c != NULL);
    return c->alphSize;
}

 SEQT
cm_get_gap (const cmt c) {
    assert(c != NULL);
    return c->gap;
}

 SEQT
cm_get_gap_3d (const cm_3dt c) {
    assert(c != NULL);
    return c->gap;
}

 int
cm_get_affine_flag (cmt c) {
    assert(c != NULL);
    return c->cost_model_type;
}

 int
cm_get_affine_flag_3d (cm_3dt c) {
    assert(c != NULL);
    return c->cost_model_type;
}

 int
cm_get_gap_opening_parameter (cmt c) {
    assert(c != NULL);
    return c->gap_open;
}

 int
cm_get_gap_opening_parameter_3d (const cm_3dt c) {
    assert(c != NULL);
    return c->gap_open;
}



 SEQT
cm_calc_median (SEQT *tcm, SEQT a, SEQT b, int alphSize) {
    SEQT *res;
    assert (alphSize >= 0);
    assert ((1 << alphSize) > a);
    assert ((1 << alphSize) > b);
    res = tcm + cm_calc_cost_position_seqt (a, b, alphSize);
    return (*res);
}

 int
cm_calc_cost (int *tcm, SEQT a, SEQT b, int alphSize) {
    int *res;
    assert (alphSize >= 0);
    assert ((1 << alphSize) > a);
    assert ((1 << alphSize) > b);
    res = tcm + cm_calc_cost_position_seqt (a, b, alphSize);
    return (*res);
}

 SEQT
cm_calc_median_3d (SEQT *tcm, SEQT a, SEQT b, SEQT c, int alphSize) {
    // if (alphSize <= 0) failwith ("Alphabet size = 2");
    // if ((1 << alphSize) <= a) failwith ("2a is bigger than alphabet size");
    // if ((1 << alphSize) <= b) failwith ("b is bigger than alphabet size");
    return (*(tcm + cm_calc_cost_position_3d (a, b, c, alphSize)));
}

 int
cm_calc_cost_3d (int *tcm, SEQT a, SEQT b, SEQT c, int alphSize) {
    // if (alphSize <= 0) failwith ("Alphabet size = 2");
    // if ((1 << alphSize) <= a) failwith ("2a is bigger than alphabet size");
    // if ((1 << alphSize) <= b) failwith ("b is bigger than alphabet size");
    return (*(tcm + cm_calc_cost_position_3d (a, b, c, alphSize)));
}

 SEQT
cm_calc_cost_3d_seqt (SEQT *tcm, SEQT a, SEQT b, SEQT c, int alphSize) {
    // if (alphSize <= 0) failwith ("Alphabet size = 2");
    // if ((1 << alphSize) <= a) failwith ("2a is bigger than alphabet size");
    // if ((1 << alphSize) <= b) failwith ("b is bigger than alphabet size");
    return (*(tcm + cm_calc_cost_position_3d (a, b, c, alphSize)));
}

 int
cm_calc_tmm (int *tmm, int a, int b, int alphSize) {
    return (cm_calc_cost (tmm, a, b, alphSize));
}

 int
cm_calc_median_position (SEQT a, SEQT b, int alphSize) {
    return (cm_calc_cost_position (a, b, alphSize));
}

/*
 * Position of the first memory location of the transformation cost matrix given
 * a bigarray from ocaml.
 */
 int *
cm_get_transformation_cost_matrix (const cmt a) {
    return (a->cost);
}

 int *
cm_get_tail_cost (const cmt a) {
    return (a->tail_cost);
}

 int *
cm_get_prepend_cost (const cmt a) {
    return (a->prepend_cost);
}

 int *
cm_get_transformation_cost_matrix_3d (const cm_3dt a) {
    return (a->cost);
}

 int *
cm_get_row (int *tcm, SEQT a, int alphSize) {
    // if (alphSize <= 0) failwith ("Alphabet size = 3");
    // if ((1 << alphSize) <= a) failwith ("3a is bigger than alphabet size");
    return (tcm + (a << alphSize));
}

 int *
cm_get_row_3d (int *tcm, SEQT a, SEQT b, int alphSize) {
    // if (alphSize <= 0) failwith ("Alphabet size = 4");
    // if ((1 << alphSize) <= a) failwith ("4a is bigger than alphabet size");
    // if ((1 << alphSize) <= b) failwith ("b is bigger than alphabet size");
    return (tcm + (((a << alphSize) + b) << alphSize));
}