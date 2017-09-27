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
#include <assert.h>
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/bigarray.h"
#include "caml/custom.h"
#include "caml/intext.h"
#include "caml/fail.h"
#include "cm.h"
#include "noaddset.h"
#include "sankoff.h"

#ifdef _WIN32
#define MAX_INT 2147483647
#endif

/*
 * In Sankoff characters, the metric can actually make no sense at all, so we
 * can't assume much about the properties of the transformation cost matrix.
 * We can actually evaluate several parameters from this matrix to improve the
 * computational cost (for example, if the transformation cost matrix is
 * actually a constant, we can treat them as non-additive characters!);
 * In this version of the function, no assumptions can be done; all we know is
 * that the intermediate and the cost between states has been precomputed in a
 * transformation cost matrix that can be easily handled using the cm library.
 * So here are the functions needed.
 * When a certain pair of children characters are to be evaluated, the cost of
 * each possible state for the character should be calculated. This function
 * stores the cost of each of the possible combinations. The ss in the function
 * means 'single character' as it should be applied on each chraracter at a
 * time. This is the slowest version of sankoff character state calculation
 * (from the precomputed set).
 * @param a_sz The cardinality of the state set.
 * @param ch1 An array with the cost of each of the possible states in the first
 * child of p.
 * @param ch2 An array with the cost of each of the possible states in the
 * second child of p.
 * @param p The parent of ch1 and ch2. Its contents are to be modified.
 * @param tcm A pointer to the precalculated transformation cost matrix for the
 * possible states of the character.
 * @return The total cost of the best possible transformation from ch1 to ch2.
 *
 * Brief Description of the algorithm:
 * 1. Initialize the costs to a large value */
/* 2. For each state in p, each state in ch1 and each state in ch2 */
/* 3. If a better cost is found, then store it. */
#ifdef _WIN32
__inline void
#else
inline void
#endif
sankoff_sc_down_pre (const int *ch1, const int *ch2, int *p, \
        const int *tcm, int a_sz) {
    int i, j, k;
    int *tmp_row, tmp_cost;
    for (i = 0; i < a_sz; i++) {
        *(p + i) = MAX_INT; /* 1. */
        for (j = 0; j < a_sz; j++) {
            tmp_row = cm_get_row_3d (tcm, i, j, a_sz);
            for (k = 0; k < a_sz; k++) {  /* 2. */
                tmp_cost = *(tmp_row + k) + *(ch1 + j) + *(ch2 + k);
                if (*(p + i) > tmp_cost) {
                    *(p + i) = tmp_cost;
                }
            }
        }
    }
    return;
}

/*
 * Each character has a total of a_sz possible states, therefore, the array is
 * compossed of small subarrays of size a_sz. For this reason, each subarray is
 * processed in the sankoff_sc_down_pre function, while in this function the
 * each character is processed. Therefore, the step of the for loop is of size
 * a_sz instead of the probably expected length 1.
 */
#ifdef _WIN32
__inline void
#else
inline void
#endif
sankoff_down_pre (struct storage ch1, struct storage ch2, struct storage p, \
        int *tcm, int a_sz, int n) {
    int i;
    int *cur_p, *cur_ch1, *cur_ch2, *cur_best;
    cur_ch1 = ch1.prel;
    cur_ch2 = ch2.prel;
    cur_p = p.prel;
    cur_best = p.best;
    for (i = 0; i < n; i++) {
        sankoff_sc_down_pre (cur_ch1, cur_ch2, cur_p, tcm, a_sz);
        cur_p += a_sz;
        cur_ch1 += a_sz;
        cur_ch2 += a_sz;
    }
    return;
}

/* CAML bindings (see character2.ml documentation) */
value
sankoff_CAML_downpass (value ch1, value ch2, value p, value mtx, value n) {
    CAMLparam5(ch1, ch2, p, cm, n);
    struct storage cch1, cch2, cp;
    int res;
    int *tcm, alphabet_size;
    cch1 = noaddset_create_structure (ch1, n);
    cch2 = noaddset_create_structure (ch2, n);
    cp = noaddset_create_structure (p, n);
    cmtx = Cost_matrix_struct(mtx);
    alphabet_size = cmtx->a_sz;
    tcm = cm_get_transformation_cost_matrix (cmtx);
    sankoff_down_pre (cch1, cch2, cp, tcm, alphabet_size, n);
    CAMLreturn(Val_unit);
}
