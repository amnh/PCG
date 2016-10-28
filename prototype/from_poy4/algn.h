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

/******************************************************************************/
/*                        Pairwise Standard Alignment                         */
/******************************************************************************/
/*
 * As standard, all the caml binding functions are called algn_CAML_<function
 * name>
 */

/** Fill a row in a two dimentional alignment
 *
 *  When pairwise alignment is performed, two sequences are compared over a
 *  transformation cost matrix. Let's call them sequences x and y written over
 *  some alphabet a of length |a|. Each base of x
 *  is represented by a column in the transformation cost matrix and each base of
 *  y by a row. However, note that the actual values that are added during the
 *  alignment are produced by comparing every single base of x with only |a|
 *  elements. Now, in order to to make these operations vectorizable, we perform
 *  the comparisons with |a| precalculated vectors. This puts in context the
 *  explanation of each parameter in the function.
 * 
 *  @param nwMtx is the cost matrix row to be filled with values.
 *  @param pm is the row above nwMtx in the cost matrix being filled.
 *  @param gap_row is the cost of aligning each base in x with a gap.
 *  @param alg_row is the cost of aligning each base in x wit hthe base
 *  represented by the base of the row of nwMtx in y.
 *  @param dirMtx is the directional matrix for the backtrace
 *  @param c is the cost of an insertion. As an insertion can only occur for one
 *  particular base in the alphabet, corresponding to the base in y represented
 *  by the row that is being filled.
 *  @param st is the starting cell for the filling.
 *  @param end is the final cell for the filling.
 *  If you modify this code check algn_fill_cube as there is sinwMtxilar code there
 *  used in the first plane of the alignment. It didn't use this function because
 *  the direction codes are different for three dimensional alignments.
 */ 

#ifndef ALGN_H

#define ALGN_H 1

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "algn.h"
#include "cm.h"
#include "debug.h"
#include "matrices.h"
#include "seq.h"
#include "zarr.h"

#define VERY_LARGE_NUMBER 100000 // large number, but as this gets added to itself repeatedly, small enough that it won't overflow. 

/*
 * As standard, all the caml binding functions are called algn_CAML_<function
 * name>
 */

static inline void
algn_fill_row (int *curRow, const int *prevRow, const int *gap_row, 
               const int *alg_row, DIRECTION_MATRIX *dirMtx, int c, int i, int end);

static inline int
algn_fill_plane (const seq_p seq1, int *precalcMtx, int seq1_len, 
                 int seq2_len, int *curRow, DIRECTION_MATRIX *dirMtx, const cost_matrices_2d_p c);

inline void
algn_fill_row_uk (int *nwMtx, const int *pm, const int *gap_row, 
                  const int *alg_row, unsigned char *dm, int c, int l, int lowerbound, 
                  int upperbound);

inline int
algn_fill_plane_uk (const struct seq *seq1, int *prec, int seq1_len, 
                    int seq2_len, int *nwMtx, unsigned char *dm, int uk, const struct cost_matrices_2d *c);

static inline void
fill_moved (int seq3_len, const int *prev_m, const int *upper_m, 
            const int *diag_m, const int *seq1_gap_seq3, const int *gap_seq2_seq3, 
            const int *seq1seq2seq3, int *curRow, DIRECTION_MATRIX *dirMtx);

static inline void
fill_parallel (int seq3_len, const int *prev_m, const int *upper_m, 
               const int *diag_m, int seq1_gap_gap, int gap_seq2_gap, int seq1_seq2_gap, int *curRow, 
               DIRECTION_MATRIX *dirMtx);

/**
 *  @param seq1 is a pointer to the sequence seq1 (vertical)
 *  @param seq2 is horizontal 1 
    **** Note that seq1 <= seq2 ****
 *
 *  @param precalcMtx is a pointer to the precalculated_cost_matrices, a 
 *    three-dimensional matrix that holds
 *    the transitionn costs for the entire alphabet (of all three sequences)
 *    with the sequence seq3. The columns are the bases of seq3, and the rows are 
 *    each of the alphabet characters (possibly including ambiguities). See 
 *    cm_precalc_4algn_3d for more information). 
 *  @param seq1_len, @param seq2_len and @param seq3_len are the lengths of the three 
 *    sequences to be aligned
 *  @param nwMtx is a pointer to the first element of the alignment cube that will
 *    hold the matrix of the dynamic programming algorithm, 
 *  @param dm holds the direction information for the backtrace. 
 *  @param uk is the value of the Ukkonen barriers (not used in this version of the program)
 * 
 * TODO: figure out wtf this means:
 *  consider all combinations:
 *  seq1, gap, gap   -> const for plane
 *  gap, seq2, gap   -> const const per row
 *  seq1, seq2, gap  -> const const per row
 *  gap, seq2, seq3  -> vector (changes on each row)
 *  seq1, gap, seq3  -> vector (change per plane)
 *  seq1, seq2, seq3 -> vector (changes on each row)
 *  gap, gap, seq3   -> vector (the last one to be done, not parallelizable)
 *
 *  All following fns have the same argument values, when present
 */

int
algn_fill_cube (const seq_p seq1, const seq_p seq2, const int *precalcMtx, 
                int seq1_len, int seq2_len, int seq3_len, int *curRow, DIRECTION_MATRIX *dirMtx, 
                int uk, int gap, int alphSize);

int
algn_nw_2d (const seq_p seq1, const seq_p seq2, const cost_matrices_2d_p c, nw_matrices_p m, int uk);

/** Creates N-W matrices, then does alignment
 *  
 */
int
algn_nw_3d (const seq_p seq1, const seq_p seq2, const seq_p seq3,
            const cost_matrices_3d_p c, nw_matrices_p m, int uk);

void
algn_print_bcktrck_2d (const seq_p seq1, const seq_p seq2, const nw_matrices_p m);

void
algn_print_dynmtrx_2d (const seq_p seq1, const seq_p seq2, nw_matrices_p m);

/** takes two previously aligned sequences, @param seq1 & @param seq2, for which some align function has been called,
 *  and extracts their
 *  edited version into @param ret_seq1 and @param ret_seq2, using the alignment matrix @param m and the transformation
 *  cost mstrix @param c. *Nota bene:* Make sure the m and c are the same as used in the alignment of
 *  the sequence for the call of cost_2. No check of an appropriate call of cost_2
 *  is made, therefore the behavior of the function in that case is undefined.
 *  As passed in, unaligned seq1 is always longer than seq2.
 *  If @param swapped == 1, then seq1 and seq2 are in their original order. Otherwise, len_seq2 > len_seq1
 *  so they have been switched before the call (meaning that seq1 is still the longest).
 *  Depending on the case, deletion or insertion may be biased toward either longer or shorter.
 *  @param st_seq1 and @param st_seq2 are 0 if there are no limits, have values otherwise.
 */
void
algn_backtrace_2d ( const seq_p seq1, const seq_p seq2, 
               seq_p ret_seq1, seq_p ret_seq2, 
               const nw_matrices_p m, const cost_matrices_2d_p c, 
               int st_seq1, int st_seq2, 
               int swapped 
              );

/** As backtrace_2d, but for three sequences */
void
algn_backtrace_3d (const seq_p seq1, const seq_p seq2, seq_p seq3, 
              seq_p r1, seq_p r2, seq_p r3, const cost_matrices_3d_p c, nw_matrices_p m);


inline void
algn_get_median_2d (seq_p seq1, seq_p seq2, cost_matrices_2d_p m, seq_p sm);

/* 
 * Given three aligned sequences seq1, seq2, and seq3, the median between them is
 * returned in the sequence sm, using the cost matrix stored in m.
 */
inline void
algn_get_median_3d (seq_p seq1, seq_p seq2, seq_p seq3, cost_matrices_3d_p m, seq_p sm);

// TODO: document following four fns
void
initialize_matrices_affine (int go, const seq_p si, const seq_p sj, 
                            const cost_matrices_2d_p c, 
                            int *close_block_diagonal, int *extend_block_diagonal, 
                            int *extend_vertical, int *extend_horizontal, int *final_cost_matrix, 
                            DIRECTION_MATRIX *direction_matrix, const int *precalcMtx);

// TODO: what is nobt? no backtrace? And why the 3? It's not 3d. Maybe third iteration of fn? In that case remove 3, as it's confusing.
int
algn_fill_plane_3_affine_nobt (const seq_p si, const seq_p sj, int leni, int lenj, 
                            const cost_matrices_2d_p c, int *extend_horizontal, int *extend_vertical, 
                            int *close_block_diagonal, int *extend_block_diagonal, const int *precalcMtx, 
                            int *gap_open_prec, int *sj_horizontal_extension);

void
backtrace_affine (DIRECTION_MATRIX *direction_matrix, const seq_p si, const seq_p sj, 
                  seq_p median, seq_p medianwg, seq_p resi, seq_p resj, const cost_matrices_2d_p c);

int
algn_fill_plane_3_affine (const seq_p si, const seq_p sj, int leni, int lenj, 
                       int *final_cost_matrix, DIRECTION_MATRIX *direction_matrix, 
                       const cost_matrices_2d_p c, int *extend_horizontal, int *extend_vertical, 
                       int *close_block_diagonal, int *extend_block_diagonal, const int *precalcMtx, 
                       int *gap_open_prec, int *sj_horizontal_extension);

void
algn_get_median_2d_no_gaps (seq_p seq1, seq_p seq2, cost_matrices_2d_p m, seq_p sm);

void
algn_get_median_2d_with_gaps (seq_p seq1, seq_p seq2, cost_matrices_2d_p m, seq_p sm);

void
algn_union (seq_p seq1, seq_p seq2, seq_p su);

#endif /* ALGN_H */
