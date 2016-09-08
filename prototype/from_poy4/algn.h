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
 *  @param dirMtx is the directional matrix for the backtrack
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

#include "matrices.h"
#include "cm.h"
#include "seq.h"

/*
 * As standard, all the caml binding functions are called algn_CAML_<function
 * name>
 */

static inline void
algn_fill_row (int *curRow, const int *prevRow, const int *gap_row, 
               const int *alg_row, DIRECTION_MATRIX *dirMtx, int c, int i, int end);

static inline int
algn_fill_plane (const seq_p s1, int *precalcMtx, int s1_len, 
                 int s2_len, int *curRow, DIRECTION_MATRIX *dirMtx, const cost_matrices_p c);

inline void
algn_fill_row_uk (int *nwMtx, const int *pm, const int *gap_row, \
                  const int *alg_row, unsigned char *dm, int c, int l, int lowerbound, \
                  int upperbound);

inline int
algn_fill_plane_uk (const struct seq *s1, int *prec, int s1_len, \
        int s2_len, int *nwMtx, unsigned char *dm, int uk, const struct cost_matrices *c);

static inline void
fill_moved (int s3_len, const int *prev_m, const int *upper_m, 
            const int *diag_m, const int *s1gs3, const int *gs2s3, 
            const int *s1s2s3, int *curRow, DIRECTION_MATRIX *dirMtx);

static inline void
fill_parallel (int s3_len, const int *prev_m, const int *upper_m, 
               const int *diag_m, int s1gg, int gs2g, int s1s2g, int *curRow, 
               DIRECTION_MATRIX *dirMtx);

/**
 *  s1 is a pointer to the sequence s1 (vertical)
 *  s2 is horizontal 1 
    **** Note that s1 <= s2 ****
 *
 *  prec is a pointer to the precalculatedcost_matrices a three-dimensional matrix that holds
 *    the transitionn costs for the entire alphabet (of all three sequences)
 *    with the sequence s3. The columns are the bases of seq3, and the rows are 
 *    each of the alphabet characters (possibly including ambiguities). See 
 *    cm_precalc_4algn_3d for more information). 
 *  s1_len, s2_len and s3_len are the lengths of the three sequences
 *    to be aligned
 *  nwMtx is a pointer to the first element of the alignment cube that will
 *    hold the matrix of the dynamic progranwMtxing algorithm, 
 *  dm holds the direction information for the backtrack. 
 *  uk is the value of the Ukkonen barriers (not used in this version of the program)
 * 
 * TODO: figure out what this means:
 *  consider all combinations:
 *  s1, g, g -> const for plane
 *  g, s2, g -> const const per row
 *  s1, s2, g -> const const per row
 *  g, s2, s3 -> vector (changes on each row)
 *  s1, g, s3 -> vector (change per plane)
 *  s1, s2, s3 -> vector (changes on each row)
 *  g, g, s3 -> vector (the last one to be done, not parallelizable)
 *
 *  All following fns have the same argument values, when present
 */
inline int
algn_fill_cube (const seq_p s1, const seq_p s2, const int *precalcMtx, 
                int s1_len, int s2_len, int s3_len, int *curRow, DIRECTION_MATRIX *dirMtx, 
                int uk, int gap, int alphSize);

int
algn_nw (const seq_p s1, const seq_p s2, const cost_matrices_p c, nw_matrices_p m, int uk);

inline int
algn_nw_3d (const seq_p s1, const seq_p s2, const seq_p s3,
        const cm_3dt c, nw_matrices_p m, int uk);

void
print_bcktrck (const seq_p s1, const seq_p s2, const nw_matrices_p m);

void
print_dynmtrx (const seq_p s1, const seq_p s2, nw_matrices_p m);

void
backtrack_2d ( const seq_p s1, const seq_p s2, 
               seq_p r1, seq_p r2, 
               const nw_matrices_p m, const cost_matrices_p c, 
               int st_s1, int st_s2, 
               int algn_s1, int algn_s2, 
               int swapped 
              );

void
backtrack_3d (const seq_p s1, const seq_p s2, seq_p s3, \
        seq_p r1, seq_p r2, seq_p r3, nw_matrices_p m, const cm_3dt c);

inline void
algn_get_median_2d (seq_p s1, seq_p s2, cost_matrices_p m, seq_p sm);

/* 
 * Given three aligned sequences s1, s2, and s3, the median between them is
 * returned in the sequence sm, using the cost matrix stored in m.
 */
inline void
algn_get_median_3d (seq_p s1, seq_p s2, seq_p s3, cm_3dt m, seq_p sm);

#endif /* ALGN_H */
