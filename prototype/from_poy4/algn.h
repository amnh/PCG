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

inline void
algn_fill_row (int *nwMtx, const int *pm, const int *gap_row, \
        const int *alg_row, unsigned char *dm, int c, int l);

inline int
algn_fill_plane (const seqt s1, int *prec, int s1_len, \
        int s2_len, int *nwMtx, unsigned char *dm, int uk, const cmt c);

inline void
algn_fill_row_uk (int *nwMtx, const int *pm, const int *gap_row, \
                  const int *alg_row, unsigned char *dm, int c, int l, int lowerbound, \
                  int upperbound);

inline int
algn_fill_plane_uk (const struct seq *s1, int *prec, int s1_len, \
        int s2_len, int *nwMtx, unsigned char *dm, int uk, const struct cm *c);

static inline void
fill_moved (int s3_len, int *prev_m, int *upper_m, int *diag_m, int *s1gs3, \
        int *gs2s3, int *s1s2s3, int *nwMtx);

static inline void
fill_parallel (int s3_len, int *prev_m, int *upper_m, int *diag_m, \
        int *s1gs3, int *gs2s3, int *s1s2s3, int *nwMtx);

/**
 *  s1 is a pointer to the sequence s1 (vertical)
 *  s2 is horizontal 1 
    **** Note that s1 <= s2 ****
 *
 *  prec is a pointer to the precalculated cm, a three-dimensional matrix that holds
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
algn_fill_cube (const seqt s1, const seqt s2, int *prec, \
        int s1_len, int s2_len, int s3_len, int *nwMtx, unsigned char *dm, int uk, \
        int gap, int alphSize);

int
algn_nw (const seqt s1, const seqt s2, const cmt c, matricest m, int uk);

inline int
algn_nw_3d (const seqt s1, const seqt s2, const seqt s3,
        const cm_3dt c, matricest m, int uk);

void
print_bcktrck (const seqt s1, const seqt s2, const matricest m);

void
print_dynmtrx (const seqt s1, const seqt s2, matricest m);

inline void
backtrack_2d (const seqt s1, const seqt s2, seqt r1, \
        seqt r2, const matricest m, const cmt c);

inline void
backtrack_3d (const seqt s1, const seqt s2, seqt s3, \
        seqt r1, seqt r2, seqt r3, matricest m, const cm_3dt c);

inline void
algn_get_median_2d (seqt s1, seqt s2, cmt m, seqt sm);

/* 
 * Given three aligned sequences s1, s2, and s3, the median between them is
 * returned in the sequence sm, using the cost matrix stored in m.
 */
inline void
algn_get_median_3d (seqt s1, seqt s2, seqt s3, cm_3dt m, seqt sm);

#endif /* ALGN_H */
