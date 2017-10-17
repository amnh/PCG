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

/* Alignment libray in C */

#ifndef ALGN_H

#define ALGN_H 1

#include <stdio.h>
#include <malloc.h>
#include <stdio.h>
#include <assert.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include "matrices.h"
#include "cm.h"
#include "seq.h"

/*
 * As standard, all the caml binding functions are called algn_CAML_<function
 * name>
 */

inline void
algn_fill_row (int *mm, const int *pm, const int *gap_row, \
        const int *alg_row, unsigned char *dm, int c, int l);

inline int
algn_fill_plane (const seqt s1, int *prec, int s1_len, \
        int s2_len, int *mm, unsigned char *dm, int uk, const cmt c);

inline void
algn_fill_row_uk (int *mm, const int *pm, const int *gap_row, \
        const int *alg_row, unsigned char *dm, int c, int l, int lowerbound \
        int upperbound);

inline int
algn_fill_plane_uk (const struct seq *s1, int *prec, int s1_len, \
        int s2_len, int *mm, unsigned char *dm, int uk, const struct cm *c);

static inline void
fill_moved (int s3_len, int *prev_m, int *upper_m, int *diag_m, int *s1gs3, \
        int *gs2s3, int *s1s2s3, int *mm);

static inline void
fill_parallel (int s3_len, int *prev_m, int *upper_m, int *diag_m, \
        int *s1gs3, int *gs2s3, int *s1s2s3, int *mm);

/*
 * s1 is a pointer to the sequence s1 (vertical), and s2 (horizontal 1) defined
 * in the same way. prec is a pointer to the three dimensional matrix that holds
 * the alignment values for all the combinations of the alphabet of sequences
 * s1, s2 and s3, with the sequence s3 (see cm_precalc_4algn_3d for more
 * information). s1_len, s2_len and s3_len is the length of the three sequences
 * to be aligned, and *mm is the first element of the alignment cube that will
 * hold the matrix of the dynamic programming algorithm, while dm does the same
 * job, holding the direction information for the backtrack. uk is the value of
 * the Ukkonen barriers (not used in this version of the program
 * conside all combinations
 * s1, g, g -> const for plane
 * g, s2, g -> const const per row
 * s1, s2, g -> const const per row
 * g, s2, s3 -> vector (changes on each row)
 * s1, g, s3 -> vector (change per plane)
 * s1, s2, s3 -> vector (changes on each row)
 * g, g, s3 -> vector (the last one to be done, not parallelizable
 *
 */
inline int
algn_fill_cube (const seqt s1, const seqt s2, int *prec, \
        int s1_len, int s2_len, int s3_len, int *mm, unsigned char *dm, int uk, \
        int gap, int a_sz);

inline int
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
