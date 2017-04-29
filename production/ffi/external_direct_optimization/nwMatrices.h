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

#ifndef NWMATRICES_H

#define NWMATRICES_H

/** The following consts are to define possible moves in an NW matrix.
 *  As we're only saving one possible matrix, we don't need ambiguities,
 *  Thus for 2d we only have 3 possible states, rather than 7.
 *
 *  Remember that we bias toward the shorter sequence, so INSERT puts a gap
 *  in the longer sequence and DELETE puts a gap in the shorter sequence. TODO: make sure shorter seq is on left
 *
 *  Likewise, for 3d we should need only 7 states and not 2^7 - 1.
 */
#define DIAGONAL (1 << 0)
#define BEHIND   (1 << 1)
#define UPPER    (1 << 2)
#define ALIGN    DIAGONAL
#define INSERT   BEHIND
#define DELETE   UPPER
#define SHIFT_V  3
#define SHIFT_H  6
#define ALIGN_V  (ALIGN << SHIFT_V)
#define DELETE_V (DELETE << SHIFT_V)
#define ALIGN_H  (ALIGN << SHIFT_H)
#define INSERT_H (INSERT << SHIFT_H)
#define G_A_G    (1 << 0)     /** Previously P1. Move in pages (i.e., put gaps in for 1 & 3). */
#define A_A_G    (1 << 1)     /** Previously P2. Move in column and page. */
#define A_G_G    (1 << 2)     /** Previously P3. Move in columns */
#define G_A_A    (1 << 3)     /** Previously S1. Move in page and row. */
#define A_A_A    (1 << 4)     /** Previously S2. Move in all three. */
#define A_G_A    (1 << 5)     /** Previously S3. Move in column and row. */
#define G_G_A    (1 << 6)     /** Previously SS. Move in rows. */

// TODO: Can this be a char, instead?
#define DIR_MTX_ARROW_t  unsigned short

#define Matrices_struct(a) ((struct nwMatrices *) Data_custom_val(a))

// TODO: change ints to unsigned ints?
struct nwMatrices {
            /****** In each of the following calculations, seq length includes opening gap *******/
    size_t            cap_nw;         /* Total length of available memory allocated to matrix or cube ==
                                       *   | for 2d: 12 * max(len_s1, len_s2)
                                       *   | for 3d: len_s1 * len_s2 * len_s3
                                       */
    int               cap_eff;        // Length of the efficiency matrix; at least as large as cap_nw.
                                      // int because is originally set as -1
                                      // TODO: figure out what this actually is
    size_t            cap_pre;        // Length of the precalculated matrix == max(len_s1, len_s2) * (alphSize + 1) ---extra 1 is for gap
    int              *nw_costMtx;     // NW cost matrix for both 2d and 3d alignment
    DIR_MTX_ARROW_t  *nw_dirMtx;      // Matrix for backtrace directions in a 2d alignment
    int              *nw_costMtx3d_d; // Matrix for 3d alignment, just a set of pointers into nw_costMtx -- alloced internally.
    DIR_MTX_ARROW_t  *nw_dirMtx3d_d;  // Matrix for backtrace directions in a 3d alignment, just a set of pointers
                                      //     into nw_costMtx --- alloced internally
    int              *precalcMtx;     /* a three-dimensional matrix that holds
                                       * the transition costs for the entire alphabet (of all three sequences)
                                       * with the sequence seq3. The columns are the bases of seq3, and the rows are
                                       * each of the alphabet characters (possibly including ambiguities). See
                                       * cm_precalc_4algn_3d for more information).
                                       */
};

typedef struct nwMatrices *nw_matrices_p;

void print_matrices(nw_matrices_p m, size_t alphSize);

/*
 * Calculates the amount of memory required to perform a three dimensional
 * alignment between sequences of length w, d, h with ukkonen barriers set to k
 */
size_t
mat_size_of_3d_matrix (size_t w, size_t d, size_t h); // originally had a fourth parameter, k for ukkunonen

/*
 * Calculates the amount of memory required to perform a two dimensional
 * alignment between sequences of length w and d. This is a small amount of
 * memory, so no ukkonen barriers for this.
 */
size_t
mat_size_of_2d_matrix (size_t w, size_t h);

/*
 * Rearrange or reallocate memory if necessary to perform an alignment between
 * sequences of length w, d and h. Note that for 2d alignments is necessary to
 * set h=0, and uk=0.
 * Order of sequences is unimportant here, as just reallocing.
 */
void
mat_setup_size (nw_matrices_p m, size_t len_seq1, size_t len_seq2, size_t len_seq3, size_t lcm);

/* Printout the contents of the matrix */
void
mat_print_algn_2d (nw_matrices_p m, size_t w, size_t h);

void
mat_print_algn_3d (nw_matrices_p m, size_t w, size_t h, size_t d);

#endif /* NWMATRICES_H */
