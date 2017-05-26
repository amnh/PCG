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

#include "costMatrix.h"

/** The following consts are to define possible moves in an NW matrix.
 *  As we're only saving one possible matrix, we don't need ambiguities,
 *  Thus for 2d we only have 3 possible states, rather than 7.
 *
 *  Remember that we bias toward the shorter character, so INSERT puts a gap
 *  in the longer character and DELETE puts a gap in the shorter character. TODO: make sure shorter character is on left
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

#define Matrices_struct(a) ((struct nwMatrices_t *) Data_custom_val(a))

typedef struct alignment_matrices_t {
            /****** In each of the following calculations, character length includes opening gap *******/
    size_t           cap_nw;           /** Total length of available memory allocated to matrix or cube ==
                                        *    | for 2d: 12 * max(len_s1, len_s2)
                                        *    | for 3d: len_s1 * len_s2 * len_s3
                                        */
    size_t           cap_eff;          /** Length of the efficiency matrix; at least as large as cap_nw.
                                        *  int because is originally set as -1
                                        */ // TODO: figure out what this actually is
    size_t           cap_pre;          /** Length of the precalculated matrix == max(len_s1, len_s2) * (alphSize + 1)
                                        *  ---extra 1 is for gap
                                        */
    unsigned int    *algn_costMtx;     /** NW cost matrix for both 2d and 3d alignment */
    DIR_MTX_ARROW_t *algn_dirMtx;      /** Matrix for backtrace directions in a 2d alignment */
    unsigned int    *algn_costMtx3d;   /** Matrix for 3d alignment, just a set of pointers into nw_costMtx
                                        *  --- alloced internally.
                                        */
    DIR_MTX_ARROW_t *algn_dirMtx3d;    /** Matrix for backtrace directions in a 3d alignment, just a set of pointers into nw_costMtx
                                        *  --- alloced internally
                                        */
    unsigned int    *algn_precalcMtx;  /** a three-dimensional matrix that holds
                                         *  the transition costs for the entire alphabet (of all three characters)
                                         *  with the character char3. The columns are the bases of char3, and the rows are
                                         *  each of the alphabet characters (possibly including ambiguities). See
                                         *  cm_precalc_4algn_3d for more information).
                                         */
} alignment_matrices_t;

void algnMtx_print(alignment_matrices_t *m, size_t alphSize);


/*
 * Fills a precalculated matrix with the cost of comparing each elment in the
 * character inChar with each element in the alphabet specified in the transformation
 * cost matrix costMtx.
 *
 * @param costMtx is the transformation cost matrix to calculate the precalculated
 *  vectors.
 * @param toOutput is the matrix that will hold the output.
 * @param s is the character for which the cost matrix will be precalculated.
 *
 * This function is only valid for two dimensional alignments.
 * TODO: why is this in cm instead of matrices?
 */
void
algnMtx_precalc_4algn_2d(       alignment_matrices_t *alignmentMatrices
                        , const cost_matrices_2d_t   *costMatrix
                        , const dyn_character_t      *inChar);


/*
 * Fills a three-dimensional precalculation alignment matrix for char3
 * See algnMtx_precalc_4algn_2d for further information. This is the
 * corresponding function for three dimensional alignments.
 */
void
algnMtx_precalc_4algn_3d(       unsigned int       *outPrecalcMtx
                        , const cost_matrices_3d_t *costMtx
                        , const dyn_character_t    *char3);


unsigned int *
algnMtx_get_precal_row ( unsigned int *p
                       , elem_t        item
                       , size_t        len
                       );


/*
 * Retrieves a pointer to the memory position stored in the precalculated array
 * of costs for the alphabet in three dimensions, vs. a character s3. This is
 * used in the 3d alignments procedures for vectorization. to is a pointer to
 * the precalculated cube, s3l is the length of the character included in the
 * precalculated cube, alphSize is the costMtxDimension of the alphabet of the character, s1c is
 * the character from s1, and s2c is defined in an analogous manner. s3p is the
 * position in the character s3 of interest. s3p should be less than s3l.
 */
//static inline int *
//algnMtx_get_pos_in_precalc (const int *toOutput, int s3l, int alphSize, int s1c, int s2c, int s3p);

/*
 * During the 3d alignments, calculations are performed for each element in the
 * array using the complete vectors of the precalculated matrix. This function
 * retrieves the first element in those precalculated arrays. The parameters
 * definitions are analogous to those explained in cm_get_pos_in_precalc
 */
unsigned int *
algnMtx_get_row_precalc_3d ( unsigned int *outPrecalcMtx
                      , size_t        char3Len
                      , size_t        alphSize
                      , size_t        char1idx
                      , size_t        char2idx
                      );


/*
 * Retrieves a pointer to the memory position stored in the precalculated array
 * of costs for the alphabet in three dimensions, vs. a character s3. This is
 * used in the 3d alignments procedures for vectorization. to is a pointer to
 * the precalculated cube, s3l is the length of the character included in the
 * precalculated cube, alphSize is the costMtxDimension of the alphabet of the character, s1c is
 * the character from s1, and s2c is defined in an analogous manner. s3p is the
 * position in the character s3 of interest. s3p should be less than s3l.
 */
//static inline int *
//cm_get_pos_in_precalc (const int *toOutput, int s3l, int alphSize, int s1c, int s2c, int s3p);

/*
 * During the 3d alignments, calculations are performed for each element in the
 * array using the complete vectors of the precalculated matrix. This function
 * retrieves the first element in those precalculated arrays. The parameters
 * definitions are analogous to those explained in cm_get_pos_in_precalc
 */
unsigned int *
algnMtx_get_row_precalc_3d ( unsigned int *outPrecalcMtx
                      , size_t        char3Len
                      , size_t        alphSize
                      , size_t        char1idx
                      , size_t        char2idx
                      );


/*
 * Calculates the amount of memory required to perform a three dimensional
 * alignment between characters of length w, d, h with ukkonen barriers set to k
 */
size_t
algnMat_size_of_3d_matrix (size_t w, size_t d, size_t h); // originally had a fourth parameter, k for ukkunonen

/*
 * Calculates the amount of memory required to perform a two dimensional
 * alignment between characters of length w and d. This is a small amount of
 * memory, so no ukkonen barriers for this.
 */
size_t
algnMat_size_of_2d_matrix (size_t w, size_t h);

/*
 * Rearrange or reallocate memory if necessary to perform an alignment between
 * characters of length w, d and h. Note that for 2d alignments is necessary to
 * set h=0, and uk=0.
 * Order of characters is unimportant here, as just reallocing.
 */
void
algnMat_setup_size (alignment_matrices_t *m, size_t len_char1, size_t len_char2, size_t len_char3, size_t lcm);

/* Printout the contents of the matrix */
void
algnMat_print_algn_2d (alignment_matrices_t *m, size_t w, size_t h);

void
algnMat_print_algn_3d (alignment_matrices_t *m, size_t w, size_t h, size_t d);

#endif /* NWMATRICES_H */
