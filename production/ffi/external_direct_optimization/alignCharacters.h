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
 *  When pairwise alignment is performed, two characters are compared over a
 *  transformation cost matrix. Let's call them characters x and y written over
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
 *  If you modify this code check algn_fill_3dMtx as there is sinwMtxilar code there
 *  used in the first plane of the alignment. It didn't use this function because
 *  the direction codes are different for three dimensional alignments.
 */

#ifndef ALIGN_CHARACTERS_H
#define ALIGN_CHARACTERS_H

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

//#include "alignCharacters.h"
#include "costMatrix.h"
#include "debug_constants.h"
#include "alignmentMatrices.h"
#include "dyn_character.h"

// TODO: consider changing this number
#define VERY_LARGE_NUMBER 100000 // large number, but as this gets added to itself repeatedly, small enough that it won't overflow.

/*
 * As standard, all the caml binding functions are called algn_CAML_<function
 * name>
 */
/*
static inline void
algn_fill_row (       int *curRow
              , const int *prevRow
              , const int *gap_row
              , const int *alg_row
              ,       DIR_MTX_ARROW_t *dirMtx
              ,       int c
              ,       int i
              ,       int end
              );
*/

/*
static inline int
algn_fill_plane ( const dyn_char_p char1
                ,       int *precalcMtx
                ,       size_t char1_len
                ,       size_t char2_len
                ,       int *curRow
                ,       DIR_MTX_ARROW_t *dirMtx
                , const cost_matrices_2d_t *c
                );
*/


/* These two seem to be missing in .c file:
inline void
algn_fill_row_uk (int *nwMtx, const int *pm, const int *gap_row,
                  const int *alg_row, unsigned char *dm, int c, int l, int lowerbound,
                  int upperbound);

inline int
algn_fill_plane_uk (const dyn_character_t *char1, int *prec, int char1_len,
                    int char2_len, int *nwMtx, unsigned char *dm, int uk, const struct cost_matrices_2d *c);
*/


/*
static inline void
fill_moved (       size_t char3_len
           , const int *prev_m
           , const int *upper_m
           , const int *diag_m
           , const int *char1_gap_char3
           , const int *gap_char2_char3
           , const int *char1_char2_char3
           ,       int *curRow
           ,       DIR_MTX_ARROW_t *dirMtx
           );
*/


void
fill_parallel (       size_t char3_len
              , const int *prev_m
              , const int *upper_m
              , const int *diag_m
              ,       int char1_gap_gap
              ,       int gap_char2_gap
              ,       int char1_char2_gap
              ,       int *curRow
              ,       DIR_MTX_ARROW_t *dirMtx
              );

/**
 *  @param char1 is a pointer to the character char1 (vertical)
 *  @param char2 is horizontal 1
    **** Note that char1 <= char2 ****
 *
 *  @param precalcMtx is a pointer to the precalculated_cost_matrices, a
 *    three-dimensional matrix that holds
 *    the transitionn costs for the entire alphabet (of all three characters)
 *    with the character char3. The columns are the bases of char3, and the rows are
 *    each of the alphabet characters (possibly including ambiguities). See
 *    cm_precalc_4algn_3d for more information).
 *  @param char1_len, @param char2_len and @param char3_len are the lengths of the three
 *    characters to be aligned
 *  @param nwMtx is a pointer to the first element of the alignment 3dMtx that will
 *    hold the matrix of the dynamic programming algorithm,
 *  @param dm holds the direction information for the backtrace.
 *  @param uk is the value of the Ukkonen barriers (not used in this version of the program)
 *
 * TODO: figure out wtf this means:
 *  consider all combinations:
 *  char1, gap, gap   -> const for plane
 *  gap, char2, gap   -> const const per row
 *  char1, char2, gap  -> const const per row
 *  gap, char2, char3  -> vector (changes on each row)
 *  char1, gap, char3  -> vector (change per plane)
 *  char1, char2, char3 -> vector (changes on each row)
 *  gap, gap, char3   -> vector (the last one to be done, not parallelizable)
 *
 *  All following fns have the same argument values, when present
 */

int
algn_fill_3dMtx ( const dyn_char_p       char1
                , const dyn_char_p       char2
                , const int             *precalcMtx
                ,       size_t           char1_len
                ,       size_t           char2_len
                ,       size_t           char3_len
                ,       int             *curRow
                ,       DIR_MTX_ARROW_t *dirMtx
                // ,       int              uk
                ,       elem_t           gap_char
                ,       size_t           alphSize
                );

int
algn_nw_2d ( const dyn_char_p            char1
           , const dyn_char_p            char2
           , const cost_matrices_2d_t   *c
           ,       alignment_matrices_t *nwMtxs
           ,       int                   uk
           );

/** Creates N-W matrices, then does alignment
 *  deltawh is width of ukkonnen barrier
 */
int
algn_nw_3d ( const dyn_char_p            char1
           , const dyn_char_p            char2
           , const dyn_char_p            char3
           , const cost_matrices_3d_t   *costMtx
           ,       alignment_matrices_t *nwMtxs
        // , int deltawh
           );

void
algn_print_bcktrck_2d (const dyn_char_p char1, const dyn_char_p char2, const alignment_matrices_t *m);

void
algn_print_dynmtrx_2d (const dyn_char_p char1, const dyn_char_p char2, alignment_matrices_t *m);

/** takes two previously aligned characters, @param char1 & @param char2, for which some align function has been called,
 *  and extracts their
 *  edited version into @param ret_char1 and @param ret_char2, using the alignment matrix @param m and the transformation
 *  cost mstrix @param c. *Nota bene:* Make sure the m and c are the same as used in the alignment of
 *  the character for the call of cost_2. No check of an appropriate call of cost_2
 *  is made, therefore the behavior of the function in that case is undefined.
 *  As passed in, unaligned char is always shorter than char2.
 *  If @param swapped == 1, then char1 and char2 are in their original order. Otherwise, len_char2 > len_char1
 *  so they have been switched before the call (meaning that char1 is still the shortest).
 *  Depending on the case, deletion or insertion may be biased toward either longer or shorter.
 *  @param st_char1 and @param st_char2 are 0 if there are no limits, have values otherwise.
 */
void
algn_backtrace_2d ( const dyn_char_p char1
                  , const dyn_char_p char2
                  ,       dyn_char_p ret_char1
                  ,       dyn_char_p ret_char2
                  , const alignment_matrices_t *nwMatrix
                  , const cost_matrices_2d_t *costMatrix
                  ,       int st_char1
                  ,       int st_char2
                  ,       int swapped
                  );

/** As backtrace_2d, but for three characters */
void
algn_backtrace_3d ( const dyn_char_p char1
                  , const dyn_char_p char2
                  ,       dyn_char_p char3
                  ,       dyn_char_p r1
                  ,       dyn_char_p r2
                  ,       dyn_char_p r3
                  , const cost_matrices_3d_t *costMatrix
                  ,       alignment_matrices_t *nwMatrix
                  );

/* replaced with gaps and no gaps versions below
inline void
algn_get_median_2d (dyn_char_p char1, dyn_char_p char2, cost_matrices_2d_t *m, dyn_char_p sm);
*/

/*
 * Given three aligned characters char1, char2, and char3, the median between them is
 * returned in the character sm, using the cost matrix stored in m.
 */
void
algn_get_medians_3d ( dyn_char_p         char1
                    , dyn_char_p         char2
                    , dyn_char_p         char3
                    , cost_matrices_3d_t *costMatrix
                    , dyn_char_p         ungapped_median
                    , dyn_char_p         gapped_median
                    );

// TODO: document following four fns
void
algn_initialize_matrices_affine (       unsigned int        gap_open_cost
                                , const dyn_char_p          shortChar
                                , const dyn_char_p          longChar
                                , const cost_matrices_2d_t *costMatrix
                                ,       unsigned int       *close_block_diagonal
                                ,       unsigned int       *extend_block_diagonal
                                ,       unsigned int       *extend_vertical
                                ,       unsigned int       *extend_horizontal
                                ,       unsigned int       *final_cost_matrix
                                ,       DIR_MTX_ARROW_t    *direction_matrix
                                , const unsigned int       *precalcMtx
                                );

// TODO: what is nobt? no backtrace? And why the 3? It's not 3d. Maybe third iteration of fn? In that case remove 3, as it's confusing.
int
algn_fill_plane_2d_affine_nobt ( const dyn_char_p         si
                               , const dyn_char_p         sj
                               ,       size_t             leni
                               ,       size_t             lenj
                               , const cost_matrices_2d_t *c
                               ,       unsigned int       *extend_horizontal
                               ,       unsigned int       *extend_vertical
                               ,       unsigned int       *close_block_diagonal
                               ,       unsigned int       *extend_block_diagonal
                               , const unsigned int       *precalcMtx
                               ,       unsigned int       *gap_open_prec
                               ,       unsigned int       *sj_horizontal_extension
                               );

void
algn_backtrace_affine ( const dyn_char_p          shortChar
                      , const dyn_char_p          longChar
                      ,       DIR_MTX_ARROW_t    *direction_matrix
                      ,       dyn_char_p          median
                      ,       dyn_char_p          medianwg
                      ,       dyn_char_p          resultShort
                      ,       dyn_char_p          resultLong
                      , const cost_matrices_2d_t *costMatrix
                      );

int
algn_fill_plane_2d_affine ( const dyn_char_p          si
                          , const dyn_char_p          sj
                          ,       size_t              leni
                          ,       size_t              lenj
                          , const unsigned int       *final_cost_matrix
                          ,       DIR_MTX_ARROW_t    *direction_matrix
                          , const cost_matrices_2d_t *costMatrix
                          , const unsigned int       *extend_horizontal
                          , const unsigned int       *extend_vertical
                          , const unsigned int       *close_block_diagonal
                          , const unsigned int       *extend_block_diagonal
                          , const unsigned int       *precalcMtx
                          , const unsigned int       *gap_open_prec
                          , const unsigned int       *sj_horizontal_extension
                          );

void
algn_get_median_2d_no_gaps ( dyn_char_p          char1
                           , dyn_char_p          char2
                           , cost_matrices_2d_t *costMatrix
                           , dyn_char_p          sm
                           );

void
algn_get_median_2d_with_gaps ( dyn_char_p          char1
                             , dyn_char_p          char2
                             , cost_matrices_2d_t *costMatrix
                             , dyn_char_p          sm
                             );

void
algn_union ( dyn_char_p char1
           , dyn_char_p char2
           , dyn_char_p unionChar
           );

#endif /* ALIGN_CHARACTERS_H */
