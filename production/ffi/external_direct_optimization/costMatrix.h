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

/* A cost matrix library                                                      */

#ifndef COSTMATRIX_H
#define COSTMATRIX_H

#define Cost_matrix_struct(a) ((struct cost_matrices_2d *) Data_custom_val(a))
#define Cost_matrix_struct_3d(a) ((struct cost_matrices_3d *) Data_custom_val(a))
#include "debug_constants.h"
#include "nwMatrices.h"
#include "dyn_character.h"

/*
 * Check cost_matrices_3d for further information. This is the corresponding data
 * structure for two dimensional character alignment.
 */
struct cost_matrices_2d {
    unsigned int alphSize;      // alphabet size including gap, and including ambiguities if
                                // combinations == True
    size_t costMatrixDimension; // n in an n x n matrix
    unsigned int gap_char;      // gap character value (1 << (alphSize - 1))
    int cost_model_type;        /* The type of cost model to be used in the alignment,
                                 * i.e. affine or not.
                                 * Based on cost_matrix.ml, values are:
                                 * • linear == 0
                                 * • no_alignment == 1 ** I don't believe this is every used.
                                 * • affine == 2
                                 */
    int combinations;           /* This is a flag set to true if we are going to accept
                                 * all possible combinations of the elements in the alphabet
                                 * in the alignments. This is not true for protein characters
                                 * for example, where the number of elements of the alphabet
                                 * is already too big to build all the possible combinations.
                                 */
    int gap_open;               /* The cost of opening a gap. This is only useful in
                                 * certain cost_model_types (type 2: affine, based on my reading of ML code).
                                 */
    int is_metric;              /* if tcm is symmetric
                                 *
                                 * > MISSING IN 3D
                                 */
    int all_elements;           // total number of elements. This is alphSize if we're using only unambiguous elems, otherwise |power set|
    int *cost;                  /* The transformation cost matrix, including ambiguities,
                                 * storing the **best** cost for each ambiguity pair
                                 */
    elem_t *median;               /* The matrix of possible medians between elements in the
                                 * alphabet. The best possible medians according to the cost
                                 * matrix.
                                 */
    int *worst;                 /* The transformation cost matrix, including ambiguities,
                                 * storing the **worst** cost for each ambiguity pair
                                 *
                                 * > MISSING IN 3D
                                 */
    int *prepend_cost;          /* The cost of going from gap -> each base. For ambiguities, use best cost.
                                 * Set up as all_elements x all_elements
                                 * matrix, but seemingly only first row is used.               <-- TODO: fix this, and in tail_cost below.
                                 * Missing in 3d because current version of 3d sets gap cost
                                 * as constant.
                                 *
                                 * > MISSING IN 3D
                                 */
    int *tail_cost;             /* As prepend_cost, but with reverse directionality,
                                 * so base -> gap.
                                 * As with prepend_cost, seems to be allocated as too large.
                                 * Missing in 3d because current version of 3d sets gap cost
                                 * as constant.
                                 *
                                 * > MISSING IN 3D
                                 */
};

/*
 * A pointer to the cost_matrices_2d structure.
 */
typedef struct cost_matrices_2d *cost_matrices_2d_p;

/** A three dimesional cost matrix
 *
 * For three way character alignment, this structure holds the cost of
 * transforming the elements of an alphabet.  A cost matrix can only be applied
 * on a particular alphabet.
 */
struct cost_matrices_3d {
    size_t alphSize;             /** The number of elements in the alphabet */
    size_t costMatrixDimension;  /** Based on alphSize */
    unsigned int gap_char;       /** The integer representing a gap character in the alphabet */
    int cost_model_type;         /** The type of cost model to be used in the alignment */
    int combinations;            /** This is a flag set to true if we are going to accept
                                     all possible combinations of the elements in the alphabet
                                     in the alignments. This is not true for protein characters
                                     for example, where the number of elements of the alphabet
                                     is already too big to build all the possible combinations.
                                  */
    int gap_open;                /** The cost of opening a gap. This is only useful in
                                     certain cost_model_type's. */
    int all_elements;            /** The integer that represents all the combinations, used
                                     for ambiguities */
    int *cost;                   /** The transformation cost matrix. The ordering is row-major,
                                  *  and the lookup is a->b, where a is a row label and b is
                                  *  a column label
                                  */
    elem_t *median;                /** The matrix of possible medians between elements in the
                                     alphabet. The best possible medians according to the cost
                                     matrix. */
};

/*
 * A pointer to a three dimensional cost matrix
 */
typedef struct cost_matrices_3d *cost_matrices_3d_p;

void cm_print_2d (cost_matrices_2d_p c);

void cm_print_3d (cost_matrices_3d_p c);

void cm_print_matrix (int *costMatrix, size_t w, size_t h);

/*
 * Creates a cost matrix with memory allocated for an alphabet of size alphSize
 * (not including the gap representation which is internally chosen), and whose
 * size must consider all possible combinations of characters in the alphabet
 * iff combinations != 0. Set the affine gap model paramters to the values
 * stored in do_aff, gap_open, in the cost matrix res.
 * In case of error the function fails with the message "Memory error.".
 */
void cm_alloc_set_costs_2d (int alphSize, int combinations, int do_aff, int gap_open, \
        int is_metric, int all_elements, cost_matrices_2d_p res);


void
cm_set_cost_2d (int a, int b, int v, cost_matrices_2d_p c);

void
cm_set_cost_3d (int a, int b, int cp, int v, cost_matrices_3d_p c);


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
int *
cm_get_row_precalc_3d (const int *toOutput, int s3l, int alphSize, int s1c, int s2c);

/*
 * Gets the total number of possible combinations of an alphabeet of size
 * alphSize. The size of the alphabet must be bigger than 0.
 */
//static inline int
//cm_combinations_of_alphabet (const int alphSize);

/*
 * Calculates the median position in a transformation cost matrix for an
 * alphabet of size alphSize and elements a and b.
 */
/*
static inline int
cm_calc_median_position (elem_t a, elem_t b, int alphSize);
*/

/*
 * The median between to elements in the alphabet hold by t.
 * @param t is a transformation cost matrix
 * @param a is an element in the alphabet of t
 * @param b is an element in the alphabet of t
 * @return an element in the alphabet of t which is a median between a and b
 * according to the transformation cost matrix hold in t.
 */
elem_t
cm_get_median ( const cost_matrices_2d_p t
              ,       elem_t a
              ,       elem_t b
              );

/*
 * Retrieves the transformation cost of the elements a and b as stored in the
 * transformation cost matrix tcm, containing information for an alphabet of
 * size alphSize.
 */
int
cm_calc_cost ( int *tcm
             , elem_t a
             , elem_t b
             , int alphSize
             );

/*
 * Gets the row in the transformation cost matrix tcm where the transformations
 * of character a are located, when tcm holds information for an alphabet of
 * size alphSize.
 */
/*
static inline int *
cm_get_row ( int *tcm
           , elem_t a
           , int alphSize
           );
*/

/* set the value of c->worst at location (a,b) to v */
void
cm_set_worst ( int a
             , int b
             , int v
             , cost_matrices_2d_p c
             );

/*
 * Fills a precalculated matrix with the cost of comparing each elment in the
 * character s with each element in the alphabet specified in the transformation
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
cm_precalc_4algn ( const cost_matrices_2d_p costMtx
                 ,       nw_matrices_p toOutput
                 , const dyn_char_p s
                 );

/*
 * Gets the precalculated row for a particular character in the alphabet.
 * @param p is the precalculated matrix.
 * @param item is the element in the alphabet that produced p that should be
 *  generated.
 * @param len is the length of the character that was source of the precalculated
 *  matrix.
 */
const int *
cm_get_precal_row ( const int *p
                  ,       elem_t item
                  ,       int len
                  );



/** As with 2d, but doesn't compute worst, prepend or tail */
void
cm_alloc_set_costs_3d ( int alphSize
                      , int combinations
                      , int do_aff
                      , int gap_open
                      , int all_elements
                      , cost_matrices_3d_p res
                      );

/*
 * The median between three alphabet elements a, b and c.
 * @param t is the transformation cost matrix
 * @param a is the first element in the alphabet
 * @param b is the second element in the alphabet
 * @param c is the third element in the alphabet
 * @return an element of the alphabet contained in t that provides the best
 * median between a, b, and c, according to the transformation cost matrix
 * contained in t.
 */
elem_t
cm_get_median_3d (const cost_matrices_3d_p t, elem_t a, elem_t b, elem_t c);

/*
 * Gets the row in the transformation cost matrix tcm where the transformations
 * of character a are located, when tcm holds information for an alphabet of
 * size alphSize.
 */
//static inline int *
//cm_get_row_3d (int *tcm, elem_t a, elem_t b, int alphSize);

/*
 * Fills a three-dimensional precalculation alignment matrix for character s
 * See cm_precalc_4algn for further information. This is the
 * corresponding function for three dimensional alignments.
 * TODO: Why is this here, and not in matrices.c?
 */
void
cm_precalc_4algn_3d (const cost_matrices_3d_p c, int *toOutput, const dyn_char_p s);

/*
 * Deallocates the memory structure iff there are no more pointers to it,
 * otherwise it will just decrease the garbage collection counter.
 */
void
cm_free (cost_matrices_2d_p c);

int
cm_get_gap_opening_parameter (cost_matrices_2d_p c);

int
cm_get_cost (int a, int b, cost_matrices_2d_p c);

int
cm_get_value (int a, int b, int *p, int alphSize);

void
cm_set_prepend_2d (int a, int b, cost_matrices_2d_p c);

void
cm_set_tail_2d (int a, int b, cost_matrices_2d_p c);

void
cm_set_median_2d (elem_t a, elem_t b, elem_t v, cost_matrices_2d_p c);

void
cm_set_median_3d (elem_t a, elem_t b, elem_t cp, elem_t v, cost_matrices_3d_p c);

void
cm_print_median (elem_t *m, size_t w, size_t h);


#endif /* COSTMATRIX_H */

