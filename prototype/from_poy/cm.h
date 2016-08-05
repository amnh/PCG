/* POY 5.1.1. A phylogenetic analysis program using Dynamic Homologies.       */
/* Copyright (C) 2014 Andrés Varón, Lin Hong, Nicholas Lucaroni, Ward Wheeler,*/
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

#ifndef CM_H

#define CM_H 1
#define Cost_matrix_struct(a) ((struct cm *) Data_custom_val(a))
#define Cost_matrix_struct_3d(a) ((struct cm_3d *) Data_custom_val(a))
#include "matrices.h"
#include "seq.h"

/*
 * Check cm_3d for further information. This is the corresponding data
 * structure for two dimensional sequence alignment. 
 */

struct cm {
    // add following for level
    int ori_a_sz; //original alphabet size
    int level; //level = 1,2,3....n. n = ori_a_sz
    int map_sz; // size of combmap matrix
    int gap_startNO; // first combination code with gap 
    /*for a alphabet with original size 5 and level 2, it looks like this
     *  1 A
        2 C
        3 G
        4 T
        5 -
        6 [AC]
        7 [AG]
        8 [CG]
        9 [AT]
        10 [CT]
        11 [GT]
        12 [A-]
        13 [C-]
        14 [G-]
        15 [T-]
        for this alphabet, ori_a_sz = 5, level=2, gap_startNO = 12.
    */
    // add above for level
    long int total_size; //number of elements in each matrix(cost,median,worst,etc),size of int/char not included
    int a_sz;//alphabet size
    int lcm;
    int gap;
    int cost_model_type;
    int combinations;
    int gap_open;
    int is_metric;
    int is_identity;
    int all_elements;
    int tie_breaker; //0=random,1=pick first,2=pick last
    int *combmap; // the combination codelist to combination code map [a,b] --> c
    int *comb2list; // the combination code to combination code list map c-->[a,b]
    int *cost;
    SEQT *median;
    int *worst;         /* Missing in 3d */
    int *prepend_cost;  /* Missing in 3d */
    int *tail_cost;     /* Missing in 3d */
};


/*
 * A pointer to the cm structure.
 */
typedef struct cm * cmt;

/* check if we are using "level", return 1 when true, 0 for false */
int cm_check_level (cmt c);

int cm_get_min_non0_cost (cmt c);

/* Retrieves the alphabet size flag from the transformation cost matrix.  */
int cm_get_alphabet_size (cmt c);

// Retrieves the original alphabet size
int cm_get_ori_a_size (cmt c);

// Retrieves the tie_breaker, 0=random,1=pick first,2=pick last
int cm_get_tie_breaker (cmt c);

// Retrieves the level value
int cm_get_level (cmt c);

// Retrieves the postion of first code in alpahbet that has "gap" 
int cm_get_gap_startNO (const cmt c);

// Retrieves the value at position (a,b) of combination map matrix
int cm_get_combmap(int *tcm, int a, int b, int mapsize);

/* Retrieves the gap code as defined in the transformation cost matrix.  */
SEQT cm_get_gap (const cmt c);

/*
 * Retrieves a pointer to the memory position stored in the precalculated array
 * of costs for the alphabet in three dimensions, vs. a sequence s3. This is
 * used in the 3d alignments procedures for vectorization. to is a pointer to
 * the precalculated cube, s3l is the length of the sequence included in the
 * precalculated cube, a_sz is the lcm of the alphabet of the sequence, s1c is
 * the character from s1, and s2c is defined in an analogous manner. s3p is the
 * position in the sequence s3 of interest. s3p should be less than s3l.
 */
int * cm_get_pos_in_precalc (const int *to, int s3l, int a_sz, int s1c, int s2c, int s3p);

/* 
 * During the 3d alignments, calculations are performed for each element in the
 * array using the complete vectors of the precalculated matrix. This function
 * retrieves the first element in those precalculated arrays. The parameters
 * definitions are analogous to those explained in cm_get_pos_in_precalc 
 */
int * cm_get_row_precalc_3d (const int *to, int s3l, int a_sz, int s1c, int s2c);

/*
 * Retrieves the affine flag from the transformation cost matrix. Remember this
 * flag is 1 if true, otherwise 0. 
 */
int cm_get_affine_flag (cmt c);

/*
 * Gets the total number of possible combinations of an alphabeet of size
 * a_sz. The size of the alphabet must be bigger than 0.
 */
int cm_combinations_of_alphabet (const int a_sz);

/*
 * Calculates the median position in a transformation cost matrix for an
 * alphabet of size a_sz and elements a and b.
 */
int cm_calc_median_position (SEQT a, SEQT b, int a_sz);

/*
 * The median between to elements in the alphabet hold by t.
 * @param t is a transformation cost matrix
 * @param a is an element in the alphabet of t
 * @param b is an element in the alphabet of t
 * @return an element in the alphabet of t which is a median between a and b
 * according to the transformation cost matrix hold in t.
 */
SEQT cm_get_median (const cmt t, SEQT a, SEQT b);

/* Add for "level"
 * Retrieves the transformation cost of the elements a and b as stored in the
 * transformation cost matrix tcm, containing information for an alphabet of
 * size mapsize. 
 */
int cm_get_cost (int *tcm, int a, int b, int mapsize) ;


/*
 * Retrieves the transformation cost of the elements a and b as stored in the
 * transformation cost matrix tcm, containing information for an alphabet of
 * size a_sz. 
 */
int cm_calc_cost (int *tcm, SEQT a, SEQT b, int a_sz);

/* 
 * The transformation cost matrix, as stored in ocaml, may have the actual
 * matrices located at certain offset from the start of the matrix. This
 * function retrieves the actual starting position.
 */
#ifdef _WIN32
__inline int *
#else
inline int *
#endif
cm_get_transformation_cost_matrix (const cmt a);

#ifdef _WIN32
__inline int *
#else
inline int *
#endif
cm_get_tail_cost (const cmt a);

#ifdef _WIN32
__inline int *
#else
inline int *
#endif
cm_get_prepend_cost (const cmt a);

/*
 * Gets the row in the transformation cost matrix tcm where the transformations
 * of character a are located, when tcm holds information for an alphabet of
 * size a_sz.
 */
#ifdef _WIN32
__inline int *
#else
inline int *
#endif
cm_get_row (int *tcm, SEQT a, int a_sz);

#ifdef _WIN32
__inline int *
#else
inline int *
#endif
cm_get_row_level (int *tcm, SEQT a, int a_sz);


/* 
 * Fills a precalculated matrix with the cost of comparing each elment in the
 * sequence s with each element in the alphabet specified in the transformation
 * cost matrix c. 
 * @param c is the transformation cost matrix to calculate the precalculated
 * vectors.
 * @param to_output is the matrix that will hold the output. 
 * @param s is the sequence for which the cost matrix will be precalculated.
 * This function is only valid for two dimensional alignments.
 */
void
cm_precalc_4algn (const cmt c, matricest to_output, const seqt s);

/* 
 * Gets the precalculated row for a particular character in the alphabet.
 * @param p is the precalculated matrix.
 * @param item is the element in the alphabet that produced p that should be
 * generated.
 * @param len is the length of the sequence that was source of the precalculated
 * matrix.
 */
const int * cm_get_precal_row (const int *p, SEQT item, int len);

/** A three dimesional cost matrix 
 *
 * For three way sequence alignment, this structure holds the cost of
 * transforming the elements of an alphabet.  A cost matrix can only be applied
 * on a particular alphabet.
 */
struct cm_3d {
    long int total_size; ///number of elements in each matrix(cost,median,worst,etc),size of int/char not included
    int a_sz;               /** The number of elements in the alphabet */
    int lcm;                /** The logarithm base 2 of a_sz */
    int gap;                /** The integer representing a gap in the alphabet */
    int cost_model_type;    /** The type of cost model to be used in the alignment */
    int combinations;       /** This is a flag set to true if we are going to accept
                              all possible combinations of the elements in the alphabet
                              in the alignments. This is not true for protein sequences 
                              for example, where the number of elements of the alphabet 
                              is already too big to build all the possible combinations.
                              */
    int gap_open;           /** The cost of opening a gap. This is only useful in 
                              certain cost_model_type's. */
    int all_elements;       /** The integer that represents all the combinations, used 
                              for ambiguities */
    int ori_a_sz;          //original alphabet size, add for level
    int map_sz;            // size of the map for combination code list to combination code, add for level
    int level;              // level value, add for level
    int gap_startNO;        // the position of fisrt code in alphabet [x1,x2,x3....,x1/x2,x1/x3,.....] that has "gap".
    int tie_breaker;
    int *comblist_2_combcode;   // the combination codelist to combination code map [a,b] --> c
    int *combcode_2_comblist; // the combination code to combination code list map c-->[a,b]
    int *cost;              /** The transformation cost matrix. */
    SEQT *median;            /** The matrix of possible medians between elements in the 
                              alphabet. The best possible medians according to the cost 
                              matrix. */
};

/* 
 * A pointer to a three dimensional cost matrix 
 */ 
typedef struct cm_3d * cm_3dt;

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
#ifdef _WIN32
__inline SEQT
#else
inline SEQT
#endif
cm_get_median_3d (const cm_3dt t, SEQT a, SEQT b, SEQT c);

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_alphabet_size_3d (cm_3dt c);

/*
 * Retrieves the gap code as defined in the transformation cost matrix. 
 */
#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_gap_3d (const cm_3dt c);

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_level_3d (const cm_3dt c);

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_ori_a_sz_3d (const cm_3dt c);

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_map_sz_3d (const cm_3dt c);

/*
 * Retrieves the affine flag from the transformation cost matrix. Remember this
 * flag is 1 if true, otherwise 0. 
 */
#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_affine_flag_3d (cm_3dt c);

/*
 * Retrieves the gap opening cost under affine gap cost model. If affine gap
 * cost model is false, the retrieved value could make no sense at all.
 */
#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_gap_opening_parameter_3d (const cm_3dt c);

/*
 * Retrieves the gap opening cost under affine gap cost model. If affine gap
 * cost model is false, the retrieved value could make no sense at all.
 */
#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_gap_opening_parameter_3d (const cm_3dt c);

/*
 * Gets the row in the transformation cost matrix tcm where the transformations
 * of character a are located, when tcm holds information for an alphabet of
 * size a_sz.
 */
#ifdef _WIN32
__inline int *
#else
inline int *
#endif
cm_get_row_3d (int *tcm, SEQT a, SEQT b, int a_sz);

/*
 * Fills a precalculation for sequence s for a three dimensional sequence
 * alignment. Look in cm_precalc_4algn for further information. This is the
 * corresponding function for three dimensional alignments.
 */
void
cm_precalc_4algn_3d (const cm_3dt c, int *to_output, const seqt s);

/*
 * Deallocates the memory structure iff there are no more pointers to it,
 * otherwise it will just decrease the garbage collection counter. 
 */
#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_free (cmt c);

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_gap_opening_parameter (cmt c);

#endif /* CM_H */
