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

/**@section Additive Characters
 *
 * Additive characters are quite common in biological research for phylogenetic
 * analysis under Maximum Parsimony. In this particular class of characters, the
 * required operations for a fast analysis doesn't require a large number of
 * operaations (as opposed to dynamic homology characters). For this reason, a
 * special kind of  structure in C is used for the caculations that allows us a
 * much larger number of character optimizations. 
 */
#ifndef ADD_H

#ifdef __ALTIVEC__
#include <altivec.h>
#elif __MMX__ && ! defined __LP64__
#include <xmmintrin.h>
#endif

#define ADD_H 1

/* Are we using a 64 bit environment? */
#ifdef __LP64__
#define native_int long
#define serialize_native serialize_int_8
#define deserialize_native deserialize_sint_8
#else
#define native_int int
#define serialize_native serialize_int_4
#define deserialize_native deserialize_sint_4
#endif

/* Are we using vectorization ? */
#ifdef __ALTIVEC__
typedef vector unsigned char vUInt8;
typedef vector unsigned int  vUInt32;
#define vec_emms()
#define CAPACITY_CHAR 16
#define CAPACITY_INT 4
#define VECTORIZE 1
#elif __MMX__ && ! defined __LP64__
typedef __m64 vUInt8;
typedef __m64 vUInt32;
#define CAPACITY_CHAR 8
#define CAPACITY_INT 2
#define VECTORIZE 1
/* Some macros to emulate altivec in Intel with MMX */
#define vec_cmpgt(a, b) (_mm_cmpgt_pi8 (a, b))
#define vec_sel(b, a, c) (_mm_or_si64(_mm_and_si64(c, a), \
            _mm_andnot_si64(c, b)))
#define vec_cmpeq(a, b) (_mm_cmpeq_pi8 (a, b))
#define vec_cmplt(a, b) (_mm_cmpgt_pi8 (b, a))
#define vec_and(a, b) (_mm_and_si64(a, b))
#define vec_or(a, b) (_mm_or_si64(a, b))
/* Here we will change the way we where defining -1 in __m64
#define vec_nor(a, b) (_mm_xor_si64(_mm_or_si64 (a, b), (__m64)0xFLL))
*/
#define vec_nor(a, b) (_mm_xor_si64(_mm_or_si64 (a, b), _mm_setr_pi32(-1, -1)))
#define vec_sub(a, b) (_mm_sub_pi8(a, b))
#define vec_min(a, b) (_mm_min_pu8(a, b))
#define vec_max(a, b) (_mm_max_pu8(a, b))
#define vec_emms _mm_empty
#else
#define CAPACITY_CHAR 1
#endif

/** A character set structure.
 *
 * In additive characters there is a minimum and a maximum value as each
 * character is defined by a range of valid integers. In the application, each
 * homologous character is assigned a unique code; for this reason, inside each
 * set, there is a code assigned, and for this same reason, no two characters in
 * a set can have the same code, as they can't be homologous. 
 *
 * Finally, each set is either generated from the input (the observed taxonomic
 * units), or it's produced as a median of two children nodes, and therefore,
 * the definition of the set of valid states for each character has an
 * associated cost, equal to the distance between the two homologous characters
 * in the children. */
#ifdef VECTORIZE
typedef union {
    vUInt8 v;
    unsigned char c[CAPACITY_CHAR];
} ucv;

typedef unsigned int uiv;
#else
typedef unsigned char ucv;
typedef unsigned int uiv;
#endif

struct add_st {
    native_int len;         /**< Number of characters in the set */
    native_int true_len;    /**< True length of the array when vectorized */
    int total;              /**< Total cost of the sum of the cost vector */
    int scode;              /**< The code of the character set. */
    ucv *min;               /**< Array of minimums of character */
    ucv *max;               /**< Array of maximas of characters */
    ucv *cost;              /**< Array of cost of characters */
    ucv *union_min;         /**< Array of union of min of children of vertex */
    ucv *union_max;         /**< Array of union of max of children of vertex */
};

#define ADD_NUM_VECTORS 5   /**< The number of ucv vectors in struct add_st */

/** A pointer to the additive set structure */
typedef struct add_st * add_stt; 

/** Allocate
 *
 * Allocates a new additive character set structure.
 * @param len is the number of characters the set contains. 
 * @return a pointer to the freshly allocated structure. If the allocation
 * fails, a NULL pointer is returned. */
add_stt
add_alloc (native_int len, native_int true_len);

/**Deallocate
 *
 * Deallocates an additive character set. 
 * @param a is the additive character set to be deallocated. */
void
add_free (add_stt a);

/**Duplicate
 *
 * Allocates a fresh copy of an additive character set with the contents of a
 * preexisting character.
 * @param x is the character set that is being duplicated.
 * @return a pointer to the freshly allocated character set. If a memory
 * allocation occurrs, a NULL pointer is returned. */
add_stt
add_dup (add_stt x);

/**Copy
 *
 * Copies the contents of one additive character set to another addtive
 * character set. The two character sets must have the same len (this condition
 * is checked in an assertion, and therefore it's non-recoverable).
 * @param src is the source of the information being copied. 
 * @param dst is the target structure where the information of src is being
 * copied. */
void
add_copy (add_stt dst, add_stt src);

/**Downpass in a pair of character sets.
 *
 * Performs a full calculation of medians (intersection or union) between all
 * the elements in a pair of character sets. The function requires that
 * the application of add_compare between the two sets yields 0, and add_compare
 * between any of them and the resulting set is also zero. It is also required
 * that the length of the three sets (cardinality) is the same.
 * @param a is the first character set to be compared.
 * @param b is the second character set to be compared.
 * @param res is the resulting character set where the medians of all the
 * homologous characters of a and b will be stored (remember that homologous
 * means characters with a shared code). 
 */
void
add_downpass (add_stt a, add_stt b, add_stt res);

/**Median of two character sets.
 *
 * Calculates the median of two character sets. Both sets should have the same
 * conditions established in add_downpass. 
 * @param a is the first additive character set being compared.
 * @param b is the second additive character set being compared.
 * @return a fresh additive character set x that guarantees that 
 * add_distance (x, a) + add_distance (x, b) <= add_distance (a, b). If an error
 * occurs return a NULL pointer.
 * @see add_downpass
 * @see add_distance
 */
add_stt
add_median (add_stt a, add_stt b);

/**Disatance between two character sets.
 *
 * Calculates the distance between two character sets. It is equivalent to the
 * summation of the distance between all the pairs of homologous characters. The
 * function has the same restrictions on the input as add_downpass.
 * @param a is the first additive character set being compared.
 * @param b is the second additive character set being compared.
 * @return the distance between the character sets. This value must be a Natural
 * number, if an error occurs, (-1) is returned. 
 * @see add_downpass
 */
int
add_distance (add_stt a, add_stt b);

#endif /* ADD_H */
