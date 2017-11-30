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

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "costMatrix.h"

static inline int
ceil_log_2 (int v)
{
    int i = 0;
    while (v != 0) {
        i++;
        v = v >> 1;
    }
    return (i + 1);
}

static inline int
cm_combinations_of_alphabet (const int alphSize)
{
    assert (alphSize >= 0);
    return ((1 << alphSize) - 1); // ignore empty set
}


/** Each address in the array is divided into two parts. Shift first element's value to the left alphSize,
 *  then || with second element's value. That will give you a size_t that holds the address into the array
 *  for the two elements.
 */
static inline size_t
cm_calc_cost_position_2d (elem_t a, elem_t b, size_t alphSize)
{
    // assert(alphSize >= 0); // alphSize is size_t, always >= 0
    return ( (((size_t) a) << alphSize) + (size_t) b );
}


void cm_print_2d (cost_matrices_2d_t *costMatrix)
{
    printf("\nCost matrix fields:\n");
    printf("  alphabet size:        %zu\n", costMatrix->alphSize);
    printf("  costMatrixDimension:  %zu\n", costMatrix->costMatrixDimension);
    printf("  gap_char:             %d\n",  costMatrix->gap_char);
    printf("  cost model:           %d\n",  costMatrix->cost_model_type);
    printf("  include ambiguities:  %d\n",  costMatrix->include_ambiguities);
    printf("  gap open:             %d\n",  costMatrix->gap_open_cost);
    printf("  is metric:            %d\n",  costMatrix->is_metric);
    printf("  num elements:         %zu\n", costMatrix->num_elements);

    printf("\n  Cost matrix:\n    ");
    cm_print_matrix_2d(costMatrix->cost,         costMatrix->alphSize + 1, costMatrix->alphSize + 1);
    printf("  Prepend cost\n    ");
    cm_print_matrix_2d(costMatrix->prepend_cost, 1,                        costMatrix->alphSize + 1);
 //   printf("  Worst\n    ");
 //   cm_print_matrix(c->worst,        costMatrix->alphSize + 1, costMatrix->alphSize + 1);
    printf("  Tail cost:\n    ");
    cm_print_matrix_2d(costMatrix->tail_cost,    1,                         costMatrix->alphSize);
    printf("  Median costs:\n    ");
    cm_print_matrix_2d(costMatrix->median,        costMatrix->alphSize + 1, costMatrix->alphSize + 1);
}


void
cm_print_matrix_2d (unsigned int *costMatrix, size_t height, size_t alphSize) {
    for (size_t i = 0; i < height; i++) {
        //fprintf(stdout,"%zu: ", i);
        for (size_t j = 0; j < alphSize; j++)
            printf ("%4d", *(costMatrix + (alphSize * i) + j));
        printf ("\n    ");
    }
}


void
cm_free (cost_matrices_2d_t *costMatrix)
{
    free (costMatrix->cost);
    free (costMatrix->median);
    free (costMatrix->worst);
    free (costMatrix->prepend_cost);
    free (costMatrix->tail_cost);
    free (costMatrix);
}

/** As with cm_calc_cost_position_2d, but with three elements, so first element gets shifted alphSize twice, etc. */
static inline size_t
cm_calc_cost_position_3d (elem_t a, elem_t b, elem_t c, size_t alphSize)
{
    // assert(alphSize >= 0); alphSize is size_t, always >= 0
    return ((((a << alphSize) + b) << alphSize) + c);
}


static inline void
cm_set_affine (cost_matrices_2d_t *costMatrix, int do_aff, int gapOpenCost)
{
    assert(costMatrix != NULL);
    costMatrix->cost_model_type = do_aff;
    costMatrix->gap_open_cost   = gapOpenCost;
}


/*
 * Creates a cost matrix with memory allocated for an alphabet of size alphSize
 * (not including the gap representation which is internally chosen), and whose
 * size must consider all possible combinations of characters in the alphabeet
 * iff combinations != 0. Set the affine gap model parameters to the values
 * stored in do_aff, gap_open_cost, in the cost matrix res.
 * In case of error the function fails with the message "Memory error.".
 */
void
cm_alloc_2d ( cost_matrices_2d_t *res
            , size_t              alphSize
            , size_t              combinations
            , int                 do_aff
            , unsigned int        gap_open_cost
            , int                 is_metric
            , size_t              num_elements
            )
{
    if (DEBUG_COST_M)  printf("\n---cm_alloc_set_costs_2d\n");

// #ifndef USE_LARGE_ALPHABETS
//     if (alphSize > 255) {
//         // TODO: update this error message from POY
//         printf("Apparently you are analyzing large alphabets. This version \
//                 of PCG was compiled without the --enable-large-alphabets option. \
//                 To run this analysis you need to enable that option at compile time. \
//                 Either recompile the program yourself or request a version suited \
//                 for your needs in the PCG mailing list. Thanks!");
//         exit(1);
//     }
// #endif
    if (combinations) {
        res->gap_char            = 1 << (alphSize - 1);
        res->alphSize            = cm_combinations_of_alphabet (alphSize); // 2 ^ alphSize - 1 is |power set of alphSize|
        res->costMatrixDimension = alphSize;
        res->include_ambiguities = 1;
    } else {
        res->gap_char            = alphSize;
        res->alphSize            = alphSize;
        res->costMatrixDimension = ceil_log_2 (alphSize + 1);
        res->include_ambiguities = 0;
    }

    res->num_elements = num_elements;
    res->is_metric    = is_metric;

    cm_set_affine (res, do_aff, gap_open_cost);

    size_t size = 2
                * (1 << (res->costMatrixDimension))
                * (1 << (res->costMatrixDimension))
                * sizeof(int); // size for cost matrices

    if (size == 0) {
        printf("Your cost matrix is too large to fit in memory. I can't continue with your data loading.\n");
        exit(1);
    }
    res->cost         = calloc (size, 1);
    res->worst        = calloc (size, 1);
    res->prepend_cost = calloc (size, 1);
    res->tail_cost    = calloc (size, 1);

    size = 2
         * (1 << (res->costMatrixDimension))
         * (1 << (res->costMatrixDimension))
         * sizeof(elem_t); // size for median matrix

    if (size == 0) {
        printf("Your cost matrix is too large to fit in your memory. I can't continue with your data loading.\n");
        exit(1);
    }
    res->median = (elem_t *) calloc (size, 1);

    if ((res->cost == NULL) || (res->median == NULL)) {
        free (res->cost);
        free (res->median);
        printf("Memory error during cost matrix allocation.\n");
        exit(1);
    }
}


static inline elem_t
cm_calc_median_2d ( unsigned int *tcm
                  , elem_t a
                  , elem_t b
                  , int alphSize
                  )
{
    elem_t *res;
    unsigned int one        = 1;
    unsigned int upperBound = one << alphSize;
    assert (alphSize   >= 0);
    assert (upperBound >  a);
    assert (upperBound >  b);
    res = tcm + cm_calc_cost_position_2d(a, b, alphSize);
    return (*res);
}

unsigned int
cm_calc_cost_2d ( unsigned int *tcm
                , elem_t        a
                , elem_t        b
                , size_t        alphSize
                )
{
    unsigned int *res;
    unsigned int one = 1;
    unsigned int upperBound = one << alphSize;
    // assert(alphSize >= 0); alphSize is size_t, always >= 0
    assert (upperBound > a);
    assert (upperBound > b);
    res = tcm + cm_calc_cost_position_2d(a, b, alphSize);
    return (*res);
}


unsigned int *
cm_get_row (unsigned int *tcm, elem_t a, size_t alphSize) {
    unsigned int one = 1;
    unsigned int upperBound = one << alphSize;

    if (alphSize <= 0) {
        printf("Alphabet size = 3\n");
        exit(1);
    }
    if (upperBound <= a) {
        printf("3a is bigger than alphabet size\n");
        exit(1);
    }
    return (tcm + (a << alphSize));
}


void
cm_set_value_2d (elem_t a, elem_t b, elem_t v, elem_t *p, int alphSize) {
    p[ cm_calc_cost_position_2d (a, b, alphSize) ] = v;
}

unsigned int
cm_get_value_2d (elem_t a, elem_t b, unsigned int *p, size_t alphSize) {
    return p[ cm_calc_cost_position_2d (a, b, alphSize) ];
}


void
cm_set_cost_2d (cost_matrices_2d_t *costMtx, elem_t elem1, elem_t elem2, unsigned int val) {
    cm_set_value_2d (elem1, elem2, val, costMtx->cost, costMtx->costMatrixDimension);
}

unsigned int
cm_get_cost_2d (cost_matrices_2d_t *costMtx, elem_t elem1, elem_t elem2) {
    return cm_get_value_2d (elem1, elem2, costMtx->cost, costMtx->costMatrixDimension);
}

// TODO: This seems never to be used
// void
// cm_set_worst (cost_matrices_2d_t *costMtx, elem_t elem1, elem_t elem2, unsigned int val) {
//     cm_set_value_2d (elem1, elem2, val, costMtx->worst, costMtx->costMatrixDimension);
// }


void
cm_set_prepend_2d (cost_matrices_2d_t *c, int a, int b) {
    c->prepend_cost[a] = b;
}

void
cm_set_tail_2d (cost_matrices_2d_t *c, int a, int b) {
    c->tail_cost[a] = b;
}

void
cm_set_median_2d (cost_matrices_2d_t *costMtx, elem_t elem1, elem_t elem2, unsigned int val) {
    cm_set_value_2d (elem1, elem2, val, costMtx->median, costMtx->costMatrixDimension);
}


int
cm_compare (cost_matrices_2d_t *a, cost_matrices_2d_t *b) {
    int cmp, len_g;
    size_t len, len1;
    if      (a->alphSize != b->alphSize)                       return (a->alphSize            - b->alphSize);
    else if (a->include_ambiguities != b->include_ambiguities) return (a->include_ambiguities - b->include_ambiguities);
    else if (a->cost_model_type != b->cost_model_type)         return (a->cost_model_type     - b->cost_model_type);
    else if (a->gap_open_cost != b->gap_open_cost)             return (a->gap_open_cost       - b->gap_open_cost);
    else if (a->is_metric != b->is_metric)                     return (a->is_metric           - b->is_metric);
    else {
        len_g = 2 * (1 << (a->costMatrixDimension)) * (1 << (a->costMatrixDimension));
        len   = len_g * sizeof(int);
        len1  = len_g * sizeof(elem_t);
        cmp   = memcmp (a->cost, b->cost, len);
        if (cmp != 0) return (cmp);
        cmp   = memcmp (a->median, b->median, len1);
        if (cmp != 0) return (cmp);
        cmp   = memcmp (a->worst, b->worst, len);
        return (cmp);
    }
}

void
cm_copy_contents (int *src, int *tgt, int len) {
    int i;
    for (i = 0; i < len; i++)
        *(tgt + i) = *(src + i);
}


void
cm_copy_contents_dyn_char_p (elem_t *src, elem_t *tgt, int len) {
    int i;
    for (i = 0; i < len; i++)
        *(tgt + i) = *(src + i);
}


elem_t
cm_get_median_2d ( const cost_matrices_2d_t *t
                 ,       elem_t             a
                 ,       elem_t             b
                 ) {
    return (cm_calc_median_2d( (t->median), a, b, t->costMatrixDimension ));
}


void cm_print_3d (cost_matrices_3d_t *costMatrix)
{
    printf("\nCost matrix fields:\n");
    printf("  alphabet size:        %zu\n", costMatrix->alphSize);
    printf("  costMatrixDimension:  %zu\n", costMatrix->costMatrixDimension);
    printf("  gap_char:             %u\n",  costMatrix->gap_char);
    printf("  cost model:           %d\n",  costMatrix->cost_model_type);
    printf("  include ambiguities:  %d\n",  costMatrix->include_ambiguities);
    printf("  gap open:             %d\n",  costMatrix->gap_open_cost);
    printf("  num elements:         %zu\n", costMatrix->num_elements);

    printf("\n  Cost matrix:\n    ");
    cm_print_matrix_3d(costMatrix->cost,   costMatrix->alphSize + 1);
    printf("  Median costs:\n    ");
    cm_print_matrix_3d(costMatrix->median, costMatrix->alphSize + 1);
}


void
cm_print_matrix_3d (unsigned int *costMatrix, size_t costMatrixDimension) { // don't take entire cost matrix because I want to print only
                                                                            // medians or costs
    for (size_t i = 0; i < costMatrixDimension; i++) {
        //fprintf(stdout,"%zu: ", i);
        for (size_t j = 0; j < costMatrixDimension; j++) {
            for (size_t k = 0; k < costMatrixDimension; k++) {
                printf ("%4d", costMatrix[cm_calc_cost_position_3d(i, j, k, costMatrixDimension)]);
            }
            printf ("\n");
            for (size_t num_tabs = 0; num_tabs < j; num_tabs++) {
                printf("    ");
            }
        }
    }
    printf ("\n    ");
}


void
cm_3d_free (cost_matrices_2d_t *costMatrix)
{
    free (costMatrix->cost);
    free (costMatrix->median);
    free (costMatrix);
}


static inline void
cm_set_affine_3d (cost_matrices_3d_t *costMatrix, int do_aff, int gapOpenCost)
{
    assert(costMatrix != NULL);
    costMatrix->cost_model_type = do_aff;
    costMatrix->gap_open_cost   = gapOpenCost;
}


/*
 * Creates a cost matrix with memory allocated for an alphabet of size alphSize
 * (not including the gap representation which is internally chosen), and whose
 * size must consider all possible combinations of characters in the alphabeet
 * iff combinations != 0. Set the affine gap model paramters to the values
 * stored in do_aff, gap_open_cost, in the cost matrix res.
 * In case of error the function fails with the message "Memory error.".
 */
void
cm_alloc_3d ( cost_matrices_3d_t *res
            , int                alphSize
            , int                combinations
            , int                do_aff
            , int                gap_open_cost
            , int                num_elements
            )
{
    int size;
    if (DEBUG_COST_M) {
        printf ("Allocating a three dimensional matrix:\n");
        printf ("alphabet size: %d \n", alphSize);
        printf ("combinations:  %d \n", combinations);
        printf ("cost model:    %d \n", do_aff);
        printf ("gap open cost: %d \n", gap_open_cost);
    }
    // TODO: check the following code. Does combinations need to be reset? Are the dimensions set right?
    res->gap_char            = 1 << (alphSize - 1);
    if (combinations != 0) {
        res->alphSize            = cm_combinations_of_alphabet (alphSize); // 2 ^ alphSize - 1 is |power set of alphSize|
        res->costMatrixDimension = alphSize;
        res->include_ambiguities = 1;
    } else {
       // res->gap_char            = alphSize;
        res->alphSize            = alphSize;
        res->costMatrixDimension = ceil_log_2 (alphSize + 1);
        res->include_ambiguities = 0;
    }

    res->num_elements = num_elements;
    cm_set_affine_3d (res, do_aff, gap_open_cost);
    size              = (1 << (res->costMatrixDimension + 1))
                      * (1 << (res->costMatrixDimension + 1))
                      * (1 << (res->costMatrixDimension + 1));

    res->cost         = calloc (size * sizeof(int),  1);
    res->median       = calloc (size * sizeof(elem_t), 1);

    if ((res->cost == NULL) || (res->median == NULL)) {
        free (res->cost);
        free (res->median);
        printf("Memory error during cost matrix allocation.\n");
        exit(1);
    }
}


inline int
cm_get_gap_opening_parameter_3d (const cost_matrices_3d_t *c) {
    assert(c != NULL);
    return c->gap_open_cost;
}


// static inline size_t
// cm_calc_cost_position_3d (int a, int b, int c, size_t alphSize) {
//     assert(alphSize >= 0);
//     return (((a << alphSize) + b) << alphSize) + c;
// }


unsigned int
cm_get_value_3d( const unsigned int *p
               ,       elem_t        a
               ,       elem_t        b
               ,       elem_t        c
               ,       size_t        alphSize
               )
{
    return p[ cm_calc_cost_position_3d (a, b, c, alphSize) ];
}


elem_t
cm_get_median_3d( const cost_matrices_3d_t *matrix
                ,       elem_t              a
                ,       elem_t              b
                ,       elem_t              c
                )
{
    unsigned int upperBound = ((elem_t) 1) << matrix->costMatrixDimension;
    if (DEBUG_3D) printf("alphSize: %zu, upperBound: %2u;  elements a: %2u, b: %2u, c: %2u;  median: %2u\n"
                        , matrix->costMatrixDimension
                        , upperBound
                        , a
                        , b
                        , c
                        , matrix->median[cm_calc_cost_position_3d (a, b, c, matrix->costMatrixDimension)]
                        );

    // TODO: change these to asserts:
    if (matrix->costMatrixDimension <= 0) {
        printf("Alphabet size <= 0");
        exit(1);
    }
    if (upperBound <= a) {
        printf("Element a has a larger than allowed value: %u.\n", a);
        exit(1);
    }
    if (upperBound <= b) {
        printf("Element b has a larger than allowed value: %u.\n", b);
        exit(1);
    }
    if (upperBound <= c) {
        printf("Element c has a larger than allowed value: %u.\n", c);
        exit(1);
    }
    //TODO: use cm_get_val_3d()
    return( cm_get_value_3d(matrix->median, a, b, c, matrix->costMatrixDimension) );
}

/* TODO: dead code?
static inline int
cm_calc_cost_3d (int *tcm, elem_t a, elem_t b, elem_t c, int alphSize) {
    if (alphSize <= 0) {
        printf("Alphabet size = 2\n");
        exit(1);
    }
    if ((1 << alphSize) <= a) {
        printf("2a is bigger than alphabet size\n");
        exit(1);
    }
    if ((1 << alphSize) <= b) {
        printf("b is bigger than alphabet size\n");
        exit(1);
    }
    return (*(tcm + cm_calc_cost_position_3d (a, b, c, alphSize)));
}

static inline elem_t
cm_calc_cost_3d_dyn_char_p (elem_t *tcm, elem_t a, elem_t b, elem_t c, int alphSize) {
    if (alphSize <= 0) {
        printf("Alphabet size = 2\n");
        exit(1);
    }
    if ((1 << alphSize) <= a) {
        printf("2a is bigger than alphabet size\n");
        exit(1);
    }
    if ((1 << alphSize) <= b) {
        printf("b is bigger than alphabet size\n");
        exit(1);
    }
    return (*(tcm + cm_calc_cost_position_3d (a, b, c, alphSize)));
}


static inline int
cm_calc_tmm (int *tmm, int a, int b, int alphSize) {
    return (cm_calc_cost (tmm, a, b, alphSize));
}

inline int
cm_calc_median_position (elem_t a, elem_t b, int alphSize) {
    return (cm_calc_cost_position (a, b, alphSize));
}
*/


unsigned int *
cm_get_row_3d( unsigned int *tcm
             , elem_t        char1
             , elem_t        char2
             , size_t        alphSize
             )
{
    unsigned int one = 1;
    unsigned int upperBound = one << alphSize;

    if (alphSize <= 0) {
        printf("Alphabet size = 4\n");
        exit(1);
    }
    if (upperBound <= char1) {
        printf("%u is bigger than alphabet size\n", char1);
        exit(1);
    }
    if (upperBound <= char2) {
        printf("%u is bigger than alphabet size\n", char2);
        exit(1);
    }
    return (tcm + (((char1 << alphSize) + char2) << alphSize));
}


// void
// cm_set_value_3d_dyn_char_p ( elem_t       elem1
//                            , elem_t       elem2
//                            , elem_t       elem3
//                            , unsigned int val
//                            , unsigned int *matrix_array
//                            , int           alphSize
//                            )
// {
//     *(matrix_array + (cm_calc_cost_position_3d (elem1, elem2, elem3, alphSize))) = val;
// }

static inline void
cm_set_value_3d ( unsigned int *matrix_array
                , elem_t        elem1
                , elem_t        elem2
                , elem_t        elem3
                , unsigned int  val
                , size_t        alphSize
                )
{
    matrix_array[cm_calc_cost_position_3d (elem1, elem2, elem3, alphSize)] = val;
}

void
cm_set_cost_3d( cost_matrices_3d_t *costMtx
              , elem_t              elem1
              , elem_t              elem2
              , elem_t              elem3
              , unsigned int        val
              )
{
    cm_set_value_3d (costMtx->cost, elem1, elem2, elem3, val, costMtx->costMatrixDimension);
}

unsigned int
cm_get_cost_3d ( const cost_matrices_3d_t *costMtx
               ,       elem_t              elem1
               ,       elem_t              elem2
               ,       elem_t              elem3
               )
{
    return cm_get_value_3d (costMtx->cost, elem1, elem2, elem3, costMtx->costMatrixDimension);
}


void
cm_set_median_3d( const cost_matrices_3d_t *costMtx
                ,       elem_t              elem1
                ,       elem_t              elem2
                ,       elem_t              elem3
                ,       elem_t              val
                )
{
    cm_set_value_3d (costMtx->median, elem1, elem2, elem3, val, costMtx->costMatrixDimension);
}
