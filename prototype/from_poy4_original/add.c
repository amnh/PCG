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

/******************************************************************************
 * The Additive characters library has been changing to accomodate the
 * performance requirements of this kind of characters. This are the following
 * characteristics that constraint it's usage:
 * - Before starting any run, the contents of the library have to be initialized
 *   using the add_memstack_initialize and the add_all_codes_inititalize
 *   functions. These operations will allocate memory in two static internal
 *   structures, and assume that every character set used from then and on will
 *   correspond to the type and with codes assigned in the
 *   add_all_codes_inititalize call.
 * - In order to reset the library, the add_clear_init has to be called. This
 *   library will free all the static items from the library.
 *
 *   Enjoy!
******************************************************************************/
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/bigarray.h"
#include "caml/fail.h"
#include "caml/custom.h"
#include "caml/intext.h"
#include "caml/alloc.h"
#define NDEBUG 1
#define TESTER 1
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "memstack.h"
#include "add.h"

#define mem_create(a,b) NULL
#define mem_malloc(a,b) malloc(b)
#define mem_free(a,b) free(b)
#define mem_memstack_free(a) NULL

/* A macro to extract the contents of the OCaml custom value */
#define Add_st_struct(a) ((add_stt *) (Data_custom_val(a)))

/*****************************************************************************/
/* Global Static Structures */

/* A stack (not really a queue) of memory arrays for the add_st, items are
 * attempted to be allocated from this stack whenever is possible. */
static memstack_t add_st_mem = NULL;
/* A stack (not really a queue) of memory arrays for the ucv vectors */
static memstack_t ucv_mem = NULL;

/*****************************************************************************/
/* Library Initialization and Finalization */

/* Initializes the memory queues to reduce the number of allocations to hold
 * items elements, and every vector capable of handling len elements. */
void
add_memstack_initialize (int items, int len) {
    /* Allocate the structures and then the vectors themselves, ADD_NUM_VECTORS
     * times the number of items, as each add_st holds 5 of those vectors */
    if (NULL == add_st_mem) {
        add_st_mem = mem_create (items, sizeof (struct add_st));
        ucv_mem = mem_create (items * ADD_NUM_VECTORS, len * sizeof (ucv));
    }
}

/* Length of the arrays (vectors) used to hold min, max, cost, union_min and
 * union_max in the struct add_st. */
#ifdef VECTORIZE
/* In vectorized (altivec and mmx) based architectures, we hold the characters
 * in groups of CAPACITY_CHAR items. Therefore, the total number of elements
 * that we really need to allocate is limited by that size */
native_int
add_array_length (native_int len) {
    return ((len + (CAPACITY_CHAR - 1)) / CAPACITY_CHAR);
}

int
add_true_array_length (native_int len) {
    return (len * CAPACITY_CHAR);
}
#else
/* In non vectorized architectures, we really need to allocate as many elements
 * as len says. */
native_int
add_array_length (native_int len) {
    return len;
}

native_int
add_true_array_length(native_int len) {
    return (len * CAPACITY_CHAR);
}
#endif

/** Printing the contents of a vector (for debugging purposes). */
#ifdef VECTORIZE
void
print_vector (char *msg, vUInt8 b, int hex) {
    ucv a;
    a.v = b;
    if (hex)
#ifdef __ALTIVEC__
    printf ("%s %x %x %x %x %x %x %x %x %x %x %x %x %x %x %x %x\n", msg, \
            a.c[0], a.c[1], a.c[2], a.c[3], a.c[4], a.c[5], a.c[6], a.c[7], \
            a.c[8], a.c[9], a.c[10], a.c[11], a.c[12], a.c[13], a.c[14], \
            a.c[15]);
    else
    printf ("%s %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n", msg, \
            a.c[0], a.c[1], a.c[2], a.c[3], a.c[4], a.c[5], a.c[6], a.c[7], \
            a.c[8], a.c[9], a.c[10], a.c[11], a.c[12], a.c[13], a.c[14], \
            a.c[15]);
#elif defined __MMX__ && ! defined __LP64__
    printf ("%s %x %x %x %x %x %x %x %x\n", msg, \
            a.c[0], a.c[1], a.c[2], a.c[3], a.c[4], a.c[5], a.c[6], a.c[7]);
    else
    printf ("%s %d %d %d %d %d %d %d %d\n", msg, \
            a.c[0], a.c[1], a.c[2], a.c[3], a.c[4], a.c[5], a.c[6], a.c[7]);
#endif
    fflush (stdout);
}
#endif

/*****************************************************************************/
/* Memory management and internals for the Ocaml bindings */

/* Allocating structures with len elements and assigned code for the set. */
add_stt
add_alloc (native_int len, native_int true_len) {
    add_stt res;
    assert (len >= 0);
    assert (add_st_mem->len == sizeof (struct add_st));
    res = (add_stt) mem_malloc (add_st_mem, sizeof (struct add_st));
    if (NULL != res) { /* Allocation succeeded */
        /* Store all the necessary information in the structure. */
        res->len = len;
        res->true_len = true_len;
        res->total = 0;
        res->cost = (ucv *) mem_malloc (ucv_mem, len * sizeof (ucv));
        res->min = (ucv *) mem_malloc (ucv_mem, len * sizeof (ucv));
        res->max = (ucv *) mem_malloc (ucv_mem, len * sizeof (ucv));
        res->union_min = (ucv *) mem_malloc (ucv_mem, len * sizeof (ucv));
        res->union_max = (ucv *) mem_malloc (ucv_mem, len * sizeof (ucv));
        if ((NULL != res->cost) && (NULL != res->min) && (NULL != res->max) &&
                (NULL != res->union_min) && (NULL != res->union_max))
            return res; /* Allocation ok */
    }
    return NULL;
}

void
add_free (add_stt x) {
    mem_free (ucv_mem, x->union_max);
    mem_free (ucv_mem, x->union_min);
    mem_free (ucv_mem, x->cost);
    mem_free (ucv_mem, x->max);
    mem_free (ucv_mem, x->min);
    mem_free (add_st_mem, x);
    return;
}

void
add_CAML_free (value v) {
    add_stt c;
    c = *(Add_st_struct(v));
    add_free (c);
    return;
}

/* TODO: The serialization functions need to be properly updated */
void
add_CAML_serialize (value c, unsigned long *wsize_32, \
        unsigned long *wsize_64) {
    native_int real_allocated_len;
    add_stt nc;
    nc = *(Add_st_struct(c));
    *wsize_64 = *wsize_32 = sizeof (add_stt);
    real_allocated_len = add_true_array_length(nc->len);
    // serialize_native(nc->len);
    // serialize_native(nc->true_len);
    // serialize_int_4(nc->total);
    // serialize_int_4(nc->scode);
    // serialize_block_1(nc->min, real_allocated_len);
    // serialize_block_1(nc->max, real_allocated_len);
    // serialize_block_1(nc->cost, real_allocated_len);
    // serialize_block_1(nc->union_min, real_allocated_len);
    // serialize_block_1(nc->union_max, real_allocated_len);
    return;
}

unsigned long
add_CAML_deserialize (void *v) {
    add_stt *res, final;
    native_int len, true_len, real_allocated_len;
    res = (add_stt *) v;
    // len = deserialize_native();
    // true_len = deserialize_native();
    real_allocated_len = add_true_array_length(len);
    final = *res = add_alloc (len, true_len);
    final->len=len;
    final->true_len=true_len;
    // final->total = deserialize_sint_4();
    // final->scode = deserialize_sint_4();
    // deserialize_block_1(final->min, real_allocated_len);
    // deserialize_block_1(final->max, real_allocated_len);
    // deserialize_block_1(final->cost, real_allocated_len);
    // deserialize_block_1(final->union_min, real_allocated_len);
    // deserialize_block_1(final->union_max, real_allocated_len);
    return (sizeof(add_stt));
}

int
add_compare_data (add_stt a, add_stt b) {
    /* Compare min and max later, the first difference not zero is returned */
    int i, len;
    unsigned char *mina, *minb, *maxa, *maxb;
    unsigned char tmp;
    len = a->len;
    mina = (unsigned char *) a->min;
    minb = (unsigned char *) b->min;
    maxa = (unsigned char *) a->max;
    maxb = (unsigned char *) b->min;
    for (i = 0; i < len; i++) {
        tmp = mina[i] - minb[i];
        if ('\0' != tmp) return ((int) tmp);
    }
    for (i = 0; i < len; i++) {
        tmp = maxa[i] - maxb[i];
        if ('\0' != tmp) return ((int) tmp);
    }
    return 0;
}

int
add_CAML_compare (value a, value b) {
    CAMLparam2 (a, b);
    int res;
    res = add_compare_data (*(Add_st_struct(a)), \
            *(Add_st_struct(b)));
    CAMLreturn (res);
}

static struct custom_operations additive = {
    "http://www.amnh.org/poy/character_additive/0.1",
    &add_CAML_free,
    &add_CAML_compare,
    custom_hash_default,
    // add_CAML_serialize,
    // add_CAML_deserialize
};

/*****************************************************************************/
/* C FUNCTIONALITY */

/* Convert a vector of additive characters to a string representation */
char *
add_to_string (add_stt a) {
    char * res;
    int i = 0, j = 0;
    int total_chars, len;
    /* Allocate enough space for the characters that will be printed, and ensure
     * we will not overflow the generated string */
    unsigned char *min, *max, *cost;
    min = (unsigned char *) a->min;
    max = (unsigned char *) a->max;
    cost = (unsigned char *) a->cost;
    total_chars = (33 * a->true_len) + 1;
    res = calloc (total_chars, sizeof (char));
    if (NULL != res) {
        res[total_chars - 1] = '\0';
        for (i = 0, j = 0; i < a->true_len; i++) {
            len = sprintf (res + j, ";(%d, %d, %d)", min[i], max[i], cost[i]);
            j += len;
        }
    }
    return (res);
}

#ifdef VECTORIZE
#define VECTOR vUInt8
#else
#define VECTOR ucv
#endif
void
add_copy (add_stt res, add_stt src) {
    size_t len1;
    VECTOR *smin;
    VECTOR *smax;
    VECTOR *scost;
    VECTOR *dmin;
    VECTOR *dmax;
    VECTOR *dcost;
    VECTOR *sumin;
    VECTOR *sumax;
    VECTOR *dumin;
    VECTOR *dumax;
    assert (res->len == src->len);
    /* Copy the information from one to the other */
    res->total = src->total;
    smin  = (VECTOR *) src->min;
    smax  = (VECTOR *) src->max;
    scost = (VECTOR *) src->cost;
    sumin = (VECTOR *) src->union_min;
    sumax = (VECTOR *) src->union_max;
    dmin  = (VECTOR *) res->min;
    dmax  = (VECTOR *) res->max;
    dcost = (VECTOR *) res->cost;
    dumin = (VECTOR *) res->union_min;
    dumax = (VECTOR *) res->union_max;
    len1 = src->len * (sizeof (VECTOR));
    memcpy (dmin, smin, len1);
    memcpy (dmax, smax, len1);
    memcpy (dcost, scost, len1);
    memcpy (dcost, scost, len1);
    memcpy (dumin, sumin, len1);
    memcpy (dumax, sumax, len1);
    return;
}
#undef VECTOR

add_stt
add_dup (add_stt src) {
    add_stt res;
    res = add_alloc (src->len, src->true_len);
    if (NULL != res) /* Allocation succeeded so we can copy the contents */
        add_copy (res, src);
    return res;
}

#ifndef VECTORIZE
#ifdef _WIN32
__inline int
#else
inline int
#endif
add_intersection (unsigned char x_min, unsigned char x_max, \
        unsigned char y_min, unsigned char y_max, unsigned char *z_min, \
        unsigned char *z_max) {
    assert (x_min <= y_min);
    /* Who is before who */
    if (x_min <= y_min) { /* x is before y */
        if (x_max >= y_min) { /* the intersection is not empty */
            if (y_max >= x_max) { /* y is not contained in x */
                *z_min = y_min;
                *z_max = x_max;
            }
            else {
                *z_min = y_min;
                *z_max = y_max;
            }
            if (!NDEBUG) {
                printf ("The intersection results are : \n");
                printf ("(%d, %d) - 0\n", x_min, x_max);
                printf ("(%d, %d) - 0\n", y_min, y_max);
                printf ("(%d, %d) - 0\n", *z_min, *z_max);
            }
            return 1;
        }
        else return 0;
    }
    else return (add_intersection (y_min, y_max, x_min, x_max, z_min, z_max));
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
add_union (unsigned char x_min, unsigned char x_max, unsigned char y_min, \
        unsigned char y_max, unsigned char *z_min, unsigned char *z_max) {
    if (x_max <= y_min) { /* If they don't overlap return the space in between. */
        *z_min = x_max;
        *z_max = y_min;
        if (!NDEBUG) printf ("(%d, %d) - %d\n", x_max, y_min, y_min - x_max);
        return 1;
    }
    else {
        *z_min = y_min;
        *z_max = x_max;
        return 0;
    }
}
#endif

/* A function that sorts the elements in a and b so that
 * res_a->min <= res_b->min */
#ifdef VECTORIZE
#ifdef _WIN32
__inline void
#else
inline void
#endif
add_sort (vUInt8 mina, vUInt8 minb, vUInt8 maxa, vUInt8 maxb, \
        vUInt8 *res_min_a, vUInt8 *res_max_a, vUInt8 *res_min_b, \
        vUInt8 *res_max_b) {
    vUInt8 compa_a_b;

    /* If b starts before a then swap them */
    /* Compare mina and minb */
    /* Store the minimum of mina and minb */
    /* Store the maximum between mina and minb */
    /* Store the partner of the minimum */
    /* Store the partner of the maximum */
    compa_a_b = vec_cmpgt (minb, mina);
    *res_min_a = vec_sel (minb, mina, compa_a_b);
    *res_min_b = vec_sel (mina, minb, compa_a_b);
    *res_max_a = vec_sel (maxb, maxa, compa_a_b);
    *res_max_b = vec_sel (maxa, maxb, compa_a_b);
    vec_emms();
    return;
}
#endif

    /* Add up all consecutive pairs of characters */
#ifdef __ALTIVEC__ /** COMMENTING */
    /* For this addition we use the parallelization to get the total in log 16
     * steps. */
#ifdef _WIN32
__inline vUInt8
#else
inline vUInt8
#endif
add_vector_sum (vUInt8 total) {
    if (!NDEBUG) print_vector ("Total : ", total, 0);
    vUInt8 tmp1, tmp2;
    tmp1 = vec_perm (total, total, \
            (vUInt8) (1, 1, 3, 3, 5, 5, 7, 7, 9, 9, 11, 11, 13, 13, 15, 15));
    tmp2 = vec_add (tmp1, total);
    if (!NDEBUG) print_vector ("Total : ", tmp2, 0);
    /* Add up positions separated at distance 2 */
    tmp1 = vec_perm (tmp2, tmp2,
            (vUInt8) (2, 1, 0, 3, 6, 5 ,4, 7, 10, 9, 8, 11, 14, 13, 12, 15));
    total = vec_add (tmp1, tmp2);
    if (!NDEBUG) print_vector ("Total : ", total, 0);
    /* Add up positions separated at distance 4 */
    tmp2 = vec_perm (total, total,
            (vUInt8) (4, 1, 2, 3, 0, 5, 6, 7, 12, 9, 10, 11, 8, 13, 14, 15));
    tmp1 = vec_add (tmp2, total);
    if (!NDEBUG) print_vector ("Total : ", tmp1, 0);
    /* Add up positions separated at distance 8, the result is in position 0 and
     * 7 */
    tmp2 = vec_perm (tmp1, tmp1, \
            (vUInt8) (8, 1, 2, 3, 4, 5, 6, 7, 0, 9, 10, 11, 12, 13, 14, 15));
    total = vec_add (tmp1, tmp2);
    if (!NDEBUG) print_vector ("Total : ", total, 0);
    return total;
}
#elif defined __MMX__ && ! defined __LP64__
    /* We can't use the nice permutations of altivec, so we better use the
     * serial addition. */
#ifdef _WIN32
__inline vUInt8
#else
inline vUInt8
#endif
add_vector_sum (vUInt8 total) {
    if (!NDEBUG) print_vector ("Total : ", total, 0);
    ucv addition;
    int i;
    unsigned char res = 0;
    addition.v = total;
    for (i = 0; i < CAPACITY_CHAR; i++)
        res += addition.c[i];
    addition.c[0] = res;
    if (!NDEBUG) print_vector ("Total : ", total, 0);
    if (!NDEBUG) printf ("The result of total is %d, and I am storing %d\n", res, addition.c[0]);
    return addition.v;
}
#endif

/* If we are using the altivec velocity engine, then we handle small sets of
 * char as the characters. */
#ifdef VECTORIZE
#ifdef _WIN32
__inline vUInt8
#else
inline vUInt8
#endif
add_item  (vUInt8 mina, vUInt8 minb, vUInt8 maxa, vUInt8 maxb, \
        vUInt8 *fin_min, vUInt8 *fin_max, vUInt8 *fin_cost) {
    vUInt8 res_min_a;    /* The minimum between a and b */
    vUInt8 res_min_b;    /* The maximum between a and b */
    vUInt8 res_max_a;    /* The partner of the minimum */
    vUInt8 res_max_b;    /* The partner of the maximum */
    vUInt8 intersect;    /* True if the ranges intersect */
    vUInt8 not_inter;    /* True if the ranges don't intersect */
    vUInt8 total;        /* Vector for total calculation */
    vUInt8 contained;    /* True if b is contained in a */
    /* First sort the intervals */
    if (!NDEBUG) {
        print_vector ("Min a : ", mina, 0);
        print_vector ("Max a : ", maxa, 0);
        print_vector ("Min b : ", minb, 0);
        print_vector ("Max b : ", maxb, 0);
    }
    add_sort (mina, minb, maxa, maxb, &res_min_a, &res_max_a, &res_min_b, \
            &res_max_b);
    if (!NDEBUG) {
        print_vector ("Sorted res_min_a : ", res_min_a, 0);
        print_vector ("Sorted res_max_a : ", res_max_a, 0);
        print_vector ("Sorted res_min_b : ", res_min_b, 0);
        print_vector ("Sorted res_max_b : ", res_max_b, 0);
    }
    /* Finally the actual procedure to get the median */
    /* If the maxa is greater than minb then they intersect */
    /* If maxb is less than maxa then b is contained in a */
    /* To find the intersection:
     * - if they intersect, minb is the lower limit of the intersection,
     *   otherwise it's maxa the lower limit.
     * - if they intersect, the maximum is either maxa or maxb (if b is
     *   contained in a), otherwise minb. We store maxa by default.
     * - If b is contained in a, then replace maxa with maxb in the
     *   intersection.
     */
    intersect = vec_or (vec_cmpgt (res_max_a, res_min_b), \
            vec_cmpeq (res_max_a, res_min_b));
    if (!NDEBUG) print_vector ("Intersection ", intersect, 1);
    not_inter = vec_nor (intersect, intersect);
    if (!NDEBUG) print_vector ("Not intersection ", not_inter, 1);
    contained = vec_cmpgt (res_max_a, res_max_b);
    if (!NDEBUG) print_vector ("Contained : ", contained, 1);
    *fin_min = vec_sel (res_max_a, res_min_b, intersect);
    *fin_max = vec_sel (res_min_b, res_max_a, intersect);
    *fin_max = vec_sel (*fin_max, res_max_b, contained);
    if (!NDEBUG) {
        print_vector ("Final min : ", *fin_min, 0);
        print_vector ("Final max : ", *fin_max, 0);
    }
    /* Now it's time to calculate the total cost, we'll try to parallelize it.
     * We will be using a butterfly for this procedure, so we substract and then
     * select only those elements that have no intersection. The actual buterfly
     * for the sum was moved to the add_vector_sum function. */
    total = vec_sub (*fin_max, *fin_min);
    if (!NDEBUG) print_vector ("Total : ", total, 0);
    total = vec_sel (not_inter, total, not_inter);
    *fin_cost = total;
    total = add_vector_sum (total);
    return total;
}
#else
/* If this is the case then use the regular intersections an unions. */
int
add_item (unsigned char mina, unsigned char minb, unsigned char maxa, \
        unsigned char maxb, unsigned char *fin_min, unsigned char *fin_max) {
    if (mina <= minb) {
        if (minb <= maxa) { /* There is intersection */
            if (maxb > maxa) { /* b is not contained in a */
                *fin_min = minb;
                *fin_max = maxa;
                return 0;
            }
            else { /* b is contained in a */
                *fin_min = minb;
                *fin_max = maxb;
                return 0;
            }
        }
        else { /* There is no intersection */
            *fin_min = maxa;
            *fin_max = minb;
            return ((int) (minb - maxa));
        }
    }
    else return (add_item (minb, mina, maxb, maxa, fin_min, fin_max));
}
#endif

void
add_downpass (add_stt a, add_stt b, add_stt res) {
    int i;
    ucv tmp;
#ifdef VECTORIZE
    unsigned char tmp_total;
#endif
    res->total = 0;
    assert (a->len == b->len);
    assert (res->len == b->len);
    for (i = 0; i < a->len; i++) {
#ifdef VECTORIZE
        tmp.v = add_item ((*(a->min + i)).v, (*(b->min + i)).v, \
                (*(a->max + i)).v, (*(b->max + i)).v, (vUInt8 *) \
                (res->min + i), (vUInt8 *) (res->max + i), (vUInt8 *)
                (res->cost + i));
        tmp_total = tmp.c[0]; /* Doing this directly in the next line
                                 was the source of a bug in MMX */
        if (!NDEBUG) printf ("The total I see is %d\n", tmp_total);
        res->total = res->total + (int) tmp_total;
        vec_emms();
#else
        tmp = add_item (a->min[i], b->min[i], a->max[i], b->max[i], \
                res->min + i, res->max + i);
        res->cost[i] = tmp;
        res->total += tmp;
#endif
    }
    return;
}

void
add_set_minimum (add_stt dst, add_stt src) {
    /* First a serial version of this code */
    int i;
    dst->total = 0;
#ifndef VECTORIZE
    for (i = 0; i < dst->len; i++) {
        if (dst->cost[i] > src->cost[i]) dst->cost[i] = src->cost[i];
        dst->total = dst->total + dst->cost[i];
    }
#else
    ucv res;
    unsigned char tmp_res;
    vUInt8 *dstt, *srct;
    dstt = (vUInt8 *) dst->cost;
    srct = (vUInt8 *) src->cost;
    for (i = 0; i < dst->len; i++) {
        if (!NDEBUG) {
            print_vector ("Current src : ", srct[i], 0);
            print_vector ("Current dst : ", dstt[i], 0);
        }
        res.v = dstt[i] = vec_min (dstt[i], srct[i]);
        res.v = add_vector_sum (res.v);
        if (!NDEBUG) {
            print_vector ("Resulting dst : ", dstt[i], 0);
            print_vector ("Resulting sum : ", res.v, 0);
        }
        tmp_res = res.c[0];
        dst->total = ((int) tmp_res) + dst->total;
        if (!NDEBUG) {
            printf ("The total I'm adding is %d", tmp_res);
            printf (" for a current total of %d\n", dst->total);
        }
    }
    vec_emms();
#endif
}

add_stt
add_median (add_stt a, add_stt b) {
    add_stt res;
    res = add_alloc (a->len, a->true_len);
    if (NULL != res)
        add_downpass (a, b, res);
    return res;
}

int
add_distance_2 (add_stt n, add_stt a, add_stt b) {
    int total;
    add_stt res, res2;
    res = add_alloc (n->len, n->true_len);
    add_downpass (n, a, res);
    res2 = add_alloc (n->len, n->true_len);
    add_downpass (n, b, res2);
    add_set_minimum (res, res2);
    total = res->total;
    add_free (res);
    add_free (res2);
    return (total);
}

int
add_distance (add_stt a, add_stt b) {
    add_stt res;
    int total;
    res = add_alloc (a->len, a->true_len);
    add_downpass (a, b, res);
    total = res->total;
    add_free (res);
    return (total);
}

native_int
add_total (add_stt a) {
    return a->total;
}

#ifdef VECTORIZE
void
add_true_union (vUInt8 amin, vUInt8 amax, vUInt8 bmin, vUInt8 bmax, \
        vUInt8 *minres, vUInt8 *maxres) {
    vUInt8 tmp1, tmp2;
    tmp1 = vec_cmplt (amin, bmin);
    *minres = vec_sel (bmin, amin, tmp1);
    tmp2 = vec_cmpgt (amax, bmax);
    *maxres = vec_sel (bmax, amax, tmp2);
    vec_emms();
    return;
}
#else
void
add_true_union (unsigned char amin, unsigned char amax, unsigned char bmin, \
        unsigned char bmax, unsigned char *minres, unsigned char *maxres) {
    if (amin <= bmin) {
        *minres = amin;
        if (amax <= bmax) *maxres = bmax;
        else *maxres = amax;
    }
    else {
        *minres = bmin;
        if (amax <= bmax) *maxres = bmax;
        else *maxres = amax;
    }
    return;
}
#endif

void
add_full_union (add_stt a, add_stt b, add_stt c) {
    assert (a->len == b->len);
    assert (a->len == c->len);
    int i, len;
    len = a->len;
#ifdef VECTORIZE
    vUInt8 *mina, *maxa, *minb, *maxb, *minc, *maxc;
    mina = (vUInt8 *) a->min;
    minb = (vUInt8 *) b->min;
    minc = (vUInt8 *) c->min;
    maxa = (vUInt8 *) a->max;
    maxb = (vUInt8 *) b->max;
    maxc = (vUInt8 *) c->max;
    for (i = 0; i < len; i++)
        add_true_union (mina[i], maxa[i], minb[i], maxb[i], minc + i, maxc + i);
    vec_emms();
#else
    for (i = 0; i < len; i++)
        add_true_union (a->min[i], a->max[i], b->min[i], b->max[i], \
                c->min + i, c->max + i);
#endif
    return;
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
add_is_contained (unsigned char parmin, unsigned char parmax, \
        unsigned char nmin, unsigned char nmax, unsigned char *resmin, \
        unsigned char *resmax) {
    if ( (parmin >= nmin) && (parmax <= nmax)) {
        *resmin = parmin;
        *resmax = parmax;
        return 1;
    }
    else return 0;
}

#ifdef VECTORIZE
void
add_median_3 (add_stt p, add_stt n, add_stt c1, add_stt c2, add_stt res) {
    vUInt8 *pmin, *nmin, *pmax, *nmax, *min1, *min2, *max1, *max2, \
        *resmin, *resmax;
    vUInt8 u12min, u12max, ipn, ipc1c2, mpc1c2, npmin, nnmin, npmax, nnmax, \
        nmin1, nmin2, nmax1, nmax2, upmin, upmax, uu12min, uu12max, ipn2;
    native_int i, len;
    len = p->len;
    pmin = (vUInt8*) p->min;
    pmax = (vUInt8*) p->max;
    nmin = (vUInt8*) n->min;
    nmax = (vUInt8*) n->max;
    min1 = (vUInt8*) c1->min;
    max1 = (vUInt8*) c1->max;
    min2 = (vUInt8*) c2->min;
    max2 = (vUInt8*) c2->max;
    resmin = (vUInt8*) res->min;
    resmax = (vUInt8*) res->max;
    for (i = 0; i < len; i++) {
        if (!NDEBUG) {
            print_vector ("Before sort parent min", pmin[i], 0);
            print_vector ("Before sort parent max", pmax[i], 0);
            print_vector ("Before sort node min", nmin[i], 0);
            print_vector ("Before sort node max", nmax[i], 0);
            print_vector ("Before sort c1 min", min1[i], 0);
            print_vector ("Before sort c1 max", max1[i], 0);
            print_vector ("Before sort c2 min", min2[i], 0);
            print_vector ("Before sort c2 max", max2[i], 0);
        }
        add_sort (pmin[i], nmin[i], pmax[i], nmax[i], &npmin, &npmax, \
                &nnmin, &nnmax);
        add_sort (min1[i], min2[i], max1[i], max2[i], &nmin1, &nmax1, \
                &nmin2, &nmax2);
        if (!NDEBUG) {
            print_vector ("After sort parent min", npmin, 0);
            print_vector ("After sort parent max", npmax, 0);
            print_vector ("After sort node min", nnmin, 0);
            print_vector ("After sort node max", nnmax, 0);
            print_vector ("After sort c1 min", nmin1, 0);
            print_vector ("After sort c1 max", nmax1, 0);
            print_vector ("After sort c2 min", nmin2, 0);
            print_vector ("After sort c2 max", nmax2, 0);
        }
        u12min = vec_min (nmin1, nmin2);
        u12max = vec_max (nmax1, nmax2);
        add_sort (pmin[i], u12min, pmax[i], u12max, &upmin, &upmax, &uu12min, \
                &uu12max);
        ipn2 = vec_or (vec_cmpgt (pmin[i], nmin[i]), vec_cmpeq (pmin[i], nmin[i]));
        ipn = vec_or (vec_cmplt (pmax[i], nmax[i]), vec_cmpeq (pmax[i], nmax[i]));
        ipn = vec_and (ipn2, ipn);
        resmin[i] = vec_sel (resmin[i], nnmin, ipn);
        resmax[i] = vec_sel (resmax[i], vec_min (npmax, nnmax), ipn);
        if (!NDEBUG) {
            print_vector ("After selecting intersection, min ", resmin[i], 0);
            print_vector ("After selecting intersection, max ", resmax[i], 0);
        }
        ipc1c2 = vec_or (vec_cmpgt (upmax, uu12min), vec_cmpeq (upmax, uu12min));
        ipc1c2 = vec_and (ipc1c2, vec_nor (ipn, ipn));
        resmin[i] = vec_sel (resmin[i], uu12min, ipc1c2);
        resmax[i] = vec_sel (resmax[i], vec_min (upmax, uu12max), ipc1c2);
        if (!NDEBUG) {
            print_vector ("After selecting intersection with children min ", resmin[i], 0);
            print_vector ("After selecting intersection with children max ", resmax[i], 0);
        }
        mpc1c2 = vec_and (vec_nor (ipn, ipn), vec_nor (ipc1c2, ipc1c2));
        resmin[i] = vec_sel (resmin[i], upmax, mpc1c2);
        resmax[i] = vec_sel (resmax[i], uu12min, mpc1c2);
        if (!NDEBUG) {
            print_vector ("After selecting median with children min ", resmin[i], 0);
            print_vector ("After selecting median with children max ", resmax[i], 0);
        }
    }
    vec_emms();
}
#else
void
add_median_3 (add_stt p, add_stt n, add_stt c1, add_stt c2, add_stt res) {
    native_int len, i;
    int inter1, inter2;
    unsigned char tmpmin, tmpmax;
    /* Check the assertions: all the characters must have the same length and
     * must have the same general code. */
    assert (p->len == n->len);
    assert (c1->len == c2->len);
    assert (n->len == c1->len);
    assert (res->len == c1->len);
    len = p->len;
    for (i = len - 1; i >= 0; i--) {
        /* Check if there is an intersection between the current node and it's
         * parent. If it is the case, set it and continue with the next
         * character. */
        inter1 = add_is_contained (p->min[i], p->max[i], n->min[i], n->max[i], \
                res->min + i, res->max + i);
        if (0 == inter1) { /* There is no intersection */
            /* Calculate either the intersection or the union */
            add_true_union (c1->min[i], c1->max[i], c2->min[i], \
                    c2->max[i], &tmpmin, &tmpmax);
            inter2 = add_intersection (tmpmin, tmpmax, p->min[i], p->max[i], \
                    res->min + i, res->max + i);
            if (0 == inter2) { /* Uff, there is no intersection now ... */
                if (p->min[i] < tmpmin)
                    add_union (p->min[i], p->max[i], tmpmin, tmpmax, \
                            res->min + i, res->max + i);
                else
                    add_union (tmpmin, tmpmax, p->min[i], p->max[i], \
                            res->min + i, res->max + i);
                add_intersection (res->min[i], res->max[i], n->min[i], n->max[i], \
                        res->min + i, res-> max + i);
            }
        }
    }
    return;
}
#endif

/* CAML bindings */
value
add_CAML_distance_and_median (value a, value b) {
    // CAMLparam2(a, b);
    // CAMLlocal2(res, final);
    add_stt tmp, *tmp2, na, nb;
    na = *(Add_st_struct(a));
    nb = *(Add_st_struct(b));
    tmp = add_median (na, nb);
    if (NULL != tmp) {
        final = caml_alloc_tuple(2);
        // res = caml_alloc_custom (&additive, sizeof (add_stt), 1, 1000000);
        tmp2 = Add_st_struct(res);
        *tmp2 = tmp;
        Store_field(final, 0, caml_copy_double ((double) add_total(tmp)));
        Store_field(final, 1, res);
        CAMLreturn (final);
    }
    // else caml_failwith ("Out of memory.");
    CAMLreturn(final); /* This condition is never reached! */
}

value
add_CAML_total (value a) {
    CAMLparam1(a);
    add_stt res;
    res = *(Add_st_struct(a));
    CAMLreturn(caml_copy_double ((double) res->total));
}

value
add_CAML_median (value a, value b) {
    CAMLparam2(a, b);
     (tmp);
    // add_stt nCAMLlocal1a, nb, nres;
    add_stt *tmp2;
    na = *(Add_st_struct(a));
    nb = *(Add_st_struct(b));
    nres = add_median (na, nb);
    // tmp = caml_alloc_custom (&additive, sizeof (add_stt), 1, 1000000);
    tmp2 = Add_st_struct (tmp);
    *tmp2 = nres;
    CAMLreturn(tmp);
}

value
add_CAML_distance (value a, value b) {
    CAMLparam2 (a, b);
    add_stt na, nb;
    int res;
    na = *(Add_st_struct(a));
    nb = *(Add_st_struct(b));
    res = add_distance (na, nb);
    if (-1 != res) CAMLreturn(caml_copy_double ((double) res));
    // else caml_failwith ("Out of memory.");
}

value
add_CAML_distance_2 (value n, value a, value b) {
    CAMLparam3 (n, a, b);
    add_stt nn, na, nb;
    int res;
    nn = *(Add_st_struct(n));
    na = *(Add_st_struct(a));
    nb = *(Add_st_struct(b));
    res = add_distance_2 (nn, na, nb);
    if (-1 != res) CAMLreturn(caml_copy_double ((double) res));
    // else caml_failwith ("Out of memory.");
}

value
add_CAML_create (value min, value max) {
    CAMLparam2(min, max);
    // CAMLlocal1(final);  /* The result of the operations */
    native_int len;     /* Length of the set of characters being loaded */
    native_int i;
    add_stt res, *tmp;
    unsigned char *mmin, *mmax, *mcost;
    /* All the arrays have the same length */
    len = Wosize_val(min);
    assert (len == Wosize_val(max));
    res = add_alloc (add_array_length (len), len);
    if (NULL != res) {  /* Allocation suceeded */
        res->total = 0;
        mmin = (unsigned char *) res->min;
        mmax = (unsigned char *) res->max;
        mcost = (unsigned char *) res->cost;
        for (i = 0; i < len; i++) {
            /* Copy the arrays excepting the last subvector */
            mmin[i] = (unsigned char) Int_val (Field(min, i));
            mmax[i] = (unsigned char) Int_val (Field(max, i));
            mcost[i] = (unsigned char) 0;
        }
#ifdef VECTORIZE
        for ( ; 0 != i % CAPACITY_CHAR; i++) {
            mmin[i] = 0;
            mmax[i] = 0;
            mcost[i] = 0;
        }
#endif
        /* Allocate the caml type and return it */
        // final = caml_alloc_custom (&additive, sizeof (add_stt), 1, 1000000);
        tmp = Add_st_struct(final);
/*         if (!NDEBUG) { */
/*             printf ("Observed characters min:\n"); */
/*             for (i = 0; i < res->len; i++)  */
/*                 print_vector ("", (*(res->min + i)).v, 0); */
/*             printf ("Observed characters max:\n"); */
/*             for (i = 0; i < res->len; i++) */
/*                 print_vector ("", (*(res->max + i)).v, 0); */
/*         } */
        *tmp = res;
        CAMLreturn (final);
    } /* Hum, we couldn't allocate the memory, we fail in the ocaml side. */
    // else caml_failwith ("Out of memory.");
}

value
add_CAML_copy (value dst, value src) {
    CAMLparam2(dst, src);
    add_stt ndst, nsrc;
    ndst = *(Add_st_struct(dst));
    nsrc = *(Add_st_struct(src));
    add_copy (ndst, nsrc);
    CAMLreturn(Val_unit);
}

value
add_CAML_dup (value src) {
    CAMLparam1(src);
    // CAMLlocal1(res);
    add_stt tmp, nsrc;
    nsrc = *(Add_st_struct(src));
    tmp = add_dup (nsrc);
    // res = caml_alloc_custom (&additive, sizeof(add_stt), 1, 1000000);
    *(Add_st_struct(res)) = tmp;
    CAMLreturn(res);
}

value
add_CAML_set_state (value s, value p, value min, value max) {
    CAMLparam4(s, p, min, max);
    add_stt ns;
    native_int pos;
    ns = *(Add_st_struct(s));
    pos = Long_val(p);
#ifdef VECTORIZE
    assert (ns->len * CAPACITY_CHAR > pos);
    (*(ns->min)).c[pos] = Int_val(min);
    (*(ns->max)).c[pos] = Int_val(max);
#else
    assert (ns->len > pos);
    ns->min[pos] = Int_val(min);
    ns->max[pos] = Int_val(max);
#endif
    CAMLreturn(Val_unit);
}

value
add_CAML_get_max (value s, value p) {
    CAMLparam2(s, p);
    add_stt ns;
    native_int pos;
    ns = *(Add_st_struct(s));
    pos = Long_val(p);
#ifdef VECTORIZE
    assert (ns->len * CAPACITY_CHAR > pos);
    CAMLreturn(Val_int((*(ns->max)).c[pos]));
#else
    assert (ns->len > pos);
    CAMLreturn(Val_int(ns->max[pos]));
#endif
}

value
add_CAML_get_min (value s, value p) {
    CAMLparam2(s, p);
    add_stt ns;
    native_int pos;
    ns = *(Add_st_struct(s));
    pos = Long_val(p);
#ifdef VECTORIZE
    assert (CAPACITY_CHAR * ns->len > pos);
    CAMLreturn(Val_int((*(ns->min)).c[pos]));
#else
    assert (ns->len > pos);
    CAMLreturn(Val_int(ns->min[pos]));
#endif
}

value
add_CAML_get_cost (value s, value p) {
    CAMLparam2(s, p);
    add_stt ns;
    native_int pos;
    ns = *(Add_st_struct(s));
    pos = Long_val(p);
#ifdef VECTORIZE
    assert (CAPACITY_CHAR * ns->len > pos);
    CAMLreturn(caml_copy_double ((double) (*(ns->cost)).c[pos]));
#else
    assert (ns->len > pos);
    CAMLreturn(caml_copy_double ((double) ns->cost[pos]));
#endif
}

value
add_CAML_get_length (value s) {
    CAMLparam1(s);
    add_stt ns;
    ns = *(Add_st_struct(s));
    CAMLreturn(Val_long(ns->true_len));
}

value
add_CAML_register (value a) {
    CAMLparam1(a);
    caml_register_custom_operations (&additive);
    CAMLreturn(Val_unit);
}

value
add_CAML_median_imp (value a, value b, value res) {
    CAMLparam3(a, b, res);
    add_stt na, nb, nres;
    na = *(Add_st_struct(a));
    nb = *(Add_st_struct(b));
    nres = *(Add_st_struct(res));
    add_downpass (na, nb, nres);
    CAMLreturn(Val_unit);
}

value
add_CAML_median_3 (value p, value n, value c1, value c2) {
    CAMLparam4(p, n, c1, c2);
    // CAMLlocal1(nres);
    add_stt tmp, np, nn, nc1, nc2, *res;
    np = *(Add_st_struct(p));
    nn = *(Add_st_struct(n));
    nc1 = *(Add_st_struct(c1));
    nc2 = *(Add_st_struct(c2));
    tmp = add_alloc (nn->len, nn->true_len);
    memcpy (tmp->union_min, nn->union_min, nn->len);
    memcpy (tmp->union_max, nn->union_max, nn->len);
    memcpy (tmp->cost, nn->cost, nn->len);
    // nres = caml_alloc_custom (&additive, sizeof(add_stt), 1, 1000000);
    res = Add_st_struct(nres);
    add_median_3 (np, nn, nc1, nc2, tmp);
    *res = tmp;
    CAMLreturn(nres);
}

value
add_CAML_full_union (value a, value b, value c) {
    CAMLparam3(a, b, c);
    add_stt na, nb, nc;
    na = *(Add_st_struct(a));
    nb = *(Add_st_struct(b));
    nc = *(Add_st_struct(c));
    add_full_union (na, nb, nc);
    CAMLreturn(Val_unit);
}

value
add_CAML_to_string (value a) {
    CAMLparam1(a);
    // CAMLlocal1(resv);
    add_stt na;
    char *res;
    na = *(Add_st_struct(a));
    res = add_to_string (na);
    resv = caml_copy_string (res + 1);
    free (res);
    CAMLreturn (resv);
}

value
add_CAML_register_mem (value unit) {
    CAMLparam1(unit);
    caml_register_global_root ((void *) &add_st_mem);
    caml_register_global_root ((void *) &ucv_mem);
    CAMLreturn(Val_unit);
}

value
add_CAML_compare_data (value a, value b) {
    CAMLparam2(a, b);
    int res;
    add_stt na, nb;
    na = *(Add_st_struct (a));
    nb = *(Add_st_struct (b));
    res = add_compare_data (na, nb);
    CAMLreturn (Val_int (res));
}
