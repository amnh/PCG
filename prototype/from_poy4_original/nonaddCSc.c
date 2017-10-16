/*
 * $Id: nonaddCSc.c 2857 2008-05-19 13:22:27Z andres $
 */
#ifndef _WIN32
#include <stdint.h>
#endif
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <caml/alloc.h>
#include <caml/intext.h>
#define NDEBUG 1
#include <assert.h>
#include "nonaddCSc.h"
#include <stdio.h>
#include <string.h>
#include <math.h>



/** \file
 * \brief C interface to OCaml for bit-packed non-additive characters.
 *
 * This file implements processing a set of equally-weighted non-additive
 * characters.  Data are held in the #_naca_t structure, and we use
 * vectors (type #vect) to speed up processing on superscalar and
 * cache-limited platforms.
 */


/** \name Platform-specific
    @{
    These are platform-specific features to allow for the processing of multiple
    data types on platforms that support different types of vectorization.  We
    abstract all our operations on vectors to allow for superscalar
    architectures to make use of them.  Either way, we pack consecutive elements
    into vectors, so that even on systems that do not support MMX or AltiVec, we
    can keep all our data in cache as we perform our operations. */

/* Default to short */
#ifndef CHARSIZE
/** \brief Size in bits of the characters to be represented.
 *
 * Note that this source file supports characters of size 8, 16, and 32 (with
 * the OCaml-based restriction that only 31 bits of the 32 may be used on 32-bit
 * systems).  You can define #CHARSIZE on the command line; otherwise, it
 * defaults to 16.
 * \hideinitializer
 * */
#define CHARSIZE 16
#endif

#define MIN(a,b) = ( (a) < (b) ? (a) : (b) )
#define VCOUNT(a, b, c) {\
int __vcount__temp = c-1; \
nac *__vcount__tv = (nac *)&a; \
if (__vcount__temp > BLOCK_LEN - 1) __vcount__temp = BLOCK_LEN - 1; \
for(; __vcount__temp >= 0 ; __vcount__temp --) {\
if(__vcount__tv[__vcount__temp]) b++; \
}}


#if CHARSIZE == 8
#define CHARTYPE unsigned char

#elif CHARSIZE == 16
/** \def CHARTYPE
 * \brief Type for characters of size #CHARSIZE.
 *
 * One of <tt>unsigned char</tt>, <tt>unsigned short</tt>, or <tt>unsigned
 * int</tt>
 * \hideinitializer
 */
#define CHARTYPE unsigned short

#elif CHARSIZE == 32
#define CHARTYPE unsigned int

#else
#error "Unrecognized character size."

#endif
/** \brief Basic type for each element */
typedef CHARTYPE nac;


#define FLOAT_OPERATIONS

/** \def VECT_SIZE
 * \brief Number of bits in a vector */
/** \def BLOCK_LEN
 * \brief Number of characters in a block */

/** \def VLOOP_BEGIN
 * Macro to begin processing a vector \hideinitializer */
/** \def VLOOP_END
 * Macro to end processing a vector \hideinitializer */
/** \def VAND
 * Macro to compute the logical AND of two vectors */
/** \def VOR
 * Macro to compute the logical OR of two vectors */
/** \def VCMP
 * Macro to compare two vectors.  Returns a mask of 1s for each equal element,
 * and a mask of 0s for each non-equal element. */
/** \def VSET
 * Macro to set vector \c a to the result of \c b */

/** \def ALLOC_DATA
 * Allocate an array of vectors big enough to hold \c size elements */
/** \def GET_DATA
 * Return a #nac * for working with the data element in #nacat
 */
/** \def FREE_DATA
 * Free the data allocated with an #ALLOC_DATA */
/** \typedef vect
    \brief Vector type for target platform

    This is the natively-supported type for vectors:  MMX or AltiVec registers,
    or, if that is not available, a simple array type. */



/** This macro helps us debug allocations and frees */
/* inline vect * */
/* nonadd_print_ptr (const char *str, vect *v) */
/* { */
/* /\*     fprintf (stderr, "%s %p\n", str, v); *\/ */
/*     return v; */
/* } */
#define nonadd_print_ptr(a, b) (b)



#if defined( __APPLE_ALTIVEC__ )
/* To use this, we need the options:
 * -maltivec -mabi=altivec -mpim-altivec */

/* Note that I am NOT SURE that this will also work on 32-bit PowerPC
 * architectures.  It should.  Hopefully I will be able to test that soon. */

#define VECT_SIZE 128
#define BLOCK_LEN (VECT_SIZE / CHARSIZE)

/* characters */
typedef vector CHARTYPE vect;

#define ZERO_VECTOR (union _vectnac_u)  ((vect ){ 0x0000000000000000ULL, 0x0000000000000000ULL })

/* These are used later for bitwise operations */
#define VLOOP_BEGIN
#define VLOOP_END
#define VAND(a, b) vec_and (a, b)
#define VOR(a, b)  vec_or  (a, b)
#define VCMP(a, b) vec_cmpeq (a, b)
#define VSET(a, b) a = (b)
#define VGET(a) a

#define ALLOC_DATA(size) (vect *) \
        nonadd_print_ptr("alloc",malloc (sizeof(vect) * ((size / BLOCK_LEN) + 1)))
#define GET_DATA(nacat) ((nac *)nacat->data)
#define FREE_DATA(nacat) (free (nonadd_print_ptr("free",(nacat->data))))



#elif defined( __MMX__ )
/* Simple MMX instructions can operate on vectors of size up to 8 bytes in
 * length;  and they can operate on two longs, four shorts, or eight chars. */

#include <xmmintrin.h>
#include <malloc.h>

/* in bits */
#define VECT_SIZE 64

#ifdef __ICC
#define ZERO_VECTOR (union _vectnac_u) { (0x0000000000000000ULL) }
#else
#define ZERO_VECTOR (union _vectnac_u) ((vect) 0x0000000000000000ULL)
#endif

/* # of characters in vect */
#define BLOCK_LEN (VECT_SIZE / CHARSIZE)

typedef __m64 vect;

#undef  FLOAT_OPERATIONS
#define FLOAT_OPERATIONS _mm_empty()

#define VLOOP_BEGIN
#define VLOOP_END
#define VAND(a, b) _mm_and_si64 (a, b)
#define VOR(a, b)  _mm_or_si64  (a, b)
#define VSET(a, b) a = (b)
#define VGET(a) a

# if CHARSIZE == 8
#define VCMP(a, b) _mm_cmpeq_pi8 (a, b)

# elif CHARSIZE == 16
#define VCMP(a, b) _mm_cmpeq_pi16 (a, b)

# elif CHARSIZE == 32
#define VCMP(a, b) _mm_cmpeq_pi32 (a, b)

# endif

#define ALLOC_DATA(size) (vect *) \
nonadd_print_ptr ("alloc", malloc (sizeof(vect) * ((size / BLOCK_LEN) + 1)))
#define GET_DATA(nacat) ((nac *)nacat->data)
#define FREE_DATA(nacat) (free (nonadd_print_ptr ("free", nacat->data)))


#else  /* no MMX */
#ifndef VECT_SIZE
#define VECT_SIZE 64
#endif

#define BLOCK_LEN (VECT_SIZE / CHARSIZE)

#define ZERO_VECTOR (union _vectnac_u) ((nac) 0x0UL)

typedef nac vect[BLOCK_LEN];

#define VLOOP_BEGIN int __iter_for_vloop; \
for (__iter_for_vloop = 0; __iter_for_vloop < BLOCK_LEN; __iter_for_vloop++) {
#define VLOOP_END }
#define VAND(a, b) ( (a) & (b) )
#define VOR(a, b)  ( (a) | (b) )
#define VGET(a) ( (a)[__iter_for_vloop] )
#define VCMP(a, b) ( ((a) == (b)) ? ( ~((CHARTYPE) 0)) : 0 )

#define VSET(a, b) (a)[__iter_for_vloop] = (b)

#undef VCOUNT
#define VCOUNT(a, b, c) {\
nac *__vcount__tv = (nac *)&(a); \
if( __vcount__tv[__iter_for_vloop] && __iter_for_vloop < c ) b++; \
}


#define ALLOC_DATA(size) (vect *) \
nonadd_print_ptr("alloc",malloc (sizeof(vect) * ((size / BLOCK_LEN) + 1)))
#define GET_DATA(nacat) ((nac *)nacat->data)
#define FREE_DATA(nacat) (free (nonadd_print_ptr("free",nacat->data)))

#endif

/** @} */


#ifdef _WIN32
__inline void
#else
inline void
#endif
nonadd_print_vect (const char *str, vect *vv)
{
    nac *v = (nac *)vv;
    int i;

    printf ("%s: ", str);
    for (i = 0; i < BLOCK_LEN; i++) {
        nac tmp = v[i];
        if (!tmp)
            printf ("0");

        while (tmp) {
            if (tmp & 1)
                printf ("1");
            else
                printf ("0");

            tmp >>= 1;
        }
        printf (" ");
    }
    printf ("\n");
}

const long nonadd_max_size = CHARSIZE;

/** Structure that holds information about our record.

    While this structure itself is mutable, the OCaml interface to it is almost
    entirely immutable.  A few exceptions are made (they are clearly marked
    below) to allow for convenience functions when creating a new element of
    this type; once that element is created, however, calling mutating functions
    is <em>completely unsafe.</em>  The OCaml interface in
    <tt>nonaddCS.mli</tt> ensures that this does not happen.
*/
struct _naca_t
{
    long len;                   /** number of elements total */
    long heur;                  /** heuristic code to use */
    long code;                  /** code for the overall set */
    long added_cost;            /** cost for making this element as a median */
    vect *data;                 /** data are stored as \typedef vect elements */
};
typedef struct _naca_t * nacat;

/* currently unused */
void
nonadd_print (const nacat n, const char *label)
{
    long i;
    nac *data = GET_DATA (n);
    printf ("\n%s representation:\n", label);
    for (i = 0; i < n->len; i++) {
        printf ("%ld\t%ld\n", i, (long)data[i]);
    }
}

#ifdef _WIN32
__inline nac
#else
inline nac
#endif
nonadd_nac_of_bit (long i)              /* which bit to set */
{
    long res = 1L << i;
    return res;
}

union _vectnac_u {
    nac n;
    vect v;
};

static union _vectnac_u *_zero_nonadd_vector = NULL;

/** Inline function to calculate the median of two elements. */
#ifdef _WIN32
__inline void
#else
inline void
#endif
nonadd_make_union_par (const nacat au,
        const nacat bu,
        nacat res               /** Variable into which to write the result */
    )
{
    long i;
    const long upto = (long) ceil (res->len / (float) BLOCK_LEN);
    vect v_int;
    union _vectnac_u zero = ZERO_VECTOR;
    nac *res_nacp;
    long nelts = res->len;

    res_nacp = & (zero.n);
    for (i = 0; i < BLOCK_LEN; i++) {
        res_nacp[i] = 0;
    }

    for (i = 0; i < upto; i++) {
#define v_res res->data[i]
        VLOOP_BEGIN;

        VSET (v_res, VOR (VGET (au->data[i]), VGET (bu->data[i])));

        VLOOP_END;
#undef v_res

        nelts -= BLOCK_LEN;
    }

    FLOAT_OPERATIONS;

}

/** Inline function to calculate the median of two elements. */
#ifdef _WIN32
__inline void
#else
inline void
#endif
nonadd_make_union (const nacat self,
        const nacat au,
        const nacat bu,
        nacat res               /** Variable into which to write the result */
    )
{
    assert (self->len == au->len);
    assert (au->len == bu->len);
    long i;
    const long upto = (long) ceil (res->len / (float) BLOCK_LEN);
    vect v_int;
    union _vectnac_u zero = ZERO_VECTOR;
    nac *res_nacp;
    long nelts = res->len;

    res_nacp = & (zero.n);
    for (i = 0; i < BLOCK_LEN; i++) {
        res_nacp[i] = 0;
    }

    for (i = 0; i < upto; i++) {
#define v_res res->data[i]
        VLOOP_BEGIN;

        VSET (v_int, VOR (VGET (au->data[i]), VGET (bu->data[i])));
        VSET (v_res, VOR (VGET (self->data[i]), VGET (v_int)));

        VLOOP_END;
#undef v_res

        nelts -= BLOCK_LEN;
    }

    FLOAT_OPERATIONS;

}


/** Inline function to calculate the median of two elements. */
#ifdef _WIN32
__inline void
#else
inline void
#endif
nonadd_median (const nacat a,
        const nacat b,
        nacat res               /** Variable into which to write the result */
    )
{
    long i;
    const long upto = (long) ceil (res->len / (float) BLOCK_LEN);
    vect v_int;
    vect v_unn;
    union _vectnac_u zero = ZERO_VECTOR;
    nac *res_nacp;
    long added_cost = 0;
    long nelts = res->len;

    res_nacp = & (zero.n);
    for (i = 0; i < BLOCK_LEN; i++) {
        res_nacp[i] = 0;
    }

    for (i = 0; i < upto; i ++) {
#define v_res res->data[i]
        VLOOP_BEGIN;

        VSET (v_int, VAND (VGET (a->data[i]), VGET (b->data[i])));
        VSET (v_unn, VOR  (VGET (a->data[i]), VGET (b->data[i])));
        VSET (v_res, VCMP (VGET (v_int), VGET (zero.v)));

        VCOUNT(v_res,added_cost,nelts);
        VSET (v_res, VAND (VGET (v_res), VGET (v_unn)));
        VSET (v_res, VOR  (VGET (v_res), VGET (v_int)));

        VLOOP_END;
#undef v_res

        nelts -= BLOCK_LEN;
    }

    FLOAT_OPERATIONS;

    res->added_cost = added_cost;
}

/** Inline function to take the 3-median for the up (final) pass.  This function
    follows the Fitch 1971 algorithm.
*/
#ifdef _WIN32
__inline void
#else
inline void
#endif
nonadd_median_3 (const nacat _A,       /** Ancestor */
          const nacat _N,       /** Preliminary state */
          const nacat _D1,      /** Child one */
          const nacat _D2,      /** Child two */
          nacat _F              /** Where to write final state */
    )
{
    long i;
    const long upto = (long) ceil (_F->len / (float) BLOCK_LEN);
    vect intermed, temp, was_union, is_contained;
    union _vectnac_u zero = ZERO_VECTOR;
    nac *res_nacp;

    res_nacp = (nac *) & (zero.n);
    for (i = 0; i < BLOCK_LEN; i++) {
        res_nacp[i] = 0;
    }

    for (i = 0; i < upto; i ++) {

#define F _F->data[i]
#define A _A->data[i]
#define N _N->data[i]
#define D1 _D1->data[i]
#define D2 _D2->data[i]
        VLOOP_BEGIN;

        VSET (intermed, VOR (VGET (D1), VGET (D2)));
        VSET (intermed, VAND (VGET (A), VGET (intermed)));
        VSET (intermed, VOR (VGET (N), VGET (intermed)));

        VSET (temp, VGET (intermed));

        VSET (was_union, VAND (VGET (D1), VGET (D2)));
        VSET (was_union, VCMP (VGET (was_union), VGET (zero.v)));

        VSET (intermed, VOR (VGET (intermed), VGET (A)));
        VSET (intermed, VAND (VGET (was_union), VGET (intermed)));
        VSET (temp, VOR (VGET (temp), VGET (intermed)));

        /* ---------- */

        VSET (is_contained, VCMP (VAND (VGET (A), VGET (N)), VGET (A)));
        VSET (F, VAND (VGET (is_contained), VGET (A)));

        /* here it is actually: is *NOT* contained */
        VSET (is_contained, VCMP (VGET (is_contained), VGET (zero.v)));
        VSET (temp, VAND (VGET (is_contained), VGET (temp)));

        VSET (F, VOR (VGET (F), VGET (temp)));
/*         nonadd_med_print_vect ("ancestor", VGET (A)); */
/*         nonadd_med_print_vect ("prelim", VGET (N)); */
/*         nonadd_med_print_vect ("child1", VGET (D1)); */
/*         nonadd_med_print_vect ("child2", VGET (D2)); */
/*         nonadd_med_print_vect ("final", VGET (F)); */
/*         printf ("\n"); fflush (stdout); */

/*         VLOOP_END; */
/*     } */
/*     for (i = 0; i < upto; i++) { */
/*         VLOOP_BEGIN; */

        VLOOP_END;

#undef F
#undef A
#undef N
#undef D1
#undef D2
    }

    FLOAT_OPERATIONS;
}

/** Inline function to calculate the rerooting median of two elements.  We use
 * this to find the root node between two nodes for which the final states are
 * known.  See Goloboff 1993.
 *
 * Note: what do we do for the stored unions? */
#ifdef _WIN32
__inline void
#else
inline void
#endif
nonadd_reroot_median (const nacat a,
               const nacat b,
               nacat res        /** Variable into which to write the result */
    )
{
    long i;
    const long upto = (long) ceil (res->len / (float) BLOCK_LEN);
    long nelts = res->len;

    for (i = 0; i < upto; i ++) {

        VLOOP_BEGIN;
        VSET (res->data[i], VOR  (VGET (a->data[i]), VGET (b->data[i])));
        VLOOP_END;

        nelts -= BLOCK_LEN;
    }

    FLOAT_OPERATIONS;
}
#define Nonadd_Custom_val(v,n) n = (nacat) Data_custom_val(v); \
                                if (n->len > 0) n->data = (vect *) ((nacat) (n + 1)); \
                                else n->data = NULL
/** Compare two values.  They should have the same number of elements and the
 * same codes. */
int
nonadd_nacat_compare (value v1, value v2)
{
    CAMLparam2 (v1, v2);
    nacat n1, n2;
    long i, diff;
    nac *data1, *data2;

    Nonadd_Custom_val(v1,n1);
    Nonadd_Custom_val(v2,n2);

    diff = n1->len - n2->len;
    if (diff != 0)
        CAMLreturn (diff);

    data1 = GET_DATA (n1);
    data2 = GET_DATA (n2);

    for (i = 0; i < n1->len; i++) {
        diff = data1[i] - data2[i];
        if (diff != 0)
            CAMLreturn (diff);
    }

    CAMLreturn (0);
}

/** \name Serialization
    @{
    We need to use the correct serialization functions for our element types.
 */
#if __LP64__
#define caml_serialize_long caml_serialize_int_8
#define caml_deserialize_long caml_deserialize_sint_8
#else
#define caml_serialize_long caml_serialize_int_4
#define caml_deserialize_long caml_deserialize_sint_4
#endif

#if CHARSIZE == 8
#define caml_serialize_int caml_serialize_int_1
#define caml_serialize_block caml_serialize_block_1
#define caml_deserialize_uint caml_deserialize_uint_1
#define caml_deserialize_block caml_deserialize_block_1

#elif CHARSIZE == 16
#define caml_serialize_int caml_serialize_int_2
#define caml_serialize_block caml_serialize_block_2
#define caml_deserialize_uint caml_deserialize_uint_2
#define caml_deserialize_block caml_deserialize_block_2

#elif CHARSIZE == 32
#define caml_serialize_int caml_serialize_int_4
#define caml_serialize_block caml_serialize_block_4
#define caml_deserialize_uint caml_deserialize_uint_4
#define caml_deserialize_block caml_deserialize_block_4

#endif

/** @} */

#define compute_size(len) ((sizeof (struct _naca_t) + ((sizeof(vect) * ((len / BLOCK_LEN) + 1)))))
/** Serialize a #nacat value */
void
nonadd_nacat_serialize (value v,
                      unsigned long *wsize_32, unsigned long *wsize_64)
{
    nacat n;
    nac *data;

    Nonadd_Custom_val(v,n);
    data = GET_DATA (n);

    /* We always write the same size */
    *wsize_32 = *wsize_64 = compute_size(n->len);
    caml_serialize_long (n->len);
    caml_serialize_long (n->heur);
    caml_serialize_long (n->code);
    caml_serialize_long (n->added_cost);
    if (n->len > 0) 
        caml_serialize_block_1 (data, (sizeof(vect) * ((n->len / BLOCK_LEN) + 1)));
    return;
}

/** Deserialize a #nacat value */
unsigned long
nonadd_nacat_deserialize (void *dst)
{
    nacat n;
    nac *data;

    n = (nacat) dst;
    n->len = caml_deserialize_long();
    n->heur = caml_deserialize_long();
    n->code = caml_deserialize_long();
    n->added_cost = caml_deserialize_long();
    n->data = (vect *) ((struct _naca_t *) (n + 1));
    if (n->len > 0)
        caml_deserialize_block_1 (n->data, (sizeof(vect) * ((n->len / BLOCK_LEN) + 1)));
    else n->data = NULL;
    return (compute_size(n->len));
}



/** Struct to define a nonadditive character set as an OCaml custom block */
static struct custom_operations naca_custom = {
    "http://www.amnh.org/poy/char_nonadd_/1.0",
    custom_finalize_default,
    nonadd_nacat_compare,
    custom_hash_default,
    nonadd_nacat_serialize,
    nonadd_nacat_deserialize
};


/* Register the unmarshal function */
void char_nonadd_CAML_register_unmarshal()
{
    register_custom_operations (&naca_custom);
}



/** Allocates a new #_naca_t structure of given length without initializing the
 * contents.  #_naca_t.len, #_naca_t.heur, #_naca_t.added_cost */
void
nonadd_make_new_unsafe (int len, value v)
{
    nacat art;

    Nonadd_Custom_val(v,art);
    art->len = len;
    art->heur = 0;
    art->added_cost = 0;
    if(len != 0) 
        art->data = (vect *) ((struct _naca_t *) (art + 1));
    else 
        art->data = NULL;
    return;
}

/** Makes a new #_naca_t structure, initializing data and codes (to an illegal
 * state).  Codes are initialized sequentially starting at zero, and data are
 * all initialized to zero, which is an illegal state for these characters. */
value
char_nonadd_CAML_make_new (value len, value code)
{
    CAMLparam2 (len, code);
    CAMLlocal1 (v);
    long ilen, icode, i;
    nacat art;
    nac *data;

    ilen = Long_val (len);
    icode = Long_val (code);
    v = caml_alloc_custom (&naca_custom, (compute_size(ilen)), 1, 45000);
    nonadd_make_new_unsafe (ilen, v);

    Nonadd_Custom_val(v,art);
    art->code = icode;

    data = GET_DATA (art);

    CAMLreturn (v);
}

/** CAML interface to #nonadd_make_new_unsafe. */
value
char_nonadd_CAML_make_new_unsafe (value len, value code)
{
    CAMLparam2 (len, code);
    CAMLlocal1 (v);
    long ilen, icode;
    nacat art;

    ilen = Long_val (len);
    icode = Long_val (code);
    v = caml_alloc_custom (&naca_custom, (compute_size(ilen)), 1, 45000);
    nonadd_make_new_unsafe (ilen, v);

    Nonadd_Custom_val(v,art);
    art->code = icode;

    CAMLreturn (v);
}

/** Get the code of the current character set. */
value
char_nonadd_CAML_code (value v)
{
    CAMLparam1 (v);
    nacat art;

    Nonadd_Custom_val(v,art);
    CAMLreturn (Val_long (art->code));
}

/** Set the code of the current character set.  This function does not mutate
    the value; instead, it returns a copy.
value
char_nonadd_CAML_set_code (value v1, value code)
{
    CAMLparam2 (v1, code);
    CAMLlocal1 (v2);
    nacat art1, art2;

    Nonadd_Custom_val(v1,art1);
    v2 = caml_alloc_custom (&naca_custom,
                           sizeof(struct _naca_t), 1, 45000);
    nonadd_clone (art1, v2);
    art2 = (nacat)Data_custom_val (v2);

    art2->code = Long_val (code);

    CAMLreturn (v2);
}
*/

void
char_nonadd_CAML_set_code_mutate (value v1, value code)
{
    CAMLparam2 (v1, code);
    nacat art;

    Nonadd_Custom_val(v1,art);
    art->code = Long_val (code);

    CAMLreturn0;
}

/* Warning: mutates! */
void
char_nonadd_CAML_set_elt_code (value v, value vi, value vcode)
{
    CAMLparam3 (v, vi, vcode);
    nacat art;

    failwith ("We don't support codes in nonadditive characters");
    Nonadd_Custom_val(v,art);

    CAMLreturn0;
}

void
char_nonadd_CAML_set_elt_bit (value v, value loc, value val)
{
    CAMLparam3 (v, loc, val);
    nacat art;
    long iloc;
    long ival;
    nac *data;

    /* We don't update the union set right now ... */
    failwith ("char_nonadd_CAML_set_elt_bit no longer supported");

    iloc = Long_val (loc);
    ival = Long_val (val);

    /* If val is out-of-bounds, we raise an exception to OCaml */
    if (ival > nonadd_max_size)
        caml_invalid_argument ("Argument out of machine-imposed bounds");

    Nonadd_Custom_val(v,art);
    data = GET_DATA (art);

    /* Check that this is the only copy of data before mutating! */
    data[iloc] |= nonadd_nac_of_bit (ival);

    CAMLreturn0;
}

/* Warning:  mutating! */
void
char_nonadd_CAML_set_elt (value v, value vi, value vval)
{
    CAMLparam3 (v, vi, vval);
    nacat art;
    nac *data;
    nac setto;

    setto = Unsigned_long_val (vval);
    Nonadd_Custom_val(v,art);
    data = GET_DATA (art);
    data[Long_val (vi)] = setto;
    CAMLreturn0;
}

value
char_nonadd_CAML_basic_union (value v, value self, value a, value b)
{
    CAMLparam4 (v, self, a, b);
    nacat na, nb, nself, nres;
    long len;

    Nonadd_Custom_val(a,na);
    Nonadd_Custom_val(b,nb);
    Nonadd_Custom_val(self,nself);
    len = na->len;
    assert (nself->len == na->len);
    assert (na->len == nb->len);

    /* performs an allocation */
    Nonadd_Custom_val(v,nres);

    /* Copy the codes and heuristic from a */
    /* memcpy (dest, source, bytes) */
    /* Write the median to nres->data */
    assert (nself->len == na->len);
    assert (na->len == nb->len);
    nonadd_make_union (nself, na, nb, nres);
    CAMLreturn (Val_unit);
}

value
char_nonadd_CAML_basic_union_par (value v, value a, value b)
{
    CAMLparam3 (v, a, b);
    nacat na, nb, nres;
    long len;

    Nonadd_Custom_val(a,na);
    Nonadd_Custom_val(b,nb);
    len = na->len;
    assert (na->len == nb->len);

    /* performs an allocation */
    assert (na->len == nb->len);
    Nonadd_Custom_val(v,nres);

    assert (nres->len == nb->len);
    /* Copy the codes and heuristic from a */
    /* memcpy (dest, source, bytes) */
    /* Write the median to nres->data */
    nonadd_make_union_par (na, nb, nres);
    CAMLreturn (Val_unit);
}

value
char_nonadd_CAML_basic_median (value v, value a, value b)
{
    CAMLparam3 (v, a, b);
    nacat na, nb, nres;
    long len;

    Nonadd_Custom_val(a,na);
    Nonadd_Custom_val(b,nb);
    len = na->len;
    assert (len = nb->len);

    /* performs an allocation */
    Nonadd_Custom_val(a,na);
    Nonadd_Custom_val(b,nb);
    Nonadd_Custom_val(v,nres);

    /* Copy the codes and heuristic from a */
    /* memcpy (dest, source, bytes) */
    nres->heur = na->heur;

    /* Write the median to nres->data */
    nonadd_median (na, nb, nres);
    CAMLreturn (Val_unit);
}

value
char_nonadd_CAML_reroot_median (value v, value a, value b)
{
    CAMLparam3 (v, a, b);
    nacat na, nb, nres;
    long len;

    Nonadd_Custom_val(a,na);
    len = na->len;

    /* performs an allocation */
    Nonadd_Custom_val(a,na);
    Nonadd_Custom_val(b,nb);
    Nonadd_Custom_val(v,nres);

    /* Copy the codes and heuristic from a */
    /* memcpy (dest, source, bytes) */
    nres->heur = na->heur;

    /* Write the median to nres->data */
    nonadd_reroot_median (na, nb, nres);
    FLOAT_OPERATIONS;           /* do we need this? */
    CAMLreturn (v);
}

value
char_nonadd_CAML_basic_median_mutate (value a, value b, value res)
{
    CAMLparam3 (a, b, res);
    nacat na, nb, nres;
    long len;

    Nonadd_Custom_val(a,na);
    Nonadd_Custom_val(b,nb);
    Nonadd_Custom_val(res,nres);

    len = na->len;

    assert (na->len == nb->len);
    assert (na->len == nres->len);

    /* Copy the codes and heuristic from a */
    /* memcpy (dest, source, bytes) */
    nres->heur = na->heur;

    /* Write the median to nres->data */
    nonadd_median (na, nb, nres);
    CAMLreturn (Val_unit);
}

value
char_nonadd_CAML_median_3 (value v, value vA, value vN, value vD1, value vD2)
{
    CAMLparam5 (v, vA, vN, vD1, vD2);
    nacat A, N, D1, D2, F;


    /* performs an allocation */
    Nonadd_Custom_val(vA,A);
    Nonadd_Custom_val(vN,N);
    Nonadd_Custom_val(vD1,D1);
    Nonadd_Custom_val(vD2,D2);
    Nonadd_Custom_val(v,F);

    /* Copy the codes and heuristic from a */
    /* memcpy (dest, source, bytes) */
    F->heur = A->heur;

    FLOAT_OPERATIONS;

    F->added_cost = N->added_cost;

    /* Using the rules from Fitch 1971 */
    nonadd_median_3 (A, N, D1, D2, F);
    FLOAT_OPERATIONS;

    CAMLreturn (Val_unit);
}

void
char_nonadd_CAML_median_3_mutate (value vA, value vN, value vD1, value vD2,
                                  value vRes)
{
    CAMLparam5 (vA, vN, vD1, vD2, vRes);
    nacat A, N, D1, D2, F;

    Nonadd_Custom_val(vA,A);
    Nonadd_Custom_val(vN,N);
    Nonadd_Custom_val(vD1,D1);
    Nonadd_Custom_val(vD2,D2);
    Nonadd_Custom_val(vRes,F);

    assert (A->len == F->len);

    /* Copy the codes and heuristic from a */
    /* memcpy (dest, source, bytes) */
    F->heur = A->heur;

    FLOAT_OPERATIONS;

    F->added_cost = N->added_cost;

    /* Using the rules from Fitch 1971 */
    nonadd_median_3 (A, N, D1, D2, F);
    FLOAT_OPERATIONS;
    CAMLreturn0;
}

#ifdef _WIN32
__inline long
#else
inline long
#endif
nonadd_equal (nacat na, nacat nb)
{
    long i;
    nac *dataa, *datab;
    dataa = GET_DATA (na);
    datab = GET_DATA (nb);

    for (i = 0; i < na->len; i++) {
        if (dataa[i] != datab[i])
            return 0L;
    }
    return 1L;
}

/* Distance is the cardinality of the set of differences between a and b */
value
char_nonadd_CAML_distance (value a, value b)
{
    CAMLparam2 (a, b);
    nacat na, nb;
    long len;
    long res;
    nac *adata, *bdata;
    long i;

    Nonadd_Custom_val(a,na);
    Nonadd_Custom_val(b,nb);
    len = na->len;

    if (len == 0) {
        CAMLreturn (Val_long (0));
    }

    adata = GET_DATA (na);
    bdata = GET_DATA (nb);

    res = 0;

    for (i = 0; i < len; i++) {
        if (0 == (adata[i] & bdata[i]))
            res ++;
    }

    CAMLreturn (Val_long (res));
}

/* Return a list of distances; these are either 0 or 1. */
value
char_nonadd_CAML_distance_list (value a, value b)
{
    CAMLparam2 (a, b);
    CAMLlocal3 (list, temp_pair, temp_list);
    CAMLlocal2 (d_one, d_zero);
    long i;
    nacat na, nb;
    nac *adata, *bdata;

    d_one = caml_copy_double (1.);
    d_zero = caml_copy_double (0.);

    Nonadd_Custom_val(a,na);
    Nonadd_Custom_val(b,nb);

    adata = GET_DATA (na);
    bdata = GET_DATA (nb);

    list = Val_int (0);

    assert (na->len == nb->len);
    for (i = na->len - 1; i >= 0; i--) {
        temp_pair = caml_alloc_tuple (2);
        Nonadd_Custom_val(a,na);
        Nonadd_Custom_val(b,nb);
        Store_field (temp_pair, 0, Val_long (0));
        Store_field (temp_pair, 1,
                     ((adata[i] & bdata[i]) ?
                      d_zero :
                      d_one));

        temp_list = caml_alloc_tuple (2);
        Nonadd_Custom_val(a,na);
        Nonadd_Custom_val(b,nb);
        Store_field (temp_list, 0, temp_pair);
        Store_field (temp_list, 1, list);

        list = temp_list;
    }

    CAMLreturn (list);
}

value
char_nonadd_CAML_median_cost (value a)
{
    CAMLparam1 (a);
    CAMLlocal1 (temp);
    nacat art;
    long ltemp;
    double dtemp;

    Nonadd_Custom_val(a,art);

    ltemp = art->added_cost;

    FLOAT_OPERATIONS;
    dtemp = ltemp;

    CAMLreturn (caml_copy_double (dtemp));
}

value
char_nonadd_CAML_equal (value a, value b)
{
    CAMLparam2 (a, b);
    nacat na, nb;
    Nonadd_Custom_val(a,na);
    Nonadd_Custom_val(b,nb);

    if (nonadd_equal (na, nb))
        CAMLreturn (Val_true);
    else
        CAMLreturn (Val_false);
}

/* Number of elements */
value
char_nonadd_CAML_cardinal (value a)
{
    CAMLparam1 (a);
    nacat na;
    Nonadd_Custom_val(a,na);
    CAMLreturn (Val_long (na->len));
}

value
char_nonadd_CAML_elt_to_list (value va, value vindex)
{
    CAMLparam2 (va, vindex);
    CAMLlocal2 (res, temp_val);
    long i, index;
    unsigned long val;
    nacat a;
    nac *adata;

    Nonadd_Custom_val(va,a);
    index = Long_val (vindex);
    adata = GET_DATA (a);

    i = 0;
    val = adata[index];

    res = Val_int (0);          /* empty list */

    /* Right-shift and check the rightmost bit until all the set bits have been
     * added to the list */
    while (val != 0) {
        if (val & 1) {
            temp_val = caml_alloc_tuple (2);
            Store_field (temp_val, 0, Val_long (i));
            Store_field (temp_val, 1, res);
            res = temp_val;
        }
        val >>= 1;
        ++i;
    }

    CAMLreturn (res);
}

/* Return the integer stored for a given character.  This should only be used
 * (if at all) within the Char_nonadd_c module. */
value
char_nonadd_CAML_to_int (value va, value vindex)
{
    CAMLparam2 (va, vindex);
    nacat a;
    long i;
    nac *adata;

    Nonadd_Custom_val(va,a);
    adata = GET_DATA (a);
    i = Long_val (vindex);
    CAMLreturn (Val_long (adata[i]));
}

value
char_nonadd_CAML_to_list (value va)
{
    CAMLparam1 (va);
    CAMLlocal5 (res, temp_list, temp_val, temp_elt, d_zero);
    nacat a;
    long i;
    nac *adata;
    nacat copy_data;
    long initial_len;

    d_zero = caml_copy_double (0.);

    Nonadd_Custom_val(va,a);
    copy_data = a;
    initial_len = a->len;
    adata = GET_DATA (a);
    res = Val_long (0);          /* empty list */

    for (i = a->len - 1; i >= 0; i--) {
        assert (a == copy_data);
        assert (a->len == initial_len);
        temp_elt = caml_alloc_tuple (2);
        Store_field (temp_elt, 0, Val_long (0));
        Store_field (temp_elt, 1, Val_long (adata[i]));

        temp_val = caml_alloc_tuple (3);
        Store_field (temp_val, 0, Val_long (0));
        Store_field (temp_val, 1, temp_elt);
        Store_field (temp_val, 2, d_zero);

        temp_list = caml_alloc_tuple (2);
        Store_field (temp_list, 0, temp_val);
        Store_field (temp_list, 1, res);

        res = temp_list;
    }

    CAMLreturn (res);
}

value
char_nonadd_CAML_poly_items (value c, value pol) {
    CAMLparam2 (c, pol);
    nacat art;
    nac *data;
    int res = 0, i, j;
    int counter, tmp;
    int pol_counter;
    
    pol_counter = Int_val(pol);
    Nonadd_Custom_val(c,art);
    data = GET_DATA (art);
    for (i = 0; i < art->len; i++) {
        counter = 0;
        tmp = data[i];
        for (j = 0; j < CHARSIZE; j++) {
            counter += (tmp & 1);
            tmp = tmp << 1;
        }
        if (counter == pol_counter) res++;
    }

    CAMLreturn (Val_int (res));

}

value
char_nonadd_CAML_of_list_helper (value v, value list, value vlen)
{
    CAMLparam3 (v, list, vlen);
    long len, i;
    nacat art;
    nac *adata;
    CAMLlocal1 (elt);

    len = Int_val (vlen);
    Nonadd_Custom_val(v,art);
    adata = GET_DATA (art);

    for (i = 0; i < len; i++) {
        elt = Field (list, 0);

        adata[i] = Unsigned_long_val (Field (Field (elt, 1), 1));

        list = Field (list, 1);
    }

    CAMLreturn (Val_unit);
}

value
char_nonadd_CAML_get_heu (value a)
{
    CAMLparam1 (a);
    nacat art;

    Nonadd_Custom_val(a,art);

    CAMLreturn (Val_long (art->heur));
}


value
char_nonadd_CAML_dist_2 (value va, value vb, value vc)
{
    CAMLparam3 (va, vb, vc);
    long dist = 0;
    nacat a, b, c;
    nac *adata, *bdata, *cdata;
    long len;
    long i;

    Nonadd_Custom_val(va,a);
    Nonadd_Custom_val(vb,b);
    Nonadd_Custom_val(vc,c);

    adata = GET_DATA (a);
    bdata = GET_DATA (b);
    cdata = GET_DATA (c);

    len = a->len;
    dist = 0;

    for (i = 0; i < len; i++) {
        if (!(adata[i] & bdata[i]) && !(adata[i] & cdata[i]))
            dist++;
    }

    CAMLreturn (Val_long (dist));
}
