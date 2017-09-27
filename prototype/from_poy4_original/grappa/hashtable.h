/* $Id: hashtable.h 48 2005-04-13 15:56:25Z ron $
   Written by Adam Siepel, Spring 2001 
   Copyright 2001, Adam Siepel */

/* Custom hashtable.  Expects keys to be signed permutations, and only
   keeps track of membership -- i.e., does not allow association of
   data with keys */

/* Compile with -DUSEDB to use "db" rather than the custom hashtable */

/* Compile with -DTHREADSAFE to permit multi-threaded access to a
   single hashtable */

#ifndef HASH_H
#define HASH_H

#include <stdio.h>
#ifdef THREADSAFE
#include "mythread_rwlock.h"
#endif

#ifdef USEDB
#include "db_185.h"
#endif

#define PAGESIZE 4096           /* size of a page in bytes; used in
                                   choosing the number of buckets in the
                                   hashtable */
#define SAMPLE_PERCENTAGE 0.2   /* percentage of digits to sample when
                                   calculating hashkeys */
#define MIN_DIGITS 4            /* minimum number of digits to sample
                                   (unless the number of genes is even
                                   fewer) */
#define MULTIPLIER 31           /* Kernighan and Pike report this
                                   number as having been empirically
                                   supported in hashing functions */

typedef struct hashtable Hashtable;
struct hashtable
{
    int ngenes;
#ifdef USEDB
    DB *db;
#ifdef THREADSAFE
    mythread_rwlock_t rwlock;
#endif

#else
    int **table;                /* the lookup table: each entry points
                                   to a dynamically-allocated array.
                                   A linked list is NOT used; an array
                                   has much better caching properties */
    int nbuckets;               /* number of "buckets" in table */
    int bucketsize;             /* starting size of buckets */
    int *sizes;                 /* actual sizes of buckets (sizes can
                                   change due to reallocs) */
    int idxdigits;              /* number of digits to sample for hash
                                   function */
#ifdef THREADSAFE
    mythread_rwlock_t *rwlock;  /* array of read-write locks; one lock
                                   each bucket */
#endif
#endif
};

/* Return a new hashtable, initialized according to specified parameters */
Hashtable *new_hashtable ( int ngenes, int expected_size,
                           float loading_factor );

/* Insert a new permutation in the hash */
void ht_insert ( Hashtable * h, int *perm );

/* Find a permutation in the hash (return 1 if found, 0 if not found);
   if create == 1, permutation will be added if it is not already
   present */
int ht_find ( Hashtable * h, int *perm, int create );

/* Free memory associated with hashtable */
void ht_free ( Hashtable * h );

#ifndef USEDB
/* Insert given a key value */
void ht_insert_key ( Hashtable * h, int *perm, int k );

void ht_clear ( Hashtable * h );

/* Reallocate memory for a bucket that has exceeded its capacity */
void ht_realloc_bucket ( Hashtable * h, int k );

/* Hashing function */
int hash ( Hashtable * h, int *perm );
#endif

#endif
