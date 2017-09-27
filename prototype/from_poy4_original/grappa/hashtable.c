/* $Id: hashtable.c 48 2005-04-13 15:56:25Z ron $
   Written by Adam Siepel, Spring 2001 
   Copyright 2001, Adam Siepel */

/* Custom hashtable.  Expects keys to be signed permutations, and only
   keeps track of membership -- i.e., does not allow association of
   data with keys */

/* Compile with -DUSEDB to use "db" behind the interface presented by
   these functions */

/* Compile with -DTHREADSAFE to allow multiple threads to have
   concurrent access to a single hashtable.  Note: we assume that "db"
   is not threadsafe, and enforce locking ourselves when USEDB is
   defined (db man page says "None of the access methods provide any
   form of concurrent access, locking, or transactions").  When USEDB
   is not defined (i.e., when the custom hashtable is in use), we
   allow parallel computation of the hashing function, and enforce
   locking separately within each bucket.  In all cases, locking is
   accomplished according to the rules of the readers-writers problem */

#include "hashtable.h"
#include "errno.h"
#include <sys/stat.h>
#include <fcntl.h>
#include "stdlib.h"
#include "math.h"
#include "med_util.h"

/* Initialize the hash */
Hashtable *
new_hashtable ( int ngenes, int expected_size, float loading_factor )
{
    Hashtable *h = ( Hashtable * ) malloc ( sizeof ( Hashtable ) );
#ifdef USEDB
    h->db = dbopen ( NULL, O_RDWR, S_IRWXU, DB_HASH, NULL );
    if ( h->db == NULL )
    {
        fprintf ( stderr, "Error creating hashtable.\n" );
        exit ( errno );
    }
    h->ngenes = ngenes;
#ifdef THREADSAFE
    mythread_rwlock_init ( &h->rwlock );
#endif
#else
    int i;
    h->ngenes = ngenes;

    /* Choose the number of slots so as to fill pages evenly */
    h->nbuckets = PAGESIZE / sizeof ( int * );  /* experiments indicate that
                                                   there is little value in
                                                   having this size be larger
                                                   than one page */

    /* The size of each bucket depends on the loading factor, the expected
       size of the table, and the number of buckets */
    h->bucketsize =
        ceil ( loading_factor * expected_size / ( float ) h->nbuckets );

    /* Choose the number of digits to sample when calculating hashkeys */
    h->idxdigits = SAMPLE_PERCENTAGE * h->ngenes;
    if ( h->idxdigits < MIN_DIGITS )
        h->idxdigits = MIN_DIGITS;
    if ( h->idxdigits > h->ngenes )
        h->idxdigits = h->ngenes;

    h->table = ( int ** ) calloc ( h->nbuckets, sizeof ( int * ) );
    h->sizes = ( int * ) calloc ( h->nbuckets, sizeof ( int ) );
    if ( h->table == NULL || h->sizes == NULL )
    {
        fprintf ( stderr, "Error allocating space for hashtable.\n" );
        exit ( errno );
    }
#ifdef THREADSAFE
    h->rwlock =
        ( mythread_rwlock_t * ) calloc ( h->nbuckets,
                                         sizeof ( mythread_rwlock_t ) );
    if ( h->rwlock == NULL )
    {
        fprintf ( stderr, "Error allocating space for hashtable.\n" );
        exit ( errno );
    }
#endif

    for ( i = 0; i < h->nbuckets; i++ )
    {
        h->table[i] = NULL;     /* we will allocate these as needed */
        h->sizes[i] = h->bucketsize;
#ifdef THREADSAFE
        mythread_rwlock_init ( &h->rwlock[i] );
#endif
    }

#endif
    return h;
}

/* Insert specified permutation */
void
ht_insert ( Hashtable * h, int *perm )
{
#ifdef USEDB
    DBT key, data;

#ifdef THREADSAFE
    mythread_rwlock_wrlock ( &h->rwlock );
#endif

    key.data = perm;
    key.size = h->ngenes * sizeof ( int );
    data.data = malloc ( sizeof ( int ) );
    *( int * ) data.data = 1;
    data.size = sizeof ( int * );

    if ( h->db->put ( h->db, &key, &data, 0 ) != 0 )
    {
        fprintf ( stderr, "Error inserting in hashtable.\n" );
        exit ( errno );
    }

#ifdef THREADSAFE
    mythread_rwlock_wrunlock ( &h->rwlock );
#endif

#else
    int k;

    k = hash ( h, perm );

#ifdef THREADSAFE
    mythread_rwlock_wrlock ( &h->rwlock[k] );
#endif

    ht_insert_key ( h, perm, k );

#ifdef THREADSAFE
    mythread_rwlock_wrunlock ( &h->rwlock[k] );
#endif

#endif
}

#ifndef USEDB
void
ht_insert_key ( Hashtable * h, int *perm, int k )
{
    int i = 0;

    if ( h->table[k] == NULL )
    {
        h->table[k] =
            ( int * ) calloc ( h->sizes[k] * h->ngenes, sizeof ( int ) );
        if ( h->table[k] == NULL )
        {
            fprintf ( stderr, "Error allocating memory for hashtable.\n" );
            exit ( errno );
        }
        for ( i = 0; i < h->sizes[k]; i++ )
            h->table[k][i * h->ngenes] = 0;
    }

    for ( i = 0;; i++ )
    {
        if ( i == h->sizes[k] )
            ht_realloc_bucket ( h, k );
        if ( h->table[k][i * h->ngenes] == 0 )
        {
            permcopy ( &h->table[k][i * h->ngenes], perm, h->ngenes );
            break;
        }
    }
}

void
ht_clear ( Hashtable * h )
{
    int i, j;
    for ( i = 0; i < h->nbuckets; i++ )
    {
        if ( h->table[i] != NULL )
            for ( j = 0; j < h->sizes[i] &&
                  h->table[i][j * h->ngenes] != 0; j++ )
                h->table[i][j * h->ngenes] = 0;
    }
}

#endif

/* Find specified permutation.  Return 1 if it is present, 0
   otherwise.  The "create" parameter allows you to require that the
   entry be created if it cannot be found (even if create is 1, the
   return value will still indicate whether the entry existed
   previously) */
int
ht_find ( Hashtable * h, int *perm, int create )
{
#ifdef USEDB
    DBT key, data;
    int retval;

    key.data = perm;
    key.size = h->ngenes * sizeof ( int );

#ifdef THREADSAFE
    if ( create == 1 )
        mythread_rwlock_wrlock ( &h->rwlock );
    else
        mythread_rwlock_rdlock ( &h->rwlock );
#endif

    retval = h->db->get ( h->db, &key, &data, 0 );

    if ( retval == 1 && create == 1 )
    {
        data.data = malloc ( sizeof ( int ) );
        *( int * ) data.data = 1;
        data.size = sizeof ( int * );
        if ( h->db->put ( h->db, &key, &data, 0 ) != 0 )
        {
            fprintf ( stderr, "Error inserting in hashtable.\n" );
            exit ( errno );
        }
    }

#ifdef THREADSAFE
    if ( create == 1 )
        mythread_rwlock_wrunlock ( &h->rwlock );
    else
        mythread_rwlock_rdunlock ( &h->rwlock );
#endif

    return ( retval == 0 ? 1 : 0 );

#else
    int i, k;
    int retval = 0;
    k = hash ( h, perm );
/*   printf("%d\n", k);  */

#ifdef THREADSAFE
    if ( create == 1 )
        mythread_rwlock_wrlock ( &h->rwlock[k] );
    else
        mythread_rwlock_rdlock ( &h->rwlock[k] );
#endif

    if ( h->table[k] == NULL )
    {
        if ( create == 1 )
        {
            ht_insert_key ( h, perm, k );
        }
    }
    else
    {
        for ( i = 0;; i++ )
        {
            if ( i == h->sizes[k] )
            {
                if ( create == 1 )
                {
                    ht_realloc_bucket ( h, k );
                }
                else
                    break;
            }
            if ( h->table[k][i * h->ngenes] == 0 )
            {
                if ( create == 1 )
                    permcopy ( &h->table[k][i * h->ngenes], perm, h->ngenes );
                break;
            }
            else if ( permcmp ( &h->table[k][i * h->ngenes],
                                perm, h->ngenes ) == 0 )
            {
                retval = 1;
                break;
            }
        }
    }
#ifdef THREADSAFE
    if ( create == 1 )
        mythread_rwlock_wrunlock ( &h->rwlock[k] );
    else
        mythread_rwlock_rdunlock ( &h->rwlock[k] );
#endif

    return retval;
#endif
}

/* Free memory for hash */
void
ht_free ( Hashtable * h )
{
#ifdef USEDB
    if ( h->db->close ( h->db ) != 0 )
    {
        fprintf ( stderr, "Error closing hashtable.\n" );
        exit ( errno );
    }
#else
    int i;
    for ( i = 0; i < h->nbuckets; i++ )
        free ( h->table[i] );
    free ( h->table );
#endif
    free ( h->sizes );
    free ( h );
}

#ifndef USEDB
void
ht_realloc_bucket ( Hashtable * h, int k )
{
    int tmp, i;
    tmp = h->sizes[k];
    h->sizes[k] *= 2;
    h->table[k] = ( int * ) realloc ( h->table[k],
                                      h->sizes[k] * h->ngenes *
                                      sizeof ( int ) );
    if ( h->table[k] == NULL )
    {
        fprintf ( stderr, "Error allocating extra space for hashtable.\n" );
        exit ( errno );
    }
    for ( i = tmp; i < h->sizes[k]; i++ )
        h->table[k][i * h->ngenes] = 0;
}

/* Hashing function: operates on a modest percentage of the digits */
int
hash ( Hashtable * h, int *perm )
{
    int i, a;
    unsigned long k;

    a = h->ngenes / h->idxdigits;   /* sample every ath digit in the perm,
                                       for a total of idxdigits samples */

    k = 0;
    for ( i = 0; i < h->idxdigits; i++ )
    {
        k = MULTIPLIER * k + abs ( perm[i * a] );
    }
    return k % h->nbuckets;
}
#endif
