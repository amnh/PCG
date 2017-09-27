/* $Id: med_util.c 48 2005-04-13 15:56:25Z ron $
   Written by Adam Siepel, Spring 2001 
   Copyright 2001, Adam Siepel */

/* Miscellaneous utility functions used by reversal median code */

#include "med_util.h"
#include "invdist.h"
#include "simpleio.h"
#include <math.h>

/* Returns the median score of a potenial median with respect to three
   specified "vertex" genomes.  "gens" is assumed to be an array of at
   least three pointers to genome_structs, and all genomes are assumed
   to be of size "ngenes".  Currently, a circular measure is always
   used. */
int
median_score ( struct genome_struct **gens,
               struct genome_struct *median, int ngenes, distmem_t * distmem )
{

    return ( invdist_noncircular ( median, gens[0], 0, ngenes, distmem ) +
             invdist_noncircular ( median, gens[1], 0, ngenes, distmem ) +
             invdist_noncircular ( median, gens[2], 0, ngenes, distmem ) );
}

/* Allocates space for and returns a new permutation equal to the
   specified one. */
int *
newperm ( int *perm, int ngenes )
{
    int i;
    int *newperm = calloc ( ngenes, sizeof ( int ) );
    for ( i = 0; i < ngenes; i++ )
        newperm[i] = perm[i];
    return newperm;
}

/* Copies one permutation to another (space must already be allocated
   for the new one).  Use like strcpy. */
void
permcopy ( int *dest, int *source, int sz )
{
    int i;
    for ( i = 0; i < sz; i++ )
        dest[i] = source[i];
}

/* Compares two permutations (use like strcmp) */
int
permcmp ( int *perm1, int *perm2, int n )
{
    int i;
    for ( i = 0; i < n; i++ )
        if ( perm1[i] != perm2[i] )
            return 1;
    return 0;
}

/* Creates and returns a distmem_t structure for use in calculating
   pairwise inversion distances */
distmem_t *
new_distmem ( int ngenes )
{
    distmem_t *distmem = ( distmem_t * ) malloc ( sizeof ( distmem_t ) );

    /* below snipped from GRAPPA */
    distmem->hammingArr =
        ( int * ) malloc ( ( ngenes + 1 ) * 2 * sizeof ( int ) );
    if ( distmem->hammingArr == ( int * ) NULL )
        fprintf ( stderr, "ERROR: hammingArr NULL\n" );
    distmem->perm1 = ( int * ) malloc ( ( 2 * ngenes + 2 ) * sizeof ( int ) );
    if ( distmem->perm1 == ( int * ) NULL )
        fprintf ( stderr, "ERROR: perm1 NULL\n" );
    distmem->perm2 = ( int * ) malloc ( ( 2 * ngenes + 2 ) * sizeof ( int ) );
    if ( distmem->perm2 == ( int * ) NULL )
        fprintf ( stderr, "ERROR: perm2 NULL\n" );
    distmem->perm = ( int * ) malloc ( ( 2 * ngenes + 2 ) * sizeof ( int ) );
    if ( distmem->perm == ( int * ) NULL )
        fprintf ( stderr, "ERROR: perm NULL\n" );
    distmem->done = ( int * ) malloc ( ( 2 * ngenes + 2 ) * sizeof ( int ) );
    if ( distmem->done == ( int * ) NULL )
        fprintf ( stderr, "ERROR: done NULL\n" );
    distmem->greyEdges =
        ( int * ) malloc ( ( 2 * ngenes + 2 ) * sizeof ( int ) );
    if ( distmem->greyEdges == ( int * ) NULL )
        fprintf ( stderr, "ERROR: greyEdges NULL\n" );
    distmem->stack = ( int * ) malloc ( ( 2 * ngenes + 2 ) * sizeof ( int ) );
    if ( distmem->stack == ( int * ) NULL )
        fprintf ( stderr, "ERROR: stack NULL\n" );
    distmem->oriented =
        ( int * ) malloc ( ( 2 * ngenes + 2 ) * sizeof ( int ) );
    if ( distmem->oriented == ( int * ) NULL )
        fprintf ( stderr, "ERROR: oriented NULL\n" );
    distmem->cc = ( int * ) malloc ( ( 2 * ngenes + 2 ) * sizeof ( int ) );
    if ( distmem->cc == ( int * ) NULL )
        fprintf ( stderr, "ERROR: cc NULL\n" );
    distmem->labeled =
        ( int * ) malloc ( ( 2 * ngenes + 2 ) * sizeof ( int ) );
    if ( distmem->labeled == ( int * ) NULL )
        fprintf ( stderr, "ERROR: labeled NULL\n" );
    distmem->components = ( component_t * )
        malloc ( ( 2 * ngenes + 2 ) * sizeof ( component_t ) );
    if ( distmem->components == ( component_t * ) NULL )
        fprintf ( stderr, "ERROR: components NULL\n" );
    distmem->uf = UFalloc ( 2 * ngenes + 2 );

    return distmem;
}

/* Frees all memory associated with the specified distmem_t structure
   (including that allocated for the structure itself) */
void
free_distmem ( distmem_t * distmem )
{
    free ( distmem->hammingArr );
    free ( distmem->perm1 );
    free ( distmem->perm2 );
    free ( distmem->perm );
    free ( distmem->done );
    free ( distmem->greyEdges );
    free ( distmem->stack );
    free ( distmem->oriented );
    free ( distmem->cc );
    free ( distmem->labeled );
    free ( distmem->components );
    free ( distmem->uf );
    free ( distmem );
}

/* "Condenses" three genomes by reducing to single genes all sequences
   of three or more genes present in all three genomes (considering
   reverse complements to be equal).  Return value indicates whether
   condensing occurred (1->condensing, 0->no condensing).  */
int
condense ( struct genome_struct **old_gens, int old_ngenes,
           struct genome_struct **new_gens, int new_ngenes )
{
    int retval = 0;

    return retval;
}

/* Finds the "circular identity" of the specified permutation,
   which we define as the equivalent permutation that begins with the
   gene "+1".*/
void
find_circular_identity ( int *perm, int *id, int ngenes )
{
    int i, j;
    int sign = 1;

    for ( i = 0; i < ngenes; i++ )
    {
        if ( perm[i] == 1 )
            break;
        else if ( perm[i] == -1 )
        {
            sign = -1;
            break;
        }
    }

    if ( i == ngenes )
    {
        fprintf ( stderr,
                  "FATAL ERROR: permutation has no element labeled '1'.\n" );
        exit ( 1 );
    }

    if ( sign == 1 )
    {
        for ( j = 0; j < ngenes; j++ )
        {
            id[j] = perm[i];
            if ( ++i == ngenes )
                i = 0;
        }
    }
    else
    {                           /* sign == -1 */
        for ( j = 0; j < ngenes; j++ )
        {
            id[j] = -perm[i];
            if ( --i < 0 )
                i = ngenes - 1;
        }
    }
}

/* Copies a permutation, simultaneously representing the effect of the
   specified reversal */
void
copy_with_reversal ( int *dest, int *src, int n, Reversal * rev )
{
    int i, mid;
    for ( i = 0; i < rev->start; i++ )
        dest[i] = src[i];

    mid = ceil ( ( rev->stop - rev->start ) / 2.0 );
    for ( i = 0; i < mid; i++ )
    {
        dest[rev->start + i] = -1 * src[rev->stop - 1 - i];
        dest[rev->stop - 1 - i] = -1 * src[rev->start + i];
    }

    for ( i = rev->stop; i < n; i++ )
        dest[i] = src[i];
}
