/* $Id: sorting_reversal_median.c 48 2005-04-13 15:56:25Z ron $
   Written by Adam Siepel, Fall 2001
   Copyright 2001, Adam Siepel */

/* Code to find an exact reversal median of three signed permutations.
   Algorithm is derived in Chapter 4 of Siepel, A., "Exact Algorithms
   for the Reversal Median Problem," Master's Thesis, University of
   New Mexico, 2001. */

#include <stdio.h>
#include "structs.h"
#include "vertex_factory.h"
#include "hashtable.h"
#include "priority_stack.h"
#include "all_sorting_reversals.h"
#include "med_util.h"
#include "sorting_reversal_median.h"
#include "invdist.h"
#include "simpleio.h"
#include <math.h>
int VIncrease = 100;
extern VertexFactory *newvf;
/* Find a reversal median of the given signed permutations and store
   it in the genome_struct "median" (expected to be allocated by
   calling code).  Input permutations are expected to be specified as
   gen[0], gen[1], and gen[2].  Number of genes ngenes must be
   specified also.  A MedianMemory struct may or may not be passed in;
   if this argument is NULL, a new one will be created. */
void
find_reversal_median ( struct genome_struct *median,
                       struct genome_struct **gen,
                       int ngenes, MedianMemory * medmem )
{

    int i, j, k, pass, found_one = 0, stop, nneighbors;
    int d[NPERMS][NPERMS];
    Vertex *vorig;
    int MIN_MED = 0, MAX_MED = 0, distbetween = 0;
    struct genome_struct dummy;
    struct genome_struct *g[NPERMS];
    MedianMemory *mm = NULL;

    /* use existing MedianMemory struct or create a new one */
    if ( medmem == NULL )
        mm = new_median_memory ( ngenes, 0, ( ngenes + 1 ) * 2 );
    else
    {
        mm = medmem;
        reset_median_memory ( mm );
    }

    /* find pairwise distances and set lower bound for median score */
    for ( MIN_MED = 0, i = 0; i < NPERMS - 1; i++ )
    {
        for ( j = i + 1; j < NPERMS; j++ )
        {
            d[i][j] = d[j][i] =
                invdist_noncircular ( gen[i], gen[j], 0, ngenes,
                                      mm->distmem );
            MIN_MED += d[i][j];
        }
    }
    MIN_MED = ceil ( ( MIN_MED ) / 2.0 ) * VIncrease / 100;

    /* establish starting vertex */
    vorig = get_vertex ( mm->vf );

    /* find longest edge and initialize accordingly */
    for ( g[0] = NULL, i = 0; i < NPERMS - 1 && g[0] == NULL; i++ )
    {
        for ( j = i + 1; j < NPERMS && g[0] == NULL; j++ )
        {
            /* these are simply the other 2 pairwise distances (besides
               d[i][j]) */
            int oth1 = d[i + 1][( j + 1 ) % NPERMS];
            int oth2 =
                ( j - i == 1 ) ? d[i][( j + 1 ) % NPERMS] : d[i + 1][j];

            /* see if d[i][j] is the longest of the 3 pw distances */
            if ( d[i][j] >= oth1 && d[i][j] >= oth2 )
            {
                /* set g[0] (the origin) to vertex opposite longest distance, and
                   g[1], g[2] to the other two */
                int origidx = ( j - i == 1 ) ? ( j + 1 ) % NPERMS : i + 1;
                g[0] = gen[origidx];
                g[1] = gen[i];
                g[2] = gen[j];
                vorig->d1 = d[origidx][i];
                vorig->d2 = d[origidx][j];
                distbetween = d[i][j];
            }
        }
    }

    /* initialize global upper bound */
    MAX_MED = vorig->d1 + vorig->d2;

    /* complete initialization of starting vertex */
    permcopy ( vorig->perm, g[0]->genes, ngenes );
    vorig->best_possible_score = MIN_MED;
    vorig->worst_possible_score = MAX_MED;
    vorig->distance = 0;

    /* initialize median struct, which will hold best median so far */
/*  median->genes = (int*)malloc(ngenes * sizeof(int));*/
    permcopy ( median->genes, vorig->perm, ngenes );

    /* This is the main loop, controlling the two passes through the
       algorithm */
    for ( pass = 1; pass <= 2; pass++ )
    {                           /* I changed 2 to 1 */
        stop = 0;
        nneighbors = 0;
        ht_clear ( mm->h );
        ht_insert ( mm->h, vorig->perm );
        ps_push ( mm->ps, vorig, MAX_MED );

        /* Loop until median is found or no more possibilities remain (the
           latter can be true only during pass 1) */
        while ( stop == 0 )
        {
            Vertex *n, *v;
            Reversal *rev;
            int count;

            if ( ( v = ( Vertex * ) ps_pop ( mm->ps ) ) == NULL ||
                 v->best_possible_score >= MAX_MED )
            {
                stop = 1;
                break;
            }

            dummy.genes = v->perm;  /* to accommodate conventions of
                                       find_all_sorting_reversals */

            for ( i = 0; i < NREVTYPES; i++ )
                for ( j = 0; j < NPERMS; j++ )
                    clear_list ( &mm->revs[i][j] );

            /* find sorting and neutral reversals wrt each of g[0], g[1],
               and g[2] */
            for ( j = 0; j < NPERMS; j++ )
            {
                if ( pass == 1 && j == 0 )
                    continue;
                /* sorting and neutral reversals wrt
                   origin not necessary on first pass */

                /* only find neutral reversals during pass 2 */
                find_all_sorting_reversals ( &mm->revs[SORTING][j],
                                             pass ==
                                             1 ? NULL : &mm->revs[NEUTRAL][j],
                                             &dummy, g[j], ngenes, mm->rsm );
            }


            /* mark each set of reversals with the appropriate bit masks */
            for ( i = 0; i < NREVTYPES; i++ )
            {
                for ( j = 0; j < NPERMS; j++ )
                {
                    for ( k = 0; k < list_size ( &mm->revs[i][j] ); k++ )
                    {
                        rev = ( Reversal * ) list_get ( &mm->revs[i][j], k );
                        mm->mark[rev->start][rev->stop] |= mm->MASK[i][j];
                    }
                }
            }


            /* now enumerate candidate reversals */
            if ( pass == 1 )
            {                   /* pass 1; here we only consider
                                   sorting reversals with respect to
                                   both g[1] and g[2] */
                for ( i = 0; i < list_size ( &mm->revs[SORTING][1] ); i++ )
                {
                    rev =
                        ( Reversal * ) list_get ( &mm->revs[SORTING][1], i );
                    if ( mm->mark[rev->start][rev->stop] & mm->
                         MASK[SORTING][2] )
                        push ( &mm->candidates, rev );
                }
            }
            else
            {                   /* pass 2; here we have to consider
                                   all anti-sorting reversals wrt g[0] */
                for ( i = 0; i < ngenes + 1; i++ )
                {
                    for ( j = i + 1; j < ngenes + 1; j++ )
                    {
                        if ( !( mm->mark[i][j] & mm->MASK[SORTING][0] ) &&
                             !( mm->mark[i][j] & mm->MASK[NEUTRAL][0] ) )
                        {
                            rev =
                                ( Reversal * ) malloc ( sizeof ( Reversal ) );
                            rev->start = i;
                            rev->stop = j;
                            push ( &mm->candidates, rev );
                        }
                    }
                }
            }

            /* now consider each candidate in turn */
            count = 0;
            while ( ( rev = pop_queue ( &mm->candidates ) ) != NULL )
            {
                count++;
                n = get_vertex ( mm->vf );

                /* generate permutation induced by reversal */
                copy_with_reversal ( n->perm, v->perm, ngenes, rev );

                /* check for mark in hash table */
                if ( ht_find ( mm->h, n->perm, 0 ) == 1 )
                /*CHANGED by Jijun, 1->0 */
                {
                    return_vertex ( mm->vf, n );
                    if ( pass == 2 )
                        free ( rev );
                    continue;   /* already visited vertex; go on to
                                   next candidate */
                }

                /* this must always be true */
                n->distance = v->distance + 1;

                /* set distance wrt d[1] according to whether a sorting,
                   neutral, or anti-sorting reversal */
                if ( mm->mark[rev->start][rev->stop] & mm->MASK[SORTING][1] )
                    n->d1 = v->d1 - 1;
                else if ( mm->mark[rev->start][rev->stop] & mm->
                          MASK[NEUTRAL][1] )
                    n->d1 = v->d1;
                else
                    n->d1 = v->d1 + 1;

                /* same wrt d[2] */
                if ( mm->mark[rev->start][rev->stop] & mm->MASK[SORTING][2] )
                    n->d2 = v->d2 - 1;
                else if ( mm->mark[rev->start][rev->stop] & mm->
                          MASK[NEUTRAL][2] )
                    n->d2 = v->d2;
                else
                    n->d2 = v->d2 + 1;

                /* calculate best and worst possible scores according to
                   theorem of Chapter 2 */
                n->best_possible_score =
                    n->distance +
                    ceil ( ( n->d1 + n->d2 + distbetween ) / 2.0 );
                n->worst_possible_score = n->distance + n->d1 + n->d2;
                /* this simpler formula is equivalent
                   to the one in the written thesis,
                   because n->d1 <= d[0][1], n->d2 <=
                   d[0][2] */


                /* if meets lower bound, then stop */
                if ( n->worst_possible_score <= MIN_MED )
                {
                    permcopy ( median->genes, n->perm, ngenes );
                    stop = 1;
                    found_one = 1;
                    break;
                }
                /* if best possible is better than current upper bound, add to
                   priority stack */
                if ( n->best_possible_score < MAX_MED )
                    ps_push ( mm->ps, n, n->best_possible_score );
                else
                    return_vertex ( mm->vf, n );
                /* if worst possible is better than any median so far, set n as
                   median and lower upper bound */
                if ( n->worst_possible_score < MAX_MED )
                {
                    permcopy ( median->genes, n->perm, ngenes );
                    MAX_MED = n->worst_possible_score;
                }
                if ( pass == 2 )
                    free ( rev );
            }

            /* unmark all marked reversals and free reversals */
            for ( i = 0; i < NREVTYPES; i++ )
            {
                for ( j = 0; j < NPERMS; j++ )
                {
                    while ( ( rev = pop_queue ( &mm->revs[i][j] ) ) != NULL )
                    {
                        mm->mark[rev->start][rev->stop] = 0;
                        free ( rev );
                    }
                }
            }
            if ( pass == 2 )
                while ( ( rev = pop_queue ( &mm->candidates ) ) != NULL )
                    free ( rev );
            return_vertex ( mm->vf, v );
        }

        if ( pass == 1 )
        {
            if ( found_one == 1 )   /* found perfect median */
                break;
            else if ( MAX_MED == ( d[0][1] + d[1][2] + d[0][2] ) / 2.0 + 1 )
                /* in this case, we know we can do no
                   better, because we know there is no
                   perfect median */
                break;
        }
    }

    if ( medmem == NULL )
        free_median_memory ( mm, ngenes );
}


/* Return a new MedianMemory struct containing various data structures
   used by "find_reversal_median".  This mechanism simply helps to
   avoid intensive allocation and deallocation of memory */
MedianMemory *
new_median_memory ( int ngenes, int minm, int maxm )
{

    int i, j;
    MedianMemory *mm;

    mm = ( MedianMemory * ) malloc ( sizeof ( MedianMemory ) );

    mm->mark = ( int ** ) malloc ( ( ngenes + 1 ) * sizeof ( int * ) );
    for ( i = 0; i < ngenes + 1; i++ )
        mm->mark[i] = ( int * ) malloc ( ( ngenes + 1 ) * sizeof ( int ) );

    if ( newvf != NULL )
    {
        mm->vf = newvf;
        clean_vf ( newvf, ngenes, NULL, NULL );
    }
    else
        mm->vf = new_vf ( VFSIZE, ngenes, NULL, NULL );
    /*new_vf(VFSIZE, ngenes, NULL, NULL); */

    for ( i = 0; i < NREVTYPES; i++ )
        for ( j = 0; j < NPERMS; j++ )
            mm->MASK[i][j] = pow ( 2, i * NPERMS + j );

    /* set up priority stack, reversal sorting memory */
    mm->ps = new_ps ( minm, maxm, QUEUECAPACITY, sizeof ( Vertex * ) );
    mm->rsm = new_reversal_sorting_memory ( ngenes );

    /* initialize lists used to keep track of neutral, sorting,
       anti-sorting, and candidate reversals */
    for ( i = 0; i < NREVTYPES; i++ )
        for ( j = 0; j < NPERMS; j++ )
            init_list ( &mm->revs[i][j], ( ngenes + 1 ) * ngenes,
                        sizeof ( Reversal * ) );
    init_list ( &mm->candidates, ( ngenes + 1 ) * ngenes,
                sizeof ( Reversal * ) );

    for ( i = 0; i < ngenes + 1; i++ )
        for ( j = 0; j < ngenes + 1; j++ )
            mm->mark[i][j] = 0;

    mm->distmem = new_distmem ( ngenes );

    mm->h = new_hashtable ( ngenes, HASH_EXPECTED_SIZE, HASH_LOADING_FACTOR );

    return ( mm );
}

/* Reset an existing MedianMemory structure, for use in finding a new
   median */
void
reset_median_memory ( MedianMemory * mm )
{
    ps_clear ( mm->ps );
    clear_list ( &mm->candidates );
}

/* Free the contents of a MedianMemory struct */
void
free_median_memory ( MedianMemory * mm, int ngenes )
{
    int i, j;
    ps_free ( mm->ps );
    /*vf_free(mm->vf); */
    for ( i = 0; i < NREVTYPES; i++ )
        for ( j = 0; j < NPERMS; j++ )
            free_list ( &mm->revs[i][j] );
    free_list ( &mm->candidates );
    free_reversal_sorting_memory ( mm->rsm );
    for ( i = 0; i < ngenes + 1; i++ )
        free ( mm->mark[i] );
    free ( mm->mark );
    ht_free ( mm->h );
    free_distmem ( mm->distmem );
    free ( mm );
}
