/* $Id: inversion_median.c 48 2005-04-13 15:56:25Z ron $
   Written by Adam Siepel, Spring 2001 
   Copyright 2001, Adam Siepel */

/* Find and return an optimal inversion median of three signed
   permutations.  This is an implementation of the "metric median"
   algorithm described in Chapter 2 of Siepel, A., "Exact Algorithms
   for the Reversal Median Problem," Master's Thesis, University of
   New Mexico, 2001.  It is by far slower than the "sorting median"
   algorithm, which is implemented in sorting_reversal_median.c.  Use
   the latter except for testing purposes.  */

/* Compile with -DTHREADSAFE to enable multithreading.  Set
   NTHREADGROUPS and NTHREADSPGROUP below for level of parallelism.
   Note that this code is greatly complicated by the use of
   multithreading.  It may be desirable to preprocess it with cpp -C
   to see a less convoluted version. */


#include "inversion_median.h"
#include <math.h>
#include "invdist.h"
#include <assert.h>
#include <errno.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "simpleio.h"
#ifndef WINNT
#include <unistd.h>
#endif
#include "med_util.h"

#ifdef THREADSAFE
#include <semaphore.h>
#include <pthread.h>
#define NTHREADGROUPS 1         /* number of groups of threads
                                   independently drawing vertices from
                                   the priority stack */
#define NTHREADSPGROUP 4        /* number of threads within each
                                   group, working in parallel to
                                   examine all neighbors of a single
                                   vertex */
#endif

#define VFSIZE 10000
#define HASH_EXPECTED_SIZE 5000 /* this is deliberately low, since the
                                   realloc operations seem fairly
                                   efficient */
#define HASH_LOADING_FACTOR 3
extern VertexFactory *newvf;
void
find_inversion_median ( struct genome_struct *median,
                        struct genome_struct **gen,
                        int ngenes, distmem_t * distmem )
{

    SearchVars *sv;
    ThreadData *td;
    Vertex *v;
    int d01, d02, d12;

#ifdef THREADSAFE
    int t, retval;
    pthread_t *tn =
        ( pthread_t * ) calloc ( NTHREADGROUPS, sizeof ( pthread_t ) );
#endif

    /* initialize variables global within search */
    sv = ( SearchVars * ) malloc ( sizeof ( SearchVars ) );
    sv->ngenes = ngenes;
    sv->distmem = distmem;
    if ( newvf != NULL )
    {
        sv->vf = newvf;
        clean_vf ( sv->vf, ngenes, NULL, NULL );    /*&clear_memory, sv); */
    }
    else
        sv->vf = new_vf ( VFSIZE, sv->ngenes, &clear_memory, sv );
    sv->bestmed = median->genes;    /*(int*)calloc(sv->ngenes, sizeof(int)); */

    /* set bound for median dist */
    /* find cost of vertex at intersection of shortest two sides */
    d01 = invdist_noncircular ( gen[0], gen[1], 0, sv->ngenes, sv->distmem );
    d12 = invdist_noncircular ( gen[1], gen[2], 0, sv->ngenes, sv->distmem );
    d02 = invdist_noncircular ( gen[0], gen[2], 0, sv->ngenes, sv->distmem );
    sv->MIN_MED = ceil ( ( d01 + d12 + d02 ) / 2.0 );

    v = get_vertex ( sv->vf );

    /* find longest edge and set search variables accordingly */
    if ( d01 >= d12 && d01 >= d02 )
    {
        sv->MAX_MED = d12 + d02;
        permcopy ( v->perm, gen[2]->genes, sv->ngenes );
        v->worst_possible_score = d01 + d12;
        sv->origin = gen[2];
        sv->other1 = gen[0];
        sv->other2 = gen[1];
        sv->distbetween = d01;
    }
    else if ( d12 >= d01 && d12 >= d02 )
    {
        sv->MAX_MED = d01 + d02;
        permcopy ( v->perm, gen[0]->genes, sv->ngenes );
        sv->origin = gen[0];
        sv->other1 = gen[1];
        sv->other2 = gen[2];
        sv->distbetween = d12;
    }
    else
    {
        sv->MAX_MED = d01 + d12;
        permcopy ( v->perm, gen[1]->genes, sv->ngenes );
        sv->origin = gen[1];
        sv->other1 = gen[0];
        sv->other2 = gen[2];
        sv->distbetween = d02;
    }

    permcopy ( sv->bestmed, v->perm, sv->ngenes );
    v->best_possible_score = sv->MIN_MED;
    v->worst_possible_score = sv->MAX_MED;
    v->distance = 0;
/*   printf("%d\n", sv->MIN_MED); */

    /* initialize hashtable */
    sv->h =
        new_hashtable ( sv->ngenes, HASH_EXPECTED_SIZE, HASH_LOADING_FACTOR );

    ht_insert ( sv->h, v->perm );

    sv->ps =
        new_ps ( sv->MIN_MED, sv->MAX_MED, QUEUECAPACITY,
                 sizeof ( Vertex * ) );
    ps_push ( sv->ps, v, sv->MIN_MED );

    /* loop until median found */
    sv->stop = 0;
    sv->nneighbors = 0;

#ifdef THREADSAFE
    sem_init ( &sv->loop_sem, 0, 1 );
    sem_init ( &sv->stop_sem, 0, 0 );
    pthread_mutex_init ( &sv->nwaiting_mutex, NULL );
    pthread_mutex_init ( &sv->max_med_mutex, NULL );
    sv->nwaiting = 0;
    td = ( ThreadData * ) calloc ( NTHREADGROUPS, sizeof ( ThreadData ) );
    for ( t = 0; t < NTHREADGROUPS; t++ )
    {
        /* set up struct to pass to thread */
        td[t].sv = sv;
        td[t].thread_group = t;
        td[t].thread = 0;       /* here we are only creating the
                                   "pilot" thread for each group */

        /* spawn thread */
        if ( ( retval =
               pthread_create ( &tn[t], NULL, &do_loop, &td[t] ) ) != 0 )
        {
            fprintf ( stderr, "Error spawning thread: %d", retval );
            exit ( 1 );
        }
    }
    sem_wait ( &sv->stop_sem ); /* wait until search is complete */
    for ( t = 0; t < NTHREADGROUPS; t++ )
    {
        retval = pthread_join ( tn[t], NULL );  /* make sure all threads finished;
                                                   also reclaim resources */
        if ( retval != 0 )
        {
            fprintf ( stderr, "Error joining thread: %d", retval );
            exit ( 1 );
        }
    }

#else
    td = ( ThreadData * ) malloc ( sizeof ( ThreadData ) );
    td->sv = sv;
    do_loop ( td );
#endif

    ps_free ( sv->ps );
    /*vf_free(sv->vf); */
    ht_free ( sv->h );
    free ( td );
    free ( sv );
/*   printf("%d\n", sv->nneighbors);   */
/* median->genes = sv->bestmed;*/
}

/* This routine controls the major loop of the search, during each
   iteration of which a new vertex is drawn from the priority stack.
   If multithreading is enabled, this routine is executed in parallel
   by a "pilot thread" from each of the thread groups */
void *
do_loop ( void *sv_void )
{
    ThreadData *td;
    SearchVars *sv;
    Vertex *v;

#ifdef THREADSAFE
    pthread_t *threads;
    ThreadData *tdnew;
    int t, retval;
#endif

    td = ( ThreadData * ) sv_void;
    sv = td->sv;
    td->v = &v;

#ifdef THREADSAFE
    if ( td->thread_group == 0 )
        td->distmem = sv->distmem;
    else
        td->distmem = new_distmem ( sv->ngenes );

    /* spawn NTHREADSPGROUP - 1 worker threads */
    td->workers_sem = ( sem_t * ) calloc ( NTHREADSPGROUP, sizeof ( sem_t ) );
    td->pilot_sem = ( sem_t * ) calloc ( NTHREADSPGROUP, sizeof ( sem_t ) );
    threads = ( pthread_t * ) calloc ( NTHREADSPGROUP, sizeof ( pthread_t ) );
    tdnew = ( ThreadData * ) calloc ( NTHREADSPGROUP, sizeof ( ThreadData ) );
    /* wasted first struct is worth its
       cost in saved indexing complexity */
    for ( t = 1; t < NTHREADSPGROUP; t++ )
    {
        sem_init ( &td->workers_sem[t], 0, 0 );
        sem_init ( &td->pilot_sem[t], 0, 0 );

        /* set up struct to pass to thread */
        tdnew[t].sv = td->sv;
        tdnew[t].thread_group = td->thread_group;
        tdnew[t].thread = t;
        tdnew[t].v = td->v;
        tdnew[t].workers_sem = td->workers_sem;
        tdnew[t].pilot_sem = td->pilot_sem;
        tdnew[t].distmem = new_distmem ( sv->ngenes );

        /* spawn thread */
        if ( ( retval = pthread_create ( &threads[t], NULL,
                                         &worker_thread_loop,
                                         &tdnew[t] ) ) != 0 )
        {
            fprintf ( stderr, "Error spawning worker thread: %d", retval );
            exit ( 1 );
        }
    }

#else
    td->distmem = sv->distmem;
#endif

    while ( !sv->stop )
    {

#ifdef THREADSAFE
        pthread_mutex_lock ( &sv->nwaiting_mutex );
        sv->nwaiting++;
        /* if all thread groups are waiting, we have permanently exhausted
           the stack, and the search can be terminated */
        if ( sv->nwaiting == NTHREADGROUPS && NTHREADGROUPS > 1 && !sv->stop )
        {
            set_stop ( sv );
        }
        pthread_mutex_unlock ( &sv->nwaiting_mutex );
        if ( sv->stop == 1 )
            break;              /* in case set immediately above */

        sem_wait ( &sv->loop_sem ); /* will wait until EITHER the stack is
                                       nonempty OR the search has been
                                       terminated */

        if ( sv->stop == 1 )
            break;

        /* there's a problem here: it's possible (though quite unlikely)
           for one thread to be waiting to decrement the count, while
           another increments it to NTHREADGROUPS */
        pthread_mutex_lock ( &sv->nwaiting_mutex );
        sv->nwaiting--;
        pthread_mutex_unlock ( &sv->nwaiting_mutex );
#endif

        v = ( Vertex * ) ps_pop ( sv->ps );

        if ( v == NULL )
        {
#ifdef THREADSAFE
            if ( NTHREADGROUPS > 1 )
                continue;       /* This case is unlikely but possible;
                                   if it occurs we need to go to the
                                   top of the loop and wait, in case
                                   other threads are still pushing
                                   data onto the pstack */

            else
#endif
                break;
        }

        if ( v->best_possible_score >= sv->MAX_MED )
        {
#ifdef THREADSAFE
            if ( NTHREADGROUPS > 1 )
                continue;       /* throw it out and continue to loop
                                   (another thread group could turn up
                                   something better) */
            else
#endif
                set_stop ( sv );    /* can terminate; the scores can get
                                       no better */
        }
        else
        {
#ifdef THREADSAFE
            /* notify workers that they can begin searching */
            for ( t = 1; t < NTHREADSPGROUP && !sv->stop; t++ )
                sem_post ( &td->workers_sem[t] );
#endif

            search_neighbors ( td );

#ifdef THREADSAFE
            /* wait for all workers to finish before proceeding */
            for ( t = 1; t < NTHREADSPGROUP && !sv->stop; t++ )
            {
                sem_wait ( &td->pilot_sem[t] );
            }
#endif
        }

        return_vertex ( sv->vf, v );
    }

#ifdef THREADSAFE
    /* free any waiting worker threads */
    for ( t = 1; t < NTHREADSPGROUP; t++ )
        sem_post ( &td->workers_sem[t] );

    /* collect worker threads */
    for ( t = 1; t < NTHREADSPGROUP; t++ )
    {
        if ( ( retval = pthread_join ( threads[t], NULL ) ) != 0 )
        {
            fprintf ( stderr, "Error joining worker thread: %d", retval );
            exit ( 1 );
        }
    }

    /* free thread-related memory */
    for ( t = 1; t < NTHREADSPGROUP; t++ )
        free_distmem ( tdnew[t].distmem );
    free ( tdnew );
    free ( threads );
    if ( td->thread_group > 0 )
        free_distmem ( td->distmem );
    free ( td->workers_sem );
    free ( td->pilot_sem );
#endif

    return NULL;
}

/* Set the stop flag and release any waiting thread groups */
void
set_stop ( SearchVars * sv )
{
#ifdef THREADSAFE
    int i;
#endif

    sv->stop = 1;

#ifdef THREADSAFE
    /* make sure all waiting threads are released */
    for ( i = 0; i < NTHREADGROUPS; i++ )
    {
        sem_post ( &sv->loop_sem );
    }

    sem_post ( &sv->stop_sem );
#endif
}

#ifdef THREADSAFE
/* This routine is a wrapper for the minor loop of the search (see
   search_neighbors, below) used when there are multiple threads per
   thread group.  All threads in a group other than the "pilot
   thread", which we call "worker threads", execute this routine in
   parallel.  Essentially, it causes them to wait until the pilot is
   ready with a new vertex, then to execute their portion of the
   search of its neighbors, then to signal that they have finished.
   The semaphore operations bracketing the call to search_neighbors
   function as "barriers" allowing the pilot and all workers to
   synchronize. */
void *
worker_thread_loop ( void *vp )
{
    ThreadData *td;

    td = ( ThreadData * ) vp;
    while ( !td->sv->stop )
    {

        /* wait for new vertex to be ready */
        sem_wait ( &td->workers_sem[td->thread] );

        /* process neighbors */
        if ( td->sv->stop == 0 )
        {                       /* need to double check, because we
                                   might have waited a while */
            search_neighbors ( td );
        }
        sem_post ( &td->pilot_sem[td->thread] );
    }

    return NULL;
}
#endif

/* This is the minor loop of the search, in which all neighbors of a
   given vertex are examined and pushed on the stack as feasible
   vertices, rejected as infeasible, or declared valid medians.  Most
   of the work of the algorithm occurs here.  When multithreading is
   active, all threads in a thread group execute this routine
   concurrently on different subsets of the set of all possible
   neighbors of a given vertex (see worker_thread_loop, above).  */
void
search_neighbors ( ThreadData * td )
{
    int i, j, d1, d2;
    struct genome_struct g;
    SearchVars *sv;
    Vertex *n;
    int *id;

    sv = td->sv;
    id = ( int * ) calloc ( sv->ngenes, sizeof ( int ) );

    for ( i = 0; i < sv->ngenes && !sv->stop; i++ )
    {
#ifdef THREADSAFE
        /* partition evenly among the worker threads in a group */
        for ( j = i + td->thread; j < sv->ngenes && !sv->stop;
              j += NTHREADSPGROUP )
        {
#else
        for ( j = i; j < sv->ngenes && !sv->stop; j++ )
        {
#endif
            n = get_neighbor ( sv, *td->v, i, j );

            /* do hash lookup on the basis of the circular identity, to save
               space and time */
/*       find_circular_identity(n->perm, id, sv->ngenes); */

            if ( ht_find ( sv->h, n->perm, 0 ) == 1 )   /* already visited */
                return_vertex ( sv->vf, n );

            else
            {                   /* previously unmarked */
                sv->nneighbors++;

                /* find distances to vertices */
                g.genes = n->perm;
                n->distance =
                    invdist_noncircular ( &g, sv->origin, 0, sv->ngenes,
                                          td->distmem );

                if ( n->distance <= ( *td->v )->distance )
                    continue;

/*                ht_insert ( sv->h, id );*/

                d1 = invdist_noncircular ( &g, sv->other1, 0, sv->ngenes,
                                           td->distmem );
                d2 = invdist_noncircular ( &g, sv->other2, 0, sv->ngenes,
                                           td->distmem );

                n->best_possible_score =
                    n->distance +
                    ceil ( ( d1 + d2 + sv->distbetween ) / 2.0 );
                n->worst_possible_score = d1 + d2 + n->distance;

                if ( n->worst_possible_score == sv->MIN_MED )
                {
#ifdef THREADSAFE
                    pthread_mutex_lock ( &sv->max_med_mutex );
#endif
                    permcopy ( sv->bestmed, n->perm, sv->ngenes );
#ifdef THREADSAFE
                    pthread_mutex_unlock ( &sv->max_med_mutex );
#endif
                    set_stop ( sv );
                    return_vertex ( sv->vf, n );
                }

                else
                {
                    if ( n->best_possible_score < sv->MAX_MED )
                    {
                        ps_push ( sv->ps, n, n->best_possible_score );
#ifdef THREADSAFE
                        sem_post ( &sv->loop_sem );
#endif
                        if ( n->worst_possible_score < sv->MAX_MED )
                        {
#ifdef THREADSAFE
                            pthread_mutex_lock ( &sv->max_med_mutex );
#endif
                            sv->MAX_MED = n->worst_possible_score;
                            permcopy ( sv->bestmed, n->perm, sv->ngenes );
#ifdef THREADSAFE
                            pthread_mutex_unlock ( &sv->max_med_mutex );
#endif
                        }
                    }
                    else
                    {           /* not feasible -- prune it away */
                        return_vertex ( sv->vf, n );
                    }
                }
            }
        }
    }
    free ( id );
}

/* Create and return a vertex representing the neighbor obtained from
   v through an inversion from "start" to "end" */
Vertex *
get_neighbor ( SearchVars * sv, Vertex * v, int start, int end )
{
    int i, mid;
    Vertex *n;

    n = get_vertex ( sv->vf );

    for ( i = 0; i < start; i++ )
        n->perm[i] = v->perm[i];

    mid = ceil ( ( end - start + 1 ) / 2.0 );
    for ( i = 0; i < mid; i++ )
    {
        n->perm[start + i] = -1 * v->perm[end - i];
        n->perm[end - i] = -1 * v->perm[start + i];
    }

    for ( i = end + 1; i < sv->ngenes; i++ )
        n->perm[i] = v->perm[i];

    return n;
}

/* Attempt to free space by flushing priority stack */
void
clear_memory ( void *vp )
{
    Vertex *v;
    List l;
    SearchVars *sv;

    sv = ( SearchVars * ) vp;
    ps_flush ( sv->ps, &l, sv->MAX_MED );
    while ( !empty ( &l ) )
    {
        v = ( Vertex * ) pop_stack ( &l );
        return_vertex ( sv->vf, v );
    }
    free_list ( &l );
}
