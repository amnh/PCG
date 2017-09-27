/* $Id: all_inversion_medians.c 47 2005-04-13 15:47:19Z ron $
   Written by Adam Siepel, Spring 2001
   Copyright 2001, Adam Siepel */

/* Find and return ALL optimal inversion medians of three signed
   permutations.  This code is slightly adapted from
   inversion_median.c.  */

/* This code should be considered experimental.  For example, it has
   not been tested using multithreading. */

/* Compile with -DTHREADSAFE to enable multithreading.  Set
   NTHREADGROUPS and NTHREADSPGROUP below for level of parallelism.
   Note that this code is greatly complicated by the use of
   multithreading.  It may be desirable to preprocess it with cpp -C
   to see a less convoluted version. */

#include "inversion_median.h"
#include "all_inversion_medians.h"
#include <math.h>
#include <invdist.h>
#include <assert.h>
#include <errno.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "simpleio.h"
#include <unistd.h>
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

#define NMEDS 100
#define VFSIZE 10000
#define HASH_EXPECTED_SIZE 5000 /* this is deliberately low, since the
                                   realloc operations seem fairly
                                   efficient */
#define HASH_LOADING_FACTOR 3


void
find_all_inversion_medians ( List * medians,
                             struct genome_struct **gen,
                             int ngenes, distmem_t * distmem )
{

    SearchVars *sv;
    ThreadData *td;
    Vertex *v;
    int d01, d02, d12;
    int *newmed;
    void *item;
    List tmplist;

#ifdef THREADSAFE
    int t, retval;
    pthread_t *tn =
        ( pthread_t * ) calloc ( NTHREADGROUPS, sizeof ( pthread_t ) );
#endif

    /* initialize variables global within search */
    sv = ( SearchVars * ) malloc ( sizeof ( SearchVars ) );
    sv->ngenes = ngenes;
    sv->distmem = distmem;
    sv->vf = new_vf ( VFSIZE, sv->ngenes, &clear_memory, sv );

    /* set bound for median dist */
    /* find cost of vertex at intersection of shortest two sides */
    d01 = invdist_circular ( gen[0], gen[1], sv->ngenes, sv->distmem );
    d12 = invdist_circular ( gen[1], gen[2], sv->ngenes, sv->distmem );
    d02 = invdist_circular ( gen[0], gen[2], sv->ngenes, sv->distmem );
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

    sv->bestmeds =
        new_ps ( sv->MIN_MED, sv->MAX_MED, NMEDS, sizeof ( int * ) );
    newmed = ( int * ) calloc ( sv->ngenes, sizeof ( int ) );
    permcopy ( newmed, v->perm, sv->ngenes );
    ps_push ( sv->bestmeds, newmed, sv->MAX_MED );

    v->best_possible_score = sv->MIN_MED;
    v->worst_possible_score = sv->MAX_MED;

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
        if ( ( retval = pthread_create ( &tn[t], NULL, &do_loop_all_meds,
                                         &td[t] ) ) != 0 )
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
    do_loop_all_meds ( td );
#endif

    ps_free ( sv->ps );
    vf_free ( sv->vf );
    ht_free ( sv->h );
    free ( td );
    printf ( "%d\n", sv->nneighbors );

    /* copy highest-priority items in the priority stack, and free all
       others */
    ps_flush ( sv->bestmeds, &tmplist,
               sv->bestmeds->idx + sv->bestmeds->min );
    while ( ( item = pop_queue ( &tmplist ) ) != NULL )
        free ( item );
    copy_list ( medians, &sv->bestmeds->stacks[sv->bestmeds->idx] );
    ps_free ( sv->bestmeds );
}

/* This routine controls the major loop of the search, during each
   iteration of which a new vertex is drawn from the priority stack.
   If multithreading is enabled, this routine is executed in parallel
   by a "pilot thread" from each of the thread groups */
void *
do_loop_all_meds ( void *sv_void )
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
                                         &worker_thread_loop_all_meds,
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

        if ( v->best_possible_score > sv->MAX_MED )
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

            search_neighbors_all_meds ( td );

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
worker_thread_loop_all_meds ( void *vp )
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
            search_neighbors_all_meds ( td );
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
search_neighbors_all_meds ( ThreadData * td )
{
    int i, j, d1, d2;
    struct genome_struct g;
    SearchVars *sv;
    Vertex *n;
    int *id;
    int *newmed;

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
            find_circular_identity ( n->perm, id, sv->ngenes );

            if ( ht_find ( sv->h, id, 0 ) == 1 )    /* already visited */
                return_vertex ( sv->vf, n );

            else
            {                   /* previously unmarked */

                sv->nneighbors++;

                /* find distances to vertices */
                g.genes = n->perm;
                n->distance =
                    invdist_circular ( &g, sv->origin, sv->ngenes,
                                       td->distmem );

                if ( n->distance <= ( *td->v )->distance )
                    continue;

                ht_insert ( sv->h, id );

                d1 = invdist_circular ( &g, sv->other1, sv->ngenes,
                                        td->distmem );
                d2 = invdist_circular ( &g, sv->other2, sv->ngenes,
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
                    newmed = ( int * ) calloc ( sv->ngenes, sizeof ( int ) );
                    permcopy ( newmed, n->perm, sv->ngenes );
                    ps_push ( sv->bestmeds, newmed, n->worst_possible_score );
#ifdef THREADSAFE
                    pthread_mutex_unlock ( &sv->max_med_mutex );
#endif
                }

                if ( n->worst_possible_score < sv->MAX_MED )
                {
#ifdef THREADSAFE
                    pthread_mutex_lock ( &sv->max_med_mutex );
#endif
                    sv->MAX_MED = n->worst_possible_score;
                    newmed = ( int * ) calloc ( sv->ngenes, sizeof ( int ) );
                    permcopy ( newmed, n->perm, sv->ngenes );
                    ps_push ( sv->bestmeds, newmed, n->worst_possible_score );
#ifdef THREADSAFE
                    pthread_mutex_unlock ( &sv->max_med_mutex );
#endif
                }

                if ( n->best_possible_score <= sv->MAX_MED )
                {
                    ps_push ( sv->ps, n, n->best_possible_score );
#ifdef THREADSAFE
                    sem_post ( &sv->loop_sem );
#endif
                }
                else
                {               /* not feasible -- prune it away */
                    return_vertex ( sv->vf, n );
                }
            }
        }
    }
    free ( id );
}
