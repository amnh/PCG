/* $Id: inversion_median.h 48 2005-04-13 15:56:25Z ron $
   Written by Adam Siepel, Spring 2001 
   Copyright 2001, Adam Siepel */

#ifndef INVMED_H
#define INVMED_H

#include <stdio.h>
#include "structs.h"
#include "vertex_factory.h"
#include "hashtable.h"
#include "priority_stack.h"

#define QUEUECAPACITY 500000
/* This structure encapsulates variables that must be specific to a
   particular search, but shared across threads working on that search */
typedef struct search_vars SearchVars;
struct search_vars
{
    Hashtable *h;
    PriorityStack *ps;
    VertexFactory *vf;
    PriorityStack *bestmeds;
    int *bestmed;
    int stop;
    int MAX_MED;
    int MIN_MED;
    int ngenes;
    int nneighbors;
    distmem_t *distmem;
    struct genome_struct *other1;
    struct genome_struct *other2;
    struct genome_struct *origin;
    int distbetween;

#ifdef THREADSAFE
    pthread_mutex_t nwaiting_mutex;
    pthread_mutex_t max_med_mutex;
    sem_t loop_sem, stop_sem;
    int nwaiting;
#endif
};

/* This structure is simply a package of information that can be
   passed among threads; it includes all of the information that must
   be accessible to each thread */
typedef struct thread_data ThreadData;
struct thread_data
{
    SearchVars *sv;             /* pointer to shared vars */
    int thread_group;           /* the number of this thread's group */
    int thread;                 /* the number of this thread (within
                                   its group) */
    Vertex **v;                 /* vertex whose neighbors this thread
                                   is currently examining */
    distmem_t *distmem;         /* memory structure to use in distance
                                   calculations */
#ifdef THREADSAFE
    sem_t *workers_sem, *pilot_sem; /* semaphores for coordinating threads
                                       in the same group */
#endif
};

/* Assumes gen is an array of 3 pointers to genome_structs */
void find_inversion_median ( struct genome_struct *median,
                             struct genome_struct **gen,
                             int ngenes, distmem_t * distmem );

/* Generates neighbor of specified vertex created by an inversion from
   start to end */
Vertex *get_neighbor ( SearchVars * sv, struct vertex *v,
                       int start, int end );

/* Main loop executed by each thread */
void *do_loop ( void *t );

/* Sets "stop" flag and signals all waiting threads */
void set_stop ( SearchVars * sv );

void search_neighbors ( ThreadData * td );
void *worker_thread_loop ( void *vp );

void clear_memory ( void *vp );

#endif
