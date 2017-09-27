/* $Id: sorting_reversal_median.h 48 2005-04-13 15:56:25Z ron $
   Written by Adam Siepel, Fall 2001
   Copyright 2001, Adam Siepel */

/* Code to find an exact reversal median of three signed permutations.
   Algorithm is derived in Chapter 4 of Siepel, A., "Exact Algorithms
   for the Reversal Median Problem," Master's Thesis, University of
   New Mexico, 2001. */

#ifndef REVMED_H
#define REVMED_H

#include "all_sorting_reversals.h"
#include "vertex_factory.h"
#include "hashtable.h"
#define VFSIZE 10000            /* size of vertex factory */

#define HASH_EXPECTED_SIZE 5000 /* this is deliberately low, since the
                                   realloc operations seem fairly
                                   efficient */
#define HASH_LOADING_FACTOR 3

#define NPERMS 3                /* this does not imply that the code
                                   will work properly for any number
                                   of permutations! */
#define QUEUECAPACITY 500000

#define NREVTYPES 2
enum rev_type
{ SORTING, NEUTRAL };

typedef struct median_memory_struct MedianMemory;
struct median_memory_struct
{
    VertexFactory *vf;
    PriorityStack *ps;
    Hashtable *h;
    int **mark;
    int MASK[NREVTYPES][NPERMS];
    ReversalSortingMemory *rsm;
    List candidates;
    List revs[NREVTYPES][NPERMS];
    distmem_t *distmem;
};

MedianMemory *new_median_memory ( int ngenes, int minm, int maxm );

void reset_median_memory ( MedianMemory * mm );
void free_median_memory ( MedianMemory * mm, int );

void find_reversal_median ( struct genome_struct *median,
                            struct genome_struct **gen,
                            int ngenes, MedianMemory * medmem );


#endif
