/* $Id: all_inversion_medians.h 47 2005-04-13 15:47:19Z ron $
   Written by Adam Siepel, Spring 2001
   Copyright 2001, Adam Siepel */

/* Find and return ALL optimal inversion medians of three signed
   permutations.  This code is slightly adapted from
   inversion_median.c. */

/* This code should be considered experimental.  For example, it has
   not been tested using multithreading. */

/* Compile with -DTHREADSAFE to enable multithreading.  Set
   NTHREADGROUPS and NTHREADSPGROUP below for level of parallelism.
   Note that this code is greatly complicated by the use of
   multithreading.  It may be desirable to preprocess it with cpp -C
   to see a less convoluted version. */

#ifndef ALLINVMEDS_H
#define ALLINVMEDS_H

#include "inversion_median.h"

void find_all_inversion_medians ( List * medians,
                                  struct genome_struct **gen,
                                  int ngenes, distmem_t * distmem );

void *do_loop_all_meds ( void *sv_void );

void *worker_thread_loop_all_meds ( void *vp );

void search_neighbors_all_meds ( ThreadData * td );

#endif
