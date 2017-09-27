/* $Id: med_util.h 48 2005-04-13 15:56:25Z ron $
   Written by Adam Siepel, Spring 2001 
   Copyright 2001, Adam Siepel */

/* Miscellaneous utility functions used by reversal median code */

#ifndef MEDUTIL_H
#define MEDUTIL_H

#include "structs.h"
#include "all_sorting_reversals.h"

/* Returns the median score of a potenial median with respect to three
   specified "vertex" genomes.  "gens" is assumed to be an array of at
   least three pointers to genome_structs, and all genomes are assumed
   to be of size "ngenes".  Currently, a circular measure is always
   used. */
int median_score ( struct genome_struct **gens,
                   struct genome_struct *starting_median,
                   int ngenes, distmem_t * distmem );

/* Allocates space for and returns a new permutation equal to the
   specified one. */
int *newperm ( int *perm, int ngenes );

/* Copies one permutation to another (space must already be allocated
   for the new one).  Use like strcpy. */
void permcopy ( int *dest, int *source, int sz );

/* Compares two permutations (use like strcmp) */
int permcmp ( int *perm1, int *perm2, int n );

/* Creates and returns a distmem_t structure for use in calculating
   pairwise inversion distances */
distmem_t *new_distmem ( int NUM_GENES );

/* Frees all memory associated with the specified distmem_t structure
   (including that allocated for the structure itself) */
void free_distmem ( distmem_t * distmem );

/* "Condenses" three genomes by reducing to single genes all sequences
   of three or more genes present in all three genomes (considering
   reverse complements to be equal).  Return value indicates whether
   condensing occurred (1->condensing, 0->no condensing).  */
int condense ( struct genome_struct **old_gens, int old_ngenes,
               struct genome_struct **new_gens, int new_ngenes );

/* Finds the "circular identity" of the specified permutation,
   which we define as the equivalent permutation that begins with the
   gene "+1".*/
void find_circular_identity ( int *perm, int *id, int ngenes );

/* Copies a permutation, simultaneously representing the effect of the
   specified reversal */
void copy_with_reversal ( int *dest, int *src, int n, Reversal * rev );

#endif
