/* $Id: all_sorting_reversals.h 48 2005-04-13 15:56:25Z ron $
   Written by Adam Siepel, Summer 2001 
   Copyright 2001, Adam Siepel */

/* Code to enumerate all sorting reversals of one signed permutation
   with respect to another.  Algorithm is derived in Siepel, A., "An
   Algorithm to Find All Sorting Reversals," RECOMB 2002, and in
   Siepel, A., "Exact Algorithms for the Reversal Median Problem,"
   Master's Thesis, University of New Mexico, 2001. */

#ifndef ALLSORT_H
#define ALLSORT_H

#include "structs.h"
#include "priority_stack.h"
#include "bitvector.h"

#define NEGINFINITY -9999999

typedef struct bit_matrix_struct BitMatrix;
struct bit_matrix_struct
{
    int size;
    short **v;
};

typedef struct reversal_struct Reversal;
struct reversal_struct
{
    int start, stop;
};

enum comp_type
{ ORIENTED, SIMPLEHURDLE, SUPHURDLE, PROTNHURDLE,
    TRIVIAL, PSEUDOHURDLE
};

typedef struct cc_struct ConnectedComponent;
struct cc_struct
{
    enum comp_type type;
    List double_superhurdle_partners;
    List cyclelist;
    int begidx, endidx;
    int protecting_superhurdle;
    int protected_pseudohurdle;
    int chain, anchor;
    int mult_protected;
#ifdef BITWISE_DETECT
    BitVector *v;
    BitVector p;
    List grey_edges;
    List beg;
    List end;
#endif
};

typedef struct rsm_struct ReversalSortingMemory;
struct rsm_struct
{
    int *inv, *inv2, *upi, *be, *cycle, *comp_label_by_pos,
        *comp_label_by_cycle, ngenes, *cc_beg, *cc_end, *cc_e, *cc_parent;
    List *cyclelist, *mhurdles, *V, *stack;
    ConnectedComponent *conn_comp;
#if BITWISE_DETECT
    BitVector bv_a, bv_p, bv_l, *bv_v;
    int *ge_mark;
    List bv_pos;
#else
    int *upi_cpy, *comp_label_cpy, *inv2_cpy, *new_comp_labels, *be_cpy,
        *comp_mark, *comp_count;
    List *comp_grey_edges;
#endif
};


void find_all_sorting_reversals ( List * l, List * m,
                                  struct genome_struct *pi,
                                  struct genome_struct *origin, int ngenes,
                                  ReversalSortingMemory * rsm );

void find_connected_components ( int begidx, int len, int *inv2, int *upi,
                                 int *dest_comp_label,
                                 int *source_comp_label, int restrict_to,
                                 ReversalSortingMemory * mem );

void build_uoc_list ( List * uoc_list, ConnectedComponent * conn_comp,
                      int *comp_label_by_pos, int n );

void find_superhurdles ( List * uoc_list, ConnectedComponent * conn_comp,
                         int nuoc, int ncomp, int *nhurdles,
                         int *nsuperhurdles );

int find_single_hurdle ( int u, List * adj_list, List * cycles );

void find_double_superhurdles ( List * uoc_list,
                                ConnectedComponent * conn_comp, int nuoc,
                                int *nhurdles );

void build_overlap_matrix ( ConnectedComponent * cc, int *upi, int *inv2,
                            int n, int c, List * cyclelist );

int new_nhurdles_plus_nfortresses ( int ccidx, Reversal * rev,
                                    int n, int ncomp, int nuoc,
                                    int nhurdles, int nfortresses,
                                    ReversalSortingMemory * mem );

void detect_new_unoriented_components ( Reversal * rho,
                                        ConnectedComponent * cc,
                                        ReversalSortingMemory * mem );

void count_hurdles_after_reversal ( List * V, int *comp_label_by_position,
                                    ConnectedComponent * conn_comp,
                                    Reversal * rho,
                                    int *inv, int n, int ncomp, int nuoc,
                                    int *new_nhurdles, int *new_nfortresses );

int new_nhurdles_plus_nfortresses_cc ( int ccidx, Reversal * rev,
                                       int n, int ncomp, int nuoc,
                                       int nhurdles, int nfortresses,
                                       ReversalSortingMemory * mem );

void add_all_merging_reversals ( List * l, ConnectedComponent * c1,
                                 ConnectedComponent * c2, List * cyclelist,
                                 int c );

void add_all_cutting_reversals ( List * l, ConnectedComponent * cc,
                                 List * cyclelist );

int eliminating_components_creates_fortress ( int h1, int h2,
                                              ConnectedComponent * conn_comp,
                                              int nhurdles,
                                              int nsuperhurdles );

int form_double_superhurdle ( int h1, int h2,
                              ConnectedComponent * conn_comp );

ReversalSortingMemory *new_reversal_sorting_memory ( int ngenes );
void free_reversal_sorting_memory ( ReversalSortingMemory * mem );

void copy_for_reversal ( Reversal * rev, int begidx, int len, int *inv2_cpy,
                         int *upi_cpy, int *comp_label_cpy, int *be_cpy,
                         ReversalSortingMemory * mem );

void find_separating_hurdles ( int *is_separator, int *sepclass,
                               int *bc_hurdchain, int *nseparators, int n,
                               int ncomp, ReversalSortingMemory * mem,
                               List ** ocs, int *lidx, int *llabel,
                               int *is_separated, int neutral_mode,
                               int nfortresses );

#endif
