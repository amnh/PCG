/* this is the inversion median solver implemented by Alberto
   and integrated into GRAPPA by Tao Liu.
   There are many global variables used and are initialized in
   the main function
*/
#ifndef INVERSION_MEDIAN_H
#define INVERSION_MEDIAN_H

#include <stdio.h>
#include <stdlib.h>
#include "structs.h"
#include "invdist.h"

#define TWO 2
#define ONE_GOOD 1
#define ONE_BAD 0
#define TLIM 3600.0
#define MILL 1000000
#define NONE -1
#define SOME 0
#define TRUE 1
#define FALSE 0
#define VIO_EPS 0.0001


/* constant definitions */
#define MAXQ 3                  /* maximum number of permutations in the instance */
#define MAX_CYCLES 100000       /* maximum number of active cycle variables */



/* global variables */

int Num_Genes;
int MAXNOD;
int MAXM;
int MAXEDG;

int **pi;


int vc;                         /* cardinality of the node set = 2 * n + 2 */
int mc;                         /* cardinality of a perfect matching = vc / 2 */
int ec;                         /* cardinaliity of the edge set = 
                                   vc * ( vc - 1 ) / 2 - vc / 2 */
int **e_ind;
int *i_ind;
int *j_ind;
int **pm;
int *pme, *ke, *pmmate, *kmate, *unexp, *inv_unexp;

int nedge;                      /* number of edge variables */
int bedge;                      /* first index of an edge variable */
int ncycvar;                    /* current number of half-cycle variables */
int ncycnz;                     /* current total number of half-cycle edges */
int bcycvar;                    /* first index af a half-cycle variable */
int ndegree;                    /* number of degree equations */
int bdegree;                    /* first index of a degree equation */
int ncyceq;                     /* number of cycle equations */
int bcyceq;                     /* first index of a cycle equation */
int nsec;                       /* current number of SEC inequalities */
int bsec;                       /* first index of a SEC inequality */

unsigned at_root;               /* root node flag */

double bestub;                  /* best upper bound on the problem */
int bestsol;                    /* best solution value for the problem */
int *pibest;
int **fid;
FILE *fout;
distmem_t *localDistmem;

/* global branch-and-bound variables */



/* data structures */

typedef struct
{
    int perm;                   /* permutation associated with the half cycle */
    int len;                    /* length of the half cycle */
    int *edge;                  /* indices of the edges in the half cycle */
    double value;               /* value in the current LP */
    unsigned active;            /* flag telling whether the ass. variable is active in LP */
    unsigned recovered;         /* flag telling whether the ass. variable was rec. in LP */
    int nitslack;               /* number of consecutive iterations with variable slack in LP */
} half_cycle;

half_cycle **cycvar;            /* current half-cycle variables */
int UB;                         /* initial upper bound on the problem */
int best_now;                   /* current best solution value */
int nunreach;                   /* number of nodes unreached so far */
/* number of cycles formed by permutation matchings */

int *unreach;
int *inv_unreach;
int **ncycles;
int **rdist;
int *hamilmate;
int *hamate;
int **permate;
int *solmate;

int treenodes;                  /* number of branching nodes eval. so far (mod. MILL) */
int millnodes;                  /* number of branching nodes eval. so far (in MILL) */
float ti, tf;                   /* initial and final time for algorithm */
int realub;                     /* best valid upper bound on the problem */
int realbest;                   /* real best solution value */
int metricub;                   /* initial (metric) upper bound on the problem */
int *piupd;

struct genome_struct **gen;
struct genome_struct *genupd;   /* genome for updating sol. */
struct genome_struct **id;


/* ********** functions   ************ */

void init_global_variables ( int, distmem_t * distmem );
void genome_init ( int i );
void median_init (  );

/* calculate the median score */
int median_distance ( int *med );

/* elaps:    total time elapsed */
/* optimal:  termination condition: TRUE = optimal sol. found */
void termin ( float elaps, int optimal );

/* mate:  permutation matching on input */
/* per:   permutation on output */
void mat_2_per ( int ngenes, int vc, int *mate, int *per );

/* pmi:  given permutation matching */
/* k:    index of permutation       */
void find_half_cycles ( int *pmi, int k, int mc, int *ncyc );

/*  lncyc:      number of cycles already formed by the partial matching and the original permutation matchings 
    ln:         local n referred to reduced problem 
    lub;        local upper bound associated with the problem 
    lcurr;      current last node visited by Hamiltonian matching         */
void msbrbb ( int lncyc, int ln, int lub, int lcurr );

/* Given 3 permutations and their size, return the median */
int *albert_inversion_median_circular ( struct genome_struct **gen,
                                        int ngenes, int *genes );
int *albert_inversion_median_noncircular ( struct genome_struct **gen,
                                           int ngenes, int *genes );

int *Find_circular_identity ( int *perm, int k );

void alloc_error ( char *s );
void stop_error ( char *s );
void free_variables (  );



#endif
