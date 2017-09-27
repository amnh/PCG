#ifndef STRUCTS_H
#define STRUCTS_H

#include <stdio.h>
#include <stdlib.h>
#include "uf.h"

#ifdef MALLOC_DEBUG
#include "rmalloc.h"
#endif

#ifdef GCC
#define INLINE inline
#else
#define INLINE
#endif

#define INVDISTEXE "invdist"
#define DISTMATEXE "distmat"

#define TSPOUT		0
#define MAX_NUM_GENES   2000
#define MAX_GENOMES     20 
#define TRUE            1
#define FALSE           0
#define BREAK MAX_NUM_GENES+1   /* out of reach and not 0 */
#define NOEDGE		-1          /* for edge indices, which start at 0 */
#define UNUSED		0           /* for vertex/gene indices, which cannot be 0 */

#define MAX_STR_LEN     2048
#define MAX_NAME 64
#define MAX_RAND     2147483647

/* choices of initialization */
#define INITCHOICES 8           /* number of choices - keep updating */
#define RAND 1
#define SMALLNN   2
#define SBNN   3
#define BIGNN   4
#define FASTPROP 5
#define MEDIANPROP 6
#define TRIV 7
#define ADJPARS 8

#define FAST 0
#define MEDIAN 1

/* choices of solver */
#ifdef CONCORDE
#define SOLVERCHOICES 6         /* number of choices - keep updating */
#else
#define SOLVERCHOICES 4         /* number of choices - keep updating */
#endif
#define TSP_COALESCED   1
#define TSP_BBTSP       2
#define INVERSION_MEDIAN 3
#define INVERSION_MEDIAN_FAST 4
#ifdef CONCORDE
#define TSP_GREEDYLK    5
#define TSP_CHLINKERN   6
#endif

#define LARGENUM 3000           /* 6*MAX_GENES */

/* for bbtsp.c only */
#define STAT_EXCLUDED -1
#define STAT_AVAIL     0
#define STAT_INCLUDED  1

#define DIST_BP  1
#define DIST_INV 2

extern FILE *outfile;

#ifdef MPBPA
extern int MYPROC, PROCS;
#endif

#ifdef TESTING
extern double time_linear, time_BH;
#endif


struct tNode
{
    /* the main data at each node */
    struct tNode *parent;
    struct tNode *lChild;
    struct tNode *rChild;
    struct genome_struct *genome;
    /* next three fields used to store cost/dist. of each edge */
    int *sc_parent;
    int *sc_lChild;
    int *sc_rChild;
    /* auxiliary fields */
    int leaf;
    int tag;                    /*if leaf is TRUE, gen_id is 1..n, 
                                   else gen_id is (-1)..(-n+1), added by Mi Yan */
    struct tNode *pred;         /* for retracing paths */
    int need_change;            /* to prevent unnecessary TSP calls */
};

struct qNode
{
    struct tNode *item;
    struct qNode *next;
};

struct genome_struct
{
    int *genes;
    int genome_num;
    char *encoding;
    char *gnamePtr;             /* Used to copy the gname when adding genomes to the tree */
    char parent[32];
};

struct adj_struct
{
    int vertex;
    int status;
    int weight;
    struct adj_struct *next;
};

typedef struct
{
    int A;
    int B;
    int C;
} triple_t;

typedef struct
{
    int A;
    int B;
} intpair_t;

typedef struct
{
    int I;
    int J;
    struct adj_struct *edge1;
    struct adj_struct *edge2;
} edge_t;

typedef struct
{
    int I;
    int J;
} smalledge_t;

/* Structs for invdist.c */
typedef struct
{
    int index;                  /* index of component's root */
    int oriented;               /* Boolean: Is component oriented */
    int blocks;                 /* Number of blocks in nonoriented component */
    int hurdle;                 /* Bitmask of HURDLE properties.
                                   Bit 0 = hurdle
                                   Bit 1 = wrap-around hurdle
                                   Bit 2 = superhurdle */
    int left;                   /* Index of component to the left of my rightmost block */
    int right;                  /* Index of component to the right of my rightmost block */
} component_t;

#define HURDLE          1
#define GREATHURDLE (1<<1)
#define SUPERHURDLE (1<<2)

typedef struct
{
    int *hammingArr;            /* DIST_BP:  (NUMGENES+1)*2 */

    int *perm1;                 /* DIST_INV: 2*num_genes + 2 */
    int *perm2;
    int *perm;
    int *done;
    int *greyEdges;

    int *stack;                 /* DIST_INV: size */
    int *oriented;
    int *cc;
    int *labeled;
    component_t *components;
    UFelem *uf;

} distmem_t;

#endif
