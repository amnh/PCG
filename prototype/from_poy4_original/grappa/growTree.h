/*
   This is the file to define the process of a hill climbing method.
   The idea is simple: take the first three genomes, which will create
   one tree, then pick the fourth from the remaining list, try to insert
   it to one of the three edges, and choose the one with the minimum
   score, then go on to the fifth, till the end. However, the considering
   is that the ordering of the input is important, so we will try
   to find an eliminated graph and reorder the input in order to this
   graph, hence there are many functions evolved here.
   This file is implemented by Jijun Tang and can be reached at
   tangjijun@hotmail.com, please refer to his thesis for detail.
*/

#ifndef GROW_TREE_H
#define GROW_TREE_H
#include "structs.h"
#include "const_tree.h"
struct graph_vertex
{
    int name;
    struct graph_vertex **neighbors;
    int num_neighbors;
};
struct graph_vertex *createVertex ( int num_neighbor, int name );
void addNeighbor ( struct graph_vertex *v1, struct graph_vertex *v2 );
void removeNeighbor ( struct graph_vertex *v1, struct graph_vertex *v2 );

struct vertex_stack
{
    struct stack_node *head;
};

int neighbor ( struct graph_vertex *, struct graph_vertex * );
struct stack_node
{
    struct graph_vertex *theVertex;
    struct stack_node *next;
};

struct vertex_stack *createStack (  );
struct graph_vertex *stack_pop ( struct vertex_stack * );
void stack_del ( struct vertex_stack *, struct graph_vertex * );
void stack_push ( struct vertex_stack *, struct graph_vertex * );
int stack_empty ( struct vertex_stack * );
int stack_member ( struct vertex_stack *, struct graph_vertex * );

struct union_set
{
    int rank;
    int parent;
};

void createUF ( int n, struct union_set *us );

void unionUF ( int i, int j, struct union_set *us );

int findUF ( int i, struct union_set *us );

int MST ( int num_genomes, int **distmatrix );

int growTree ( int COND, struct tNode **tree, int NUM_GENES, int NUM_GENOMES,
               int tspsolver, int thresh,
               struct adj_struct *adj_list, struct adj_struct *adj_pool,
               intpair_t * neighbors,
               int *stack, int *incycle, int *outcycle, int **weights,
               int *degree, int *otherEnd, edge_t * edges,
               struct tNode *tpool, struct genome_struct *labels,
               int *pred1, int *pred2, int *picked, int *decode,
               int *genes, int CIRCULAR,
               int distmethod, distmem_t distmem, int CORRECTION,
               struct qNode *qpool, int *edgepool,
               struct genome_struct *genome_list, smalledge_t * smalledges,
               int **status, triple_t * triple, int inittspsolver, int OPT,
               int initmethod, bin_env_t * bin_env );

void findNumbers ( struct tNode *tree, int *mtag, int *ntag );

struct tNode *addNewEdge ( int COND, struct tNode *tree, struct tNode *root,
                           struct tNode *internal, struct tNode *newNode,
                           int *score, int num_genes, int num_genomes,
                           int tspsolver, int thresh,
                           struct adj_struct *adj_list,
                           struct adj_struct *adj_pool, intpair_t * neighbors,
                           int *stack, int *incycle, int *outcycle,
                           int **weights, int *degree, int *otherEnd,
                           edge_t * edges, struct tNode *tpool,
                           struct genome_struct *labels, int *pred1,
                           int *pred2, int *picked, int *decode, int *genes,
                           int CIRCULAR, int distmethod, distmem_t distmem,
                           int CORRECTION, struct qNode *qpool, int *edgepool,
                           struct genome_struct *genome_list,
                           smalledge_t * smalledges, int **status,
                           triple_t * triple, int inittspsolver, int OPT,
                           int initmethod );

void orderGenome ( struct genome_struct *genome_list,
                   struct genome_struct *cpgenome, int **cpdist,
                   int **distmatrix, int num_genes, int num_genomes,
                   int threshHold, int start );
struct graph_vertex *findOne ( struct vertex_stack *aStack );

void eliminateGraph ( int **dist, int *order, int num, int );

void copyTree ( struct tNode *tree, struct genome_struct *labels,
                int *cpSC_lChild, int *cpSC_rChild, int *cpSC_parent,
                int num_genes );

void restoreTree ( struct tNode *tree, struct genome_struct *labels,
                   int *cpSC_lChild, int *cpSC_rChild, int *cpSC_parent,
                   int num_genes );

/* copy 1->2*/
void copyGenome ( struct genome_struct *g1, struct genome_struct *g2,
                  int num_genes );

void triangulate ( int **dist, int **origDist, double w, int num );

void intersectNodes ( struct graph_vertex *v, struct vertex_stack *aStack,
                      int num, int *intersected, int * );


#endif
