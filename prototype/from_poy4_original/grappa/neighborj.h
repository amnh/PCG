#ifndef NEIGHBORJ_H
#define NEIGHBORJ_H

#include "structs.h"
#include "const_tree.h"

double neighborj_SN ( struct genome_struct *genomes, int num_genes,
                      int num_genomes, distmem_t * distmem, int CIRCULAR,
                      int verbose );
double neighborj_SK ( struct genome_struct *genomes, int num_genes,
                      int num_genomes, distmem_t * distmem, int CIRCULAR,
                      int verbose );

double neighborj_SK_tree ( struct genome_struct *genomes, int num_genes,
                           int num_genomes, distmem_t * distmem, int CIRCULAR,
                           char *constTree );
/* constTree holds the string of the constraint tree and is preallocated */

int neighborj_score_string ( const char *constTree,
                             struct genome_struct *genome_list, int num_genes,
                             int num_genomes,
                             distmem_t * distmem, int CIRCULAR,
                             struct tNode *tpool, int *edgepool,
                             struct genome_struct *labels,
                             int initmethod, int COND,
                             struct qNode *qpool,
                             struct adj_struct *adj_list,
                             struct adj_struct *adj_pool,
                             int *stack, int *degree, int *otherEnd,
                             intpair_t * neighbors,
                             smalledge_t * smalledges,
                             edge_t * edges,
                             int *outcycle, int *pred1, int *pred2,
                             int *picked, int *decode, int inittspsolver,
                             triple_t * triple, int *incycle, int tspsolver,
                             int distmethod, int thresh, int **weights,
                             int **status, int *best_so_far,
                             env_t * const_env, int *genes,
                             int *condense_succ, int *condense_decode,
                             int orig_num_genes, int correction,
                             char *treeString );

int neighborj_score_one ( struct genome_struct *genome_list, int num_genes,
                          int num_genomes,
                          distmem_t * distmem, int CIRCULAR,
                          struct tNode *tpool, int *edgepool,
                          struct genome_struct *labels,
                          int initmethod, int COND,
                          struct qNode *qpool,
                          struct adj_struct *adj_list,
                          struct adj_struct *adj_pool, int *stack,
                          int *degree, int *otherEnd, intpair_t * neighbors,
                          smalledge_t * smalledges, edge_t * edges,
                          int *outcycle, int *pred1, int *pred2, int *picked,
                          int *decode, int inittspsolver, triple_t * triple,
                          int *incycle, int tspsolver, int distmethod,
                          int thresh, int **weights, int **status,
                          env_t * const_env, int *genes, int *condense_succ,
                          int *condense_decode, int orig_num_genes,
                          int correction );

void neighborj_SK_trees ( struct genome_struct *genome_list,
                          int num_genes, int num_genomes,
                          distmem_t * distmem, int CIRCULAR,
                          struct tNode *tpool, int *edgepool,
                          struct genome_struct *labels,
                          int initmethod, int COND,
                          struct qNode *qpool, struct adj_struct *adj_list,
                          struct adj_struct *adj_pool,
                          int *stack, int *degree, int *otherEnd,
                          intpair_t * neighbors,
                          smalledge_t * smalledges,
                          edge_t * edges,
                          int *outcycle, int *pred1, int *pred2, int *picked,
                          int *decode, int inittspsolver,
                          triple_t * triple, int *incycle, int tspsolver,
                          int distmethod, int thresh, int **weights,
                          int **status, int num_clusters, double **orig_D,
                          int *orig_valid, char **orig_branch,
                          int maxConstSize, int *best_so_far, int *score,
                          env_t * const_env, int *genes, int *condense_succ,
                          int *condense_decode, int orig_num_genes,
                          int correction, char *treeString );


int neighborj_score ( struct genome_struct *genome_list, int num_genes,
                      int num_genomes,
                      distmem_t * distmem, int CIRCULAR,
                      struct tNode *tpool, int *edgepool,
                      struct genome_struct *labels,
                      int initmethod, int COND,
                      struct qNode *qpool,
                      struct adj_struct *adj_list,
                      struct adj_struct *adj_pool, int *stack, int *degree,
                      int *otherEnd, intpair_t * neighbors,
                      smalledge_t * smalledges, edge_t * edges, int *outcycle,
                      int *pred1, int *pred2, int *picked, int *decode,
                      int inittspsolver, triple_t * triple, int *incycle,
                      int tspsolver, int distmethod, int thresh,
                      int **weights, int **status, env_t * const_env,
                      int *genes, int *condense_succ, int *condense_decode,
                      int orig_num_genes, int correction, char *treeString );



#endif
