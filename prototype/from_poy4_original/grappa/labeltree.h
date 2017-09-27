#ifndef LABELTREE_H
#define LABELTREE_H

#include "bbtsp.h"

int iterate_over_tree ( int COND, struct tNode *tree, int num_genes,
                        int num_genomes, int tspsolver, int thresh, int score,
                        struct adj_struct *adj_list,
                        struct adj_struct *adj_pool, intpair_t * neighbors,
                        int *stack, int *incycle, int *outcycle,
                        int **weights, int *degree, int *otherEnd,
                        edge_t * edges, struct tNode *tpool,
                        struct genome_struct *labels, int *pred, int *succ,
                        int *picked, int *decode, int *genes, int CIRCULAR,
                        int distmethod, distmem_t * distmem, int correction );

void dfs_label_tree ( int COND, struct tNode *root, struct tNode *tree_node,
                      int *best_so_far, int *IMPROVED,
                      int num_genes, int num_genomes,
                      int **weights, int *relabel,
                      int tspsolver, int thresh,
                      struct adj_struct *adj_list,
                      struct adj_struct *adj_pool, intpair_t * neighbors,
                      int *stack, int *incycle, int *outcycle, int *degree,
                      int *otherEnd, edge_t * edges, struct tNode *tpool,
                      struct genome_struct *labels, int *pred, int *succ,
                      int *code, int *decode, struct tNode *condmedian,
                      struct tNode *condnode1, struct tNode *condnode2,
                      struct tNode *condnode3,
                      /* pred, succ, and code have middle ptrs! */
                      int *genes, int CIRCULAR,
                      int distmethod, distmem_t * distmem, int correction );
void findMedian ( struct tNode *node[3], struct tNode *cNode,
                  int *best_so_far, int num_genes,
                  int tspsolver, int thresh,
                  struct adj_struct *adj_list, struct adj_struct *adj_pool,
                  intpair_t * neighbors, int *stack, int *outcycle,
                  int *degree, int *otherEnd, edge_t * edges,
                  int *genes, int CIRCULAR,
                  int distmethod, distmem_t * distmem, int correction );

int score_tree ( struct tNode *treeNode, int num_genes, int CIRCULAR,
                 int distmethod, distmem_t * distmem, int correction );
int bp_score ( struct tNode *node1, struct tNode *node2, int num_genes,
               int CIRCULAR, int distmethod, distmem_t * distmem,
               int correction, int oldngenes );
void add_genomes_to_tree ( struct tNode *treeNode,
                           struct genome_struct *labels,
                           struct genome_struct *genome_list, int num_genes );
void restore_genome (  );
void label_node ( struct tNode *tree_node, struct tNode *new_node,
                  int num_genes );
void leaf_tree ( struct tNode *tree );
void init_change_bits ( struct tNode *tree );

#endif
