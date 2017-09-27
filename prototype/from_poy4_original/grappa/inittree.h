#ifndef INITTREE_H
#define INITTREE_H

void SetTreeEdgePtrs ( struct tNode *tree, int *edgepool, int *index );

void initialize_tree_random ( struct tNode *tree,
                              struct genome_struct *labels, int num_genes,
                              int num_genomes );
void initialize_tree_trivial ( struct tNode *tree,
                               struct genome_struct *labels, int num_genes,
                               int num_genomes );
void initialize_tree_SNN ( int COND, struct tNode *tree, struct tNode *tpool,
                           struct genome_struct *labels, struct qNode *queue,
                           struct adj_struct *adj_list,
                           struct adj_struct *adj_pool, int **weights,
                           int *stack, int *degree, int *otherEnd,
                           intpair_t * neighbors, edge_t * edges,
                           int *incycle, int *outcycle, int *pred, int *succ,
                           int *code, int *decode, int num_genes,
                           int num_genomes, int inittspsolver, int thresh,
                           int CIRCULAR );
void initialize_tree_SBNN ( int COND, struct tNode *tree, struct tNode *tpool,
                            struct genome_struct *labels, struct qNode *queue,
                            struct adj_struct *adj_list,
                            struct adj_struct *adj_pool, int **weights,
                            int *stack, int *degree, int *otherEnd,
                            intpair_t * neighbors, edge_t * edges,
                            int *incycle, int *outcycle, int *pred, int *succ,
                            int *code, int *decode, int num_genes,
                            int num_genomes, int inittspsolver, int thresh,
                            int CIRCULAR );
void initialize_tree_BNN ( int COND, struct tNode *tree, struct tNode *tpool,
                           struct genome_struct *labels, triple_t * triple,
                           struct adj_struct *adj_list,
                           struct adj_struct *adj_pool, int **weights,
                           int *stack, int *degree, int *otherEnd,
                           intpair_t * neighbors, edge_t * edges,
                           int *incycle, int *outcycle, int *pred, int *succ,
                           int *code, int *decode, int num_genes,
                           int num_genomes, int inittspsolver, int thresh,
                           int CIRCULAR );
void initialize_tree_propagate ( int COND, struct tNode *tree,
                                 struct tNode *tpool,
                                 struct genome_struct *labels,
                                 struct qNode *queue,
                                 struct adj_struct *adj_list,
                                 struct adj_struct *adj_pool, int **weights,
                                 int *stack, int *degree, int *otherEnd,
                                 intpair_t * neighbors, edge_t * edges,
                                 int *incycle, int *outcycle, int num_genes,
                                 int num_genomes, int *pred1, int *pred2,
                                 int *picked, int *decode, int comb_method,
                                 int inittspsolver, int thresh,
                                 int CIRCULAR );
void print_tree ( struct tNode *tree, int num_genes );

#endif
