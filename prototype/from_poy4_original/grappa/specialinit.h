void initialize_tree_adjpars ( struct tNode *tree, struct tNode *tpool,
                               struct genome_struct *labels,
                               int *stack, int *degree, int *otherEnd,
                               intpair_t * neighbors, smalledge_t * edges,
                               int *incycle, int *outcycle,
                               int num_genes, int num_genomes,
                               int **weights, int **status,
                               int inittspsolver, int thresh, int CIRCULAR );
