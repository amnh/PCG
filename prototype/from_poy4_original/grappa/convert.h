#ifndef CONVERT_H
#define CONVERT_H

void printWeights ( int **adj_mat, int num_genes );

void convert2_to_tsp ( struct genome_struct *g1,
                       struct genome_struct *g2,
                       struct genome_struct *g3,
                       struct adj_struct *adj_list,
                       struct adj_struct *adj_pool,
                       int NUM_GENES, int CIRCULAR );

void convert_to_tsp ( struct genome_struct *g1,
                      struct genome_struct *g2,
                      struct genome_struct *g3,
                      int NUM_GENES, int CIRCULAR, int **weights );
#if 0
void genome_to_adj ( int **adj_matrix, struct genome_struct *genome,
                     int NUM_GENES, int NUM_GENOMES );
#endif

void init_weights ( int **weights, int NUM_GENES );

#endif
