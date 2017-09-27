#ifndef BINENCODE_H
#define BINENCODE_H

int mapToPolarity ( int gene, int num_genes );
int mapToLinear ( int gene, int num_genes );
void fillGenomes ( struct genome_struct *g, int num_genomes, int num_genes );
void fillGenomeRand ( int *genes, int num_genes );
void printGenomes ( struct genome_struct *g, int num_genomes, int num_genes );
void createGeneAdjMatrix ( int ***adj, int num_genes );
void destroyGeneAdjMatrix ( int **adj, int num_genes );
void setAdj ( int **adj, int num_genes, int g0, int g1 );
void fillGeneAdjMatrix ( int **adj, struct genome_struct *mygenomes,
                         int num_genomes, int num_genes );
void printGeneAdjMatrix ( int **adj, int num_genes );
int getAdjNum ( int **adj, int num_genes );
int containsPair ( struct genome_struct *g, int G0, int G1, int num_genes );
void encodeAdjGenomes ( int **adj, struct genome_struct *mygenomes,
                        int num_genomes, int num_genes );
void freeGenomes ( struct genome_struct *mygenomes, int num_genomes );
void printGenomeEncodings ( struct genome_struct *g, int num_genomes );
void encodeGenomes ( struct genome_struct *mygenomes,
                     int num_genomes, int num_genes );
int hamming_distance_from_encoding ( struct genome_struct *g1,
                                     struct genome_struct *g2 );
void findAllHammingDistancesFromEncoding ( struct genome_struct *mygenomes,
                                           int num_genomes, int num_genes );
int hamming_distance_quad ( struct genome_struct *g1,
                            struct genome_struct *g2, int num_genes );
int hamming_distance ( struct genome_struct *g1, struct genome_struct *g2,
                       int num_genes, int CIRCULAR, int *geneAdj );
void setBPmatrix ( int **distmatrix, struct genome_struct *genomes,
                   int num_genes, int num_genomes, distmem_t * distmem,
                   int CIRCULAR );
int hamming_distance_nomem ( struct genome_struct *g1,
                             struct genome_struct *g2, int num_genes,
                             int CIRCULAR );
void findAllHammingDistancesFromScratch ( struct genome_struct *mygenomes,
                                          int num_genomes, int num_genes,
                                          int CIRCULAR );
#endif
