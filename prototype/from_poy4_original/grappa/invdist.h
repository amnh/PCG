#ifndef INVDIST_H
#define INVDIST_H

#ifndef CYGWINNT
#include <sys/time.h>
#else
#include <time.h>
#endif
#include "structs.h"

extern double time_linear, time_BH;

void calc_invmatrix ( struct genome_struct *genomes, int num_genes,
                      int num_genomes, distmem_t * distmem, int CIRCULAR );
void calc_invmatrix_BH ( struct genome_struct *genomes, int num_genes,
                         int num_genomes, distmem_t * distmem, int CIRCULAR );

void setinvmatrix ( int **distmatrix, struct genome_struct *genomes,
                    int num_genes, int num_genomes, distmem_t * distmem,
                    int CIRCULAR );

int invdist_noncircular ( struct genome_struct *g1, struct genome_struct *g2,
                          int offset, int num_genes, distmem_t * distmem );
int invdist_circular ( struct genome_struct *g1, struct genome_struct *g2,
                       int num_genes, distmem_t * distmem );

int invdist_noncircular_BH ( struct genome_struct *g1,
                             struct genome_struct *g2, int offset,
                             int num_genes, distmem_t * distmem );
int invdist_circular_BH ( struct genome_struct *g1, struct genome_struct *g2,
                          int num_genes, distmem_t * distmem );

int invdist_noncircular_nomem ( struct genome_struct *g1,
                                struct genome_struct *g2,
                                int offset, int num_genes );
int invdist_circular_nomem ( struct genome_struct *g1,
                             struct genome_struct *g2, int num_genes );

int calculate_offset ( struct genome_struct *g1, struct genome_struct *g2,
                       int num_genes );

void connected_component ( int size, distmem_t * distmem,
                           int *num_components );
void connected_component_BH ( int size, distmem_t * distmem,
                              int *num_components );
#endif
