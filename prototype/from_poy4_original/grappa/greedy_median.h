#ifndef GREEDY_MEDIAN_H
#define GREEDY_MEDIAN_H

#include "structs.h"

void greedy_median ( int *gene, struct genome_struct *g1,
                     struct genome_struct *g2, int num_genes, int *degree,
                     int *succ1, int *succ2f, int *succ2b, int *pred1,
                     int *pred2, int *tour, int *otherEnd );

#endif
