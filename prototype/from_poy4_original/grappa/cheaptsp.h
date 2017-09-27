#ifndef CHEAPTSP_H
#define CHEAPTSP_H

#include "structs.h"

int coalestsp ( int ncount, int *tour, int use_median, int *g1, int *g2,
                int *g3, struct adj_struct *adj_list, intpair_t * neighbors,
                int *stack, int *outcycle, int *degree, int *otherEnd,
                edge_t * edges, int CIRCULAR );

#endif
