/* this is the file to define the function interface to find the max possible
   circular lower bound and the min possible bound.
   The idea is to build max or min TSP instances from the input ordering
   of genomes and solve this instance to get the ordering yields max
   or min cost, most of the code is copied from the bbtsp.c.
   The function is implemented by Jijun Tang, Jijun can be reached at
   tangjijun@hotmail.com
*/
#ifndef CIRCULAR_ORDERING_H
#define CIRCULAR_ORDERING_H

#include "structs.h"

int circular_ordering ( int num_genomes, int num_genes,
                        int flag, intpair_t * neighbors, int **distmatrix,
                        int *stack, int *outcycle, int *degree, int *otherEnd,
                        edge_t * edges, int CIRCULAR );
/* flag is to indicate max or min, ==0 for max*/

#endif
