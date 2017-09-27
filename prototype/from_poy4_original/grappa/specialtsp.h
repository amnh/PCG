#ifndef SPECIALTSP_H
#define SPECIALTSP_H

/* no prototype: try to make sure it's inlined at compile time */
/* int lb(int ncount,int picked,int **weights,int *degree,
   struct adj_struct *adj_list,int *otherEnd);
*/

#include "structs.h"

int ap_bbtsp ( int ncount, int *tour, int **weights, int **status,
               intpair_t * neighbors, int *stack,
               int *outcycle, int *degree, int *otherEnd,
               smalledge_t * edges );

int ap_coalestsp ( int ncount, int *tour, int **weights,
                   intpair_t * neighbors, int *stack,
                   int *outcycle, int *degree, int *otherEnd,
                   smalledge_t * edges );

int av_bbtsp ( int ncount, int *tour, int **weights, int **status,
               intpair_t * neighbors, int *stack,
               int *outcycle, int *degree, int *otherEnd,
               smalledge_t * edges );

int av_coalestsp ( int ncount, int *tour, int **weights,
                   intpair_t * neighbors, int *stack,
                   int *outcycle, int *degree, int *otherEnd,
                   smalledge_t * edges );

#endif
