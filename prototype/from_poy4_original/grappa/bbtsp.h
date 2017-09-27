#ifndef BBTSP_H
#define BBTSP_H

/* no prototype: try to make sure it's inlined at compile time */
/* int lb(int ncount,int picked,int **weights,int *degree,
   struct adj_struct *adj_list,int *otherEnd);
*/

void rec_dfs ( int ncount, int *degree,
               struct adj_struct *adj_list, int *stack,
               int *outcycle, int *best, int score, int picked,
               edge_t * edges, int numedges, int *otherEnd, int level );

void tsp_dfs ( int ncount, int mincost, int *found_cost, int *degree,
               struct adj_struct *adj_list, intpair_t * neighbors,
               int *stack, int *outcycle, int *otherEnd, edge_t * edges );

int bbtsp ( int ncount, int *tour, int use_median,
            int *g1, int *g2, int *g3,
            struct adj_struct *adj_list, intpair_t * neighbors,
            int *stack, int *outcycle, int *degree, int *otherEnd,
            edge_t * edges, int CIRCULAR );

#endif
