#ifndef LK_MAIN_H
#define LK_MAIN_H

#ifdef CONCORDE
int chlinkern ( int ncount, int **adj, int *tour, int *incycle,
                int *outcycle );
int greedylk ( int ncount, int **adj, int *tour, int *incycle,
               int *outcycle );
#endif

#endif
