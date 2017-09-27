/* this is the file to define the function interface for the circular ordering
   lower bound. There are two types of circular ordering: the initial and
   the improved. For the improved bound, the "fast" just switch all the
   internode one by one, so it will compute at most n-2 bounds and find the
   largest one. The other will try to switch the internal nodes for
   every combination, so it will compute at most 2^(n-3) bounds.
   The improved bound is implemented by Jijun Tang, please refer to his
   thesis for detail. Jijun can be reached at tangjijun@hotmail.com
*/


#ifndef CIRC_ORDER_H
#define CIRC_ORDER_H
/* initial primitive circular ordering bound*/
int compute_co_score ( int **distmatrix, struct tNode *, int * );

/*thorough search of all possible bounds*/
int test_all_co_score ( int best_so_far_2, int colb, int **distmatrix,
                        struct tNode *root, struct tNode *treeNode, int first,
                        int *prev, int flag );

/* just switch each once*/
int test_all_co_score_fast ( int best_so_far_2, int colb, int **distmatrix,
                             struct tNode *root, struct tNode *treeNode,
                             int first, int *prev );

#endif
