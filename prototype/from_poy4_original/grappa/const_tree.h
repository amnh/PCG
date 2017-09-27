#ifndef CONST_TREE_H
#define CONST_TREE_H

/* const_tree.h
* Author: Mi Yan
*/
#include <stdio.h>
#include <string.h>
#include "gen_tree.h"

#ifdef GMP
#include "gmp.h"
#endif

#define MAX_TREE_NODES  (2*MAX_GENOMES-1)


typedef struct ConstraintTree
{
    int tag;                    /*if tag >0, then the node is leaf */
    int parent;                 /*parent of root is -1 */
    int NumOfChild;
    int child1;
    int brother;
    int bin_tree_ptr;
} ConstraintTree_T[MAX_TREE_NODES];


typedef struct
{
    int root;
    int num_leaves;
    int tree_nodes;
    bin_env_t *dim[MAX_TREE_NODES];
} env_t;

struct tNode *first_const ( ConstraintTree_T const_tree,
                            struct tNode *tpool, int MYPROC, env_t * env );
#ifdef GMP
struct tNode *next_const ( ConstraintTree_T const_tree,
                           struct tNode *tpool, mpz_t PROCS, env_t * env );
struct tNode *gen_const ( ConstraintTree_T const_tree,
                          struct tNode *tpool, mpz_t inc, env_t * env,
                          int first );
#else
struct tNode *next_const ( ConstraintTree_T const_tree,
                           struct tNode *tpool, int PROCS, env_t * env );
struct tNode *gen_const ( ConstraintTree_T const_tree,
                          struct tNode *tpool, int inc, env_t * env,
                          int first );
#endif

int rooted2unrooted ( struct tNode *rooted_tree, struct tNode **root_bin );

int alloc4_const_tree_env ( int n, env_t * env );
int free_const_tree_env ( int n, env_t * env );
int init_const ( env_t * env, const char *str, ConstraintTree_T tree,
                 struct tNode *tpool );

int str_to_tree ( env_t * env, const char *str, ConstraintTree_T tree,
                  int *pos, int *tree_nodes, int *internal,
                  struct tNode *tpool );
struct tNode *bin_exp_to_tree ( int *exp, ConstraintTree_T const_tree, int i,
                                int *pos, struct tNode *tpool, int *internal,
                                env_t * env );

int print_tree_tag ( struct tNode *bin_tree );

#endif
