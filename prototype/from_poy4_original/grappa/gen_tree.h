#ifndef GEN_TREE_H
#define GEN_TREE_H

/*gen_tree.h
*author: Mi Yan, June 2000
*every tree is represented by an interget array, tree[0] is unique
*tree_id, tree[1] is always 0, the root.
*/
/* the last three functions are added by Jijun Tang to implement a simple
   branch and bound method based on the loose lower bound we currently have
*/
#include <setjmp.h>
#include "structs.h"

#ifdef GMP
#include "gmp.h"
#endif

#define MAX_FILENAME_LEN	 25

typedef struct
{
    int tree_id;                /*should be 0...(tree_max-1) */
    int tree_max;               /* for n, should be 2n-5 */
    int size_of_tree;
    int *tree_exp;              /*the size is 2*n-1 */
} bin_env_t;

bin_env_t *alloc4_bin_tree_env ( int n );
int free_bin_tree_env ( int n, bin_env_t * bin_env );
int init_gen_bin ( int n, bin_env_t * bin_env );
int gen_new_tree ( int *old_tree, int edge, int *new_tree, int n,
                   int size_of_tree );
int gen_tree ( int n, int exp[2 * MAX_GENOMES + 1], int dif_lev,
               bin_env_t * env );
struct tNode *exp_to_tree ( int *exp, int n, struct tNode *tpool );
int tree_to_exp ( struct tNode *tree, char *exp );
struct tNode *first ( int n, struct tNode *tpool, int MYPROC,
                      bin_env_t * env );
#ifdef GMP
struct tNode *next ( int n, struct tNode *tpool, mpz_t PROCS,
                     bin_env_t * env );
int inc_bin_tree ( int n, bin_env_t * env, mpz_t MYPROC, int *dif_lev );
void inc_bin_tree_mpz ( mpz_t rop,
                        int n, bin_env_t * bin_env, mpz_t inc, int *dif_lev );
#else
struct tNode *next ( int n, struct tNode *tpool, long long PROCS,
                     bin_env_t * env );
int inc_bin_tree ( int n, bin_env_t * env, long long MYPROC, int *dif_lev );
#endif
struct tNode *gen_tree_id ( int gnome_num, int tree_id );

#ifdef GMP
struct tNode *testonelevel ( int n, struct tNode *tpool, mpz_t PROCS,
                             bin_env_t * env, int *flag, int **distmatrix,
                             int best_so_far, int minidist );
struct tNode *testtwolevel ( int n, struct tNode *tpool, mpz_t PROCS,
                             bin_env_t * env, int *flag, int **distmatrix,
                             int best_so_far, int minidist );
#else
/* branch up one level*/
struct tNode *testonelevel ( int n, struct tNode *tpool, int PROCS,
                             bin_env_t * env, int *flag, int **distmatrix,
                             int best_so_far, int minidist );
/* branch up two level*/
struct tNode *testtwolevel ( int n, struct tNode *tpool, int PROCS,
                             bin_env_t * env, int *flag, int **distmatrix,
                             int best_so_far, int minidist );
#endif

/* a helper function for the above two*/
int gen_tree_levels ( int n, int new_tree[2 * MAX_GENOMES + 1],
                      struct tNode *tpool, int dif_lev, bin_env_t * env,
                      int NUM_GENOMES, int **distmatrix, int best_so_far,
                      int minidist );

int assert_malloc ( void *buf, char *file, int line );

#endif
