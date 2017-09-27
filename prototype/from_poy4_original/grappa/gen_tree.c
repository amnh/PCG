/*gen_tree.c
 * author: Mi Yan, June 2000
 */
#include <string.h>
#include "gen_tree.h"
#include "circ_order.h"
#ifdef GMP
#include "gmp.h"
#endif

FILE *outfile;
int *bestOrdering;
int *ordering;
int nextID;
int **switchDist;
extern int DOBRANCH;
/*if bin_tree is available, return 0*/

#ifdef GMP
void
inc_bin_tree_mpz ( mpz_t rop,
                   int n, bin_env_t * bin_env, mpz_t inc, int *dif_lev )
{
    int i;
    bin_env_t *tmp_env;
    mpz_t tmp, tmp2;

    if ( n < 3 )
    {
        mpz_set_si ( rop, -1 );
        return;
    }
    mpz_init_set ( tmp, inc );
    for ( i = n; i >= 3; i-- )
    {
        tmp_env = bin_env + ( i - 3 );
        mpz_add_ui ( tmp, tmp, tmp_env->tree_id );

        if ( mpz_cmp_si ( tmp, tmp_env->tree_max ) < 0 )
        {
            tmp_env->tree_id = mpz_get_si ( tmp );
            *dif_lev = i;
            mpz_clear ( tmp );
            mpz_set_si ( rop, 0 );
            return;
        }
        else
        {
            mpz_init ( tmp2 );
            tmp_env->tree_id = mpz_mod_ui ( tmp2, tmp, tmp_env->tree_max );
            mpz_clear ( tmp2 );
            mpz_tdiv_q_ui ( tmp, tmp, tmp_env->tree_max );
        }
    }

    mpz_set ( rop, tmp );
    mpz_clear ( tmp );
    return;
}


int
inc_bin_tree ( int n, bin_env_t * bin_env, mpz_t inc, int *dif_lev )
{
    int i;
    bin_env_t *tmp_env;
    mpz_t tmp, tmp2;

    if ( n < 3 )
    {
        return ( -1 );
    }
    mpz_init_set ( tmp, inc );
    for ( i = n; i >= 3; i-- )
    {
        tmp_env = bin_env + ( i - 3 );
        mpz_add_ui ( tmp, tmp, tmp_env->tree_id );

        if ( mpz_cmp_si ( tmp, tmp_env->tree_max ) < 0 )
        {
            tmp_env->tree_id = mpz_get_si ( tmp );
            *dif_lev = i;
            mpz_clear ( tmp );
            return ( 0 );
        }
        else
        {
            mpz_init ( tmp2 );
            tmp_env->tree_id = mpz_mod_ui ( tmp2, tmp, tmp_env->tree_max );
            mpz_clear ( tmp2 );
            mpz_tdiv_q_ui ( tmp, tmp, tmp_env->tree_max );
        }
    }

    i = mpz_sgn ( tmp );
    mpz_clear ( tmp );
    return ( i );
}
#else
int
inc_bin_tree ( int n, bin_env_t * bin_env, long long inc, int *dif_lev )
{
    int i;
    bin_env_t *tmp_env;
    long long tmp;

    if ( n < 3 )
    {
        return ( -1 );
    }
    tmp = inc;
    for ( i = n; i >= 3; i-- )
    {
        tmp_env = bin_env + ( i - 3 );
        tmp = tmp_env->tree_id + tmp;

        if ( tmp < tmp_env->tree_max )
        {
            tmp_env->tree_id = tmp;
            *dif_lev = i;
            return ( 0 );
        }
        else
        {
            tmp_env->tree_id = tmp % tmp_env->tree_max;
            tmp = tmp / tmp_env->tree_max;
        }
    }

    return ( tmp );
}
#endif

bin_env_t *
alloc4_bin_tree_env ( int n )
{
    bin_env_t *ptr_env, *bin_env;
    int i;

    if ( n < 3 )
    {
        bin_env = NULL;
        return 0;
    }

    bin_env = ( bin_env_t * ) malloc ( ( n - 2 ) * sizeof ( bin_env_t ) );
    assert_malloc ( bin_env, __FILE__, __LINE__ - 1 );

    for ( i = 3; i <= n; i++ )
    {
        ptr_env = bin_env + i - 3;
        ptr_env->tree_exp =
            ( int * ) malloc ( ( 2 * i - 1 ) * sizeof ( int ) );
        assert_malloc ( ptr_env->tree_exp, __FILE__, __LINE__ - 1 );
    }

    return ( bin_env );
}

int
free_bin_tree_env ( int n, bin_env_t * bin_env )
{
    int i;
    bin_env_t *ptr_env;

    if ( n < 3 )
        return 0;

    for ( i = 3; i <= n; i++ )
    {
        ptr_env = bin_env + i - 3;
        free ( ptr_env->tree_exp );
    }
    free ( bin_env );

    return 0;
}

int
init_gen_bin ( int n, bin_env_t * bin_env )
{
    bin_env_t *ptr_env;
    int i;

    if ( n < 3 )
        return 0;
    for ( i = 3; i <= n; i++ )
    {
        ptr_env = bin_env + i - 3;
        ptr_env->tree_id = 0;
        ptr_env->tree_max = 2 * i - 5;
        ptr_env->size_of_tree = 2 * i - 1;
    }

    return 1;
}


struct tNode *
first ( int n, struct tNode *tpool, int MYPROC, bin_env_t * env )
{
    struct tNode *tree;
    int exp[2 * MAX_GENOMES + 1];
    int dif_lev;

#ifdef GMP
    mpz_t k;
    mpz_init_set_si ( k, MYPROC );
    if ( inc_bin_tree ( n, env, k, &dif_lev ) != 0 )
    {
        mpz_clear ( k );
        return ( NULL );
    }
    mpz_clear ( k );
#else
    if ( inc_bin_tree ( n, env, MYPROC, &dif_lev ) != 0 )
        return ( NULL );
#endif

    gen_tree ( n, exp, 0, env );
    tree = exp_to_tree ( exp, n, tpool );

    return tree;
}

#ifdef GMP
struct tNode *
next ( int n, struct tNode *tpool, mpz_t PROCS, bin_env_t * env )
#else
struct tNode *
next ( int n, struct tNode *tpool, long long PROCS, bin_env_t * env )
#endif
{
    struct tNode *tree;
    int exp[2 * MAX_GENOMES + 1];
    int dif_lev;

    if ( inc_bin_tree ( n, env, PROCS, &dif_lev ) != 0 )
        return ( NULL );

    gen_tree ( n, exp, dif_lev, env );
    tree = exp_to_tree ( exp, n, tpool );
    return tree;
}


#if 0
/*from_tag and to_tag are  betwwen 0..((2n-3)-1)!!, to_tag>=from_tag */
main ( int argc, char **argv )
{
    char *outfilename = NULL, *s;
    bin_env_t *env;
    struct tNode *tpool;
    int n = 0;
    int i;
#ifdef DEBUG
    int j, exp[2 * MAX_GENOMES + 1];
    char str[MAX_STR_LEN];
#endif
    struct tNode *tree;

    /*get arguments */
    while ( --argc > 0 && ( *++argv )[0] == '-' )
    {
        for ( s = argv[0] + 1; *s != '\0'; s++ )
        {
            switch ( *s )
            {
                case 'o':
                    if ( argc <= 1 )
                        perror
                            ( "output filename expected after -o (e.g. -o filename)" );
                    argc--;
                    outfilename =
                        ( char * ) malloc ( MAX_FILENAME_LEN *
                                            sizeof ( char ) );
                    strcpy ( outfilename, *++argv );
                    break;
                case 'n':
                    n = atoi ( *++argv );
                    if ( n < 2 )
                        perror ( "n must be larger or equal to 2\n" );
                    argc--;
                    break;
                case 'h':
                    fprintf ( stdout, "Main Options:\n" );
                    fprintf ( stdout, " -o outfilename -n num_of_nodes \n" );
                    fprintf ( stdout, "\n\n" );
                    exit ( 1 );
                    break;
                default:
                    perror ( "illegal option" );
            }
        }
    }

    if ( outfilename == NULL )
        outfile = stdout;
    else
        outfile = fopen ( outfilename, "a+" );

    tpool = ( struct tNode * ) malloc
        ( ( 2 * MAX_NUM_GENES + 1 ) * sizeof ( struct tNode ) );
    if ( tpool == ( struct tNode * ) NULL )
        fprintf ( stderr, "ERROR: tpool NULL\n" );

    init_gen_bin ( n, &env );
    tree = first ( n, tpool, 0, env );

    while ( tree )
    {
        /*print each tree */
#ifdef DEBUG
        i = tree_to_exp ( tree->lChild, str );
        str[i] = '\0';
        fprintf ( stdout, "bin_tree:%s\n", str );
        fflush ( outfile );
#endif
        tree = next ( n, tpool, 1, env );
    }
    return;
}
#endif


/*substitute the i-th edge of old_ptr with the base tree*/
int
gen_new_tree ( int *old_tree, int edge, int *new_tree,
               int n, int size_of_tree )
{
    int leaf_cut, inter_cut;
    int numOfChild;
    int *old_ptr, *new_ptr;

    inter_cut = edge + 2;

    leaf_cut = inter_cut + 1;
    if ( old_tree[leaf_cut - 1] < 0 )
        numOfChild = 2;
    else
        numOfChild = 0;

    while ( numOfChild > 0 )
    {
        if ( old_tree[leaf_cut] < 0 )
            numOfChild = numOfChild + 1;
        else
            numOfChild = numOfChild - 1;
        leaf_cut++;
    }

#ifdef DEBUG1
    if ( n == 4 )
    {
        fprintf ( outfile, "edge:%d,inter_cut:%d,leaf_cut:%d\n",
                  edge, inter_cut, leaf_cut );
        fflush ( outfile );
    }
#endif

    /*insert new internal */
    memcpy ( ( char * ) new_tree, ( char * ) old_tree,
             ( inter_cut ) * sizeof ( int ) );
    new_tree[inter_cut] = -n + 2;   /*?? */
    old_ptr = old_tree + inter_cut;
    new_ptr = new_tree + inter_cut + 1;

    /*insert new leaf */
    memcpy ( ( char * ) new_ptr, ( char * ) old_ptr,
             ( leaf_cut - inter_cut ) * sizeof ( int ) );
    old_ptr = old_ptr + leaf_cut - inter_cut;
    new_ptr = new_ptr + leaf_cut - inter_cut;
    *new_ptr = n;
    new_ptr++;

    memcpy ( ( char * ) new_ptr, ( char * ) old_ptr,
             ( size_of_tree - leaf_cut ) * sizeof ( int ) );
    return 0;
}

/*generated trees are stored in new_trees in ascending order of tree_id,
  old_trees acts as a buffer*/
int
gen_tree ( int n, int new_tree[2 * MAX_GENOMES + 1],
           int dif_lev, bin_env_t * env )
{
    int old_trees[2 * MAX_GENOMES + 1];
    int i;

    if ( n == dif_lev - 1 )
    {
        memcpy ( ( char * ) new_tree, ( char * ) ( env[n - 3].tree_exp ),
                 env[n - 3].size_of_tree * sizeof ( int ) );
        return 0;
    }

    if ( n == 3 )
    {
        /*base case */
        for ( i = 0; i < env[n - 3].size_of_tree; i++ )
            new_tree[i] = 0;
        new_tree[0] = 0;        /*it is tree_id, ranged from 0..((2n-5)!!-1) */
        new_tree[1] = 1;
        new_tree[2] = -1;
        new_tree[3] = 2;
        new_tree[4] = 3;
    }
    else
    {
        gen_tree ( n - 1, old_trees, dif_lev, env );
#ifdef DEBUG
        printf ( "after return,tree_id:%d,size:%d\n", env[n - 3].tree_id,
                 env[n - 4].size_of_tree );
        fflush ( stdout );
#endif
        gen_new_tree ( old_trees, env[n - 3].tree_id, new_tree, n,
                       env[n - 4].size_of_tree );
    }

    memcpy ( ( char * ) ( env[n - 3].tree_exp ), ( char * ) new_tree,
             env[n - 3].size_of_tree * sizeof ( int ) );

    return 0;
}

/*transform pre-order tree traversal expression to tree*/
struct tNode *
exp_to_tree ( int *exp, int n, struct tNode *tpool )
{
    struct tNode *tree, *child, *ptr;
    int i, lchild;

#ifdef DEBUG1
    for ( i = 0; i < 2 * n - 1; i++ )
        fprintf ( outfile, "%d,", exp[i] );
    fprintf ( outfile, "\n" );
#endif

    /* no need for malloc: use the next slot in the tpool array */
    tree = &tpool[0];
    tree->parent = tree->rChild = tree->lChild = NULL;
    tree->leaf = FALSE;
    tree->tag = exp[1];         /*its value always be 1 */

    /* no need for malloc: use the next slot in the tpool array */
    child = &tpool[1];
    child->parent = tree;
    tree->lChild = child;
    child->tag = exp[2];
    child->lChild = child->rChild = NULL;

    ptr = child;
    lchild = 1;

    for ( i = 3; i < 2 * n - 1; i++ )
    {
        /* no need for malloc: use the next slot in the tpool array */
        child = &tpool[i - 1];
        child->parent = ptr;
        ptr->leaf = FALSE;
        child->tag = exp[i];
        child->lChild = child->rChild = NULL;

        if ( lchild == 1 )
            ptr->lChild = child;
        else
            ptr->rChild = child;

        if ( exp[i] < 0 )
        {
            ptr = child;
            lchild = 1;
        }

        if ( exp[i] > 0 )
        {
            child->lChild = child->rChild = NULL;
            child->leaf = TRUE;
            if ( lchild == 0 )
            {
                while ( ptr->rChild != NULL )
                {
                    ptr = ptr->parent;
                }
            }
            else
                lchild = 0;
        }
    }

    return ( tree );
}

int
tree_to_exp ( struct tNode *tree, char *str )
{
    int len, tag, pos;

    if ( ( tag = tree->tag ) > 0 )
    {
        sprintf ( str, "%d", tree->tag );
        len = 1;
        while ( ( tag = tag / 10 ) > 0 )
            len++;
        return len;
    }
    else
    {
        *str = '(';
        pos = 1;
        len = tree_to_exp ( tree->lChild, str + pos );
        pos = pos + len;
        *( str + pos ) = ',';
        pos = pos + 1;
        len = tree_to_exp ( tree->rChild, str + pos );
        pos = pos + len;
        *( str + pos ) = ')';
        return ( pos + 1 );
    }

}

int
assert_malloc ( void *buf, char *file, int line )
{
    if ( buf == NULL )
    {
        printf ( "malloc faliuer at line %d in file %s\n", line, file );
        fflush ( stdout );
        exit ( -1 );
    }
    return 0;
}

#ifdef GMP
struct tNode *
testonelevel ( int n, struct tNode *tpool, mpz_t PROCS, bin_env_t * env,
               int *flag, int **distmatrix, int best_so_far, int minidist )
#else
struct tNode *
testonelevel ( int n, struct tNode *tpool, int PROCS, bin_env_t * env,
               int *flag, int **distmatrix, int best_so_far, int minidist )
#endif
{
    struct tNode *tree;
    int exp[2 * MAX_GENOMES + 1];
    int dif_lev;
    int old_trees[2 * MAX_GENOMES + 1];
    int tmp_trees[2 * MAX_GENOMES + 1];
    int new_tree[2 * MAX_GENOMES + 1];
    int i, prev, colb, firstg;
    int treeid;

    if ( inc_bin_tree ( n, env, 1, &dif_lev ) != 0 )
        return ( NULL );

    DOBRANCH = 1;
    gen_tree ( n - 1, old_trees, dif_lev, env );
    tree = exp_to_tree ( old_trees, n - 1, tpool );
    tree->leaf = TRUE;
    prev = tree->tag;
    firstg = prev;
    colb = 0;
    bestOrdering = tmp_trees;
    ordering = new_tree;
    bestOrdering[0] = prev - 1;
    nextID = 1;
    for ( i = 1; i <= 2 * ( n - 2 ); i++ )
    {
        treeid = old_trees[i];
        if ( treeid < 0 )
            continue;
        colb += distmatrix[treeid - 1][prev - 1];
        prev = treeid;
        bestOrdering[nextID] = treeid;
        nextID++;
    }
    nextID = 0;
    colb += distmatrix[firstg - 1][prev - 1];
    colb = test_all_co_score ( best_so_far - minidist + 1, colb,
                               distmatrix, tree,
                               tree->lChild, firstg, &prev, 0 );
    DOBRANCH = 0;
    if ( ( colb + minidist ) > best_so_far )
    {
        *flag = 1;
        return NULL;
    }
    else
    {
        minidist = 10000;
        for ( i = 0; i < n - 2; i++ )
        {
            nextID = switchDist[bestOrdering[i]][bestOrdering[i + 1]];
            if ( nextID < minidist )
                minidist = nextID;
        }
        if ( ( colb + minidist ) > best_so_far )
        {
            *flag = 1;
            return NULL;
        }


    }
    gen_new_tree ( old_trees, env[n - 3].tree_id, exp, n,
                   env[n - 4].size_of_tree );

    memcpy ( ( char * ) ( env[n - 3].tree_exp ), ( char * ) exp,
             env[n - 3].size_of_tree * sizeof ( int ) );

    tree = exp_to_tree ( exp, n, tpool );
    return tree;
}

/* not include in the main distribution, recommend not use it*/
struct tNode *
testtwolevel ( int n, struct tNode *tpool, int NUM_GENOMES, bin_env_t * env,
               int *flag, int **distmatrix, int best_so_far, int minidist )
{
    struct tNode *tree;
    int exp[2 * MAX_GENOMES + 1];
    int dif_lev;
    int old_trees[2 * MAX_GENOMES + 1];

    if ( inc_bin_tree ( n, env, 1, &dif_lev ) != 0 )
        return ( NULL );

    if ( gen_tree_levels
         ( n - 1, old_trees, tpool, dif_lev, env, NUM_GENOMES, distmatrix,
           best_so_far, minidist ) == -1 )
    {
        *flag = 1;
        return NULL;
    }

    gen_new_tree ( old_trees, env[n - 3].tree_id, exp, n,
                   env[n - 4].size_of_tree );

    memcpy ( ( char * ) ( env[n - 3].tree_exp ), ( char * ) exp,
             env[n - 3].size_of_tree * sizeof ( int ) );

    tree = exp_to_tree ( exp, n, tpool );
    return tree;
}


/*generated trees are stored in new_trees in ascending order of tree_id,
  old_trees acts as a buffer*/
int
gen_tree_levels ( int n, int new_tree[2 * MAX_GENOMES + 1],
                  struct tNode *tpool, int dif_lev, bin_env_t * env,
                  int NUM_GENOMES, int **distmatrix, int best_so_far,
                  int minidist )
{
    int old_trees[2 * MAX_GENOMES + 1];
    int i, prev, colb, firstg;
    int treeid;
    struct tNode *tree;

    if ( n == dif_lev - 1 )
    {
        memcpy ( ( char * ) new_tree, ( char * ) ( env[n - 3].tree_exp ),
                 env[n - 3].size_of_tree * sizeof ( int ) );
        return 0;
    }

    if ( n == 3 )
    {
        /*base case */
        for ( i = 0; i < env[n - 3].size_of_tree; i++ )
            new_tree[i] = 0;
        new_tree[0] = 0;        /*it is tree_id, ranged from 0..((2n-5)!!-1) */
        new_tree[1] = 1;
        new_tree[2] = -1;
        new_tree[3] = 2;
        new_tree[4] = 3;
    }
    else
    {
        gen_tree_levels ( n - 1, old_trees, tpool, dif_lev, env, NUM_GENOMES,
                          distmatrix, best_so_far, minidist );
        if ( n == NUM_GENOMES - 1 )
        {
            tree = exp_to_tree ( old_trees, n - 1, tpool );
            tree->leaf = TRUE;
            prev = tree->tag;
            firstg = prev;
            colb = 0;
            for ( i = 1; i <= 2 * ( n - 2 ); i++ )
            {
                treeid = old_trees[i];
                if ( treeid < 0 )
                    continue;
                colb += distmatrix[treeid - 1][prev - 1];
                prev = treeid;
            }
            colb += distmatrix[firstg - 1][prev - 1];
            colb = test_all_co_score ( best_so_far - minidist + 1, colb,
                                       distmatrix, tree,
                                       tree->lChild, firstg, &prev, 0 );
            if ( ( colb + minidist ) > best_so_far )
            {
                return -1;
            }

        }
        gen_new_tree ( old_trees, env[n - 3].tree_id, new_tree, n,
                       env[n - 4].size_of_tree );
    }

    memcpy ( ( char * ) ( env[n - 3].tree_exp ), ( char * ) new_tree,
             env[n - 3].size_of_tree * sizeof ( int ) );

    return 0;
}
