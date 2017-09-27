/* const_tree.c
* Author: Mi Yan
*/
#include "const_tree.h"

#ifdef GMP
#include "gmp.h"
#endif

#if 0
main (  )
{
    char str[MAX_STR_LEN];
    ConstraintTree_T const_tree;
    struct tNode *bin_tree;
    int i, pos;
    env_t const_env;
    struct tNode *tpool;

    tpool =
        ( struct tNode * ) malloc ( ( 2 * MAX_NUM_GENES + 1 ) *
                                    sizeof ( struct tNode ) );


/*sprintf(str,"%s","((12,((13,(11,10)),1)),((9,8),(4,(3,2))),((7,6),5))");*/
    /*  sprintf(str,"%s","((1,2),3)"); */
    sprintf ( str, "%s", "(((1,(2,3)),4),(5,(6,7)))" );
    /*  sprintf(str,"%s","((1,2),3)"); */
/*  sprintf(str,"%s","(1,(2,3),4,(5,(6,7)),(8,9),(10,11),12,13)");*/
    init_const ( &const_env, str, const_tree, tpool );
    alloc4_const_tree_env ( const_env.num_leaves, &const_env );
    bin_tree = first_const ( const_tree, tpool, 0, &const_env );

    while ( bin_tree != NULL )
    {
        print_tree_tag ( bin_tree );
        printf ( "\n" );
        bin_tree = next_const ( const_tree, tpool, 1, &const_env );
    }

}
#endif

int
print_tree_tag ( struct tNode *bin_tree )
{
    if ( bin_tree == NULL )
        return 0;
    printf ( "%d,", bin_tree->tag );
    print_tree_tag ( bin_tree->lChild );
    print_tree_tag ( bin_tree->rChild );

    return 0;
}

/*n is the number of leaves of the const_tree*/
int
alloc4_const_tree_env ( int n, env_t * env )
{
    int i;
    int numOfChild = n;

    for ( i = 0; i < 2 * n - 1; i++ )
    {
        env->dim[i] = alloc4_bin_tree_env ( numOfChild + 1 );
    }

#ifdef DEBUG
    printf ( "at alloc4_const_tree_env,n:%d\n", n );
    for ( i = 0; i < 2 * n - 1; i++ )
        printf ( "env->dim[%d]:%p\n", i, env->dim[i] );
    fflush ( stdout );
#endif
    return 0;
}

int
free_const_tree_env ( int n, env_t * env )
{
    int i;
    int numOfChild = n;

    for ( i = 0; i < 2 * n - 1; i++ )
    {
        free_bin_tree_env ( numOfChild + 1, env->dim[i] );
    }

    return 0;
}


/*output :tpool: bin_tree leaves element*/
int
init_const ( env_t * env, const char *str, ConstraintTree_T const_tree,
             struct tNode *tpool )
{
    int internal, pos, root;
    int tree_nodes;
#ifdef DEBUG
    int i;
#endif

    internal = -1;
    pos = 0;
    tree_nodes = 0;
    env->num_leaves = 0;

    root =
        str_to_tree ( env, str, const_tree, &pos, &tree_nodes, &internal,
                      tpool );

    env->tree_nodes = tree_nodes;
    env->root = root;

#ifdef DEBUG
    printf ( " at the end of init_const,tree_nodes:%d\n", tree_nodes );
    for ( i = 0; i < tree_nodes; i++ )
    {
        printf ( "env->dim[%d]:%p\n", i, env->dim[i] );
    }
    fflush ( stdout );
#endif
    return 0;
}

/*output: tree is the post order traverse of the constraint tree*/
int
str_to_tree ( env_t * env, const char *str, ConstraintTree_T tree, int *pos,
              int *tree_nodes, int *internal, struct tNode *tpool )
{
    int len, tag, i, nb_child;
    int tmp[MAX_GENOMES];
    int root, cur_sub_root, pre_sub_root = -1, child1;
    struct tNode *bin_tree;

    if ( sscanf ( str + ( *pos ), "%d", &tag ) )
    {
        tree[*tree_nodes].tag = tag;
        tree[*tree_nodes].NumOfChild = 0;
        tree[*tree_nodes].parent = -1;
        tree[*tree_nodes].brother = -1;
        tree[*tree_nodes].bin_tree_ptr = env->num_leaves;

        bin_tree = &tpool[tree[*tree_nodes].bin_tree_ptr];
        bin_tree->parent = bin_tree->lChild = bin_tree->rChild = NULL;
        bin_tree->leaf = TRUE;
        bin_tree->tag = tag;

        len = 1;
        while ( ( tag = tag / 10 ) )
            len++;
        *pos = ( *pos ) + len;
        env->num_leaves++;
        ( *tree_nodes )++;

        return ( *tree_nodes - 1 );
    }

    if ( str[*pos] == '(' )
    {
        ( *pos )++;
        nb_child = 0;
        child1 = -1;

        while ( str[*pos] != ')' )
        {
            cur_sub_root = str_to_tree ( env, str, tree, pos, tree_nodes,
                                         internal, tpool );
            if ( nb_child == 0 )
            {
                child1 = cur_sub_root;
            }
            else
            {
                tree[pre_sub_root].brother = cur_sub_root;
            }
            tmp[nb_child] = cur_sub_root;
            nb_child++;
            pre_sub_root = cur_sub_root;
            if ( str[*pos] == ',' )
                ( *pos )++;
        }
        ( *pos )++;

        tree[*tree_nodes].tag = *internal;
        ( *internal )--;
        tree[*tree_nodes].NumOfChild = nb_child;
        tree[*tree_nodes].parent = -1;
        tree[*tree_nodes].child1 = child1;
        root = *tree_nodes;
        for ( i = 0; i < nb_child; i++ )
        {
            tree[tmp[i]].parent = root;
        }
        ( *tree_nodes )++;

        return ( root );
    }

    fprintf ( outfile, "Error in parsing constraint tree\n" );
    return ( -1 );
}

#if 0
int
inc_const_tree ( ConstraintTree_T const_tree, env_t env, int inc,
                 int *dif_node, int *dif_lev )
{
    int i, tmp;

    for ( i = 0; i < env.tree_nodes; i++ )
    {
        tmp =
            inc_bin_tree ( const_tree[i].NumOfChild, env.dim[i], inc,
                           dif_lev );
        if ( tmp == 0 )
        {
            *dif_node = i;
            return 0;
        }
        else if ( tmp > 0 )
            inc = tmp;
    }

    return ( tmp );
}
#endif

struct tNode *
first_const ( ConstraintTree_T const_tree,
              struct tNode *tpool, int MYPROC, env_t * env )
{
    int i;
#ifdef GMP
    mpz_t k;
#endif

    struct tNode *bin_tree;
    int leaves;

    /*initialize the enviroment */
    for ( i = 0; i < env->tree_nodes; i++ )
    {
        if ( ( i == env->root ) && ( const_tree[i].NumOfChild >= 3 ) )
            leaves = const_tree[i].NumOfChild;
        else
            leaves = const_tree[i].NumOfChild + 1;
        init_gen_bin ( leaves, env->dim[i] );
    }

#ifdef GMP
    mpz_init_set_si ( k, MYPROC );
    bin_tree = gen_const ( const_tree, tpool, k, env, 1 );
    mpz_clear ( k );
#else
    bin_tree = gen_const ( const_tree, tpool, MYPROC, env, 1 );
#endif

#ifdef DEBUG
    fprintf ( stdout, "at the end of first_const\n" );
    fflush ( stdout );
#endif

    return ( bin_tree );
}

#ifdef GMP
struct tNode *
next_const ( ConstraintTree_T const_tree,
             struct tNode *tpool, mpz_t PROCS, env_t * env )
#else
struct tNode *
next_const ( ConstraintTree_T const_tree,
             struct tNode *tpool, int PROCS, env_t * env )
#endif
{
    struct tNode *bin_tree;


    bin_tree = gen_const ( const_tree, tpool, PROCS, env, 0 );
    return ( bin_tree );
}

/*it generate the first binary tree, if no more binary tree canbe
generated, return -1; else return nodes number of the binary tree*/
#ifdef GMP
struct tNode *
gen_const ( ConstraintTree_T const_tree,
            struct tNode *tpool, mpz_t inc, env_t * env, int first )
#else
struct tNode *
gen_const ( ConstraintTree_T const_tree,
            struct tNode *tpool, int inc, env_t * env, int first )
#endif
{
    int pos, i, nb_child, dif_lev;
    int leaves, lchild, rchild;
#ifndef GMP
    int tmp;
#endif
    int exp[2 * MAX_GENOMES + 1];
    int internal = -1;
    struct tNode *lchild_bin, *rchild_bin, *root_bin, *parent_bin;

    pos = env->num_leaves;

/*scan the const_tree from down to top*/
    for ( i = 0; i < env->tree_nodes; i++ )
    {
        nb_child = const_tree[i].NumOfChild;
        if ( ( i == env->root ) && ( nb_child >= 3 ) )
            leaves = nb_child;
        else
            leaves = nb_child + 1;

        if ( nb_child > 0 )
        {
            if (
#ifdef GMP
                    ( mpz_sgn ( inc ) <= 0 )
#else
                    ( inc <= 0 )
#endif
                 )
            {
                dif_lev = leaves + 1;
            }
            else
            {
#ifdef GMP
                inc_bin_tree_mpz ( inc, leaves, env->dim[i], inc, &dif_lev );
#else
                tmp = inc_bin_tree ( leaves, env->dim[i], inc, &dif_lev );
                inc = tmp;
#endif
            }
            if ( first == 1 )
                dif_lev = 0;

            if (
#ifdef GMP
                    ( ( mpz_sgn ( inc ) != 0 ) && ( i == env->root ) )  /*?? */
#else
                    ( ( inc != 0 ) && ( i == env->root ) )
#endif
                 )
            {
                return NULL;
            }

            if ( ( i == env->root ) && ( nb_child == 2 ) )
            {                   /* build rooted binary tree */
                lchild = const_tree[i].child1;
                rchild = const_tree[lchild].brother;
                lchild_bin = &tpool[const_tree[lchild].bin_tree_ptr];
                rchild_bin = &tpool[const_tree[rchild].bin_tree_ptr];

                const_tree[i].bin_tree_ptr = pos;
                root_bin = &tpool[pos];
                root_bin->lChild = lchild_bin;
                root_bin->rChild = rchild_bin;
                root_bin->tag = internal;
                root_bin->leaf = FALSE;

                lchild_bin->parent = rchild_bin->parent = root_bin;

                /* rooted binary tree to unrooted binary tree */
                root_bin = NULL;
                rooted2unrooted ( tpool + pos, &root_bin );
                parent_bin = ( tpool + pos )->parent;
                rchild_bin = ( tpool + pos )->rChild;
                parent_bin->lChild = rchild_bin;
                rchild_bin->parent = parent_bin;
            }
            else
            {
                gen_tree ( leaves, exp, dif_lev, env->dim[i] );

                /*change pre-order expresstion of full binary tree to tree */
                root_bin = bin_exp_to_tree ( exp, const_tree, i, &pos,
                                             tpool, &internal, env );
            }
        }
    }

    return ( root_bin );
}

/* rooted binary tree to unrooted binary tree*/
int
rooted2unrooted ( struct tNode *rooted_tree, struct tNode **root_bin )
{
    struct tNode *lchild_bin, *rchild_bin;

    lchild_bin = rooted_tree->lChild;
    rchild_bin = rooted_tree->rChild;
    if ( lchild_bin->leaf )
    {
        lchild_bin->parent = NULL;
        lchild_bin->lChild = rooted_tree;
        lchild_bin->rChild = NULL;
        lchild_bin->leaf = FALSE;
        rooted_tree->parent = lchild_bin;

        *root_bin = lchild_bin;
    }
    else
    {
        rooted2unrooted ( lchild_bin, root_bin );
        lchild_bin->lChild = rooted_tree;
        rooted_tree->parent = lchild_bin;
    }

    return 0;
}

/*change pre-order expresstion of full binary tree to tree*/
struct tNode *
bin_exp_to_tree ( int *exp, ConstraintTree_T const_tree, int i,
                  int *pos, struct tNode *tpool, int *internal, env_t * env )
{
    int j, k, lchild, lastChild;
    struct tNode *parent, *tmp;
    int child[MAX_GENOMES];
    int n;
    struct tNode *bin_tree, *root_bin;


    n = const_tree[i].NumOfChild;
    child[0] = const_tree[i].child1;
    j = child[0];
    for ( k = 1; k < n; k++ )
    {
        j = const_tree[j].brother;
        child[k] = j;
    }
    lastChild = child[n - 1];

    root_bin = bin_tree = tpool + *pos;

    const_tree[i].bin_tree_ptr = *pos;
    ( *pos )++;
    bin_tree->parent = NULL;
    bin_tree->lChild = bin_tree->rChild = NULL;
    bin_tree->leaf = FALSE;
    if ( i == env->root )
    {
        tmp = tpool + const_tree[lastChild].bin_tree_ptr;
        if ( tmp->tag < 0 )
        {
            rooted2unrooted ( tpool + const_tree[lastChild].bin_tree_ptr,
                              &root_bin );
        }
        else
        {
            tmp->rChild = NULL;
            root_bin = tmp;
        }

        tmp->lChild = bin_tree;
        tmp->leaf = FALSE;
        bin_tree->parent = tmp;
    }

    bin_tree->tag = *internal;
    ( *internal )--;

    lchild = 1;
    parent = bin_tree;

    if ( i == env->root )
        n = n - 1;

    for ( j = 3; j < 2 * n + 1; j++ )
    {
        if ( exp[j] < 0 )
        {
            tpool[*pos].parent = parent;
            tpool[*pos].tag = *internal;
            ( *internal )--;

            tpool[*pos].lChild = tpool[*pos].rChild = NULL;
            if ( lchild == 1 )
                parent->lChild = &tpool[*pos];
            else
                parent->rChild = &tpool[*pos];
            parent = &tpool[*pos];
            lchild = 1;
            ( *pos )++;
        }
        else
        {
            tmp = &tpool[const_tree[child[exp[j] - 2]].bin_tree_ptr];
#ifdef DEBUG
            fprintf ( stdout, "exp[%d]:%d,lchild:%d,child:%d,tmp->tag:%d,\n",
                      j, exp[j], lchild, child[exp[j] - 2], tmp->tag );
            fflush ( stdout );
#endif
            tmp->parent = parent;
            if ( lchild == 1 )
                parent->lChild = tmp;
            else
                parent->rChild = tmp;

            if ( lchild == 0 )
                while ( parent != NULL )
                {
                    if ( parent->rChild == NULL )
                        break;
                    parent = parent->parent;
                }
            else
                lchild = 0;
        }
    }


    return root_bin;
}


int
gen_bin_tree ( int n, int tree_id, int *new_tree )
{
    int i, j;
    int old_tree[2 * MAX_GENOMES + 1];
    int size_of_tree = 2 * n - 1;

    if ( n == 3 )
    {
        /*base case */
        for ( i = 0; i < size_of_tree; i++ )
            new_tree[i] = 0;
        new_tree[0] = 0;        /*it is tree_id, ranged from 0..((2n-5)!!-1) */
        new_tree[1] = 1;
        new_tree[2] = -1;
        new_tree[3] = 2;
        new_tree[4] = 3;
    }
    else
    {
        gen_bin_tree ( n - 1, tree_id / ( 2 * n - 5 ), old_tree );
        j = tree_id % ( 2 * n - 5 );
        gen_new_tree ( old_tree, j, new_tree, n, size_of_tree );
    }
    return 0;
}
