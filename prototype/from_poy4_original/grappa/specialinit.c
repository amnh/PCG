#include <math.h>
#include "structs.h"
#include "binencode.h"
#include "lk_main.h"
#include "specialtsp.h"
#include "specialinit.h"

/* used by real_init_adjpars; returns -1, 0, or 1, indicating
   that the argument adjacency is bad, indifferent, or good
   in this subtree (as judged by a simple filtering propagation */
int
like_this_adj ( struct tNode *current, int num_genes, int i, int j,
                int CIRCULAR )
{
    /* uses postorder traversal from an internal node, so needs
       flags (leaf tag) to avoid going back */
    struct tNode *parent, *lChild, *rChild;
    int *genes;
    int k, value;
    int value1 = 0, value2 = 0, value3 = 0; /* internal nodes are neutral */

    current->leaf = TRUE;       /* flag visited */

    if ( current->genome != NULL )
    {                           /* a labelled node -- bottom of recursion */
        /* check whether this node's genome contains adjacency ij (or -j-i) */
        genes = current->genome->genes;
        /* as always, avoid modulo by detaching last adjacency */
        for ( k = 0; k < num_genes - 1; k++ )
        {
            if ( ( ( genes[k] == i ) && ( genes[k + 1] == j ) ) ||
                 ( ( genes[k + 1] == -i ) && ( genes[k] == -j ) ) )
                return -1;
        }
        if ( CIRCULAR )
        {
            if ( ( ( genes[num_genes - 1] == i ) && ( genes[0] == j ) ) ||
                 ( ( genes[0] == -i ) && ( genes[num_genes - 1] == -j ) ) )
                return -1;
        }
        /* else no match */
        return 1;
    }

    /* need to look at up to two subtrees, but which? */
    parent = current->parent;
    if ( parent )
    {
        if ( !( parent->leaf ) )
        {
            value1 = like_this_adj ( parent, num_genes, i, j, CIRCULAR );
        }
    }
    lChild = current->lChild;
    if ( lChild )
    {
        if ( !( lChild->leaf ) )
        {
            value2 = like_this_adj ( lChild, num_genes, i, j, CIRCULAR );
        }
    }
    rChild = current->rChild;
    if ( rChild )
    {
        if ( !( rChild->leaf ) )
        {
            value3 = like_this_adj ( rChild, num_genes, i, j, CIRCULAR );
        }
    }
    value = value1 + value2 + value3;
    /* now filter what gets returned -- -1/0/1 only */
    if ( value == 0 )
        return 0;
    else if ( value < 0 )
        return -1;
    else
        return 1;
}

void
real_init_adjpars ( struct tNode *tree, struct tNode *tpool,
                    struct genome_struct *labels,
                    int *stack, int *degree, int *otherEnd,
                    intpair_t * neighbors, smalledge_t * edges,
                    int *incycle, int *outcycle, int num_genes,
                    int num_genomes, int **weights, int **status,
                    int inittspsolver, int thresh, int CIRCULAR )
{
    /* runs the actual tree traversal to label internal nodes */
    int i, j, k, id, ind1, ind2, value;
    int realtspsolver, ncount;
    struct genome_struct *nodem;

#ifdef DEBUG
    fprintf ( outfile, "entering real_init_adjpars, tree=%p\n", tree );
    if ( tree != NULL )
        fprintf ( outfile, "tag=%3d\n", tree->tag );
    fprintf ( outfile, "lChild=%p, rChild=%p\n", tree->lChild, tree->rChild );
    fflush ( outfile );
#endif
    if ( tree == NULL )
        return;

    ncount = 2 * num_genes;

#define adjparspreorder
    /* must define exactly one of adjparspreorder or adjparspostorder;
       experiments tend to indicate that preorder may be better */
#ifdef adjparspostorder
    /* go process remaining nodes, if any */
    if ( tree->lChild )
        real_init_adjpars ( tree->lChild, tpool, labels,
                            stack, degree, otherEnd, neighbors, edges,
                            incycle, outcycle, num_genes, num_genomes,
                            weights, status, inittspsolver, thresh,
                            CIRCULAR );
    if ( tree->rChild )
        real_init_adjpars ( tree->rChild, tpool, labels,
                            stack, degree, otherEnd, neighbors, edges,
                            incycle, outcycle, num_genes, num_genomes,
                            weights, status, inittspsolver, thresh,
                            CIRCULAR );
#endif

    if ( tree->tag < 0 )
    {                           /* internal node needs to be processed */

        /* no storage assigned yet; get a node from the label array */
        id = tree->tag;         /* id is negative */
        i = num_genomes - id;
        nodem = tree->genome = &labels[i];
        nodem->genome_num = id;

        /* from this internal node, we run a postorder traversal
           of each of the three subtrees for each i,j pair */
        /* encoding is same as used for LK solvers:
           negative values are mapped in reverse from 0 to num_genes-1
           (e.g., -1 is mapped to 0, -2 to 1, etc.)
           and positive values are mapped directly from num_genes to ncount-1;
           BUT: remember we code (i,j) as (i,-j), so that matrix is symmetric */
        /* first set up diagonal */
        for ( i = 0; i < ncount; i++ )
            weights[i][i] = LARGENUM;
        /* now set up g,-g pairs */
        for ( i = 0; i < num_genes; i++ )
            weights[i][i + num_genes] = weights[i + num_genes][i] = -LARGENUM;
        /* Now for -i,-j */
        for ( i = 0; i < num_genes; i++ )
        {
            ind1 = -( i + 1 );
            for ( j = i + 1; j < num_genes; j++ )
            {
                /* set all flags to false */
                for ( k = 0; k < 2 * num_genomes - 2; k++ )
                {
                    tpool[k].leaf = FALSE;
                }
                tree->leaf = TRUE;  /* visited flag for like_this_adj */
                /* compute positive/negative indices from linearized indices */
                ind2 = -( j + 1 );
                value = 0;
                value +=
                    like_this_adj ( tree->parent, num_genes, ind1, ind2,
                                    CIRCULAR );
                value +=
                    like_this_adj ( tree->lChild, num_genes, ind1, ind2,
                                    CIRCULAR );
                value +=
                    like_this_adj ( tree->rChild, num_genes, ind1, ind2,
                                    CIRCULAR );
                weights[i][j] = weights[j][i] = value;
            }
        }
        /* now -i,j */
        for ( i = 0; i < num_genes; i++ )
        {
            ind1 = -( i + 1 );
            for ( j = i + num_genes + 1; j < ncount; j++ )
            {
                /* set all flags to false */
                for ( k = 0; k < 2 * num_genomes - 2; k++ )
                {
                    tpool[k].leaf = FALSE;
                }
                tree->leaf = TRUE;  /* visited flag for like_this_adj */
                /* compute positive/negative indices from linearized indices */
                ind2 = j + 1 - num_genes;
                value = 0;
                value +=
                    like_this_adj ( tree->parent, num_genes, ind1, ind2,
                                    CIRCULAR );
                value +=
                    like_this_adj ( tree->lChild, num_genes, ind1, ind2,
                                    CIRCULAR );
                value +=
                    like_this_adj ( tree->rChild, num_genes, ind1, ind2,
                                    CIRCULAR );
                weights[i][j] = weights[j][i] = value;
            }
        }
        /* now i,-j */
        for ( i = num_genes; i < ncount; i++ )
        {
            ind1 = i + 1 - num_genes;
            for ( j = ind1; j < num_genes; j++ )
            {
                /* set all flags to false */
                for ( k = 0; k < 2 * num_genomes - 2; k++ )
                {
                    tpool[k].leaf = FALSE;
                }
                tree->leaf = TRUE;  /* visited flag for like_this_adj */
                /* compute positive/negative indices from linearized indices */
                ind2 = -( j + 1 );
                value = 0;
                value +=
                    like_this_adj ( tree->parent, num_genes, ind1, ind2,
                                    CIRCULAR );
                value +=
                    like_this_adj ( tree->lChild, num_genes, ind1, ind2,
                                    CIRCULAR );
                value +=
                    like_this_adj ( tree->rChild, num_genes, ind1, ind2,
                                    CIRCULAR );
                weights[i][j] = weights[j][i] = value;
            }
        }
        /* now i,j */
        for ( i = num_genes; i < ncount; i++ )
        {
            ind1 = i + 1 - num_genes;
            for ( j = i + 1; j < ncount; j++ )
            {
                /* set all flags to false */
                for ( k = 0; k < 2 * num_genomes - 2; k++ )
                {
                    tpool[k].leaf = FALSE;
                }
                tree->leaf = TRUE;  /* visited flag for like_this_adj */
                /* compute positive/negative indices from linearized indices */
                ind2 = j + 1 - num_genes;
                value = 0;
                value +=
                    like_this_adj ( tree->parent, num_genes, ind1, ind2,
                                    CIRCULAR );
                value +=
                    like_this_adj ( tree->lChild, num_genes, ind1, ind2,
                                    CIRCULAR );
                value +=
                    like_this_adj ( tree->rChild, num_genes, ind1, ind2,
                                    CIRCULAR );
                weights[i][j] = weights[j][i] = value;
            }
        }
        /* weight matrix is now properly set up */

#ifdef DEBUG
        fprintf ( outfile, "weight matrix computed is:\n" );
        for ( i = 0; i < ncount; i++ )
        {
            fprintf ( outfile, "%3d: ", i );
            for ( j = 0; j < ncount; j++ )
            {
                fprintf ( outfile, "%3d, ", weights[i][j] );
                fflush ( outfile );
            }
            fprintf ( outfile, "\n" );
            fflush ( outfile );
        }
#endif

        /* restore proper flag values */
        for ( k = 0; k < 2 * num_genomes - 2; k++ )
        {
            if ( tpool[k].tag < 0 )
            {
                tpool[k].leaf = FALSE;
            }
            else
            {
                tpool[k].leaf = TRUE;
            }
        }
        /* now that the weights matrix is complete, solve the TSP */
        /* no condensing possible, since this does not start with
           actual genomes; thus also we have to use special versions
           of the solvers */
        /* note: should be easy here to call an LK solver */
        realtspsolver = inittspsolver;
        if ( ( num_genes <= thresh ) && ( inittspsolver != TSP_COALESCED ) )
            realtspsolver = TSP_BBTSP;
        switch ( realtspsolver )
        {
            case TSP_BBTSP:
                ap_bbtsp ( ncount, nodem->genes, weights, status, neighbors,
                           stack, outcycle, degree, otherEnd, edges );
                break;
            case TSP_COALESCED:
                ap_coalestsp ( ncount, nodem->genes, weights, neighbors,
                               stack, outcycle, degree, otherEnd, edges );
                break;
#ifdef CONCORDE
            case TSP_GREEDYLK:
                greedylk ( ncount, weights, nodem->genes, incycle, outcycle );
                break;
            case TSP_CHLINKERN:
                chlinkern ( ncount, weights, nodem->genes, incycle,
                            outcycle );
                break;
#endif
        }
    }                           /* this internal node now processed */

#ifdef adjparspreorder
    /* go process remaining nodes, if any */
    if ( tree->lChild )
        real_init_adjpars ( tree->lChild, tpool, labels,
                            stack, degree, otherEnd, neighbors, edges,
                            incycle, outcycle, num_genes, num_genomes,
                            weights, status, inittspsolver, thresh,
                            CIRCULAR );
    if ( tree->rChild )
        real_init_adjpars ( tree->rChild, tpool, labels,
                            stack, degree, otherEnd, neighbors, edges,
                            incycle, outcycle, num_genes, num_genomes,
                            weights, status, inittspsolver, thresh,
                            CIRCULAR );
#endif
    return;
}

void
initialize_tree_adjpars ( struct tNode *tree, struct tNode *tpool,
                          struct genome_struct *labels,
                          int *stack, int *degree, int *otherEnd,
                          intpair_t * neighbors, smalledge_t * edges,
                          int *incycle, int *outcycle,
                          int num_genes, int num_genomes,
                          int **weights, int **status,
                          int inittspsolver, int thresh, int CIRCULAR )
{
    /* conducts a traversal of the tree; for each internal node
       that needs labeling, runs a postorder traversal along
       each of the three edges from that node, once for each
       non-trivial entry in the weight matrix, and propagates
       up from labeled nodes (initially leaves) to the node a value
       of 1, 0, or -1, according to whether it would be bad, indifferent,
       or good to use the corresponding adjacency in a tour
       then solves the TSP, using matrix-based versions of bbtsp,
       coalestsp, or the usual LK, assigns the answer as internal
       genome, and moves on */
    /* this routine is just a shell to make the first recursive call;
       real_init_adjpars does the tree traversal and, at each node,
       calls like_this_adj n*n/2 times in each direction */
    if ( tree == NULL )
        return;

    real_init_adjpars ( tree, tpool, labels,
                        stack, degree + num_genes, otherEnd + num_genes,
                        neighbors + num_genes, edges, incycle, outcycle,
                        num_genes, num_genomes, weights, status,
                        inittspsolver, thresh, CIRCULAR );
    return;
}
