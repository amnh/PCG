#include <math.h>
#include "structs.h"
#include "convert.h"
#include "bbtsp.h"
#include "cheaptsp.h"
#include "binencode.h"
#include "greedy_median.h"
#include "condense3.h"
#include "lk_main.h"
#include "inittree.h"
#include "inversion_median_alberto.h"
#include "sorting_reversal_median.h"
extern int VIncrease;
void
SetTreeEdgePtrs ( struct tNode *tree, int *edgepool, int *index )
{
    struct tNode *node;

    node = tree;
    if ( node == NULL )
        return;
    if ( node->lChild )
    {
        node->lChild->sc_parent = node->sc_lChild = &edgepool[*index];
        *index += 1;
        SetTreeEdgePtrs ( node->lChild, edgepool, index );
    }
    if ( node->rChild )
    {
        node->rChild->sc_parent = node->sc_rChild = &edgepool[*index];
        *index += 1;
        SetTreeEdgePtrs ( node->rChild, edgepool, index );
    }
    return;
}

void
initialize_tree_random ( struct tNode *tree, struct genome_struct *labels,
                         int num_genes, int num_genomes )
{
    /* random permutations of 1..n assigned as labels, with sign chosen
       randomly as well; faster than NN, but produces poorer solutions;
       slower than trivial below, because of the many calls to random */
    struct genome_struct *gen;
    int id;

    if ( tree == NULL )
        return;
    if ( tree->leaf == FALSE )
    {
        /* index into labels array using tree->tag;
           it's negative, so compute proper index */
        id = tree->tag;
        gen = tree->genome = &labels[num_genomes - id];
        gen->genome_num = id;
        fillGenomeRand ( gen->genes, num_genes );
    }
    initialize_tree_random ( tree->lChild, labels, num_genes, num_genomes );
    initialize_tree_random ( tree->rChild, labels, num_genes, num_genomes );
    return;
}

void
initialize_tree_trivial ( struct tNode *tree, struct genome_struct *labels,
                          int num_genes, int num_genomes )
{
    /* every internal node gets the outgroup label; superfast, but
       tends to produces poorer solutions */
    struct genome_struct *gen;
    int id;
    int i;

    if ( tree == NULL )
        return;
    if ( tree->leaf == FALSE )
    {
        /* index into labels array using tree->tag;
           it's negative, so compute proper index */
        id = tree->tag;
        gen = tree->genome = &labels[num_genomes - id];
        gen->genome_num = id;
        for ( i = 0; i < num_genes; i++ )
            gen->genes[i] = i + 1;
    }
    initialize_tree_trivial ( tree->lChild, labels, num_genes, num_genomes );
    initialize_tree_trivial ( tree->rChild, labels, num_genes, num_genomes );
    return;
}

/* next two routines handle a queue; the queue array is initialized
   in main to be linked around the array; head points to the head
   of the queue, tail points to the end of the queue;
   note that this is *not* a correct queue implementation: it only
   works under the assumptions that one will never dequeue from an
   empty queue and that the queue cannot overflow
*/

INLINE struct tNode *
dequeue ( struct qNode *queue, struct qNode **head, struct qNode **tail )
{
    struct tNode *node;
#ifdef DEBUG
    if ( ( *head ) == ( *tail ) )
    {
        fprintf ( outfile, "Error: queue is empty!\n" );
        fflush ( outfile );
    }
#endif
    node = ( *head )->item;
    *head = ( *head )->next;
    return node;
}

INLINE void
add_queue ( struct qNode *queue,
            struct tNode *node, struct qNode **head, struct qNode **tail )
{
    ( *tail )->item = node;
    *tail = ( *tail )->next;
    return;
}

struct tNode *
find_tree_middle ( struct tNode *tree, struct qNode *queue,
                   struct tNode *tpool, int num_genomes )
{
    /* called from a leaf, will return a node on the middle of the
       path from that leaf to the farthest leaf from it */
    int i;
    struct tNode *farthest, *node, *next;
    struct qNode *head, *tail;

    /* use bfs to find the farthest leaf */
#ifdef DEBUG
    if ( tree->tag < 0 )
    {
        fprintf ( outfile,
                  "Error in find_tree_middle: start vertex is internal\n" );
        fflush ( outfile );
    }
#endif
    head = tail = queue;        /* initialize queue pointers */
    for ( i = 0; i <= 2 * num_genomes; i++ )
    {                           /* set all flags to false */
        tpool[i].leaf = FALSE;
    }
    node = tree;
    node->leaf = TRUE;
    node->pred = NULL;
    add_queue ( queue, node, &head, &tail );
    if ( node->lChild )
    {
        node->lChild->leaf = TRUE;
        node->lChild->pred = node;
        add_queue ( queue, node->lChild, &head, &tail );
    }
    if ( node->rChild )
    {
        node->rChild->leaf = TRUE;
        node->rChild->pred = node;
        add_queue ( queue, node->rChild, &head, &tail );
    }
    while ( head != tail )
    {
        /* loop until queue is empty -- last found is farthest */
        node = dequeue ( queue, &head, &tail ); /* get next node */
        /* here we know that we started at the root and so only have
           to go down child pointers */
        if ( node->tag < 0 )
        {                       /* only internal nodes have children */
            next = node->lChild;
            if ( next->leaf == FALSE )
            {
                next->leaf = TRUE;
                next->pred = node;
                add_queue ( queue, next, &head, &tail );
            }
            next = node->rChild;
            if ( next->leaf == FALSE )
            {
                next->leaf = TRUE;
                next->pred = node;
                add_queue ( queue, next, &head, &tail );
            }
        }
    }                           /* end while */
    /* last node seen is a leaf and is farthest from the starting pt */
    farthest = node;

    /* retrace path and count number of nodes */
    i = 0;
    next = farthest;
    while ( next != NULL )
    {
        i++;
        next = next->pred;
    }
    i = i / 2;
    next = farthest;
    while ( i > 0 )
    {
        i--;
        next = next->pred;
    }

    /* restore leaf flags before returning */
#if 0
    for ( i = 0; i <= 2 * num_genomes; i++ )
#else
    for ( i = 0; i < 2 * num_genomes - 2; i++ )
#endif
    {

        if ( tpool[i].tag < 0 )
        {
            tpool[i].leaf = FALSE;
        }
        else
        {
            tpool[i].leaf = TRUE;
        }
    }

    return ( next );
}

void
combine ( int *gene, struct genome_struct *g1, struct genome_struct *g2,
          int num_genes )
{
    int i;
#ifdef DEBUG
    fprintf ( outfile, "combine(): num_genes: %3d\n", num_genes );
    fprintf ( outfile, "combine(): trying to combine %p and %p\n", g1, g2 );
    fflush ( outfile );
#endif
    /* fast hack: simply copies g1 */
    /* randomization does not seem to help much here and causes 30% slowdown */
    for ( i = 0; i < num_genes; i++ )
    {
        gene[i] = g1->genes[i];
    }
    return;
}

void
propagate ( struct tNode *current, struct genome_struct *labels,
            int num_genes, int num_genomes, int *succ1, int *succ2f,
            int *succ2b, int *degree, int *pred1, int *pred2, int *tour,
            int *otherEnd, int comb_method )
{
    /* uses postorder traversal from an internal node, so needs
       flags (leaf tag) to avoid going back */
    struct tNode *parent, *lChild, *rChild;
    struct genome_struct *gen;
    int id, index;
#ifdef DEBUG
    fprintf ( outfile, "Entering propagate, combine=%2d current: %p\n",
              comb_method, current );
    fprintf ( outfile, " num_genes: %3d  num_genomes: %3d\n", num_genes,
              num_genomes );
    fprintf ( outfile, " pred1: %p pred2: %p tour: %p otherEnd: %p\n", pred1,
              pred2, tour, otherEnd );
    fprintf ( outfile, " succ1: %p succ2f: %p succ2b: %p degree: %p\n", succ1,
              succ2f, succ2b, degree );
    fflush ( outfile );
    if ( current->tag >= 0 )
    {
        fprintf ( outfile, "Error in propagate: node is a leaf\n" );
        fflush ( outfile );
    }
#endif

    current->leaf = TRUE;

    /* need to look at up to two subtrees, but which? */
    parent = current->parent;
    if ( parent )
    {
        if ( !( parent->leaf ) )
        {
            propagate ( parent, labels, num_genes, num_genomes,
                        succ1, succ2f, succ2b, degree, pred1, pred2, tour,
                        otherEnd, comb_method );
        }
    }
    lChild = current->lChild;
    if ( lChild )
    {
        if ( !( lChild->leaf ) )
        {
            propagate ( lChild, labels, num_genes, num_genomes,
                        succ1, succ2f, succ2b, degree, pred1, pred2, tour,
                        otherEnd, comb_method );
        }
    }
    rChild = current->rChild;
    if ( rChild )
    {
        if ( !( rChild->leaf ) )
        {
            propagate ( rChild, labels, num_genes, num_genomes,
                        succ1, succ2f, succ2b, degree, pred1, pred2, tour,
                        otherEnd, comb_method );
        }
    }

    /* now we have taken care of subtrees, so we have two labeled
       neighbors and an unlabeled one */

    /* set up a node for this genome */
    id = current->tag;
    index = num_genomes - id;
    gen = current->genome = &labels[index];
    gen->genome_num = id;

    /* combine genome1 and genome2 */
    if ( parent->genome == NULL )
    {
        if ( comb_method == 0 )
        {                       /* FASTPROP */
            combine ( gen->genes, lChild->genome, rChild->genome, num_genes );
        }
        else
        {                       /* MEDIANPROP */
            greedy_median ( gen->genes, lChild->genome, rChild->genome,
                            num_genes, degree, succ1, succ2f, succ2b, pred1,
                            pred2, tour, otherEnd );
        }
    }
    else
    {
        if ( lChild->genome == NULL )
        {
            if ( comb_method == 0 )
            {                   /* FASTPROP */
                combine ( gen->genes, parent->genome, rChild->genome,
                          num_genes );
            }
            else
            {                   /* MEDIANPROP */
                greedy_median ( gen->genes, parent->genome, rChild->genome,
                                num_genes, degree, succ1, succ2f, succ2b,
                                pred1, pred2, tour, otherEnd );
            }
        }
        else
        {                       /* by default, rChild->genome must be null */
            if ( comb_method == 0 )
            {                   /* FASTPROP */
                combine ( gen->genes, parent->genome, lChild->genome,
                          num_genes );
            }
            else
            {                   /* MEDIANPROP */
                greedy_median ( gen->genes, parent->genome, lChild->genome,
                                num_genes, degree, succ1, succ2f, succ2b,
                                pred1, pred2, tour, otherEnd );
            }
        }
    }
    return;
}

void
initialize_tree_propagate ( int COND, struct tNode *tree, struct tNode *tpool,
                            struct genome_struct *labels, struct qNode *queue,
                            struct adj_struct *adj_list,
                            struct adj_struct *adj_pool,
                            int **weights,
                            int *stack, int *degree, int *otherEnd,
                            intpair_t * neighbors, edge_t * edges,
                            int *incycle, int *outcycle,
                            int num_genes, int num_genomes,
                            int *pred1, int *pred2, int *picked, int *decode,
                            int comb_method, int inittspsolver,
                            int thresh, int CIRCULAR )
{
    /* long list of args needed for calls to propagate (really, to
       greedy_median) and to bbtsp */
    int i, id;
    int realsolver, num_cond, ncount;
    struct tNode *root, *parent, *lChild, *rChild;
    struct tNode *condnode1, *condnode2, *condnode3, *condmedian;
    struct genome_struct *gen[3];

    /* Uses a postorder traversal from a central internal node (middle
       of longest path in the tree) to propagate leaf labels to internal
       nodes in a simple way */

    if ( tree == NULL )
        return;

    /* get starting node for postorder */
    root = find_tree_middle ( tree, queue, tpool, num_genomes );
#ifdef DEBUG
    fprintf ( outfile, "ITP: root %p with tag %3d\n", root, root->tag );
#endif
    /* find_tree_middle routine used and restored leaf flags */

    /* we'll reuse some of the space passed through for convert
       and bbtsp; thus propagate will use stack for succ1,
       picked for succ2f, incycle for succ2b, and outcycle for tour */
    root->leaf = TRUE;          /* avoid passing through this node! */
    /* start up to three separate postorder traversals, one in each
       subtree of this node; but only for non-leaves  */
    parent = root->parent;
    if ( parent->tag < 0 )
    {
        propagate ( parent, labels, num_genes, num_genomes,
                    stack, picked, incycle, degree,
                    pred1, pred2, outcycle, otherEnd, comb_method );
    }
    lChild = root->lChild;
    if ( lChild->tag < 0 )
    {
        propagate ( lChild, labels, num_genes, num_genomes,
                    stack, picked, incycle, degree, pred1, pred2, outcycle,
                    otherEnd, comb_method );
    }
    rChild = root->rChild;
    if ( rChild->tag < 0 )
    {
        propagate ( rChild, labels, num_genes, num_genomes,
                    stack, picked, incycle, degree, pred1, pred2, outcycle,
                    otherEnd, comb_method );
    }
    /* restore leaf flags */
#if 0
    for ( i = 0; i <= 2 * num_genomes; i++ )
#else
    for ( i = 0; i < 2 * num_genomes - 2; i++ )
#endif
    {
        if ( tpool[i].tag < 0 )
        {
            tpool[i].leaf = FALSE;
        }
        else
        {
            tpool[i].leaf = TRUE;
        }
    }

    /* now just set up label of root from those of parent, lChild, rChild */
    /* first set up a node for this genome */
    id = root->tag;
    root->genome = &labels[num_genomes - id];
    root->genome->genome_num = id;

    /* now take the median of these three leaves */

    /* condense? */
    if ( COND )
    {                           /* condensation */
        ncount = 2 * num_genomes;
        condnode1 = &tpool[ncount + 2];
        condnode1->genome = &labels[ncount + 2];
        condnode2 = &tpool[ncount + 3];
        condnode2->genome = &labels[ncount + 3];
        condnode3 = &tpool[ncount + 4];
        condnode3->genome = &labels[ncount + 4];
        condmedian = &tpool[ncount + 5];
        condmedian->genome = &labels[ncount + 5];

        condense3 ( parent->genome->genes,
                    lChild->genome->genes,
                    rChild->genome->genes,
                    condnode1->genome->genes,
                    condnode2->genome->genes,
                    condnode3->genome->genes, num_genes, &num_cond,
#if 0
                    pred1, pred2, picked,
#else
                    pred1 + num_genes, pred2 + num_genes, picked + num_genes,
#endif
                    decode );

        /* use pred1=succ,pred2=pred,picked=code */

        if ( ( CIRCULAR && ( num_cond == 0 ) )
             || ( ( !CIRCULAR ) && ( num_cond <= 1 ) ) )
        {
            /* three identical neighbors... */
            for ( i = 0; i < num_genes; i++ )
            {                   /* so just copy on in place */
                root->genome->genes[i] = parent->genome->genes[i];
            }
            goto done;          /* and exit */
        }
        /* call solvers with condensed genomes */
        /* the LK solvers need a weight matrix, but the bbtsp solver does not */
        /* force use of  bbtsp if problem is too small */
        realsolver = inittspsolver;
        if ( ( num_cond <= thresh ) && ( inittspsolver != TSP_COALESCED ) )
            realsolver = TSP_BBTSP;
        switch ( realsolver )
        {
            case INVERSION_MEDIAN:
                gen[0] = condnode1->genome;
                gen[1] = condnode2->genome;
                gen[2] = condnode3->genome;
                find_reversal_median ( condmedian->genome, gen, num_cond,
                                       NULL );
                break;
            case INVERSION_MEDIAN_FAST:
                gen[0] = condnode1->genome;
                gen[1] = condnode1->genome;
                gen[2] = condnode1->genome;
                if ( CIRCULAR )
                    albert_inversion_median_circular ( gen,
                                                       num_cond,
                                                       condmedian->genome->
                                                       genes );
                else
                    albert_inversion_median_noncircular ( gen,
                                                          num_cond,
                                                          condmedian->genome->
                                                          genes );
                break;

            case TSP_BBTSP:
                /* does not create a weight matrix, but sets weight in the adj lists */
                convert2_to_tsp ( condnode1->genome, condnode2->genome,
                                  condnode3->genome, adj_list, adj_pool,
                                  num_cond, CIRCULAR );
                bbtsp ( 2 * num_cond, condmedian->genome->genes, FALSE, /* cannot use median that does not exist */
                        condnode1->genome->genes, condnode2->genome->genes,
                        condnode3->genome->genes,
                        adj_list, neighbors, stack, outcycle, degree,
                        otherEnd, edges, CIRCULAR );
                break;
#ifdef CONCORDE
            case TSP_CHLINKERN:
                /* creates a weight matrix -- more expensive! */
                convert_to_tsp ( condnode1->genome, condnode2->genome,
                                 condnode3->genome, num_cond, CIRCULAR,
                                 weights );
                chlinkern ( 2 * num_cond, weights, condmedian->genome->genes,
                            incycle, outcycle );
                break;
            case TSP_GREEDYLK:
                /* creates a weight matrix -- more expensive! */
                convert_to_tsp ( condnode1->genome, condnode2->genome,
                                 condnode3->genome, num_cond, CIRCULAR,
                                 weights );
                greedylk ( 2 * num_cond, weights, condmedian->genome->genes,
                           incycle, outcycle );
                break;
#endif
            case TSP_COALESCED:
                /* does not create a weight matrix, but sets weight in the adj lists */
                convert2_to_tsp ( condnode1->genome, condnode2->genome,
                                  condnode3->genome, adj_list, adj_pool,
                                  num_cond, CIRCULAR );
                coalestsp ( 2 * num_cond, condmedian->genome->genes, FALSE, /* cannot use median that does not exist */
                            condnode1->genome->genes,
                            condnode2->genome->genes,
                            condnode3->genome->genes, adj_list, neighbors,
                            stack, outcycle, degree, otherEnd, edges,
                            CIRCULAR );
                break;
        }
        /* decode the median and assign it to the node */
        decode3 ( root->genome->genes, condmedian->genome->genes,
#if 0
                  pred1 /* really, succ */ ,
#else
                  pred1 + num_genes /* really, succ */ ,
#endif
                  decode, num_cond );
    }                           /* end condensation */
    else
    {                           /* no condensation */
        /* which solver to call? */
        /* force use of  bbtsp if problem is too small */
        realsolver = inittspsolver;
        if ( ( num_genes <= thresh ) && ( inittspsolver != TSP_COALESCED ) )
            realsolver = TSP_BBTSP;
        switch ( realsolver )
        {
            case INVERSION_MEDIAN:
                gen[0] = parent->genome;
                gen[1] = lChild->genome;
                gen[2] = rChild->genome;
                find_reversal_median ( root->genome, gen, num_genes, NULL );
                break;
            case INVERSION_MEDIAN_FAST:
                gen[0] = parent->genome;
                gen[1] = lChild->genome;
                gen[2] = rChild->genome;
                if ( CIRCULAR )
                    albert_inversion_median_circular ( gen, num_genes,
                                                       root->genome->genes );
                else
                    albert_inversion_median_noncircular ( gen, num_genes,
                                                          root->genome->
                                                          genes );
                break;

            case TSP_BBTSP:
                /* does not create a weight matrix, but sets weight in the adj lists */
                convert2_to_tsp ( parent->genome, lChild->genome,
                                  rChild->genome, adj_list, adj_pool,
                                  num_genes, CIRCULAR );
                bbtsp ( 2 * num_genes, root->genome->genes, FALSE,
                        parent->genome->genes, lChild->genome->genes,
                        rChild->genome->genes, adj_list, neighbors, stack,
                        outcycle, degree, otherEnd, edges, CIRCULAR );
                break;
#ifdef CONCORDE
            case TSP_CHLINKERN:
                /* creates a weight matrix -- more expensive! */
                convert_to_tsp ( parent->genome, lChild->genome,
                                 rChild->genome, num_genes, CIRCULAR,
                                 weights );
                chlinkern ( 2 * num_genes, weights, root->genome->genes,
                            incycle, outcycle );
                break;
            case TSP_GREEDYLK:
                /* creates a weight matrix -- more expensive! */
                convert_to_tsp ( parent->genome, lChild->genome,
                                 rChild->genome, num_genes, CIRCULAR,
                                 weights );
                greedylk ( 2 * num_genes, weights, root->genome->genes,
                           incycle, outcycle );
                break;
#endif
            case TSP_COALESCED:
                /* does not create a weight matrix, but sets weight in the adj lists */
                convert2_to_tsp ( parent->genome, lChild->genome,
                                  rChild->genome, adj_list, adj_pool,
                                  num_genes, CIRCULAR );
                coalestsp ( 2 * num_genes, root->genome->genes, FALSE,
                            parent->genome->genes, lChild->genome->genes,
                            rChild->genome->genes, adj_list, neighbors, stack,
                            outcycle, degree, otherEnd, edges, CIRCULAR );
                break;
        }
    }                           /* end -- no condensation */
  done:;

    /* tspsolvers automatically assigned median to current node */
    return;
}

/* used by initialize_tree_SNN */
struct tNode *
find_closest_leaf ( struct tNode *node, struct qNode *queue )
{
    /* by "closest" we mean "the fewest edges away", since we
       have no values for edge lengths; thus we need a simple bfs */
    /* use node->leaf as a visited flag -- needed, since we have
       an unrooted tree */
    struct qNode *head, *tail;  /* queue pointers */
    struct tNode *next;

    head = tail = queue;        /* initialize queue pointers */
    node->leaf = TRUE;
    while ( 1 )
    {                           /* loop until a leaf is found -- one must exist */
        if ( node->tag >= 0 )
        {
            return node;
        }
        else
        {                       /* can move in 2 of 3 directions, but which? */
            next = node->parent;
            if ( next->leaf == FALSE )
            {
                next->leaf = TRUE;
                add_queue ( queue, next, &head, &tail );
            }
            next = node->lChild;
            if ( next->leaf == FALSE )
            {
                next->leaf = TRUE;
                add_queue ( queue, next, &head, &tail );
            }
            next = node->rChild;
            if ( next->leaf == FALSE )
            {
                next->leaf = TRUE;
                add_queue ( queue, next, &head, &tail );
            }
            node = dequeue ( queue, &head, &tail );
            /* cannot fail -- there is always a leaf */
        }
    }
}

/* used by initialize_tree_BNN */
void
real_init_tree_NN ( int COND, struct tNode *tree, struct tNode *tpool,
                    struct genome_struct *labels, triple_t * triple,
                    struct adj_struct *adj_list, struct adj_struct *adj_pool,
                    int **weights, int *stack, int *degree, int *otherEnd,
                    intpair_t * neighbors, edge_t * edges,
                    int *incycle, int *outcycle,
                    int *pred, int *succ, int *code, int *decode,
                    struct tNode *condnode1, struct tNode *condnode2,
                    struct tNode *condnode3, struct tNode *condmedian,
                    int num_genes, int num_genomes, int inittspsolver,
                    int thresh, int CIRCULAR )
{
    struct genome_struct *node1, *node2, *node3, *nodem;
    struct genome_struct *gen[3];
    int i, id;
    int realsolver, num_cond;

    if ( tree->tag >= 0 )
        return;                 /* nothing to do at a leaf */

    /* no storage assigned yet; get a node from the label array */
    id = tree->tag;
    i = num_genomes - id;
    nodem = tree->genome = &labels[i];
    nodem->genome_num = id;

    node1 = &labels[triple[i].A];
    node2 = &labels[triple[i].B];
    node3 = &labels[triple[i].C];
    /* now take the median of these three leaves */
#ifdef DEBUG
    fprintf ( outfile,
              "in real_init_NN, about to convert, id=%3d, A=%3d, B=%3d, C=%3d\n",
              id, triple[i].A, triple[i].B, triple[i].C );
#endif

    if ( COND )
    {                           /* condensation */
        condense3 ( node1->genes, node2->genes, node3->genes,
                    condnode1->genome->genes,
                    condnode2->genome->genes,
                    condnode3->genome->genes,
                    num_genes, &num_cond, succ, pred, code, decode );

        if ( num_cond == 0 )
        {                       /* three identical neighbors... */
            /* note: only works for circular genome; for linear, value is 1 */
            for ( i = 0; i < num_genes; i++ )
            {                   /* so just copy one in place */
                nodem->genes[i] = node1->genes[i];
            }
        }
        else
        {                       /* get the median the hard way ;-) */
            /* needs a small fix here: bbtsp will try to use existing
               genome at tree->genome as a tour, which will crash, since
               there is not one yet...  so pass FALSE to use_median */
            realsolver = inittspsolver;
            if ( ( num_cond <= thresh )
                 && ( inittspsolver != TSP_COALESCED ) )
                realsolver = TSP_BBTSP;
            switch ( realsolver )
            {
                case INVERSION_MEDIAN:
                    gen[0] = condnode1->genome;
                    gen[1] = condnode2->genome;
                    gen[2] = condnode3->genome;
                    find_reversal_median ( condmedian->genome, gen, num_cond,
                                           NULL );
                    break;
                case INVERSION_MEDIAN_FAST:
                    gen[0] = condnode1->genome;
                    gen[1] = condnode1->genome;
                    gen[2] = condnode1->genome;
                    if ( CIRCULAR )
                        albert_inversion_median_circular ( gen, num_cond,
                                                           condmedian->
                                                           genome->genes );
                    else
                        albert_inversion_median_noncircular ( gen, num_cond,
                                                              condmedian->
                                                              genome->genes );
                    break;

                case TSP_BBTSP:
                    convert2_to_tsp ( condnode1->genome, condnode2->genome,
                                      condnode3->genome, adj_list, adj_pool,
                                      num_cond, CIRCULAR );
                    bbtsp ( 2 * num_cond, condmedian->genome->genes, FALSE,
                            condnode1->genome->genes,
                            condnode2->genome->genes,
                            condnode3->genome->genes, adj_list, neighbors,
                            stack, outcycle, degree, otherEnd, edges,
                            CIRCULAR );
                    break;
                case TSP_COALESCED:
                    convert2_to_tsp ( condnode1->genome, condnode2->genome,
                                      condnode3->genome, adj_list, adj_pool,
                                      num_cond, CIRCULAR );
                    coalestsp ( 2 * num_cond, condmedian->genome->genes,
                                FALSE, condnode1->genome->genes,
                                condnode2->genome->genes,
                                condnode3->genome->genes, adj_list, neighbors,
                                stack, outcycle, degree, otherEnd, edges,
                                CIRCULAR );
                    break;
#ifdef CONCORDE
                case TSP_GREEDYLK:
                    /* creates a weight matrix -- more expensive! */
                    convert_to_tsp ( condnode1->genome, condnode2->genome,
                                     condnode3->genome, num_cond, CIRCULAR,
                                     weights );
                    greedylk ( 2 * num_cond, weights,
                               condmedian->genome->genes, incycle, outcycle );
                    break;
                case TSP_CHLINKERN:
                    /* creates a weight matrix -- more expensive! */
                    convert_to_tsp ( condnode1->genome, condnode2->genome,
                                     condnode3->genome, num_cond, CIRCULAR,
                                     weights );
                    chlinkern ( 2 * num_cond, weights,
                                condmedian->genome->genes, incycle,
                                outcycle );
                    break;
#endif
            }
            /* bbtsp automatically assigned median to current node,
             * but it has to be decoded */
            decode3 ( nodem->genes, condmedian->genome->genes, succ, decode,
                      num_cond );
        }
    }                           /* end condensation */
    else
    {                           /* no condensation */
        realsolver = inittspsolver;
        if ( ( num_genes <= thresh ) && ( inittspsolver != TSP_COALESCED ) )
            realsolver = TSP_BBTSP;
        switch ( realsolver )
        {
            case TSP_BBTSP:
                convert2_to_tsp ( node1, node2, node3,
                                  adj_list, adj_pool, num_genes, CIRCULAR );
                bbtsp ( 2 * num_genes, nodem->genes, FALSE,
                        node1->genes, node2->genes, node3->genes,
                        adj_list, neighbors, stack, outcycle, degree,
                        otherEnd, edges, CIRCULAR );
                break;
            case INVERSION_MEDIAN:
                gen[0] = node1;
                gen[1] = node2;
                gen[2] = node3;
                find_reversal_median ( nodem, gen, num_genes, NULL );
                break;
            case INVERSION_MEDIAN_FAST:
                gen[0] = node1;
                gen[1] = node2;
                gen[2] = node3;
                if ( CIRCULAR )
                    albert_inversion_median_circular ( gen, num_genes,
                                                       nodem->genes );
                else
                    albert_inversion_median_noncircular ( gen, num_genes,
                                                          nodem->genes );
                break;
            case TSP_COALESCED:
                convert2_to_tsp ( node1, node2, node3, adj_list,
                                  adj_pool, num_genes, CIRCULAR );
                coalestsp ( 2 * num_genes, nodem->genes, FALSE,
                            node1->genes, node2->genes, node3->genes,
                            adj_list, neighbors, stack, outcycle, degree,
                            otherEnd, edges, CIRCULAR );
                break;
#ifdef CONCORDE
            case TSP_CHLINKERN:
                /* creates a weight matrix -- more expensive! */
                convert_to_tsp ( node1, node2, node3,
                                 num_genes, CIRCULAR, weights );
                chlinkern ( 2 * num_genes, weights, nodem->genes,
                            incycle, outcycle );
                break;
            case TSP_GREEDYLK:
                /* creates a weight matrix -- more expensive! */
                convert_to_tsp ( node1, node2, node3,
                                 num_genes, CIRCULAR, weights );
                greedylk ( 2 * num_genes, weights, nodem->genes,
                           incycle, outcycle );
                break;
#endif
        }
        /* bbtsp automatically assigned median to current node */
    }                           /* end no condensation */
    /* go process remaining nodes, if any */
    if ( tree->lChild )
        real_init_tree_NN ( COND, tree->lChild, tpool, labels, triple,
                            adj_list, adj_pool, weights, stack, degree,
                            otherEnd, neighbors, edges, incycle, outcycle,
                            pred, succ, code, decode, condnode1, condnode2,
                            condnode3, condmedian, num_genes, num_genomes,
                            inittspsolver, thresh, CIRCULAR );
    if ( tree->rChild )
        real_init_tree_NN ( COND, tree->rChild, tpool, labels, triple,
                            adj_list, adj_pool, weights, stack, degree,
                            otherEnd, neighbors, edges, incycle, outcycle,
                            pred, succ, code, decode, condnode1, condnode2,
                            condnode3, condmedian, num_genes, num_genomes,
                            inittspsolver, thresh, CIRCULAR );
    return;
}

int
move_up ( struct tNode *current, triple_t * triple, int num_genomes )
{
    /* recursive routine to propagate up from the leaves the
       identity of and distance to the closest leaf */
    /* function returns the ID of the leaf, stores the two
       closest leaves (lChild and rChild) in triple.A and .B,
       and stores distance to closer one, plus 1, in sc field of parent */
    int id, ind;

    id = current->tag;
    if ( id >= 0 )
    {                           /* a leaf */
        *( current->sc_parent ) = 0;
        return ( id );
    }
    /* continue postorder ( id is negative ) */
    ind = num_genomes - id;
    triple[ind].A = move_up ( current->lChild, triple, num_genomes );
    triple[ind].B = move_up ( current->rChild, triple, num_genomes );
    /* which side has the closer leaf? */
    if ( *( current->sc_lChild ) <= *( current->sc_rChild ) )
    {
        *( current->sc_parent ) = *( current->sc_lChild ) + 1;
        id = triple[ind].A;
    }
    else
    {
        *( current->sc_parent ) = *( current->sc_rChild ) + 1;
        id = triple[ind].B;
    }
    return ( id );
}

void
move_down ( struct tNode *parent, struct tNode *current, int which,
            triple_t * triple, int num_genomes )
{
    /* recursive routine to explore the tree and set up the triple[]
       array for internal nodes, listing the three closest leaves */
    /* routine need only set triple.C, the third closest leaf, that
       in the direction of the parent */
    int id, currind;

    id = current->tag;
    if ( id >= 0 )
    {                           /* leaf, done */
        return;
    }
    /* continue preorder ( id is negative ) */
    currind = num_genomes - id;
    id = parent->tag;
    if ( id >= 0 )
    {                           /* parent is a leaf */
        triple[currind].C = id;
    }
    else
    {                           /* parent is an internal node */
        switch ( which )
        {
            case 0:            /* current is lChild of parent */
                /* so compare rChild of parent and parent of parent, if any */
                if ( *( parent->sc_rChild ) <= *( parent->sc_parent ) )
                {
                    triple[currind].C = triple[num_genomes - id].B;
                }
                else
                {
                    triple[currind].C = triple[num_genomes - id].C;
                }
                break;
            case 1:            /* current is rChild of parent */
                /* so compare lChild of parent and parent of parent, if any */
                if ( *( parent->sc_lChild ) <= *( parent->sc_parent ) )
                {
                    triple[currind].C = triple[num_genomes - id].A;
                }
                else
                {
                    triple[currind].C = triple[num_genomes - id].C;
                }
                break;
        }
    }
    move_down ( current, current->lChild, 0, triple, num_genomes );
    move_down ( current, current->rChild, 1, triple, num_genomes );
}

/* NN version for large numbers of genomes: all linear work */
void
initialize_tree_BNN ( int COND, struct tNode *tree, struct tNode *tpool,
                      struct genome_struct *labels, triple_t * triple,
                      struct adj_struct *adj_list,
                      struct adj_struct *adj_pool, int **weights, int *stack,
                      int *degree, int *otherEnd, intpair_t * neighbors,
                      edge_t * edges, int *incycle, int *outcycle, int *pred,
                      int *succ, int *code, int *decode, int num_genes,
                      int num_genomes, int inittspsolver, int thresh,
                      int CIRCULAR )
{
    /* for each internal node, locates the three nearest leaves and uses
       their exact median as initial label; slower than trivial, random,
       and propagate, due to the median calls, but reduces the number of
       relabellings and tends to produce better solutions */
    /* this is a shell for the recursive routine; its job is to set
       up an array (neighbors) for each internal node of the tree with the IDs
       of the three closest leaves -- that can be done in one pass
       over the tree, then the tree is traversed in the usual
       recursive manner for the real initialization */
    int ncount;
    struct tNode *condnode1, *condnode2, *condnode3, *condmedian;

#ifdef DEBUG
    int i;
#endif

    if ( tree == NULL )
        return;

    if ( tree->lChild != NULL )
        move_up ( tree->lChild, triple, num_genomes );
    if ( tree->rChild != NULL )
        move_up ( tree->rChild, triple, num_genomes );

    if ( tree->lChild != NULL )
        move_down ( tree, tree->lChild, 0, triple, num_genomes );
    if ( tree->rChild != NULL )
        move_down ( tree, tree->rChild, 1, triple, num_genomes );
#ifdef DEBUG
    for ( i = 1; i < 2 * num_genomes - 1; i++ )
    {
        fprintf ( outfile, "i=%3d, A=%3d, B=%3d, C=%3d\n",
                  i, triple[i].A, triple[i].B, triple[i].C );
    }
    fflush ( outfile );
#endif

    ncount = 2 * num_genomes;
    condnode1 = &tpool[ncount + 2];
    condnode1->genome = &labels[ncount + 2];
    condnode2 = &tpool[ncount + 3];
    condnode2->genome = &labels[ncount + 3];
    condnode3 = &tpool[ncount + 4];
    condnode3->genome = &labels[ncount + 4];
    condmedian = &tpool[ncount + 5];
    condmedian->genome = &labels[ncount + 5];

    /* root is a leaf with just a left subtree */
    real_init_tree_NN ( COND, tree->lChild, tpool, labels, triple, adj_list,
                        adj_pool, weights, stack, degree, otherEnd, neighbors,
                        edges, incycle, outcycle, pred + num_genes,
                        succ + num_genes, code + num_genes, decode, condnode1,
                        condnode2, condnode3, condmedian, num_genes,
                        num_genomes, inittspsolver, thresh, CIRCULAR );
    return;
}

/* used by NN version for small numbers of genomes -- quadratic work */
void
realinit_tree_SNN ( int COND, struct tNode *tree, struct tNode *tpool,
                    struct genome_struct *labels, struct qNode *queue,
                    struct adj_struct *adj_list, struct adj_struct *adj_pool,
                    int **weights, int *stack, int *degree, int *otherEnd,
                    intpair_t * neighbors, edge_t * edges,
                    int *incycle, int *outcycle,
                    int *pred, int *succ, int *code, int *decode,
                    struct tNode *condnode1, struct tNode *condnode2,
                    struct tNode *condnode3, struct tNode *condmedian,
                    int num_genes, int num_genomes, int inittspsolver,
                    int thresh, int CIRCULAR )
{
    /* for each internal node, locates the three nearest leaves and uses
       their exact median as initial label; slower than trivial, random,
       and propagate, due to the median calls, but reduces the number of
       relabellings and tends to produce better solutions */
    struct genome_struct *node1, *node2, *node3, *nodem;
    struct genome_struct *gen[3];
    int i, id;
    int realsolver, num_cond;

    /* for each internal tree node, find the closest leaves (one down each
       edge from the node) and use their median as initial label */
    if ( tree == NULL )
        return;

    if ( tree->tag < 0 )
    {                           /* internal node needs to be processed */

        /* no storage assigned yet; get a node from the label array */
        id = tree->tag;
        i = num_genomes - id;
        nodem = tree->genome = &labels[i];
        nodem->genome_num = id;

        /* set all flags to false */
        for ( i = 0; i <= 2 * num_genomes; i++ )
        {
            tpool[i].leaf = FALSE;
        }
        tree->leaf = TRUE;      /* visited flag for find_closest_leaf */
        node1 = find_closest_leaf ( tree->parent, queue )->genome;  /* leaf 1 */
        node2 = find_closest_leaf ( tree->lChild, queue )->genome;  /* leaf 2 */
        node3 = find_closest_leaf ( tree->rChild, queue )->genome;  /* leaf 3 */
        /* restore proper flag values */
        for ( i = 0; i < 2 * num_genomes - 2; i++ )
        {
            if ( tpool[i].tag < 0 )
            {
                tpool[i].leaf = FALSE;
            }
            else
            {
                tpool[i].leaf = TRUE;
            }
        }

        if ( COND )
        {                       /* condensation */
            condense3 ( node1->genes, node2->genes, node3->genes,
                        condnode1->genome->genes, condnode2->genome->genes,
                        condnode3->genome->genes,
                        num_genes, &num_cond, succ, pred, code, decode );

            if ( num_cond == 0 )
            {                   /* three identical neighbors... */
                /* note: only works for circular genome; for linear, value is 1 */
                for ( i = 0; i < num_genes; i++ )
                {               /* so just copy one in place */
                    nodem->genes[i] = node1->genes[i];
                }
            }
            else
            {                   /* get the median the hard way ;-) */
                realsolver = inittspsolver;
                if ( ( num_cond <= thresh )
                     && ( inittspsolver != TSP_COALESCED ) )
                    realsolver = TSP_BBTSP;
                VIncrease = 100;
                switch ( realsolver )
                {
                    case TSP_BBTSP:
                        convert2_to_tsp ( condnode1->genome,
                                          condnode2->genome,
                                          condnode3->genome, adj_list,
                                          adj_pool, num_cond, CIRCULAR );
                        bbtsp ( 2 * num_cond, condmedian->genome->genes,
                                FALSE, condnode1->genome->genes,
                                condnode2->genome->genes,
                                condnode3->genome->genes, adj_list, neighbors,
                                stack, outcycle, degree, otherEnd, edges,
                                CIRCULAR );
                        break;
                    case TSP_COALESCED:
                        convert2_to_tsp ( condnode1->genome,
                                          condnode2->genome,
                                          condnode3->genome, adj_list,
                                          adj_pool, num_cond, CIRCULAR );
                        coalestsp ( 2 * num_cond, condmedian->genome->genes,
                                    FALSE, condnode1->genome->genes,
                                    condnode2->genome->genes,
                                    condnode3->genome->genes, adj_list,
                                    neighbors, stack, outcycle, degree,
                                    otherEnd, edges, CIRCULAR );
                        break;
                    case INVERSION_MEDIAN:
                        gen[0] = condnode1->genome;
                        gen[1] = condnode2->genome;
                        gen[2] = condnode3->genome;
                        find_reversal_median ( condmedian->genome, gen,
                                               num_cond, NULL );
                        /*find_inversion_median(condmedian->genome, gen, num_cond, distmem); */

                        break;

                    case INVERSION_MEDIAN_FAST:
                        gen[0] = condnode1->genome;
                        gen[1] = condnode2->genome;
                        gen[2] = condnode3->genome;
                        if ( CIRCULAR )
                            albert_inversion_median_circular ( gen, num_cond,
                                                               condmedian->
                                                               genome->
                                                               genes );
                        else
                            albert_inversion_median_noncircular ( gen,
                                                                  num_cond,
                                                                  condmedian->
                                                                  genome->
                                                                  genes );
                        break;

#ifdef CONCORDE
                    case TSP_GREEDYLK:
                        /* creates a weight matrix -- more expensive! */
                        convert_to_tsp ( condnode1->genome, condnode2->genome,
                                         condnode3->genome, num_cond,
                                         CIRCULAR, weights );
                        greedylk ( 2 * num_cond, weights,
                                   condmedian->genome->genes, incycle,
                                   outcycle );
                        break;
                    case TSP_CHLINKERN:
                        /* creates a weight matrix -- more expensive! */
                        convert_to_tsp ( condnode1->genome, condnode2->genome,
                                         condnode3->genome, num_cond,
                                         CIRCULAR, weights );
                        chlinkern ( 2 * num_cond, weights,
                                    condmedian->genome->genes, incycle,
                                    outcycle );
                        break;
#endif
                }
                /* bbtsp automatically assigned median to current node,
                   but it has to be decoded */
                decode3 ( nodem->genes, condmedian->genome->genes, succ,
                          decode, num_cond );
            }
        }                       /* end condensation */
        else
        {                       /* no condensation */
            /* now take the median of these three leaves */
            realsolver = inittspsolver;
            if ( ( num_genes <= thresh )
                 && ( inittspsolver != TSP_COALESCED ) )
                realsolver = TSP_BBTSP;
            switch ( realsolver )
            {
                case TSP_BBTSP:
                    convert2_to_tsp ( node1, node2, node3, adj_list,
                                      adj_pool, num_genes, CIRCULAR );
                    /* needs a small fix here: bbtsp will try to use existing      
                       genome at tree->genome as a tour, which will crash, since
                       there is not one yet...  so pass FALSE to use_median */
                    bbtsp ( 2 * num_genes, nodem->genes, FALSE,
                            node1->genes, node2->genes, node3->genes,
                            adj_list, neighbors, stack, outcycle, degree,
                            otherEnd, edges, CIRCULAR );
                    break;
                case TSP_COALESCED:
                    convert2_to_tsp ( node1, node2, node3, adj_list,
                                      adj_pool, num_genes, CIRCULAR );
                    coalestsp ( 2 * num_genes, nodem->genes, FALSE,
                                node1->genes, node2->genes, node3->genes,
                                adj_list, neighbors, stack, outcycle, degree,
                                otherEnd, edges, CIRCULAR );

                    break;
                case INVERSION_MEDIAN:
                    gen[0] = node1;
                    gen[1] = node2;
                    gen[2] = node3;
                    find_reversal_median ( nodem, gen, num_genes, NULL );
                    break;

                case INVERSION_MEDIAN_FAST:
                    gen[0] = node1;
                    gen[1] = node2;
                    gen[2] = node3;
                    if ( CIRCULAR )
                        albert_inversion_median_circular ( gen, num_genes,
                                                           nodem->genes );
                    else
                        albert_inversion_median_noncircular ( gen, num_genes,
                                                              nodem->genes );
                    break;

#ifdef CONCORDE
                case TSP_CHLINKERN:
                    /* creates a weight matrix -- more expensive! */
                    convert_to_tsp ( node1, node2, node3,
                                     num_genes, CIRCULAR, weights );
                    chlinkern ( 2 * num_genes, weights, nodem->genes,
                                incycle, outcycle );
                    break;
                case TSP_GREEDYLK:
                    /* creates a weight matrix -- more expensive! */
                    convert_to_tsp ( node1, node2, node3,
                                     num_genes, CIRCULAR, weights );
                    greedylk ( 2 * num_genes, weights, nodem->genes,
                               incycle, outcycle );
                    break;
#endif
            }
            /* bbtsp automatically assigned median to current node */
        }                       /* end condensation */
    }                           /* internal node processed */

    /* go process remaining nodes, if any */
    if ( tree->lChild )
        realinit_tree_SNN ( COND, tree->lChild, tpool, labels, queue,
                            adj_list, adj_pool, weights, stack, degree,
                            otherEnd, neighbors, edges, incycle, outcycle,
                            pred, succ, code, decode, condnode1, condnode2,
                            condnode3, condmedian, num_genes, num_genomes,
                            inittspsolver, thresh, CIRCULAR );
    if ( tree->rChild )
        realinit_tree_SNN ( COND, tree->rChild, tpool, labels, queue,
                            adj_list, adj_pool, weights, stack, degree,
                            otherEnd, neighbors, edges, incycle, outcycle,
                            pred, succ, code, decode, condnode1, condnode2,
                            condnode3, condmedian, num_genes, num_genomes,
                            inittspsolver, thresh, CIRCULAR );
    return;
}

/* NN version for small numbers of genomes -- quadratic work */
void
initialize_tree_SNN ( int COND, struct tNode *tree, struct tNode *tpool,
                      struct genome_struct *labels, struct qNode *queue,
                      struct adj_struct *adj_list,
                      struct adj_struct *adj_pool, int **weights, int *stack,
                      int *degree, int *otherEnd, intpair_t * neighbors,
                      edge_t * edges, int *incycle, int *outcycle, int *pred,
                      int *succ, int *code, int *decode, int num_genes,
                      int num_genomes, int inittspsolver, int thresh,
                      int CIRCULAR )
{
    /* for each internal node, locates the three nearest leaves and uses
       their exact median as initial label; slower than trivial, random,
       and propagate, due to the median calls, but reduces the number of
       relabellings and tends to produce better solutions */
    /* this is just a shell that sets up the extra nodes for condensing
       and call the recrusive "rea" version */
    struct tNode *condnode1, *condnode2, *condnode3, *condmedian;
    int ncount;

    if ( tree == NULL )
        return;

    ncount = 2 * num_genomes;
    condnode1 = &tpool[ncount + 2];
    condnode1->genome = &labels[ncount + 2];
    condnode2 = &tpool[ncount + 3];
    condnode2->genome = &labels[ncount + 3];
    condnode3 = &tpool[ncount + 4];
    condnode3->genome = &labels[ncount + 4];
    condmedian = &tpool[ncount + 5];
    condmedian->genome = &labels[ncount + 5];

    realinit_tree_SNN ( COND, tree, tpool, labels, queue, adj_list, adj_pool,
                        weights, stack, degree, otherEnd, neighbors, edges,
                        incycle, outcycle, pred + num_genes, succ + num_genes,
                        code + num_genes, decode, condnode1, condnode2,
                        condnode3, condmedian, num_genes, num_genomes,
                        inittspsolver, thresh, CIRCULAR );
    return;
}

/* used by initialize_tree_SBNN */
struct tNode *
find_closest_label ( struct tNode *node, struct qNode *queue )
{
    /* by "closest" we mean "the fewest edges away", since we
       have no values for edge lengths; thus we need a simple bfs */
    /* use node->leaf as a visited flag -- needed, since we have
       an unrooted tree */
    struct qNode *head, *tail;  /* queue pointers */
    struct tNode *next;

    head = tail = queue;        /* initialize queue pointers */
    node->leaf = TRUE;
    while ( 1 )
    {                           /* loop until a leaf is found -- one must exist */
        if ( node->genome != NULL )
        {                       /* node already labeled, found */
            return node;
        }
        else
        {                       /* can move in 2 of 3 directions, but which? */
            next = node->parent;
            if ( next->leaf == FALSE )
            {
                next->leaf = TRUE;
                add_queue ( queue, next, &head, &tail );
            }
            next = node->lChild;
            if ( next->leaf == FALSE )
            {
                next->leaf = TRUE;
                add_queue ( queue, next, &head, &tail );
            }
            next = node->rChild;
            if ( next->leaf == FALSE )
            {
                next->leaf = TRUE;
                add_queue ( queue, next, &head, &tail );
            }
            node = dequeue ( queue, &head, &tail );
            /* cannot fail -- there is always a leaf */
        }
    }
}

/* used by SBNN version for small numbers of genomes -- quadratic work */
void
realinit_tree_SBNN ( int COND, struct tNode *tree, struct tNode *tpool,
                     struct genome_struct *labels, struct qNode *queue,
                     struct adj_struct *adj_list, struct adj_struct *adj_pool,
                     int **weights, int *stack, int *degree, int *otherEnd,
                     intpair_t * neighbors, edge_t * edges,
                     int *incycle, int *outcycle,
                     int *pred, int *succ, int *code, int *decode,
                     struct tNode *condnode1, struct tNode *condnode2,
                     struct tNode *condnode3, struct tNode *condmedian,
                     int num_genes, int num_genomes, int inittspsolver,
                     int thresh, int CIRCULAR )
{
    /* for each internal node, locates the three nearest already labelled nodes
       (initially, leaves) and uses their exact median as initial label;
       slower than trivial, random, and propagate, due to the median calls,
       but reduces the number of relabellings and tends to produce better solutions */
    struct genome_struct *node1, *node2, *node3, *nodem;
    struct genome_struct *gen[3];
    int i, id;
    int realsolver, num_cond;

    /* for each internal tree node, find the closest leaves (one down each
       edge from the node) and use their median as initial label */
    if ( tree == NULL )
        return;

#define SBNNpreorder
    /* must define exactly one of SBNNpreorder or SBNNpostorder;
       experiments tend to indicate that preorder may be better */
#ifdef SBNNpostorder
    /* go process remaining nodes, if any */
    if ( tree->lChild )
        realinit_tree_SBNN ( COND, tree->lChild, tpool, labels, queue,
                             adj_list, adj_pool, weights, stack, degree,
                             otherEnd, neighbors, edges, incycle, outcycle,
                             pred, succ, code, decode, condnode1, condnode2,
                             condnode3, condmedian, num_genes, num_genomes,
                             inittspsolver, thresh, CIRCULAR );
    if ( tree->rChild )
        realinit_tree_SBNN ( COND, tree->rChild, tpool, labels, queue,
                             adj_list, adj_pool, weights, stack, degree,
                             otherEnd, neighbors, edges, incycle, outcycle,
                             pred, succ, code, decode, condnode1, condnode2,
                             condnode3, condmedian, num_genes, num_genomes,
                             inittspsolver, thresh, CIRCULAR );
#endif
    if ( tree->tag < 0 )
    {                           /* internal node needs to be processed */

        /* no storage assigned yet; get a node from the label array */
        id = tree->tag;
        i = num_genomes - id;
        nodem = tree->genome = &labels[i];
        nodem->genome_num = id;

        /* set all flags to false */
        for ( i = 0; i <= 2 * num_genomes; i++ )
        {
            tpool[i].leaf = FALSE;
        }
        tree->leaf = TRUE;      /* visited flag for find_closest_leaf */
        node1 = find_closest_label ( tree->parent, queue )->genome; /* label 1 */
        node2 = find_closest_label ( tree->lChild, queue )->genome; /* label 2 */
        node3 = find_closest_label ( tree->rChild, queue )->genome; /* label 3 */
        /* restore proper flag values */
#if 0
        for ( i = 0; i <= 2 * num_genomes; i++ )
#else
        for ( i = 0; i < 2 * num_genomes - 2; i++ )
#endif
        {
            if ( tpool[i].tag < 0 )
            {
                tpool[i].leaf = FALSE;
            }
            else
            {
                tpool[i].leaf = TRUE;
            }
        }

        if ( COND )
        {                       /* condensation */
            condense3 ( node1->genes, node2->genes, node3->genes,
                        condnode1->genome->genes, condnode2->genome->genes,
                        condnode3->genome->genes,
                        num_genes, &num_cond, succ, pred, code, decode );

            if ( num_cond == 0 )
            {                   /* three identical neighbors... */
                /* note: only works for circular genome; for linear, value is 1 */
                for ( i = 0; i < num_genes; i++ )
                {               /* so just copy one in place */
                    nodem->genes[i] = node1->genes[i];
                }
            }
            else
            {                   /* get the median the hard way ;-) */
                realsolver = inittspsolver;
                if ( ( num_cond <= thresh )
                     && ( inittspsolver != TSP_COALESCED ) )
                    realsolver = TSP_BBTSP;
                switch ( realsolver )
                {
                    case INVERSION_MEDIAN:
                        gen[0] = condnode1->genome;
                        gen[1] = condnode2->genome;
                        gen[2] = condnode3->genome;
                        find_reversal_median ( condmedian->genome, gen,
                                               num_cond, NULL );
                        break;
                    case INVERSION_MEDIAN_FAST:
                        gen[0] = condnode1->genome;
                        gen[1] = condnode2->genome;
                        gen[2] = condnode3->genome;
                        if ( CIRCULAR )
                            albert_inversion_median_circular ( gen, num_cond,
                                                               condmedian->
                                                               genome->
                                                               genes );
                        else
                            albert_inversion_median_noncircular ( gen,
                                                                  num_cond,
                                                                  condmedian->
                                                                  genome->
                                                                  genes );
                        break;

                    case TSP_BBTSP:
                        convert2_to_tsp ( condnode1->genome, condnode2->genome,
                                      condnode3->genome, adj_list, adj_pool, 
                                      num_cond, CIRCULAR);
                        bbtsp ( 2 * num_cond, condmedian->genome->genes,
                                FALSE, condnode1->genome->genes,
                                condnode2->genome->genes,
                                condnode3->genome->genes, adj_list, neighbors,
                                stack, outcycle, degree, otherEnd, edges,
                                CIRCULAR );
                        break;
                    case TSP_COALESCED:
                        convert2_to_tsp ( condnode1->genome, condnode2->genome,
                                      condnode3->genome, adj_list, adj_pool, 
                                      num_cond, CIRCULAR);

                        coalestsp ( 2 * num_cond, condmedian->genome->genes,
                                    FALSE, condnode1->genome->genes,
                                    condnode2->genome->genes,
                                    condnode3->genome->genes, adj_list,
                                    neighbors, stack, outcycle, degree,
                                    otherEnd, edges, CIRCULAR );
                        break;
#ifdef CONCORDE
                    case TSP_GREEDYLK:
                        /* creates a weight matrix -- more expensive! */
                        convert_to_tsp ( condnode1->genome, condnode2->genome,
                                         condnode3->genome, num_cond,
                                         CIRCULAR, weights );
                        greedylk ( 2 * num_cond, weights,
                                   condmedian->genome->genes, incycle,
                                   outcycle );
                        break;
                    case TSP_CHLINKERN:
                        /* creates a weight matrix -- more expensive! */
                        convert_to_tsp ( condnode1->genome, condnode2->genome,
                                         condnode3->genome, num_cond,
                                         CIRCULAR, weights );
                        chlinkern ( 2 * num_cond, weights,
                                    condmedian->genome->genes, incycle,
                                    outcycle );
                        break;
#endif
                }
                /* bbtsp automatically assigned median to current node,
                   but it has to be decoded */
                decode3 ( nodem->genes, condmedian->genome->genes, succ,
                          decode, num_cond );
            }
        }                       /* end condensation */
        else
        {                       /* no condensation */
            realsolver = inittspsolver;
            if ( ( num_genes <= thresh )
                 && ( inittspsolver != TSP_COALESCED ) )
                realsolver = TSP_BBTSP;
            switch ( realsolver )
            {
                case TSP_BBTSP:
                    convert2_to_tsp ( node1, node2, node3,
                                      adj_list, adj_pool, num_genes,
                                      CIRCULAR );
                    bbtsp ( 2 * num_genes, nodem->genes, FALSE, node1->genes,
                            node2->genes, node3->genes, adj_list, neighbors,
                            stack, outcycle, degree, otherEnd, edges,
                            CIRCULAR );
                    break;
                case TSP_COALESCED:
                    convert2_to_tsp ( node1, node2, node3, adj_list,
                                      adj_pool, num_genes, CIRCULAR );
                    coalestsp ( 2 * num_genes, nodem->genes, FALSE,
                                node1->genes, node2->genes, node3->genes,
                                adj_list, neighbors, stack, outcycle, degree,
                                otherEnd, edges, CIRCULAR );
                    break;
                case INVERSION_MEDIAN:
                    gen[0] = node1;
                    gen[1] = node2;
                    gen[2] = node3;
                    find_reversal_median ( nodem, gen, num_genes, NULL );
                case INVERSION_MEDIAN_FAST:
                    gen[0] = node1;
                    gen[1] = node2;
                    gen[2] = node3;
                    if ( CIRCULAR )
                        albert_inversion_median_circular ( gen, num_genes,
                                                           nodem->genes );
                    else
                        albert_inversion_median_noncircular ( gen, num_genes,
                                                              nodem->genes );
                    break;

#ifdef CONCORDE
                case TSP_CHLINKERN:
                    /* creates a weight matrix -- more expensive! */
                    convert_to_tsp ( node1, node2, node3,
                                     num_genes, CIRCULAR, weights );
                    chlinkern ( 2 * num_genes, weights, nodem->genes,
                                incycle, outcycle );
                    break;
                case TSP_GREEDYLK:
                    /* creates a weight matrix -- more expensive! */
                    convert_to_tsp ( node1, node2, node3,
                                     num_genes, CIRCULAR, weights );
                    greedylk ( 2 * num_genes, weights, nodem->genes,
                               incycle, outcycle );
                    break;
#endif
            }
            /* bbtsp automatically assigned median to current node */
        }                       /* end condensation */
    }                           /* internal node processed */

#ifdef SBNNpreorder
    /* go process remaining nodes, if any */
    if ( tree->lChild )
        realinit_tree_SBNN ( COND, tree->lChild, tpool, labels, queue,
                             adj_list, adj_pool, weights, stack, degree,
                             otherEnd, neighbors, edges, incycle, outcycle,
                             pred, succ, code, decode, condnode1, condnode2,
                             condnode3, condmedian, num_genes, num_genomes,
                             inittspsolver, thresh, CIRCULAR );
    if ( tree->rChild )
        realinit_tree_SBNN ( COND, tree->rChild, tpool, labels, queue,
                             adj_list, adj_pool, weights, stack, degree,
                             otherEnd, neighbors, edges, incycle, outcycle,
                             pred, succ, code, decode, condnode1, condnode2,
                             condnode3, condmedian, num_genes, num_genomes,
                             inittspsolver, thresh, CIRCULAR );
#endif
    return;
}

/* NN version for small numbers of genomes -- quadratic work */
void
initialize_tree_SBNN ( int COND, struct tNode *tree, struct tNode *tpool,
                       struct genome_struct *labels, struct qNode *queue,
                       struct adj_struct *adj_list,
                       struct adj_struct *adj_pool, int **weights, int *stack,
                       int *degree, int *otherEnd, intpair_t * neighbors,
                       edge_t * edges, int *incycle, int *outcycle, int *pred,
                       int *succ, int *code, int *decode, int num_genes,
                       int num_genomes, int inittspsolver, int thresh,
                       int CIRCULAR )
{
    /* for each internal node, locates the three nearest leaves and uses
       their exact median as initial label; slower than trivial, random,
       and propagate, due to the median calls, but reduces the number of
       relabellings and tends to produce better solutions */
    /* this is just a shell that sets up the extra nodes for condensing
       and call the recrusive "rea" version */
    struct tNode *condnode1, *condnode2, *condnode3, *condmedian;
    int ncount;

    if ( tree == NULL )
        return;

    ncount = 2 * num_genomes;
    condnode1 = &tpool[ncount + 2];
    condnode1->genome = &labels[ncount + 2];
    condnode2 = &tpool[ncount + 3];
    condnode2->genome = &labels[ncount + 3];
    condnode3 = &tpool[ncount + 4];
    condnode3->genome = &labels[ncount + 4];
    condmedian = &tpool[ncount + 5];
    condmedian->genome = &labels[ncount + 5];

    realinit_tree_SBNN ( COND, tree, tpool, labels, queue, adj_list, adj_pool,
                         weights, stack, degree, otherEnd, neighbors, edges,
                         incycle, outcycle, pred + num_genes,
                         succ + num_genes, code + num_genes, decode,
                         condnode1, condnode2, condnode3, condmedian,
                         num_genes, num_genomes, inittspsolver, thresh,
                         CIRCULAR );
    return;
}

void
print_tree ( struct tNode *tree, int num_genes )
{
    int i;

    if ( tree == NULL )
        return;
    /*fprintf(outfile,"Genome %4d:",tree->tag); */
    fprintf ( outfile, "%3d = ", tree->tag );
    for ( i = 0; i < num_genes; i++ )
    {
        /*fprintf(outfile," %4d",tree->genome->genes[i]); */
        fprintf ( outfile, "%4d ", tree->genome->genes[i] );
    }
    fprintf ( outfile, "\n" );
    print_tree ( tree->lChild, num_genes );
    print_tree ( tree->rChild, num_genes );
    return;
}
