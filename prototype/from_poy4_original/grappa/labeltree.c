#include "structs.h"
#include "labeltree.h"
#include "convert.h"
#include "lk_main.h"
#include "bbtsp.h"
#include "binencode.h"
#include "invdist.h"
#include "condense3.h"
#include "cheaptsp.h"
#include "inversion_median.h"
#include "inversion_median_alberto.h"
#include "correction.h"
#include "inversion_median.h"
#include "sorting_reversal_median.h"
extern int VIncrease;
void
printedgescores ( struct tNode *tree )
{
    if ( tree == NULL )
        return;
    if ( tree->lChild )
    {
        fprintf ( outfile, "tree_score: edge %p is worth %4d\n",
                  tree->sc_lChild, *( tree->sc_lChild ) );
        fflush ( outfile );
        printedgescores ( tree->lChild );
    }
    if ( tree->rChild )
    {
        fprintf ( outfile, "tree_score: edge %p is worth %4d\n",
                  tree->sc_rChild, *( tree->sc_rChild ) );
        fflush ( outfile );
        printedgescores ( tree->rChild );
    }
    return;
}

int
iterate_over_tree ( int COND, struct tNode *tree, int num_genes,
                    int num_genomes, int tspsolver, int thresh, int score,
                    struct adj_struct *adj_list, struct adj_struct *adj_pool,
                    intpair_t * neighbors, int *stack, int *incycle,
                    int *outcycle, int **weights, int *degree, int *otherEnd,
                    edge_t * edges, struct tNode *tpool,
                    struct genome_struct *labels, int *pred1, int *pred2,
                    int *picked, int *decode, int *genes, int CIRCULAR,
                    int distmethod, distmem_t * distmem, int correction )
{
    int best_so_far = score;
    struct tNode *condnode1 = NULL, *condnode2 = NULL, *condnode3 =
        NULL, *condmedian = NULL;
    int IMPROVED = TRUE;
    int ncount;
    int relabel = 0;

    VIncrease = 100;
#ifdef DEBUG
    fprintf ( outfile, "before assigning condnodes\n" );
    printedgescores ( tree );
#endif
    if ( COND )
    {
        ncount = 2 * num_genomes;
        condnode1 = &tpool[ncount + 2];
        condnode1->genome = &labels[ncount + 2];
        condnode2 = &tpool[ncount + 3];
        condnode2->genome = &labels[ncount + 3];
        condnode3 = &tpool[ncount + 4];
        condnode3->genome = &labels[ncount + 4];
        condmedian = &tpool[ncount + 5];
        condmedian->genome = &labels[ncount + 5];
    }
#ifdef DEBUG
    fprintf ( outfile, "after assigning condnodes\n" );
    printedgescores ( tree );
#endif

    while ( IMPROVED )
    {
        IMPROVED = FALSE;
        init_change_bits ( tree );
        dfs_label_tree ( COND, tree, tree, &best_so_far, &IMPROVED,
                         num_genes, num_genomes, weights, &relabel, tspsolver,
                         thresh, adj_list, adj_pool, neighbors, stack,
                         incycle, outcycle, degree, otherEnd, edges, tpool,
                         labels, pred1 + num_genes, pred2 + num_genes,
                         picked + num_genes, decode,
                         /* the num_genes offsets to allow negative indexing */
                         condmedian, condnode1, condnode2, condnode3,
                         genes, CIRCULAR, distmethod, distmem, correction );
        leaf_tree ( tree );     /* reset visited flag */
    }
#ifdef PRINT
    printf ( "relabeled %d nodes, tree score: %d\n", relabel, best_so_far );
#endif
    return ( best_so_far );
}

/* must define exactly one of PREORDER or POSTORDER --- NOT BOTH!!! */
#define POSTORDER
void
dfs_label_tree ( int COND, struct tNode *root, struct tNode *tree_node,
                 int *best_so_far, int *IMPROVED,
                 int num_genes, int num_genomes,
                 int **weights, int *relabel,
                 int tspsolver, int thresh,
                 struct adj_struct *adj_list, struct adj_struct *adj_pool,
                 intpair_t * neighbors, int *stack, int *incycle,
                 int *outcycle, int *degree, int *otherEnd, edge_t * edges,
                 struct tNode *tpool, struct genome_struct *labels, int *pred,
                 int *succ, int *code, int *decode,
                 /* pred, succ, and code have middle ptrs! */
                 struct tNode *condmedian, struct tNode *condnode1,
                 struct tNode *condnode2, struct tNode *condnode3,
                 int *genes, int CIRCULAR,
                 int distmethod, distmem_t * distmem, int correction )
{
    int score = 0, delta;
    int i, num_cond;
    int realsolver, same, use_median = TRUE;
    struct genome_struct *pargenome, *lchgenome, *rchgenome;
    struct genome_struct *gen[3];
    int tmp_sc_parent = 0, tmp_sc_lChild = 0, tmp_sc_rChild = 0;
    if ( tree_node == NULL )
        return;

#ifdef POSTORDER
    if ( !tree_node->lChild->leaf )
    {
        dfs_label_tree ( COND, root, tree_node->lChild, best_so_far, IMPROVED,
                         num_genes, num_genomes, weights, relabel, tspsolver,
                         thresh, adj_list, adj_pool, neighbors, stack,
                         incycle, outcycle, degree, otherEnd, edges, tpool,
                         labels, pred, succ, code, decode, condmedian,
                         condnode1, condnode2, condnode3, genes, CIRCULAR,
                         distmethod, distmem, correction );
    }
    if ( tree_node->rChild )
    {
        if ( !tree_node->rChild->leaf )
        {
            dfs_label_tree ( COND, root, tree_node->rChild, best_so_far,
                             IMPROVED, num_genes, num_genomes, weights,
                             relabel, tspsolver, thresh, adj_list, adj_pool,
                             neighbors, stack, incycle, outcycle, degree,
                             otherEnd, edges, tpool, labels, pred, succ, code,
                             decode, condmedian, condnode1, condnode2,
                             condnode3, genes, CIRCULAR, distmethod, distmem,
                             correction );
        }
    }
#endif

    if ( tree_node->need_change > 0 )
    {                           /* internal node needs to be relabeled */

        if ( COND )
        {                       /* condensation */
            for ( i = 0; i < num_genes; i++ )
            {                   /* save old label */
                genes[i] = tree_node->genome->genes[i];
            }

            pargenome = tree_node->parent->genome;
            lchgenome = tree_node->lChild->genome;
            rchgenome = tree_node->rChild->genome;
#ifdef DEBUG
            fprintf ( outfile, "ptrs to old edge scores are %p, %p, and %p\n",
                      tree_node->sc_parent,
                      tree_node->sc_lChild, tree_node->sc_rChild );
            fprintf ( outfile, "old edge scores are %3d, %3d, and %3d\n",
                      *( tree_node->sc_parent ),
                      *( tree_node->sc_lChild ), *( tree_node->sc_rChild ) );
            fflush ( outfile );
#endif

            condense3 ( pargenome->genes, lchgenome->genes, rchgenome->genes,
                        condnode1->genome->genes, condnode2->genome->genes,
                        condnode3->genome->genes,
                        num_genes, &num_cond, succ, pred, code, decode );

            if ( num_cond == 0 )
            {                   /* three identical neighbors... */
                /* note: only works for circular genome; for linear, value is 1 */
                for ( i = 0; i < num_genes; i++ )
                {               /* so just copy on in place */
                    tree_node->genome->genes[i] =
                        tree_node->parent->genome->genes[i];
                }
                tree_node->need_change = 0;
                goto done;      /* and exit */
            }
            /* call solvers with condensed genomes */
            /* the LK solvers need a weight matrix, but the bbtsp solver does not */
            /* force use of  bbtsp if problem is too small */
            realsolver = tspsolver;
            if ( ( num_cond <= thresh ) && ( tspsolver != TSP_COALESCED ) )
                realsolver = TSP_BBTSP;

            switch ( realsolver )
            {
                case INVERSION_MEDIAN:
                    gen[0] = condnode1->genome;
                    gen[1] = condnode2->genome;
                    gen[2] = condnode3->genome;

                    find_reversal_median ( condmedian->genome, gen, num_cond,
                                           NULL );
                    /*find_inversion_median(condmedian->genome, gen, num_cond, distmem); */
                    if ( CIRCULAR )
                    {
                        score += invdist_circular ( gen[0],
                                                    condmedian->genome,
                                                    num_cond, distmem );
                        score +=
                            invdist_circular ( gen[1], condmedian->genome,
                                               num_cond, distmem );
                        score +=
                            invdist_circular ( gen[2], condmedian->genome,
                                               num_cond, distmem );
                    }
                    else
                    {
                        score += invdist_noncircular ( gen[0],
                                                       condmedian->genome, 0,
                                                       num_cond, distmem );
                        score +=
                            invdist_noncircular ( gen[1], condmedian->genome,
                                                  0, num_cond, distmem );
                        score +=
                            invdist_noncircular ( gen[2], condmedian->genome,
                                                  0, num_cond, distmem );

                    }
                    break;
                case INVERSION_MEDIAN_FAST:
                    gen[0] = condnode1->genome;
                    gen[1] = condnode2->genome;
                    gen[2] = condnode3->genome;
                    score = 0;
                    if ( CIRCULAR )
                    {
                        albert_inversion_median_circular ( gen, num_cond,
                                                           condmedian->
                                                           genome->genes );
                        score +=
                            invdist_circular ( gen[0], condmedian->genome,
                                               num_cond, distmem );
                        score +=
                            invdist_circular ( gen[1], condmedian->genome,
                                               num_cond, distmem );
                        score +=
                            invdist_circular ( gen[2], condmedian->genome,
                                               num_cond, distmem );
                    }
                    else
                    {
                        albert_inversion_median_noncircular ( gen, num_cond,
                                                              condmedian->
                                                              genome->genes );
                        score +=
                            invdist_noncircular ( gen[0], condmedian->genome,
                                                  0, num_cond, distmem );
                        score +=
                            invdist_noncircular ( gen[1], condmedian->genome,
                                                  0, num_cond, distmem );
                        score +=
                            invdist_noncircular ( gen[2], condmedian->genome,
                                                  0, num_cond, distmem );
                    }
                    break;
                case TSP_BBTSP:
                    /* does not create a weight matrix, but sets weight in the adj lists */
                    convert2_to_tsp ( condnode1->genome, condnode2->genome,
                                      condnode3->genome, adj_list, adj_pool,
                                      num_cond, CIRCULAR );

                    score = bbtsp ( 2 * num_cond, condmedian->genome->genes, FALSE, /* cannot use median that does not exist */
                                    condnode1->genome->genes,
                                    condnode2->genome->genes,
                                    condnode3->genome->genes, adj_list,
                                    neighbors, stack, outcycle, degree,
                                    otherEnd, edges, CIRCULAR );
                    break;
#ifdef CONCORDE
                case TSP_CHLINKERN:
                    /* creates a weight matrix -- more expensive! */
                    convert_to_tsp ( condnode1->genome, condnode2->genome,
                                     condnode3->genome, num_cond, CIRCULAR,
                                     weights );
                    score =
                        chlinkern ( 2 * num_cond, weights,
                                    condmedian->genome->genes, incycle,
                                    outcycle );
                    break;
                case TSP_GREEDYLK:
                    /* creates a weight matrix -- more expensive! */
                    convert_to_tsp ( condnode1->genome, condnode2->genome,
                                     condnode3->genome, num_cond, CIRCULAR,
                                     weights );
                    score =
                        greedylk ( 2 * num_cond, weights,
                                   condmedian->genome->genes, incycle,
                                   outcycle );
                    break;
#endif
                case TSP_COALESCED:
                    /* does not create a weight matrix, but sets weight in the adj lists */
                    convert2_to_tsp ( condnode1->genome, condnode2->genome,
                                      condnode3->genome, adj_list, adj_pool,
                                      num_cond, CIRCULAR );

                    score = coalestsp ( 2 * num_cond, condmedian->genome->genes, FALSE, /* cannot use median that does not exist */
                                        condnode1->genome->genes,
                                        condnode2->genome->genes,
                                        condnode3->genome->genes, adj_list,
                                        neighbors, stack, outcycle, degree,
                                        otherEnd, edges, CIRCULAR );
                    break;
            }
            score += ( LARGENUM * num_genes );  /* I changed to num_genes from num_cond */
#ifdef DEBUG
            fprintf ( outfile,
                      "for tree node #%3d, new median with score=%5d\n",
                      tree_node->tag, score );
            for ( i = 0; i < num_genes; i++ )
            {
                fprintf ( outfile, "%3d, ", condmedian->genome->genes[i] );
            }
            fprintf ( outfile, "\n" );
            fflush ( outfile );
#endif
            tree_node->need_change = 0;

            /* now use TSP score to compare to Hamming distance along the three
               edges; for INV distance, compute it -- cannot mix the two!
               if no improvement, restore the old label and exit */
            switch ( distmethod )
            {
                case DIST_INV:
                    tmp_sc_parent =
                        bp_score ( condmedian, condnode1, num_cond, CIRCULAR,
                                   distmethod, distmem, correction,
                                   num_genes );
                    tmp_sc_lChild =
                        bp_score ( condmedian, condnode2, num_cond, CIRCULAR,
                                   distmethod, distmem, correction,
                                   num_genes );
                    tmp_sc_rChild =
                        bp_score ( condmedian, condnode3, num_cond, CIRCULAR,
                                   distmethod, distmem, correction,
                                   num_genes );
#ifdef DEBUG
                    fprintf ( outfile,
                              "new edge scores are %3d, %3d, and %3d\n",
                              tmp_sc_parent, tmp_sc_lChild, tmp_sc_rChild );
                    fprintf ( outfile,
                              "ptrs to old edge scores are %p, %p, and %p\n",
                              tree_node->sc_parent, tree_node->sc_lChild,
                              tree_node->sc_rChild );
                    fprintf ( outfile,
                              "old edge scores are %3d, %3d, and %3d\n",
                              *( tree_node->sc_parent ),
                              *( tree_node->sc_lChild ),
                              *( tree_node->sc_rChild ) );
                    fflush ( outfile );
#endif
                    delta =
                        ( tmp_sc_parent + tmp_sc_lChild + tmp_sc_rChild ) -
                        ( *( tree_node->sc_parent ) +
                          *( tree_node->sc_lChild ) +
                          *( tree_node->sc_rChild ) );
                    break;
                case DIST_BP:
                default:
                    delta = score - ( *( tree_node->sc_parent ) +
                                      *( tree_node->sc_lChild ) +
                                      *( tree_node->sc_rChild ) );
                    if ( delta < 0 )
                    {           /* get to same state as case DIST_INV */
                        tmp_sc_parent =
                            bp_score ( condmedian, condnode1, num_cond,
                                       CIRCULAR, distmethod, distmem,
                                       correction, num_genes );
                        tmp_sc_lChild =
                            bp_score ( condmedian, condnode2, num_cond,
                                       CIRCULAR, distmethod, distmem,
                                       correction, num_genes );
                        tmp_sc_rChild =
                            bp_score ( condmedian, condnode3, num_cond,
                                       CIRCULAR, distmethod, distmem,
                                       correction, num_genes );
#ifdef DEBUG
                        fprintf ( outfile,
                                  "new edge scores are %3d, %3d, and %3d\n",
                                  tmp_sc_parent, tmp_sc_lChild,
                                  tmp_sc_rChild );
                        fflush ( outfile );
#endif
                    }
                    break;
            }

#ifdef DEBUG
            fprintf ( outfile, "score=%3d, delta=%3d\n", score, delta );
#endif
            if ( delta < 0 )
            {
                /* accept the change */
                if ( tree_node->lChild->tag < 0 )
                    tree_node->lChild->need_change = 1;
                if ( tree_node->rChild->tag < 0 )
                    tree_node->rChild->need_change = 1;
                if ( tree_node->parent->tag < 0 )
                    tree_node->parent->need_change = 1;
                /* update the distances along the three edges (both ways) */
                *( tree_node->sc_parent ) = tmp_sc_parent;
                *( tree_node->sc_lChild ) = tmp_sc_lChild;
                *( tree_node->sc_rChild ) = tmp_sc_rChild;
                /* decode the median and assign it to the node */
                decode3 ( tree_node->genome->genes, condmedian->genome->genes,
                          succ, decode, num_cond );
                /* get ready for next loop */
                *IMPROVED = TRUE;
#ifdef PRINT
                *relabel += 1;
#endif
                *best_so_far += delta;
            }
            else
            {                   /* restore old label */
                for ( i = 0; i < num_genes; i++ )
                {
                    tree_node->genome->genes[i] = genes[i];
                }
            }

        }                       /* end condensation */

        else
        {                       /* no condensation */
            for ( i = 0; i < num_genes; i++ )
            {                   /* save old label */
                genes[i] = tree_node->genome->genes[i];
            }
            /* which solver to call? */
            /* force use of  bbtsp if problem is too small */
            realsolver = tspsolver;
            if ( ( num_genes <= thresh ) && ( tspsolver != TSP_COALESCED ) )
                realsolver = TSP_BBTSP;

            switch ( realsolver )
            {
                case INVERSION_MEDIAN:
                    gen[0] = tree_node->parent->genome;
                    gen[1] = tree_node->lChild->genome;
                    gen[2] = tree_node->rChild->genome;

                    find_reversal_median ( tree_node->genome, gen, num_genes,
                                           NULL );
/*	  find_inversion_median(tree_node->genome, gen, num_genes, distmem);*/
                    score = 0;
                    if ( CIRCULAR )
                    {
                        score += invdist_circular ( gen[0],
                                                    tree_node->genome,
                                                    num_genes, distmem );
                        score +=
                            invdist_circular ( gen[1], tree_node->genome,
                                               num_genes, distmem );
                        score +=
                            invdist_circular ( gen[2], tree_node->genome,
                                               num_genes, distmem );
                    }
                    else
                    {
                        score += invdist_noncircular ( gen[0],
                                                       tree_node->genome, 0,
                                                       num_genes, distmem );
                        score +=
                            invdist_noncircular ( gen[1], tree_node->genome,
                                                  0, num_genes, distmem );
                        score +=
                            invdist_noncircular ( gen[2], tree_node->genome,
                                                  0, num_genes, distmem );
                    }
                    break;
                case INVERSION_MEDIAN_FAST:
                    gen[0] = tree_node->parent->genome;
                    gen[1] = tree_node->lChild->genome;
                    gen[2] = tree_node->rChild->genome;

                    score = 0;
                    if ( CIRCULAR )
                    {
                        albert_inversion_median_circular ( gen,
                                                           num_genes,
                                                           tree_node->genome->
                                                           genes );
                        score +=
                            invdist_circular ( gen[0], tree_node->genome,
                                               num_genes, distmem );
                        score +=
                            invdist_circular ( gen[1], tree_node->genome,
                                               num_genes, distmem );
                        score +=
                            invdist_circular ( gen[2], tree_node->genome,
                                               num_genes, distmem );
                    }
                    else
                    {
                        albert_inversion_median_noncircular ( gen,
                                                              num_genes,
                                                              tree_node->
                                                              genome->genes );
                        score +=
                            invdist_noncircular ( gen[0], tree_node->genome,
                                                  0, num_genes, distmem );
                        score +=
                            invdist_noncircular ( gen[1], tree_node->genome,
                                                  0, num_genes, distmem );
                        score +=
                            invdist_noncircular ( gen[2], tree_node->genome,
                                                  0, num_genes, distmem );
                    }
                    break;
                case TSP_BBTSP:
                    /* does not create a weight matrix, but sets weight in the adj lists */
                    convert2_to_tsp ( tree_node->parent->genome,
                                      tree_node->lChild->genome,
                                      tree_node->rChild->genome,
                                      adj_list, adj_pool, num_genes,
                                      CIRCULAR );

                    score =
                        bbtsp ( 2 * num_genes, tree_node->genome->genes,
                                use_median, tree_node->parent->genome->genes,
                                tree_node->lChild->genome->genes,
                                tree_node->rChild->genome->genes, adj_list,
                                neighbors, stack, outcycle, degree, otherEnd,
                                edges, CIRCULAR );
                    break;
#ifdef CONCORDE
                case TSP_CHLINKERN:
                    /* creates a weight matrix -- more expensive! */
                    convert_to_tsp ( tree_node->parent->genome,
                                     tree_node->lChild->genome,
                                     tree_node->rChild->genome,
                                     num_genes, CIRCULAR, weights );
                    score =
                        chlinkern ( 2 * num_genes, weights,
                                    tree_node->genome->genes, incycle,
                                    outcycle );
                    break;
                case TSP_GREEDYLK:
                    /* creates a weight matrix -- more expensive! */
                    convert_to_tsp ( tree_node->parent->genome,
                                     tree_node->lChild->genome,
                                     tree_node->rChild->genome,
                                     num_genes, CIRCULAR, weights );
                    score =
                        greedylk ( 2 * num_genes, weights,
                                   tree_node->genome->genes, incycle,
                                   outcycle );
                    break;
#endif
                case TSP_COALESCED:
                    /* does not create a weight matrix, but sets weight in the adj lists */
                    convert2_to_tsp ( tree_node->parent->genome,
                                      tree_node->lChild->genome,
                                      tree_node->rChild->genome,
                                      adj_list, adj_pool, num_genes,
                                      CIRCULAR );

#ifdef DEBUG
                    fprintf ( outfile,
                              "in labeltree, about to call coalestsp\n" );
                    fflush ( outfile );
#endif
                    score =
                        coalestsp ( 2 * num_genes, tree_node->genome->genes,
                                    use_median,
                                    tree_node->parent->genome->genes,
                                    tree_node->lChild->genome->genes,
                                    tree_node->rChild->genome->genes,
                                    adj_list, neighbors, stack, outcycle,
                                    degree, otherEnd, edges, CIRCULAR );
                    break;
            }
            score += ( LARGENUM * num_genes );
#ifdef DEBUG
            fprintf ( outfile,
                      "in labeltree, done with TSP, returned score is %3d\n",
                      score );
            fflush ( outfile );
#endif
            tree_node->need_change = 0;

            /* now use TSP score to compare to Hamming distance along the three
               edges; for INV distance, compute it -- cannot mix the two!
               if no improvement, restore the old label and exit */
            switch ( distmethod )
            {
                case DIST_INV:
                    /* avoid computing distances if the new label is the same as the
                       old one */
                    same = TRUE;
                    for ( i = 0; i < num_genes; i++ )
                    {
                        if ( genes[i] != tree_node->genome->genes[i] )
                        {
                            same = FALSE;
                            break;
                        }
                    }
                    if ( same )
                        goto done;

                    /* new label, so we have to compute distances */
                    tmp_sc_parent =
                        bp_score ( tree_node, tree_node->parent, num_genes,
                                   CIRCULAR, distmethod, distmem, correction,
                                   num_genes );
                    tmp_sc_lChild =
                        bp_score ( tree_node, tree_node->lChild, num_genes,
                                   CIRCULAR, distmethod, distmem, correction,
                                   num_genes );
                    tmp_sc_rChild =
                        bp_score ( tree_node, tree_node->rChild, num_genes,
                                   CIRCULAR, distmethod, distmem, correction,
                                   num_genes );
#ifdef DEBUG
                    fprintf ( outfile,
                              "new edge scores are %3d, %3d, and %3d\n",
                              tmp_sc_parent, tmp_sc_lChild, tmp_sc_rChild );
                    fprintf ( outfile,
                              "old edge scores are %3d, %3d, and %3d\n",
                              *( tree_node->sc_parent ),
                              *( tree_node->sc_lChild ),
                              *( tree_node->sc_rChild ) );
                    fflush ( outfile );
#endif
                    delta =
                        ( tmp_sc_parent + tmp_sc_lChild + tmp_sc_rChild ) -
                        ( *( tree_node->sc_parent ) +
                          *( tree_node->sc_lChild ) +
                          *( tree_node->sc_rChild ) );
#ifdef DEBUG
                    fprintf ( outfile,
                              "in labeltree, new (inv) edge scores are %3d, %3d, and %3d, delta is %3d\n",
                              tmp_sc_parent, tmp_sc_lChild, tmp_sc_rChild,
                              delta );
                    fflush ( outfile );
#endif
                    break;
                case DIST_BP:
                default:
                    delta = score - ( *( tree_node->sc_parent ) +
                                      *( tree_node->sc_lChild ) +
                                      *( tree_node->sc_rChild ) );
                    if ( delta < 0 )
                    {           /* get to same state as case DIST_INV */
                        tmp_sc_parent =
                            bp_score ( tree_node, tree_node->parent,
                                       num_genes, CIRCULAR, distmethod,
                                       distmem, correction, num_genes );
                        tmp_sc_lChild =
                            bp_score ( tree_node, tree_node->lChild,
                                       num_genes, CIRCULAR, distmethod,
                                       distmem, correction, num_genes );
                        tmp_sc_rChild =
                            bp_score ( tree_node, tree_node->rChild,
                                       num_genes, CIRCULAR, distmethod,
                                       distmem, correction, num_genes );
#ifdef DEBUG
                        fprintf ( outfile,
                                  "new edge scores are %3d, %3d, and %3d\n",
                                  tmp_sc_parent, tmp_sc_lChild,
                                  tmp_sc_rChild );
                        fprintf ( outfile,
                                  "old edge scores are %3d, %3d, and %3d\n",
                                  *( tree_node->sc_parent ),
                                  *( tree_node->sc_lChild ),
                                  *( tree_node->sc_rChild ) );
                        fflush ( outfile );
#endif
                    }
                    break;
            }

#ifdef DEBUG
            fprintf ( outfile, "score=%3d, delta=%3d\n", score, delta );
#endif
            if ( delta < 0 )
            {
                /* accept the change */
                if ( tree_node->lChild->tag < 0 )
                    tree_node->lChild->need_change = 1;
                if ( tree_node->rChild->tag < 0 )
                    tree_node->rChild->need_change = 1;
                if ( tree_node->parent->tag < 0 )
                    tree_node->parent->need_change = 1;
                /* update the distances along the three edges (both ways) */
                *( tree_node->sc_parent ) = tmp_sc_parent;
                *( tree_node->sc_lChild ) = tmp_sc_lChild;
                *( tree_node->sc_rChild ) = tmp_sc_rChild;
                /* get ready for next loop */
                *IMPROVED = TRUE;
                ( *relabel )++;
                *best_so_far += delta;
            }
            else
            {                   /* restore old label */
                for ( i = 0; i < num_genes; i++ )
                {
                    tree_node->genome->genes[i] = genes[i];
                }
            }
        }                       /* end -- no condensation */
      done:
        tree_node->leaf = TRUE;
    }                           /* end processing internal node */

#ifdef PREORDER
    if ( !tree_node->lChild->leaf )
    {
        dfs_label_tree ( COND, root, tree_node->lChild, best_so_far, IMPROVED,
                         num_genes, num_genomes, weights, relabel, tspsolver,
                         thresh, adj_list, adj_pool, neighbors, stack,
                         incycle, outcycle, degree, otherEnd, edges, tpool,
                         labels, pred, succ, code, decode, condmedian,
                         condnode1, condnode2, condnode3, genes, CIRCULAR,
                         distmethod, distmem );
    }
    if ( tree_node->rChild )
        if ( !tree_node->rChild->leaf )
        {
            dfs_label_tree ( COND, root, tree_node->rChild, best_so_far,
                             IMPROVED, num_genes, num_genomes, weights,
                             relabel, tspsolver, thresh, adj_list, adj_pool,
                             neighbors, stack, incycle, outcycle, degree,
                             otherEnd, edges, tpool, labels, pred, succ, code,
                             decode, condmedian, condnode1, condnode2,
                             condnode3, genes, CIRCULAR, distmethod,
                             distmem );
        }
#endif
    return;
}

int
score_tree ( struct tNode *treeNode, int num_genes, int CIRCULAR,
             int distmethod, distmem_t * distmem, int correction )
{
    int score = 0;
    if ( treeNode == NULL )
        return 0;
    printf("Scoring tree node left child %d \n", treeNode->lChild);
    if ( treeNode->lChild )
    {
        printf("I'm scoring the tree %d\n", bp_score ( treeNode, treeNode->lChild, num_genes, CIRCULAR, distmethod, distmem, correction, num_genes ));
        printf("Check the tree %d\n",*(treeNode->lChild->sc_parent));
        *( treeNode->lChild->sc_parent ) = *( treeNode->sc_lChild ) =
            bp_score ( treeNode, treeNode->lChild, num_genes, CIRCULAR,
                       distmethod, distmem, correction, num_genes );
        printf("I'm scoring the tree\n");
        score += *( treeNode->sc_lChild ) +
            score_tree ( treeNode->lChild, num_genes, CIRCULAR, distmethod,
                         distmem, correction );
    }
    if ( treeNode->rChild )
    {
        *( treeNode->rChild->sc_parent ) = *( treeNode->sc_rChild ) =
            bp_score ( treeNode, treeNode->rChild, num_genes, CIRCULAR,
                       distmethod, distmem, correction, num_genes );
        score += *( treeNode->sc_rChild ) +
            score_tree ( treeNode->rChild, num_genes, CIRCULAR, distmethod,
                         distmem, correction );
    }
    return score;
}

int
bp_score ( struct tNode *node1, struct tNode *node2, int num_genes,
           int CIRCULAR, int distmethod, distmem_t * distmem, int correction,
           int oldngenes )
{
    int dist;
    if ( node1 == NULL || node2 == NULL )
        return 0;
    switch ( distmethod )
    {
        case DIST_BP:
            dist = hamming_distance ( node1->genome, node2->genome, num_genes,
                                      CIRCULAR, distmem->hammingArr );

            break;
        case DIST_INV:
            if ( correction )
            {
                if ( CIRCULAR )
                    dist =
                        ede ( invdist_circular
                              ( node1->genome, node2->genome, num_genes,
                                distmem ), oldngenes );
                else
                    dist =
                        ede ( invdist_noncircular
                              ( node1->genome, node2->genome, 0, num_genes,
                                distmem ), oldngenes );
            }
            else
            {
                if ( CIRCULAR )
                    dist =
                        invdist_circular ( node1->genome, node2->genome,
                                           num_genes, distmem );
                else
                    dist =
                        invdist_noncircular ( node1->genome, node2->genome, 0,
                                              num_genes, distmem );
            }
            break;
        default:
            dist = -1;
            fprintf ( stderr, "ERROR: Bad distance method in bp_score()\n" );
    }
    return ( dist );
}


/* Use genome_list to initialize genome pointers in the leaves of the
   tree.
*/
void
add_genomes_to_tree ( struct tNode *treeNode, struct genome_struct *labels,
                      struct genome_struct *genome_list, int num_genes )
{
    int i, id;
    struct genome_struct *gen;

    if ( treeNode == NULL )
        return;
    if ( treeNode->leaf == TRUE )
    {
        /* because this is a leaf, the treeNode->tag is non-negative */
        /* no need for malloc: just index into the labels array */
        id = treeNode->tag;
        gen = treeNode->genome = &labels[id];
        gen->genome_num = id;   /* not really redundant -- flags it as leaf */
        id--;
        for ( i = 0; i < num_genes; i++ )
        {
            gen->genes[i] = genome_list[id].genes[i];
        }
        gen->gnamePtr = genome_list[id].gnamePtr;
    }
    add_genomes_to_tree ( treeNode->lChild, labels, genome_list, num_genes );
    add_genomes_to_tree ( treeNode->rChild, labels, genome_list, num_genes );
    return;
}

void
restore_genome (  )
{
}

void
label_node ( struct tNode *tree_node, struct tNode *new_node, int num_genes )
{
}

void
leaf_tree ( struct tNode *tree )
{
    if ( tree == NULL )
        return;
    if ( tree->tag < 0 )
        tree->leaf = FALSE;
    leaf_tree ( tree->lChild );
    leaf_tree ( tree->rChild );
    return;
}

void
init_change_bits ( struct tNode *tree )
{
    if ( tree == NULL )
        return;
    if ( tree->tag < 0 )
    {                           /* internal node */
        tree->need_change = 1;
    }
    else
    {                           /* leaf: labels are permanent */
        tree->need_change = 0;
    }
    init_change_bits ( tree->lChild );
    init_change_bits ( tree->rChild );
    return;
}

/* find median for three node, node[0-2] and cNode is the store for median*/
void
findMedian ( struct tNode *node[3], struct tNode *cNode,
             int *best_so_far, int num_genes,
             int tspsolver, int thresh,
             struct adj_struct *adj_list, struct adj_struct *adj_pool,
             intpair_t * neighbors, int *stack, int *outcycle,
             int *degree, int *otherEnd, edge_t * edges,
             int *genes, int CIRCULAR,
             int distmethod, distmem_t * distmem, int correction )
{
    int score = 0, delta;
    int i;
    int realsolver, same, use_median = TRUE;
    struct genome_struct *gen[3];
    int tmp_sc_parent = 0, tmp_sc_lChild = 0, tmp_sc_rChild = 0;
    for ( i = 0; i < num_genes; i++ )
    {                           /* save old label */
        genes[i] = cNode->genome->genes[i];
    }
    /* which solver to call? */
    /* force use of  bbtsp if problem is too small */
    realsolver = tspsolver;
    if ( ( num_genes <= thresh ) && ( tspsolver != TSP_COALESCED ) )
        realsolver = TSP_BBTSP;

    switch ( realsolver )
    {
        case INVERSION_MEDIAN:
            gen[0] = node[0]->genome;
            gen[1] = node[1]->genome;
            gen[2] = node[2]->genome;

            find_reversal_median ( cNode->genome, gen, num_genes, NULL );
/*          find_inversion_median(cNode->genome, gen, num_genes, distmem);*/
            score = 0;
            if ( CIRCULAR )
            {
                score += invdist_circular ( gen[0],
                                            cNode->genome, num_genes,
                                            distmem );
                score +=
                    invdist_circular ( gen[1], cNode->genome, num_genes,
                                       distmem );
                score +=
                    invdist_circular ( gen[2], cNode->genome, num_genes,
                                       distmem );
            }
            else
            {
                score += invdist_noncircular ( gen[0],
                                               cNode->genome, 0, num_genes,
                                               distmem );
                score +=
                    invdist_noncircular ( gen[1], cNode->genome, 0, num_genes,
                                          distmem );
                score +=
                    invdist_noncircular ( gen[2], cNode->genome, 0, num_genes,
                                          distmem );
            }
            break;
        case INVERSION_MEDIAN_FAST:
            gen[0] = node[0]->genome;
            gen[1] = node[1]->genome;
            gen[2] = node[2]->genome;
            score = 0;
            if ( CIRCULAR )
            {
                albert_inversion_median_circular ( gen,
                                                   num_genes,
                                                   cNode->genome->genes );
                score +=
                    invdist_circular ( gen[0], cNode->genome, num_genes,
                                       distmem );
                score +=
                    invdist_circular ( gen[1], cNode->genome, num_genes,
                                       distmem );
                score +=
                    invdist_circular ( gen[2], cNode->genome, num_genes,
                                       distmem );
            }
            else
            {
                albert_inversion_median_noncircular ( gen,
                                                      num_genes,
                                                      cNode->genome->genes );
                score +=
                    invdist_noncircular ( gen[0], cNode->genome, 0, num_genes,
                                          distmem );
                score +=
                    invdist_noncircular ( gen[1], cNode->genome, 0, num_genes,
                                          distmem );
                score +=
                    invdist_noncircular ( gen[2], cNode->genome, 0, num_genes,
                                          distmem );
            }

            break;
        case TSP_BBTSP:
            /* does not create a weight matrix, but sets weight in the adj lists */
            convert2_to_tsp ( node[0]->genome, node[1]->genome,
                              node[2]->genome, adj_list, adj_pool, num_genes,
                              CIRCULAR );

            score = bbtsp ( 2 * num_genes, cNode->genome->genes, use_median,
                            node[0]->genome->genes,
                            node[1]->genome->genes,
                            node[2]->genome->genes,
                            adj_list, neighbors, stack, outcycle, degree,
                            otherEnd, edges, CIRCULAR );
            break;
#ifdef CONCORDE
        case TSP_CHLINKERN:
            /* creates a weight matrix -- more expensive! */
            convert_to_tsp ( node[0]->genome, node[1]->genome,
                             node[2]->genome, num_genes, CIRCULAR, weights );
            score =
                chlinkern ( 2 * num_genes, weights, cNode->genome->genes,
                            incycle, outcycle );
            break;
        case TSP_GREEDYLK:
            /* creates a weight matrix -- more expensive! */
            convert_to_tsp ( node[0]->genome, node[1]->genome,
                             node[2]->genome, num_genes, CIRCULAR, weights );
            score =
                greedylk ( 2 * num_genes, weights, cNode->genome->genes,
                           incycle, outcycle );
            break;
#endif
        case TSP_COALESCED:
            /* does not create a weight matrix, but sets weight in the adj lists */
            convert2_to_tsp ( node[0]->genome, node[1]->genome,
                              node[2]->genome, adj_list, adj_pool, num_genes,
                              CIRCULAR );

            score =
                coalestsp ( 2 * num_genes, cNode->genome->genes, use_median,
                            node[0]->genome->genes, node[1]->genome->genes,
                            node[2]->genome->genes, adj_list, neighbors,
                            stack, outcycle, degree, otherEnd, edges,
                            CIRCULAR );
            break;
    }
    score += ( LARGENUM * num_genes );
    cNode->need_change = 0;

    /* now use TSP score to compare to Hamming distance along the three
       edges; for INV distance, compute it -- cannot mix the two!
       if no improvement, restore the old label and exit */
    switch ( distmethod )
    {
        case DIST_INV:
            /* avoid computing distances if the new label is the same as the
               old one */
            same = TRUE;
            for ( i = 0; i < num_genes; i++ )
            {
                if ( genes[i] != cNode->genome->genes[i] )
                {
                    same = FALSE;
                    break;
                }
            }
            if ( same )
                return;

            /* new label, so we have to compute distances */
            tmp_sc_parent =
                bp_score ( cNode, node[0], num_genes, CIRCULAR,
                           distmethod, distmem, correction, num_genes );
            tmp_sc_lChild =
                bp_score ( cNode, node[1], num_genes, CIRCULAR,
                           distmethod, distmem, correction, num_genes );
            tmp_sc_rChild =
                bp_score ( cNode, node[2], num_genes, CIRCULAR,
                           distmethod, distmem, correction, num_genes );
            delta = ( tmp_sc_parent + tmp_sc_lChild + tmp_sc_rChild ) -
                ( *( cNode->sc_parent ) +
                  *( cNode->sc_lChild ) + *( cNode->sc_rChild ) );
            break;
        case DIST_BP:
        default:
            delta = score - ( *( cNode->sc_parent ) +
                              *( cNode->sc_lChild ) + *( cNode->sc_rChild ) );
            if ( delta < 0 )
            {                   /* get to same state as case DIST_INV */
                tmp_sc_parent =
                    bp_score ( cNode, node[0], num_genes, CIRCULAR,
                               distmethod, distmem, correction, num_genes );
                tmp_sc_lChild =
                    bp_score ( cNode, node[1], num_genes, CIRCULAR,
                               distmethod, distmem, correction, num_genes );
                tmp_sc_rChild =
                    bp_score ( cNode, node[2], num_genes, CIRCULAR,
                               distmethod, distmem, correction, num_genes );
            }
            break;
    }

    if ( delta < 0 )
    {
        /* accept the change */
        if ( node[1]->tag < 0 )
            node[1]->need_change = 1;
        if ( node[2]->tag < 0 )
            node[2]->need_change = 1;
        if ( node[0]->tag < 0 )
            node[0]->need_change = 1;
        /* update the distances along the three edges (both ways) */
        *( cNode->sc_parent ) = tmp_sc_parent;
        *( cNode->sc_lChild ) = tmp_sc_lChild;
        *( cNode->sc_rChild ) = tmp_sc_rChild;
        /* get ready for next loop */
        *best_so_far += delta;
    }
    else
    {                           /* restore old label */
        for ( i = 0; i < num_genes; i++ )
        {
            cNode->genome->genes[i] = genes[i];
        }
    }
    return;
}
