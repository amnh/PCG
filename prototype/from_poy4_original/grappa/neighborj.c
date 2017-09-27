#include <stdio.h>
#include <stdlib.h>
#include "neighborj.h"
#include "invdist.h"
#include "const_tree.h"
#include "labeltree.h"
#include "inittree.h"
#include "specialinit.h"
#include "math.h"
#include "condense.h"

#define INT_INF      MAX_RAND
#define DBL_INF      (double)MAX_RAND
#define SMALLDELTA   0.001

double
neighborj_SN ( struct genome_struct *genomes, int num_genes, int num_genomes,
               distmem_t * distmem, int CIRCULAR, int verbose )
{
    int i, j, k;
    double NJtreescore;
    int num_clusters;
    double **D;
    int *valid;
    double Dsum;
    double Sij, minSij;
    int mini, minj;

    D = ( double ** ) malloc ( num_genomes * sizeof ( double * ) );
    if ( D == ( double ** ) NULL )
        fprintf ( stderr, "ERROR: D NULL\n" );
    for ( i = 0; i < num_genomes; i++ )
    {
        D[i] = ( double * ) malloc ( num_genomes * sizeof ( double ) );
        if ( D[i] == ( double * ) NULL )
            fprintf ( stderr, "ERROR: D[i] NULL\n" );
    }

    valid = ( int * ) malloc ( num_genomes * sizeof ( int ) );
    if ( valid == ( int * ) NULL )
        fprintf ( stderr, "ERROR: valid NULL\n" );

    NJtreescore = 0.0;

    for ( i = 0; i < num_genomes; i++ )
        valid[i] = TRUE;

    Dsum = 0.0;

    for ( i = 0; i < num_genomes; i++ )
    {
        for ( j = i + 1; j < num_genomes; j++ )
        {
            if ( CIRCULAR )
                D[i][j] =
                    ( double ) invdist_circular ( genomes + i, genomes + j,
                                                  num_genes, distmem );
            else
                D[i][j] =
                    ( double ) invdist_noncircular ( genomes + i, genomes + j,
                                                     0, num_genes, distmem );
            Dsum += D[i][j];
#ifdef DEBUG
            fprintf ( outfile, "SN D[%3d][%3d]: %5.2f   (Dsum: %5.2f)\n",
                      i, j, D[i][j], Dsum );
#endif
        }
    }

    num_clusters = num_genomes;
    while ( num_clusters > 2 )
    {
        minSij = DBL_INF;
        mini = -1;
        minj = -1;
        for ( i = 0; i < num_genomes; i++ )
        {
            if ( valid[i] )
            {
                for ( j = i + 1; j < num_genomes; j++ )
                {
                    if ( valid[j] )
                    {
                        Sij = 0.0;
                        for ( k = 0; k < num_genomes; k++ )
                        {
                            if ( ( k != i ) && ( k != j ) && valid[k] )
                                Sij +=
                                    ( i < k ? D[i][k] : D[k][i] ) + ( j <
                                                                      k ?
                                                                      D[j][k]
                                                                      :
                                                                      D[k]
                                                                      [j] );
                        }
                        Sij /= 2.0 * ( ( double ) num_clusters - 2.0 );
                        Sij += 0.5 * D[i][j];
                        Sij +=
                            ( 1.0 / ( ( double ) num_clusters - 2.0 ) ) *
                            ( Dsum - D[i][j] );
#ifdef DEBUG
                        fprintf ( outfile, "SN S[%3d][%3d]: %5.2f\n", i, j,
                                  Sij );
#endif
                        if ( Sij < minSij )
                        {
                            minSij = Sij;
                            mini = i;
                            minj = j;
                        }
                    }
                }
            }
        }

        if ( minSij == DBL_INF )
            fprintf ( stderr, "ERROR: minSij Infinite\n" );
        if ( ( mini == -1 ) || ( minj == -1 ) )
            fprintf ( stderr, "ERROR: mini or minj not set\n" );
        if ( ( valid[mini] == FALSE ) || ( valid[minj] == FALSE ) )
            fprintf ( stderr, "ERROR: i or j cannot join\n" );

#ifdef VERYVERBOSE
        if ( verbose )
        {
            fprintf ( outfile, "Saitou-Nei NJ:\t\t Step (%3d): ",
                      num_genomes - num_clusters + 1 );
            fprintf ( outfile,
                      "Join %3d and %3d into %3d, edge weight= %5.2f\n", mini,
                      minj, mini, D[mini][minj] );
        }
#endif

        valid[minj] = FALSE;
        for ( j = 0; j < mini; j++ )
        {
            if ( valid[j] )
            {
                if ( minj < j )
                    D[j][mini] = 0.5 * ( D[j][mini] + D[minj][j] );
                else
                    D[j][mini] = 0.5 * ( D[j][mini] + D[j][minj] );
            }
        }
        for ( j = mini + 1; j < num_genomes; j++ )
        {
            if ( valid[j] )
            {
                if ( minj < j )
                    D[mini][j] = 0.5 * ( D[mini][j] + D[minj][j] );
                else
                    D[mini][j] = 0.5 * ( D[mini][j] + D[j][minj] );
            }
        }
        NJtreescore += D[mini][minj];
        Dsum -= D[mini][minj];
        num_clusters--;
    }

    /* Must add last edge */
    mini = minj = -1;
    for ( i = 0; i < num_genomes; i++ )
        if ( valid[i] )
        {
            if ( mini < 0 )
                mini = i;
            else
                minj = i;
        }
    if ( ( mini < 0 ) || ( minj < 0 ) || ( mini >= minj ) )
        fprintf ( stderr, "ERROR: cannot join last two neighbors\n" );

#ifdef VERYVERBOSE
    if ( verbose )
    {
        fprintf ( outfile, "Saitou-Nei NJ:\t\t Step (%3d): ",
                  num_genomes - num_clusters + 1 );
        fprintf ( outfile, "Join %3d and %3d into %3d, edge weight= %5.2f\n",
                  mini, minj, mini, D[mini][minj] );
    }
#endif

    NJtreescore += D[mini][minj];

    if ( verbose )
    {
        fprintf ( outfile, "Saitou-Nei NJ\t\t Tree Score: %5.2f\n\n",
                  NJtreescore );
        fflush ( outfile );
    }

    free ( valid );
    for ( i = 0; i < num_genomes; i++ )
        free ( D[i] );
    free ( D );

    return NJtreescore;
}

double
neighborj_SK ( struct genome_struct *genomes, int num_genes, int num_genomes,
               distmem_t * distmem, int CIRCULAR, int verbose )
{
    int i, j;
    double NJtreescore;
    int num_clusters;
    double **D, Dij;
    double *r;
    int *valid;
    double Sij, minSij;
    int mini, minj;

    D = ( double ** ) malloc ( num_genomes * sizeof ( double * ) );
    if ( D == ( double ** ) NULL )
        fprintf ( stderr, "ERROR: D NULL\n" );
    for ( i = 0; i < num_genomes; i++ )
    {
        D[i] = ( double * ) malloc ( num_genomes * sizeof ( double ) );
        if ( D[i] == ( double * ) NULL )
            fprintf ( stderr, "ERROR: D[i] NULL\n" );
    }

    r = ( double * ) malloc ( num_genomes * sizeof ( double ) );
    if ( r == ( double * ) NULL )
        fprintf ( stderr, "ERROR: r NULL\n" );

    valid = ( int * ) malloc ( num_genomes * sizeof ( int ) );
    if ( valid == ( int * ) NULL )
        fprintf ( stderr, "ERROR: valid NULL\n" );

    NJtreescore = 0.0;

    for ( i = 0; i < num_genomes; i++ )
        valid[i] = TRUE;

    for ( i = 0; i < num_genomes; i++ )
    {
        for ( j = i + 1; j < num_genomes; j++ )
        {
            if ( CIRCULAR )
                D[i][j] =
                    ( double ) invdist_circular ( genomes + i, genomes + j,
                                                  num_genes, distmem );
            else
                D[i][j] =
                    ( double ) invdist_noncircular ( genomes + i, genomes + j,
                                                     0, num_genes, distmem );
        }
    }

    num_clusters = num_genomes;
    while ( num_clusters > 2 )
    {
        for ( i = 0; i < num_genomes; i++ )
        {
            if ( valid[i] )
            {
                r[i] = 0.0;
                for ( j = 0; j < num_genomes; j++ )
                    if ( ( valid[j] ) && ( i != j ) )
                    {
                        if ( i < j )
                            r[i] += D[i][j];
                        else
                            r[i] += D[j][i];
                    }
                r[i] /= ( ( double ) num_clusters - 2.0 );
            }
        }

        minSij = DBL_INF;
        mini = -1;
        minj = -1;
        for ( i = 0; i < num_genomes; i++ )
        {
            if ( valid[i] )
            {
                for ( j = i + 1; j < num_genomes; j++ )
                {
                    if ( valid[j] )
                    {
                        Sij = D[i][j] - ( r[i] + r[j] );
#ifdef DEBUG
                        fprintf ( outfile, "S[%3d][%3d]: %5.2f\n", i, j,
                                  Sij );
#endif
                        if ( Sij < minSij )
                        {
                            minSij = Sij;
                            mini = i;
                            minj = j;
                        }
                    }
                }
            }
        }

        if ( minSij == DBL_INF )
            fprintf ( stderr, "ERROR: minSij Infinite\n" );
        if ( ( mini == -1 ) || ( minj == -1 ) )
            fprintf ( stderr, "ERROR: mini or minj not set\n" );
        if ( ( valid[mini] == FALSE ) || ( valid[minj] == FALSE ) )
            fprintf ( stderr, "ERROR: i or j cannot join\n" );

#ifdef VERYVERBOSE
        if ( verbose )
        {
            fprintf ( outfile, "Studier-Keppler NJ:\t Step (%3d): ",
                      num_genomes - num_clusters + 1 );
            fprintf ( outfile,
                      "Join %3d and %3d into %3d, edge weight= %5.2f\n", mini,
                      minj, mini, D[mini][minj] );
        }
#endif

        valid[minj] = FALSE;
        Dij = D[mini][minj];
        for ( j = 0; j < mini; j++ )
        {
            if ( valid[j] )
            {
                if ( minj < j )
                    D[j][mini] = 0.5 * ( D[j][mini] + D[minj][j] - Dij );
                else
                    D[j][mini] = 0.5 * ( D[j][mini] + D[j][minj] - Dij );
            }
        }
        for ( j = mini + 1; j < num_genomes; j++ )
        {
            if ( valid[j] )
            {
                if ( minj < j )
                    D[mini][j] = 0.5 * ( D[mini][j] + D[minj][j] - Dij );
                else
                    D[mini][j] = 0.5 * ( D[mini][j] + D[j][minj] - Dij );
            }
        }
        NJtreescore += Dij;
        num_clusters--;
    }

    /* Must add last edge */
    mini = minj = -1;
    for ( i = 0; i < num_genomes; i++ )
        if ( valid[i] )
        {
            if ( mini < 0 )
                mini = i;
            else
                minj = i;
        }
    if ( ( mini < 0 ) || ( minj < 0 ) || ( mini >= minj ) )
        fprintf ( stderr, "ERROR: cannot join last two neighbors\n" );

#ifdef VERYVERBOSE
    if ( verbose )
    {
        fprintf ( outfile, "Studier-Keppler NJ:\t Step (%3d): ",
                  num_genomes - num_clusters + 1 );
        fprintf ( outfile, "Join %3d and %3d into %3d, edge weight= %5.2f\n",
                  mini, minj, mini, D[mini][minj] );
    }
#endif

    NJtreescore += D[mini][minj];

    if ( verbose )
    {
        fprintf ( outfile, "Studier-Keppler NJ\t Tree Score: %5.2f\n\n",
                  NJtreescore );
        fflush ( outfile );
    }

    free ( valid );
    free ( r );
    for ( i = 0; i < num_genomes; i++ )
        free ( D[i] );
    free ( D );

    return NJtreescore;
}

double
neighborj_SK_tree ( struct genome_struct *genomes,
                    int num_genes, int num_genomes,
                    distmem_t * distmem, int CIRCULAR, char *constTree )
{
    int i, j;
    double NJtreescore;
    int num_clusters;
    double **D, Dij;
    double *r;
    int *valid;
    double Sij, minSij;
    int mini, minj;
    char **branch;
    char *tmpbranch;
    int maxConstSize;

    D = ( double ** ) malloc ( num_genomes * sizeof ( double * ) );
    if ( D == ( double ** ) NULL )
        fprintf ( stderr, "ERROR: D NULL\n" );
    for ( i = 0; i < num_genomes; i++ )
    {
        D[i] = ( double * ) malloc ( num_genomes * sizeof ( double ) );
        if ( D[i] == ( double * ) NULL )
            fprintf ( stderr, "ERROR: D[i] NULL\n" );
    }

    r = ( double * ) malloc ( num_genomes * sizeof ( double ) );
    if ( r == ( double * ) NULL )
        fprintf ( stderr, "ERROR: r NULL\n" );

    valid = ( int * ) malloc ( num_genomes * sizeof ( int ) );
    if ( valid == ( int * ) NULL )
        fprintf ( stderr, "ERROR: valid NULL\n" );

    branch = ( char ** ) malloc ( num_genomes * sizeof ( char * ) );
    if ( branch == ( char ** ) NULL )
        fprintf ( stderr, "ERROR: branch NULL\n" );

    maxConstSize =
        ( ( int ) ceil ( log10 ( ( double ) num_genomes ) ) +
          4 ) * num_genomes + 1;

    for ( i = 0; i < num_genomes; i++ )
    {
        branch[i] = ( char * ) malloc ( maxConstSize * sizeof ( char ) );
        if ( branch[i] == ( char * ) NULL )
            fprintf ( stderr, "ERROR: branch[i] NULL\n" );

        sprintf ( branch[i], "%d", i + 1 );
    }
    tmpbranch = ( char * ) malloc ( maxConstSize * sizeof ( char ) );
    if ( tmpbranch == ( char * ) NULL )
        fprintf ( stderr, "ERROR: tmpbranch NULL\n" );

    NJtreescore = 0;

    for ( i = 0; i < num_genomes; i++ )
        valid[i] = TRUE;

    for ( i = 0; i < num_genomes; i++ )
    {
        for ( j = i + 1; j < num_genomes; j++ )
        {
            if ( CIRCULAR )
                D[i][j] =
                    ( double ) invdist_circular ( genomes + i, genomes + j,
                                                  num_genes, distmem );
            else
                D[i][j] =
                    ( double ) invdist_noncircular ( genomes + i, genomes + j,
                                                     0, num_genes, distmem );
        }
    }

    num_clusters = num_genomes;
    while ( num_clusters > 2 )
    {
        for ( i = 0; i < num_genomes; i++ )
        {
            if ( valid[i] )
            {
                r[i] = 0.0;
                for ( j = 0; j < num_genomes; j++ )
                    if ( ( valid[j] ) && ( i != j ) )
                    {
                        if ( i < j )
                            r[i] += D[i][j];
                        else
                            r[i] += D[j][i];
                    }
                r[i] /= ( ( double ) num_clusters - 2.0 );
            }
        }

        minSij = DBL_INF;
        mini = -1;
        minj = -1;
        for ( i = 0; i < num_genomes; i++ )
        {
            if ( valid[i] )
            {
                for ( j = i + 1; j < num_genomes; j++ )
                {
                    if ( valid[j] )
                    {
                        Sij = D[i][j] - ( r[i] + r[j] );
#ifdef DEBUG
                        fprintf ( outfile, "S[%3d][%3d]: %5.2f\n", i, j,
                                  Sij );
#endif
                        if ( Sij < minSij )
                        {
                            minSij = Sij;
                            mini = i;
                            minj = j;
                        }
                    }
                }
            }
        }

        if ( minSij == DBL_INF )
            fprintf ( stderr, "ERROR: minSij Infinite\n" );
        if ( ( mini == -1 ) || ( minj == -1 ) )
            fprintf ( stderr, "ERROR: mini or minj not set\n" );
        if ( ( valid[mini] == FALSE ) || ( valid[minj] == FALSE ) )
            fprintf ( stderr, "ERROR: i or j cannot join\n" );

        strcpy ( tmpbranch, branch[mini] );
        sprintf ( branch[mini], "(%s,%s)", tmpbranch, branch[minj] );

        valid[minj] = FALSE;
        Dij = D[mini][minj];
        for ( j = 0; j < mini; j++ )
        {
            if ( valid[j] )
            {
                if ( minj < j )
                    D[j][mini] = 0.5 * ( D[j][mini] + D[minj][j] - Dij );
                else
                    D[j][mini] = 0.5 * ( D[j][mini] + D[j][minj] - Dij );
            }
        }
        for ( j = mini + 1; j < num_genomes; j++ )
        {
            if ( valid[j] )
            {
                if ( minj < j )
                    D[mini][j] = 0.5 * ( D[mini][j] + D[minj][j] - Dij );
                else
                    D[mini][j] = 0.5 * ( D[mini][j] + D[j][minj] - Dij );
            }
        }
        NJtreescore += Dij;
        num_clusters--;
    }

    /* Must add last edge */
    mini = minj = -1;
    for ( i = 0; i < num_genomes; i++ )
        if ( valid[i] )
        {
            if ( mini < 0 )
                mini = i;
            else
                minj = i;
        }
    if ( ( mini < 0 ) || ( minj < 0 ) || ( mini >= minj ) )
        fprintf ( stderr, "ERROR: cannot join last two neighbors\n" );

    NJtreescore += D[mini][minj];

    sprintf ( constTree, "(%s,%s)", branch[mini], branch[minj] );

    free ( tmpbranch );
    for ( i = 0; i < num_genomes; i++ )
        free ( branch[i] );
    free ( branch );

    free ( valid );
    free ( r );
    for ( i = 0; i < num_genomes; i++ )
        free ( D[i] );
    free ( D );

    return NJtreescore;
}


int
neighborj_score_string ( const char *constTree,
                         struct genome_struct *genome_list, int num_genes,
                         int num_genomes,
                         distmem_t * distmem, int CIRCULAR,
                         struct tNode *tpool, int *edgepool,
                         struct genome_struct *labels,
                         int initmethod, int COND,
                         struct qNode *qpool,
                         struct adj_struct *adj_list,
                         struct adj_struct *adj_pool,
                         int *stack, int *degree, int *otherEnd,
                         intpair_t * neighbors,
                         smalledge_t * smalledges,
                         edge_t * edges,
                         int *outcycle, int *pred1, int *pred2, int *picked,
                         int *decode, int inittspsolver,
                         triple_t * triple, int *incycle, int tspsolver,
                         int distmethod,
                         int thresh, int **weights, int **status,
                         int *best_so_far, env_t * const_env,
                         int *genes,
                         int *condense_succ, int *condense_decode,
                         int orig_num_genes, int correction,
                         char *treeString )
{
    int score;
    ConstraintTree_T const_tree;
    struct tNode *tree;
    int index;
    int i;

#ifdef DEBUG
    fprintf ( outfile, "Neighbor-Joining Tree: %s\n\n", constTree );
    fflush ( outfile );
#endif

    init_const ( const_env, constTree, const_tree, tpool );
    tree = first_const ( const_tree, tpool, 0, const_env );

#ifdef DEBUG
    print_tree_tag ( tree );
#endif

    tree->leaf = TRUE;
    index = 0;
    SetTreeEdgePtrs ( tree, edgepool, &index );
    for ( i = 0; i < 2 * num_genomes + 6; i++ )
    {
        tpool[i].genome = NULL;
    }
    add_genomes_to_tree ( tree, labels, genome_list, num_genes );
    switch ( initmethod )
    {
        case RAND:
            initialize_tree_random ( tree, labels, num_genes, num_genomes );
            break;
        case SMALLNN:
            initialize_tree_SNN ( COND, tree, tpool, labels, qpool, adj_list,
                                  adj_pool, weights, stack, degree, otherEnd,
                                  neighbors, edges, incycle, outcycle, pred1,
                                  pred2, picked, decode, num_genes,
                                  num_genomes, inittspsolver, thresh,
                                  CIRCULAR );
            /* reuses pred1, pred2, picked */
            break;
        case SBNN:
            initialize_tree_SBNN ( COND, tree, tpool, labels, qpool, adj_list,
                                   adj_pool, weights, stack, degree, otherEnd,
                                   neighbors, edges, incycle, outcycle, pred1,
                                   pred2, picked, decode, num_genes,
                                   num_genomes, inittspsolver, thresh,
                                   CIRCULAR );
            /* reuses pred1, pred2, picked */
            break;
        case BIGNN:
            initialize_tree_BNN ( COND, tree, tpool, labels, triple, adj_list,
                                  adj_pool, weights, stack, degree, otherEnd,
                                  neighbors, edges, incycle, outcycle, pred1,
                                  pred2, picked, decode, num_genes,
                                  num_genomes, inittspsolver, thresh,
                                  CIRCULAR );
            /* reuses pred1, pred2, picked */
            break;
        case FASTPROP:
            initialize_tree_propagate ( COND, tree, tpool, labels, qpool,
                                        adj_list, adj_pool, weights, stack,
                                        degree, otherEnd, neighbors, edges,
                                        incycle, outcycle, num_genes,
                                        num_genomes, pred1, pred2, picked,
                                        decode, FAST, tspsolver, thresh,
                                        CIRCULAR );
            break;
        case MEDIANPROP:
            initialize_tree_propagate ( COND, tree, tpool, labels, qpool,
                                        adj_list, adj_pool, weights, stack,
                                        degree, otherEnd, neighbors, edges,
                                        incycle, outcycle, num_genes,
                                        num_genomes, pred1, pred2, picked,
                                        decode, MEDIAN, inittspsolver, thresh,
                                        CIRCULAR );
            break;
        case TRIV:
            initialize_tree_trivial ( tree, labels, num_genes, num_genomes );
            break;
        case ADJPARS:
            initialize_tree_adjpars ( tree, tpool, labels,
                                      stack, degree, otherEnd, neighbors,
                                      smalledges, incycle, outcycle,
                                      num_genes, num_genomes, weights, status,
                                      inittspsolver, thresh, CIRCULAR );
            break;
        default:
            fprintf ( stderr, "ERROR: no initialization given\n" );
    }
    score =
        score_tree ( tree, num_genes, CIRCULAR, distmethod, distmem,
                     correction );
#ifdef DEBUG
    fprintf ( outfile, "initial score=%5d\n", score );
    fflush ( outfile );
#endif
    score =
        iterate_over_tree ( COND, tree, num_genes, num_genomes, tspsolver,
                            thresh, score, adj_list, adj_pool, neighbors,
                            stack, incycle, outcycle, weights, degree,
                            otherEnd, edges, tpool, labels, pred1, pred2,
                            picked, decode, genes, CIRCULAR, distmethod,
                            distmem, correction );

    if ( score < *best_so_far )
    {
        *best_so_far = score;

#ifdef MPBPA
        if ( MYPROC == 0 )
        {
#endif
            fprintf ( outfile, "Neighbor-Joining Tree: %s\n\n", constTree );
            fflush ( outfile );

            switch ( distmethod )
            {
                case DIST_BP:
                    fprintf ( outfile, "NJ breakpoint score = %12d\n",
                              score );
                    fprintf ( outfile, "NJ inversion score  = %12d\n",
                              score_tree ( tree, num_genes, CIRCULAR,
                                           DIST_INV, distmem, correction ) );
                    fflush ( outfile );
                    break;
                case DIST_INV:
                    fprintf ( outfile, "NJ inversion score  = %12d\n",
                              score );
                    fprintf ( outfile, "NJ breakpoint score = %12d\n",
                              score_tree ( tree, num_genes, CIRCULAR, DIST_BP,
                                           distmem, correction ) );
                    fflush ( outfile );
                    break;
            }

#ifdef DEBUG
            print_tree_tag ( tree );
#endif

            print_tree_uncondensed ( tree, num_genes,
                                     condense_succ, condense_decode,
                                     orig_num_genes );
            if ( treeString != NULL )
                print_tree_nexus_noscore ( treeString, tree );

            fprintf ( outfile, "\n" );
            /* testing */

#ifdef MPBPA
        }
#endif

    }

    return ( score );
}

int
neighborj_score_one ( struct genome_struct *genome_list, int num_genes,
                      int num_genomes,
                      distmem_t * distmem, int CIRCULAR,
                      struct tNode *tpool, int *edgepool,
                      struct genome_struct *labels,
                      int initmethod, int COND,
                      struct qNode *qpool,
                      struct adj_struct *adj_list,
                      struct adj_struct *adj_pool, int *stack, int *degree,
                      int *otherEnd, intpair_t * neighbors,
                      smalledge_t * smalledges, edge_t * edges, int *outcycle,
                      int *pred1, int *pred2, int *picked, int *decode,
                      int inittspsolver, triple_t * triple, int *incycle,
                      int tspsolver, int distmethod, int thresh,
                      int **weights, int **status, env_t * const_env,
                      int *genes, int *condense_succ, int *condense_decode,
                      int orig_num_genes, int correction )
{
    double invscore_d;
    int score;
    char *constTree;
    int maxConstSize;
    int best_so_far;

    char *treeString = NULL;
    maxConstSize =
        ( ( int ) ceil ( log10 ( ( double ) num_genomes ) ) +
          4 ) * num_genomes + 1;

    constTree = ( char * ) malloc ( maxConstSize * sizeof ( char ) );
    if ( constTree == ( char * ) NULL )
        fprintf ( stderr, "ERROR: constTree NULL\n" );


    invscore_d =
        neighborj_SK_tree ( genome_list, num_genes, num_genomes, distmem,
                            CIRCULAR, constTree );

    best_so_far = INT_INF;

    score = neighborj_score_string ( constTree,
                                     genome_list, num_genes, num_genomes,
                                     distmem, CIRCULAR, tpool, edgepool,
                                     labels, initmethod, COND, qpool,
                                     adj_list, adj_pool, stack, degree,
                                     otherEnd, neighbors, smalledges, edges,
                                     outcycle,
                                     pred1, pred2, picked, decode,
                                     inittspsolver, triple, incycle,
                                     tspsolver, distmethod, thresh, weights,
                                     status, &best_so_far, const_env, genes,
                                     condense_succ, condense_decode,
                                     orig_num_genes, correction, treeString );

    free ( constTree );

    return ( score );
}

void
neighborj_SK_trees ( struct genome_struct *genome_list,
                     int num_genes, int num_genomes,
                     distmem_t * distmem, int CIRCULAR,
                     struct tNode *tpool, int *edgepool,
                     struct genome_struct *labels,
                     int initmethod, int COND,
                     struct qNode *qpool, struct adj_struct *adj_list,
                     struct adj_struct *adj_pool,
                     int *stack, int *degree, int *otherEnd,
                     intpair_t * neighbors,
                     smalledge_t * smalledges,
                     edge_t * edges,
                     int *outcycle, int *pred1, int *pred2, int *picked,
                     int *decode, int inittspsolver,
                     triple_t * triple, int *incycle, int tspsolver,
                     int distmethod, int thresh, int **weights, int **status,
                     int num_clusters, double **orig_D, int *orig_valid,
                     char **orig_branch, int maxConstSize,
                     int *best_so_far, int *count,
                     env_t * const_env,
                     int *genes,
                     int *condense_succ, int *condense_decode,
                     int orig_num_genes, int correction, char *treeString )
{
    int i, j, k, m;
    double **D, Dij;
    double *r;
    int *valid;
    double Sij, minSij;
    int mini, minj;
    char **branch;
    char *tmpbranch;
    int score;
    char *constTree;

#ifdef DEBUG
    fprintf ( outfile,
              "neighborj_SK_trees(): best_so_far: %12d  num_clust: %3d\n",
              *best_so_far, num_clusters );
    fflush ( outfile );
#endif

    if ( num_clusters < 2 )
    {
        fprintf ( stderr, "ERROR: num_clusters < 2\n" );
        exit ( -1 );
    }

    if ( num_clusters == 2 )
    {

        constTree = ( char * ) malloc ( maxConstSize * sizeof ( char ) );
        if ( constTree == ( char * ) NULL )
            fprintf ( stderr, "ERROR: constTree NULL\n" );

        /* Must add last edge */
        mini = minj = -1;
        for ( i = 0; i < num_genomes; i++ )
            if ( orig_valid[i] )
            {
                if ( mini < 0 )
                    mini = i;
                else
                    minj = i;
            }
        if ( ( mini < 0 ) || ( minj < 0 ) || ( mini >= minj ) )
            fprintf ( stderr, "ERROR: cannot join last two neighbors\n" );

        sprintf ( constTree, "(%s,%s)", orig_branch[mini],
                  orig_branch[minj] );

        score = neighborj_score_string ( constTree,
                                         genome_list, num_genes, num_genomes,
                                         distmem, CIRCULAR, tpool, edgepool,
                                         labels, initmethod, COND, qpool,
                                         adj_list, adj_pool, stack, degree,
                                         otherEnd, neighbors, smalledges,
                                         edges, outcycle, pred1, pred2,
                                         picked, decode, inittspsolver,
                                         triple, incycle, tspsolver,
                                         distmethod, thresh, weights, status,
                                         best_so_far, const_env, genes,
                                         condense_succ, condense_decode,
                                         orig_num_genes, correction,
                                         treeString );

        ( *count )++;

        free ( constTree );

        return;
    }


    r = ( double * ) malloc ( num_genomes * sizeof ( double ) );
    if ( r == ( double * ) NULL )
        fprintf ( stderr, "ERROR: r NULL\n" );

    valid = ( int * ) malloc ( num_genomes * sizeof ( int ) );
    if ( valid == ( int * ) NULL )
        fprintf ( stderr, "ERROR: valid NULL\n" );

    for ( i = 0; i < num_genomes; i++ )
        valid[i] = orig_valid[i];

    branch = ( char ** ) malloc ( num_genomes * sizeof ( char * ) );
    if ( branch == ( char ** ) NULL )
        fprintf ( stderr, "ERROR: branch NULL\n" );

    for ( i = 0; i < num_genomes; i++ )
    {
        branch[i] = ( char * ) malloc ( maxConstSize * sizeof ( char ) );
        if ( branch[i] == ( char * ) NULL )
            fprintf ( stderr, "ERROR: branch[i] NULL\n" );

        strcpy ( branch[i], orig_branch[i] );
    }
    tmpbranch = ( char * ) malloc ( maxConstSize * sizeof ( char ) );
    if ( tmpbranch == ( char * ) NULL )
        fprintf ( stderr, "ERROR: tmpbranch NULL\n" );

    D = ( double ** ) malloc ( num_genomes * sizeof ( double * ) );
    if ( D == ( double ** ) NULL )
        fprintf ( stderr, "ERROR: D NULL\n" );
    for ( i = 0; i < num_genomes; i++ )
    {
        D[i] = ( double * ) malloc ( num_genomes * sizeof ( double ) );
        if ( D[i] == ( double * ) NULL )
            fprintf ( stderr, "ERROR: D[i] NULL\n" );
    }

    /* num_clusters > 2 */
    for ( i = 0; i < num_genomes; i++ )
    {
        if ( valid[i] )
        {
            r[i] = 0.0;
            for ( j = 0; j < num_genomes; j++ )
                if ( ( valid[j] ) && ( i != j ) )
                {
                    if ( i < j )
                        r[i] += orig_D[i][j];
                    else
                        r[i] += orig_D[j][i];
                }
            r[i] /= ( ( double ) num_clusters - 2.0 );
        }
    }

    minSij = DBL_INF;

    for ( i = 0; i < num_genomes; i++ )
    {
        if ( valid[i] )
        {
            for ( j = i + 1; j < num_genomes; j++ )
            {
                if ( valid[j] )
                {
                    Sij = orig_D[i][j] - ( r[i] + r[j] );
#ifdef DEBUG
                    fprintf ( outfile, "S[%3d][%3d]: %5.2f\n", i, j, Sij );
#endif
                    if ( Sij < minSij )
                    {
                        minSij = Sij;
                    }
                }
            }
        }
    }

    if ( minSij == DBL_INF )
        fprintf ( stderr, "ERROR: minSij Infinite\n" );

    for ( i = 0; i < num_genomes; i++ )
    {
        if ( valid[i] )
        {
            for ( j = i + 1; j < num_genomes; j++ )
            {
                if ( valid[j] )
                {
                    if ( fabs ( minSij - ( orig_D[i][j] - ( r[i] + r[j] ) ) )
                         < SMALLDELTA )
                    {
                        /* Merge these and recurse */

                        strcpy ( tmpbranch, branch[i] );
                        sprintf ( branch[i], "(%s,%s)", tmpbranch,
                                  branch[j] );

                        valid[j] = FALSE;

                        for ( k = 0; k < num_genomes; k++ )
                            for ( m = k + 1; m < num_genomes; m++ )
                                D[k][m] = orig_D[k][m];

                        Dij = D[i][j];
                        for ( k = 0; k < i; k++ )
                        {
                            if ( valid[k] )
                            {
                                if ( j < k )
                                    D[k][i] =
                                        0.5 * ( D[k][i] + D[j][k] - Dij );
                                else
                                    D[k][i] =
                                        0.5 * ( D[k][i] + D[k][j] - Dij );
                            }
                        }
                        for ( k = i + 1; k < num_genomes; k++ )
                        {
                            if ( valid[k] )
                            {
                                if ( j < k )
                                    D[i][k] =
                                        0.5 * ( D[i][k] + D[j][k] - Dij );
                                else
                                    D[i][k] =
                                        0.5 * ( D[i][k] + D[k][j] - Dij );
                            }
                        }

                        neighborj_SK_trees ( genome_list, num_genes,
                                             num_genomes, distmem, CIRCULAR,
                                             tpool, edgepool, labels,
                                             initmethod, COND, qpool,
                                             adj_list, adj_pool, stack,
                                             degree, otherEnd, neighbors,
                                             smalledges, edges, outcycle,
                                             pred1, pred2, picked, decode,
                                             inittspsolver, triple, incycle,
                                             tspsolver, distmethod, thresh,
                                             weights, status,
                                             num_clusters - 1, D, valid,
                                             branch, maxConstSize,
                                             best_so_far, count, const_env,
                                             genes, condense_succ,
                                             condense_decode, orig_num_genes,
                                             correction, treeString );

                        /* Undo the merge */
                        strcpy ( branch[i], tmpbranch );

                        valid[j] = TRUE;
                    }
                }
            }
        }
    }

    for ( i = 0; i < num_genomes; i++ )
        free ( D[i] );
    free ( D );

    free ( tmpbranch );
    for ( i = 0; i < num_genomes; i++ )
        free ( branch[i] );
    free ( branch );

    free ( valid );
    free ( r );

    return;
}


int
neighborj_score ( struct genome_struct *genome_list, int num_genes,
                  int num_genomes,
                  distmem_t * distmem, int CIRCULAR,
                  struct tNode *tpool, int *edgepool,
                  struct genome_struct *labels,
                  int initmethod, int COND,
                  struct qNode *qpool,
                  struct adj_struct *adj_list, struct adj_struct *adj_pool,
                  int *stack, int *degree, int *otherEnd,
                  intpair_t * neighbors, smalledge_t * smalledges,
                  edge_t * edges, int *outcycle, int *pred1, int *pred2,
                  int *picked, int *decode, int inittspsolver,
                  triple_t * triple, int *incycle, int tspsolver,
                  int distmethod, int thresh, int **weights, int **status,
                  env_t * const_env, int *genes, int *condense_succ,
                  int *condense_decode, int orig_num_genes, int correction,
                  char *treeString )
{
    int i, j;
    int score;
    double **D;
    int *valid;
    char **branch;
    int maxConstSize;
    int count;

    D = ( double ** ) malloc ( num_genomes * sizeof ( double * ) );
    if ( D == ( double ** ) NULL )
        fprintf ( stderr, "ERROR: D NULL\n" );
    for ( i = 0; i < num_genomes; i++ )
    {
        D[i] = ( double * ) malloc ( num_genomes * sizeof ( double ) );
        if ( D[i] == ( double * ) NULL )
            fprintf ( stderr, "ERROR: D[i] NULL\n" );
    }

    valid = ( int * ) malloc ( num_genomes * sizeof ( int ) );
    if ( valid == ( int * ) NULL )
        fprintf ( stderr, "ERROR: valid NULL\n" );

    for ( i = 0; i < num_genomes; i++ )
        valid[i] = TRUE;

    for ( i = 0; i < num_genomes; i++ )
    {
        for ( j = i + 1; j < num_genomes; j++ )
        {
            if ( CIRCULAR )
                D[i][j] =
                    ( double ) invdist_circular ( genome_list + i,
                                                  genome_list + j, num_genes,
                                                  distmem );
            else
                D[i][j] =
                    ( double ) invdist_noncircular ( genome_list + i,
                                                     genome_list + j, 0,
                                                     num_genes, distmem );
        }
    }

    branch = ( char ** ) malloc ( num_genomes * sizeof ( char * ) );
    if ( branch == ( char ** ) NULL )
        fprintf ( stderr, "ERROR: branch NULL\n" );

    maxConstSize =
        ( ( int ) ceil ( log10 ( ( double ) num_genomes ) ) +
          4 ) * num_genomes + 1;

    for ( i = 0; i < num_genomes; i++ )
    {
        branch[i] = ( char * ) malloc ( maxConstSize * sizeof ( char ) );
        if ( branch[i] == ( char * ) NULL )
            fprintf ( stderr, "ERROR: branch[i] NULL\n" );

        sprintf ( branch[i], "%d", i + 1 );
    }

    score = INT_INF;

    count = 0;

    neighborj_SK_trees ( genome_list, num_genes, num_genomes,
                         distmem, CIRCULAR, tpool, edgepool,
                         labels, initmethod, COND, qpool,
                         adj_list, adj_pool, stack, degree,
                         otherEnd, neighbors, smalledges, edges, outcycle,
                         pred1, pred2, picked, decode, inittspsolver,
                         triple, incycle, tspsolver, distmethod,
                         thresh, weights, status,
                         num_genomes, D, valid, branch, maxConstSize,
                         &score, &count,
                         const_env, genes,
                         condense_succ, condense_decode,
                         orig_num_genes, correction, treeString );

#ifdef MPBPA
    if ( MYPROC == 0 )
    {
#endif
        fprintf ( outfile,
                  "Number of neighbor-joining trees evaluated: %12d\n\n",
                  count );
        fflush ( outfile );
#ifdef MPBPA
    }
#endif


    for ( i = 0; i < num_genomes; i++ )
        free ( branch[i] );
    free ( branch );

    free ( valid );
    for ( i = 0; i < num_genomes; i++ )
        free ( D[i] );
    free ( D );

    return ( score );
}
