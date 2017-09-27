#include "structs.h"
#include "convert.h"
#include "binencode.h"

void
printWeights ( int **adj_mat, int num_genes )
{
    int i, j;
    int num;

    num = 2 * num_genes;

    fprintf ( outfile, "    \t" );
    for ( j = 1; j <= num; j++ )
    {
        if ( j <= num_genes )
            fprintf ( outfile, "%3d\t", j );
        else
            fprintf ( outfile, "%3d\t", -( j - num_genes ) );
    }
    fprintf ( outfile, "\n" );
    for ( i = 1; i <= num; i++ )
    {
        if ( i <= num_genes )
            fprintf ( outfile, "%3d:\t", i );
        else
            fprintf ( outfile, "%3d:\t", -( i - num_genes ) );

        for ( j = 1; j <= num; j++ )
        {
            fprintf ( outfile, "%3d\t", adj_mat[i - 1][j - 1] );
        }
        fprintf ( outfile, "\n" );
    }
    fprintf ( outfile, "\n" );
    return;
}

/* Take 3 genomes and record the weights. This is to convert the median
   of 3 genomes problem into a tsp problem. This is done by recording all
   adjacencies and how many times they occur in the 3 genomes -- 0,1,2 or 3
 */
void
convert_to_tsp ( struct genome_struct *g1,
                 struct genome_struct *g2,
                 struct genome_struct *g3,
                 int num_genes, int CIRCULAR, int **weights )
{
    int i, i1, i2;
    int g;
    int maxpos;

    init_weights ( weights, num_genes );

    if ( CIRCULAR )
    {
        maxpos = num_genes;
    }
    else
    {
        maxpos = num_genes - 1;
    }

    /* For all adjacencies (i,j), weights[-i][j] is decremented
       and weights[j][-i] is decremented.
     */
    for ( i = 0; i < maxpos; i++ )
    {
        /* First genome */
        g = g1->genes[i];
        if ( g < 0 )
            i1 = -g;
        else
            i1 = g + num_genes;

        g = g1->genes[( i + 1 ) % num_genes];
        if ( g < 0 )
            i2 = num_genes - g;
        else
            i2 = g;

        weights[i1 - 1][i2 - 1]--;
        weights[i2 - 1][i1 - 1]--;

        /* Second genome */
        g = g2->genes[i];
        if ( g < 0 )
            i1 = -g;
        else
            i1 = g + num_genes;

        g = g2->genes[( i + 1 ) % num_genes];
        if ( g < 0 )
            i2 = num_genes - g;
        else
            i2 = g;

        weights[i1 - 1][i2 - 1]--;
        weights[i2 - 1][i1 - 1]--;

        /* Third genome */
        g = g3->genes[i];
        if ( g < 0 )
            i1 = -g;
        else
            i1 = g + num_genes;

        g = g3->genes[( i + 1 ) % num_genes];
        if ( g < 0 )
            i2 = num_genes - g;
        else
            i2 = g;

        weights[i1 - 1][i2 - 1]--;
        weights[i2 - 1][i1 - 1]--;
    }
#if 0
    printGenomes ( g1, 1, num_genes );
    printGenomes ( g2, 1, num_genes );
    printGenomes ( g3, 1, num_genes );
    printWeights ( weights, num_genes );
#endif
    return;
}

/* Should it get inlined? */
void
handle ( int i1, int i2, struct adj_struct *adj_list,
         struct adj_struct **slot, int num_genes )
{
    struct adj_struct *pred, *node, *runner, *entry;
    int found;

    entry = adj_list + i1;      /* may speed up assignments */

    /* do we already have an edge (i1,i2)? */
    pred = NULL;
    node = entry->next;         /* effectively adj_list[i1].next */
    found = FALSE;
    while ( ( !found ) && ( node != NULL ) )
    {
        if ( node->vertex == i2 )
        {
            found = TRUE;
        }
        else
        {
            pred = node;
            node = node->next;
        }
    }
    if ( found )
    {                           /* edge already in list */
        /* remove the node */
        if ( pred == NULL )
        {
            entry->next = node->next;
        }
        else
        {
            pred->next = node->next;
        }
        /* decrease its weight */
        ( node->weight )--;
        /* insert it front of the first node of same or larger weight */
        /* first find the first node of same or larger weight */
        pred = NULL;
        runner = entry->next;
        while ( runner != NULL )
        {
            if ( runner->weight >= node->weight )
            {
                break;
                /* goto ready_to_insert;  break */
            }
            else
            {
                pred = runner;
                runner = runner->next;
            }
        }
        /* ready_to_insert: */
        /* now insert in front of it */
        node->next = runner;
        if ( pred == NULL )
        {
            entry->next = node;
        }
        else
        {
            pred->next = node;
        }
    }
    else
    {                           /* new edge, so need a new node from the pool */
        /* set up the node */
        ( *slot )->vertex = i2;
        ( *slot )->weight = 2;
        ( *slot )->status = STAT_AVAIL;
        ( *slot )->next = NULL;
        /* insert it at end of list */
        if ( pred == NULL )
        {
            entry->next = *slot;
        }
        else
        {
            pred->next = *slot;
        }
        /* one less free node in the pool */
        *slot += 1;
    }
    return;
}

/* Take 3 genomes and create the adj. list, recording the weights.
   This is to convert the median of 3 genomes problem into a tsp problem
   for the bbtsp solver.  */
/* This is done by recording all adjacencies and how many times they occur
   in the 3 genomes -- 0,1,2 or 3 */
void
convert2_to_tsp ( struct genome_struct *g1,
                  struct genome_struct *g2,
                  struct genome_struct *g3,
                  struct adj_struct *adj_list,
                  struct adj_struct *adj_pool, int num_genes, int CIRCULAR )
{
    int i, i1, i2, j1, j2, k1, k2, i1first, j1first, k1first;
    struct adj_struct *slot;
#ifdef VERYVERBOSE
    struct adj_struct *node;
#endif

    /* Do it brute force: scan each genome in turn; for each adjacency,
       check if it exists in the adjacency lists; if so, decrement its
       weight; otherwise create a new node in the list and initialize
       its weight to 2.  Cost is still linear, because each node can
       only have a cst number of edges.
       Extra work is required to keep the list sorted -- possible
       weights are -largevalue, 0, 1, and 2.
       Special handling is needed for the (g,-g) edges, since their
       weight is a fixed negative constant */

    /* note: no coding/decoding, so adj_list needs adjusting */
    adj_list += num_genes;

    /* build adjacency lists */
    for ( i = -num_genes; i <= num_genes; i++ )
        adj_list[i].next = NULL;

    /* set up pool pointer */
    slot = adj_pool;

    /* For speed, handle the num_genes genes in three stages:
       1) the first adjacency, 0--1
       2) the next num_genes-2 adjacencies, 1--...--num_genes-1
       3) the last adjacency, num_genes-1--0 */

    /* Stage 1 */
    /* First genome */
    i1 = i1first = g1->genes[0];
    i2 = -g1->genes[1];
    /* now we have the endpoints of the edge */
    handle ( i1, i2, adj_list, &slot, num_genes );
    handle ( i2, i1, adj_list, &slot, num_genes );

    /* Second genome */
    j1 = j1first = g2->genes[0];
    j2 = -g2->genes[1];
    /* now we have the endpoints of the edge */
    handle ( j1, j2, adj_list, &slot, num_genes );
    handle ( j2, j1, adj_list, &slot, num_genes );

    /* Third genome */
    k1 = k1first = g3->genes[0];
    k2 = -g3->genes[1];
    /* now we have the endpoints of the edge */
    handle ( k1, k2, adj_list, &slot, num_genes );
    handle ( k2, k1, adj_list, &slot, num_genes );

    /* Stage 2 */
    /* process the three genomes, one gene pair at a time */
    for ( i = 1; i < num_genes - 1; i++ )
    {

        /* First genome */
        /* g = g1->genes[i]; Should be already set */
        i1 = -i2;
        i2 = -g1->genes[i + 1];
        /* now we have the endpoints of the edge */
        handle ( i1, i2, adj_list, &slot, num_genes );
        handle ( i2, i1, adj_list, &slot, num_genes );

        /* Second genome */
        /* g = g2->genes[i]; */
        j1 = -j2;
        j2 = -g2->genes[i + 1];
        /* now we have the endpoints of the edge */
        handle ( j1, j2, adj_list, &slot, num_genes );
        handle ( j2, j1, adj_list, &slot, num_genes );

        /* Third genome */
        /* g = g3->genes[i]; */
        k1 = -k2;
        k2 = -g3->genes[i + 1];
        /* now we have the endpoints of the edge */
        handle ( k1, k2, adj_list, &slot, num_genes );
        handle ( k2, k1, adj_list, &slot, num_genes );
    }

    if ( CIRCULAR )
    {

        /* Stage 3 */
        /* First genome */
        i1 = -i2;
        i2 = -i1first;
        /* now we have the endpoints of the edge */
        handle ( i1, i2, adj_list, &slot, num_genes );
        handle ( i2, i1, adj_list, &slot, num_genes );

        /* Second genome */
        j1 = -j2;
        j2 = -j1first;
        /* now we have the endpoints of the edge */
        handle ( j1, j2, adj_list, &slot, num_genes );
        handle ( j2, j1, adj_list, &slot, num_genes );

        /* Third genome */
        k1 = -k2;
        k2 = -k1first;
        /* now we have the endpoints of the edge */
        handle ( k1, k2, adj_list, &slot, num_genes );
        handle ( k2, k1, adj_list, &slot, num_genes );

    }

    /* Add (g,-g) edges, one in front of each list */
    for ( i = -num_genes; i < 0; i++ )
    {
        slot->vertex = -i;
        slot->weight = -LARGENUM;
        slot->status = STAT_INCLUDED;
        slot->next = adj_list[i].next;
        adj_list[i].next = slot;
        slot++;
    }
    for ( i = 1; i <= num_genes; i++ )
    {
        slot->vertex = -i;
        slot->weight = -LARGENUM;
        slot->status = STAT_INCLUDED;
        slot->next = adj_list[i].next;
        adj_list[i].next = slot;
        slot++;
    }

#ifdef VERYVERBOSE
    for ( i = -num_genes; i <= num_genes; i++ )
    {
        fprintf ( outfile, "Node %3d is adjacent to: ", i );
        node = adj_list[i].next;
        while ( node != NULL )
        {
            fprintf ( outfile, "%3d (weight %4d, status %1d) ",
                      node->vertex, node->weight, node->status );
            node = node->next;
        }
        fprintf ( outfile, "\n" );
    }
    fflush ( outfile );
#endif
    return;
}

/* Write out score matrix to a TSPLIB file so it can be solved
   approximately by the linkern tsp solver. Writes out whole matrix.
*/
#if 0
void
write_TSPLIB_file ( char *inputfile )
{
    int i, j;
    FILE *tsplib;
    tsplib = fopen ( "tsplib", "w" );

    fprintf ( tsplib, "NAME: %s\nTYPE: TSP\nCOMMENT:\n", inputfile );
    fprintf ( tsplib, "DIMENSION: %d\n", 2 * num_genes );
    fprintf ( tsplib, "EDGE_WEIGHT_TYPE: EXPLICIT\n" );
    fprintf ( tsplib, "EDGE_WEIGHT_FORMAT: FULL_MATRIX\n" );
    fprintf ( tsplib, "EDGE_WEIGHT_SECTION\n" );
    for ( i = 0; i < 2 * num_genes; i++ )
    {
        for ( j = 0; j < 2 * num_genes; j++ )
        {
            fprintf ( tsplib, "%d ", weights[i][j] );
        }
        fprintf ( tsplib, "\n" );
    }
    fprintf ( tsplib, "EOF\n" );
    fclose ( tsplib );
}
#endif

/* Take a genome and turn it into an adjacency matrix representation. Do the
   same thing to represent negative genes -- -i becomes -1 * -i + num_genes.
*/

#if 0
void
genome_to_adj ( int **adj_matrix, struct genome_struct *genome,
                int num_genes, int NUM_GENOMES )
{

    int i, i1, i2;

    /* if i or i+1 are negative, multiply by -1 and add num_genes  
       to adjust for index. Add 1 to all to make it 1-based.
     */
    for ( i = 0; i < num_genes; i++ )
    {
        if ( genome->genes[i] < 0 )
        {
            i1 = 1 + num_genes + ( abs ( genome->genes[i] ) );
        }
        else
        {
            i1 = 1 + genome->genes[i];
        }
        if ( genome->genes[( i + 1 ) % num_genes] < 0 )
        {
            i2 = 1 + num_genes +
                ( abs ( genome->genes[( i + 1 ) % num_genes] ) );
        }
        else
        {
            i2 = 1 + genome->genes[( i + 1 ) % num_genes];
        }

        adj_matrix[i1 - 1][i2 - 1] += 1;
    }

    return;
}
#endif

void
init_weights ( int **weights, int num_genes )
{
    int i, j;
    int lim;

    lim = 2 * num_genes;

    for ( i = 0; i < lim; i++ )
    {
        for ( j = i + 1; j < lim; j++ )
        {
            if ( ( j == i + num_genes ) || ( i == j + num_genes ) )
            {
                weights[i][j] = -LARGENUM;
                weights[j][i] = -LARGENUM;
            }
            else
            {
                weights[i][j] = 3;
                weights[j][i] = 3;
            }
        }
    }

    for ( i = 0; i < lim; i++ )
        weights[i][i] = LARGENUM;

    return;
}
