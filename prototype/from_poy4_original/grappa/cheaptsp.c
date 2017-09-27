#include "structs.h"
#include "bbtsp.h"
#include "math.h"

int
coalestsp ( int ncount, int *tour, int use_median, int *g1, int *g2, int *g3,
            struct adj_struct *adj_list, intpair_t * neighbors,
            int *stack, int *outcycle, int *degree, int *otherEnd,
            edge_t * edges, int CIRCULAR )
{
    /* uses coalesced simple path heuristic to build tour */

    /* ncount is effectively a constant */
    /* outcycle is the returned tour */
    /* pointers to arrays adj_list, degree, neighbors, and otherEnd are
       shifted to the middle to allow negative indexing */
    /* pointers to arrays outcycle, and edges are not shifted */

    int num_genes;
    int i, j, top;
    int count0, count1, count2, countL;
    edge_t *edges0, *edges1, *edges2, *edgesL, *newedge;
    int score, picked;
    int numedges, negEdges;
    struct adj_struct *node;
    int otherEndI, otherEndJ;
    int *visited;
    int curcity, nextcity;

    num_genes = ncount / 2;

#ifdef VERBOSE
    fprintf ( outfile, "Running coalestsp on %4d cities... \n", ncount );
    fprintf ( outfile, "first genome is: " );
    for ( i = 0; i < num_genes; i++ )
        fprintf ( outfile, " %3d", g1[i] );
    fprintf ( outfile, "\n" );
    fprintf ( outfile, "second genome is: " );
    for ( i = 0; i < num_genes; i++ )
        fprintf ( outfile, " %3d", g2[i] );
    fprintf ( outfile, "\n" );
    fprintf ( outfile, "third genome is: " );
    for ( i = 0; i < num_genes; i++ )
        fprintf ( outfile, " %3d", g3[i] );
    fprintf ( outfile, "\n" );
    fflush ( outfile );
#endif
    /* uses positive/negative indexing and so position 0 in arrays
       is always wasted */

    /* shift array index to middle to improve efficiency of loops below */
    adj_list += num_genes;
    otherEnd += num_genes;
    degree += num_genes;
    neighbors += num_genes;

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

    /* prepare a sorted list of edges (in increasing order of cost) */

    /* use distribution sort -- here ad hoc, since we only have 4 values:
       0, 1, 2, and L = -largevalue */
    /* could just use an array of pointers since adj. lists are sorted... */

    /* tally values */
    countL = count0 = count1 = count2 = 0;
    for ( i = -num_genes; i <= num_genes; i++ )
    {
        node = adj_list[i].next;
        while ( node != NULL )
        {
            j = node->vertex;
            if ( i < j )
            {
                switch ( node->weight )
                {
                    case 0:
                        count0++;
                        break;
                    case 1:
                        count1++;
                        break;
                    case 2:
                        count2++;
                        break;
                    default:
                        countL++;
                        break;
                }
            }
            node = node->next;
        }
    }
    numedges = count0 + count1 + count2 + countL;
    negEdges = countL;

    edgesL = edges;
    edges0 = edgesL + countL;
    edges1 = edges0 + count0;
    edges2 = edges1 + count1;
    for ( i = -num_genes; i <= num_genes; i++ )
    {
        node = adj_list[i].next;
        while ( node != NULL )
        {
            j = node->vertex;
            if ( i < j )
            {
                switch ( node->weight )
                {
                    case 0:
                        edges0->edge1 = node;
                        edges0->I = i;
                        edges0->J = j;
                        edges0++;
                        break;
                    case 1:
                        edges1->edge1 = node;
                        edges1->I = i;
                        edges1->J = j;
                        edges1++;
                        break;
                    case 2:
                        edges2->edge1 = node;
                        edges2->I = i;
                        edges2->J = j;
                        edges2++;
                        break;
                    default:
                        edgesL->edge1 = node;
                        edgesL->I = i;
                        edgesL->J = j;
                        edgesL++;
                        break;
                }
            }
            node = node->next;
        }
    }
    /* initialize degree to all 1s, to reflect the fact that
       the (g,-g) edges are forcibly included */
    for ( i = -num_genes; i < 0; i++ )
        degree[i] = 1;
    degree[0] = 0;              /* thus will never be used */
    for ( i = 1; i <= num_genes; i++ )
        degree[i] = 1;

#ifdef DEBUG
    if ( negEdges != num_genes )
    {
        fprintf ( outfile, "ERROR: negative edges: %d  ncount/2: %d\n",
                  negEdges, ncount / 2 );
        fflush ( outfile );
        for ( i = 0; i < numedges; i++ )
            fprintf ( outfile, "ERROR:  Edge[%3d]: (%4d, %4d), wt: %7d\n",
                      i, edges[i].I, edges[i].J, edges[i].edge1->weight );
        fprintf ( outfile, "\n" );
    }
    fflush ( outfile );
#endif

    /* initialize global variables and structures */
    score = 0;                  /* total value of edges picked so far */
    picked = 0;                 /* number of edges picked so far */
    newedge = edges;
    /* Initialize otherEnd[city] to itself */
    for ( i = -num_genes; i <= num_genes; i++ )
        otherEnd[i] = i;
    /* initially include -Z weight edges (g,-g) */
    while ( newedge->edge1->weight < 0 )
    {
        stack[picked] = picked;
        i = newedge->I;
        j = newedge->J;
        otherEnd[i] = j;
        otherEnd[j] = i;
        neighbors[i].A = j;
        neighbors[j].A = i;
        score += newedge->edge1->weight;
        picked++;
        newedge++;
    }
#ifdef DEBUG
    if ( picked != negEdges )
        fprintf ( outfile, "ERROR: picked: %d  negEdges: %d\n", picked,
                  negEdges );
    fflush ( outfile );
#endif

    /* initial best solution is actually empty (beyond g/-g edges */
    for ( i = negEdges; i < ncount; i++ )
        stack[i] = NOEDGE;      /* no such edge */

    /* greedily include all cheap edges we can */
    while ( ( picked < ncount ) && ( newedge < ( edges + numedges ) ) )
    {
        i = newedge->I;
        j = newedge->J;
#ifdef DEBUG
        fprintf ( outfile, "checking edge (%3d,%3d) for inclusion\n", i, j );
        fflush ( outfile );
#endif
        /* attempt to include newedge */
        if ( ( degree[i] <= 1 ) && ( degree[j] <= 1 ) &&
             ( ( otherEnd[i] != j ) || ( picked == ncount - 1 ) ) )
        {
#ifdef DEBUG
            fprintf ( outfile, "OK, pick it\n" );
            fflush ( outfile );
#endif
            stack[picked] = newedge - edges;    /* actual index in edges array */
            score += newedge->edge1->weight;
            otherEndI = otherEnd[i];
            otherEndJ = otherEnd[j];
            otherEnd[otherEndI] = otherEndJ;
            otherEnd[otherEndJ] = otherEndI;
            neighbors[i].B = j;
            neighbors[j].B = i;
            degree[i] += 1;
            degree[j] += 1;
            picked++;
        }
        newedge++;
    }
    /* any missing edges are from the expensive pool */
    score += 3 * ( ncount - picked );

    /* stack at this point is a stack of edges of cost 0, 1, or 2
       in the optimal solution.  Complete it into a full tour by using
       whatever cost 3 edges are needed and store results in outcycle. */
    visited = otherEnd;         /* reuse existing space */
    for ( i = -num_genes; i < 0; i++ )
        visited[i] = FALSE;
    visited[0] = TRUE;
    for ( i = 1; i <= num_genes; i++ )
        visited[i] = FALSE;

    for ( i = 0; i < ncount; i++ )
        outcycle[i] = stack[i];

    /* push on a stack cities of degree 1, if any */
    top = 0;
    for ( i = -num_genes; i <= num_genes; i++ )
    {
        if ( degree[i] == 1 )
        {
            stack[top] = i;
            top++;
        }
    }

#ifdef DEBUG
    fprintf ( outfile, "converting tour: neighbors computed are:\n" );
    for ( i = -num_genes; i <= num_genes; i++ )
        fprintf ( outfile, "neighbors[%3d].A=%3d, neighbors[%3d].B=%3d\n", i,
                  neighbors[i].A, i, neighbors[i].B );
    fflush ( outfile );
#endif

    /* Find the starting city, either a city of degree 1
       or any city if all are degree 2 */
    if ( top > 0 )
    {
        top--;
        curcity = stack[top];
    }
    else
    {
        curcity = 1;            /* not entirely arbitrary -- must match decode
                                   at end of bbtsp */
    }

    /* Note that city 0 may pick either of two edges, but this is okay */
    outcycle[0] = curcity;
    visited[curcity] = TRUE;
    for ( i = 1; i < ncount; i++ )
    {
        if ( degree[curcity] == 2 )
        {
            /* Use one of the unvisited neighbors */
            nextcity = neighbors[curcity].A;
            if ( visited[nextcity] == TRUE )
            {
                nextcity = neighbors[curcity].B;
            }
        }
        else
        {
            /* Search for a degree 1 unvisited city */
            nextcity = neighbors[curcity].A;
            if ( visited[nextcity] == TRUE )
            {
                /* Search for a j unvisited degree 1 city */
                while ( top > 0 )
                {
                    top--;
                    if ( !visited[stack[top]] )
                        break;
                }
                if ( top >= 0 )
                    nextcity = stack[top];
            }

        }
        /* Now j is a valid city to visit next */
        curcity = nextcity;
        outcycle[i] = curcity;
        visited[curcity] = TRUE;
    }
#ifdef VERBOSE
    for ( i = 0; i < ncount; i++ )
        fprintf ( outfile, "Tour[%3d]: %3d\n", i, outcycle[i] );
#endif

    /* now turn outcyle back into genes */
    if ( outcycle[0] != -outcycle[1] )
    {
        for ( i = 0; i < num_genes; i++ )
        {
            tour[i] = outcycle[2 * i];
#ifdef VERBOSE
            fprintf ( outfile, "tour[%3d]=%3d\n", i, tour[i] );
            fflush ( outfile );
#endif
        }
    }
    else
    {
        for ( i = 0; i < num_genes; i++ )
        {
            tour[i] = outcycle[( 2 * i + 1 ) % ncount];
#ifdef VERBOSE
            fprintf ( outfile, "tour[%3d]=%3d\n", i, tour[i] );
            fflush ( outfile );
#endif
        }
    }
    return score;
}
