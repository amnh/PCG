/* this is the file to define the function interface to find the max possible
   circular lower bound and the min possible bound.
   The idea is to build max or min TSP instances from the input ordering
   of genomes and solve this instance to get the ordering yields max
   or min cost, most of the code is copied from the bbtsp.c.
   The function is implemented by Jijun Tang, Jijun can be reached at
   tangjijun@hotmail.com
*/


#include "circular_ordering.h"

int
circular_ordering ( int num_genomes, int num_genes,
                    int flag, intpair_t * neighbors, int **distmatrix,
                    int *stack, int *outcycle, int *degree, int *otherEnd,
                    edge_t * edges1, int CIRCULAR )
{
    /* uses coalesced simple path heuristic to build tour */

    /* num_genomes is effectively a constant */
    /* outcycle is the returned tour */
    /* pointers to arrays adj_list, degree, neighbors, and otherEnd are
       shifted to the middle to allow negative indexing */
    /* pointers to arrays outcycle, and edges are not shifted */

    int i, j, k;
    int *count;
    edge_t **sortedges, *newedge, *edges;
    int score, picked, maxweight;
    int numedges;
    struct adj_struct *node = NULL, *tmp;
    int otherEndI, otherEndJ;
    int *visited;
    int curcity, nextcity;
    int lowscore, highscore;
    struct adj_struct *adj_list;

    maxweight = -1;
    for ( i = 0; i < num_genomes; i++ )
    {
        for ( j = i + 1; j < num_genomes; j++ )
        {
            if ( distmatrix[i][j] > maxweight )
                maxweight = distmatrix[i][j];
        }
    }
    maxweight += 1;
    count = ( int * ) malloc ( sizeof ( int ) * maxweight );
    sortedges = ( edge_t ** ) malloc ( sizeof ( edge_t * ) * maxweight );

    adj_list =
        ( struct adj_struct * ) malloc ( ( num_genomes ) *
                                         sizeof ( struct adj_struct ) );

    /* create adj-list */
    for ( i = 0; i < num_genomes; i++ )
    {
        if ( i == num_genomes - 1 )
        {
            adj_list[i].next = NULL;
        }
        else
        {
            adj_list[i].next =
                ( struct adj_struct * )
                malloc ( sizeof ( struct adj_struct ) );
            node = adj_list[i].next;
        }
        for ( j = i + 1; j < num_genomes; j++ )
        {
            node->weight = distmatrix[i][j];
            node->vertex = j;
            if ( j == num_genomes - 1 )
                node->next = NULL;
            else
            {
                node->next =
                    ( struct adj_struct * )
                    malloc ( sizeof ( struct adj_struct ) );
                node = node->next;
            }
        }

    }
    /* uses positive/negative indexing and so position 0 in arrays
       is always wasted */

    /* prepare a sorted list of edges (in increasing order of cost) */

    /* use distribution sort -- here ad hoc, since we only have 4 values:
       0, 1, 2, and L = -largevalue */
    /* could just use an array of pointers since adj. lists are sorted... */

    /* tally values */
    for ( i = 0; i < maxweight; i++ )
    {
        count[i] = 0;
    }
    for ( i = 0; i < num_genomes; i++ )
    {
        node = adj_list[i].next;
        while ( node != NULL )
        {
            j = node->vertex;
            if ( i != j )
            {
                count[node->weight]++;
            }
            node = node->next;
        }
    }
    numedges = 0;
    for ( i = 0; i < maxweight; i++ )
    {
        numedges += count[i];
    }
    edges = ( edge_t * ) malloc ( sizeof ( edge_t ) * ( numedges + 10 ) );
    sortedges[0] = edges;
    for ( i = 1; i < maxweight; i++ )
    {
        sortedges[i] = sortedges[i - 1] + count[i - 1];
    }

    for ( i = 0; i < num_genomes; i++ )
    {
        node = adj_list[i].next;
        while ( node != NULL )
        {
            j = node->vertex;
            if ( i != j )
            {
                k = node->weight;
                ( sortedges[k] )->edge1 = node;
                ( sortedges[k] )->I = i;
                ( sortedges[k] )->J = j;
                ( sortedges[k] )++;
            }
            node = node->next;
        }
    }
    for ( i = 0; i < num_genomes; i++ )
        degree[i] = 0;


    /* initialize global variables and structures */
    score = 0;                  /* total value of edges picked so far */
    lowscore = LARGENUM;
    highscore = 0;
    picked = 0;                 /* number of edges picked so far */
    if ( flag == 0 )
        newedge = edges + numedges - 1; /*no need to use + */
    else
        newedge = edges;
    /* Initialize otherEnd[city] to itself */
    for ( i = 0; i < num_genomes; i++ )
        otherEnd[i] = i;

    /* initial best solution is actually empty (beyond g/-g edges */
    for ( i = 0; i < num_genomes; i++ )
    {
        stack[i] = NOEDGE;      /* no such edge */
        neighbors[i].A = -1;
        neighbors[i].B = -1;
    }

    /* greedily include all cheap edges we can */
    while ( ( picked < num_genomes ) && ( newedge < ( edges + numedges ) ) )
    {
        i = newedge->I;
        j = newedge->J;
        /* attempt to include newedge */
        if ( ( degree[i] <= 1 ) && ( degree[j] <= 1 ) &&
             ( ( otherEnd[i] != j ) || ( picked == num_genomes - 1 ) ) )
        {
            stack[picked] = newedge - edges;    /* actual index in edges array */
            maxweight = newedge->edge1->weight;
            if ( maxweight > highscore )
                highscore = maxweight;
            if ( maxweight < lowscore )
                lowscore = maxweight;
            score += maxweight;
            otherEndI = otherEnd[i];
            otherEndJ = otherEnd[j];
            otherEnd[otherEndI] = otherEndJ;
            otherEnd[otherEndJ] = otherEndI;
            if ( neighbors[i].A == -1 )
                neighbors[i].A = j;
            else
                neighbors[i].B = j;
            if ( neighbors[j].A == -1 )
                neighbors[j].A = i;
            else
                neighbors[j].B = i;

            degree[i] += 1;
            degree[j] += 1;
            picked++;
        }
        if ( flag == 0 )
            newedge--;          /* changed to ++ */
        else
            newedge++;
    }

    /* stack at this point is a stack of edges of cost 0, 1, or 2
       in the optimal solution.  Complete it into a full tour by using
       whatever cost 3 edges are needed and store results in outcycle. */
    visited = otherEnd;         /* reuse existing space */
    visited[0] = TRUE;
    for ( i = 1; i <= num_genomes; i++ )
        visited[i] = FALSE;

    for ( i = 0; i < num_genomes; i++ )
        outcycle[i] = stack[i];

    curcity = 0;                /* not entirely arbitrary -- must match decode
                                   at end of bbtsp */

    /* Note that city 0 may pick either of two edges, but this is okay */
    outcycle[0] = curcity;
    visited[curcity] = TRUE;
    for ( i = 1; i < num_genomes; i++ )
    {
        nextcity = neighbors[curcity].A;
        if ( visited[nextcity] == TRUE )
        {
            nextcity = neighbors[curcity].B;
        }
        /* Now j is a valid city to visit next */
        curcity = nextcity;
        outcycle[i] = curcity;
        visited[curcity] = TRUE;
    }
    for ( i = 0; i < num_genomes; i++ )
    {
        node = adj_list[i].next;
        while ( node != NULL )
        {
            tmp = node->next;
            free ( node );
            node = tmp;
        }
    }
    free ( adj_list );
    free ( count );
    free ( sortedges );
    free ( edges );
    if ( flag == 0 )
        return score + highscore;
    else
        return score;
}
