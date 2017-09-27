#include "structs.h"
#include "bbtsp.h"
#include "math.h"

/* Solves TSP exactly by dfs with pruning, using the lower bound
   suggested by Sankoff and Blanchette */


void
rec_dfs ( int ncount, int *degree, struct adj_struct *adj_list,
          int *stack, int *outcycle, int *best, int score,
          int picked, edge_t * edges, int numedges, int *otherEnd, int level )
{
    /* use ordering of edges; left branch is include,
       right branch is exclude; do a dfs of the tree (so that include
       always gets explored first), keeping the best so far and using
       that and the lower bound to prune branches */

    struct adj_struct *node;
    int num_genes;
    int lb_new;
    int i, j;
    int new_i = -1, new_j = -1;
    int orig_status;
    int otherEndI, otherEndJ;
    edge_t *newedge = NULL;

    num_genes = ncount / 2;

#ifdef VERBOSE
    fprintf ( outfile, "level: %3d,  picked: %3d,  score: %7d,  best: %7d\n",
              level, picked, score, *best );
#ifdef VERYVERBOSE
    fprintf ( outfile, "Degrees:\n" );
    for ( i = -num_genes; i <= num_genes; i++ )
    {
        fprintf ( outfile, "degree of %3d is %2d", i, degree[i] );
        fprintf ( outfile, "\n" );
    }
    fflush ( outfile );
#endif
#endif
    fflush ( outfile );

    /* Is tour complete?  (we cannot create bad tours) */
    if ( picked == ncount )
    {                           /* uncommon, I presume */
#ifdef VERBOSE
        fprintf ( outfile, "found a full solution\n" );
#endif
        fflush ( outfile );
        /* yes, so we reached bottom of recursion */
        /* is the new solution better (smaller) ? */
        if ( score < *best )
        {
#ifdef VERBOSE
            fprintf ( outfile, "found a better full solution\n" );
            fflush ( outfile );
#endif
            /* yes, so copy stack to outcycle */
            for ( i = 0; i < ncount; i++ )
            {
                outcycle[i] = stack[i];
#ifdef VERBOSE
                fprintf ( outfile, "outcycle[%3d]=%3d\n", i, outcycle[i] );
                fflush ( outfile );
#endif
            }
            *best = score;
        }
        return;
    }

    /* Compute lower bound */
    /* if a vertex has degree 1, add the cheapest edge still available
       that is incident upon it; note that this adds twice as many edges
       as needed, so we'll have to divide by 2 */
    /* note: this routine is fairly expensive (quadratic in the
       number of cities); the same bounds could be derived more
       cheaply by forward and backward work */
    /* further note: if the routine ever returns a value of at least
       500,000, then it means that it was called when the remaining
       edges could not complete a tour -- this should not happen */
    lb_new = 0;
    for ( i = -num_genes; i <= num_genes; i++ )
    {
        if ( degree[i] == 1 )
        {                       /* degree 2 is done; degrees 0 & >2 are impossible */
            node = adj_list[i].next;
            while ( node != NULL )
            {
                j = node->vertex;
                if ( ( node->status == STAT_AVAIL ) && ( degree[j] == 1 ) &&
                     ( ( otherEnd[i] != j ) || ( picked == ncount - 1 ) ) )
                {
                    lb_new += node->weight;
                    goto nexti;
                }
                node = node->next;
            }
            lb_new += 3;        /* no usable edge of lower cost */
          nexti:;
        }
    }
    lb_new = lb_new / 2;
#ifdef VERBOSE
    fprintf ( outfile, "lower bound=%5d\n", lb_new );
    fflush ( outfile );
#endif

    /* continue only if bound promises possible improvement over best-so-far */
    if ( score + lb_new >= *best )
    {
#ifdef VERBOSE
        fprintf ( outfile, "pruning away...\n" );
        fflush ( outfile );
#endif
        return;
    }

    if ( level < numedges )
    {
        /* we have a chance, so get the next edge */
        newedge = edges + level;
        /*    new_i = edges[level].I;
           new_j = edges[level].J; */
        new_i = newedge->I;
        new_j = newedge->J;
#ifdef VERBOSE
        fprintf ( outfile,
                  "new edge (%3d,%3d), degrees (%3d,%3d), weight %1d, otherEnd[%3d]=%3d\n",
                  new_i, new_j, degree[new_i], degree[new_j],
                  edges[level].edge1->weight, new_i, otherEnd[new_i] );
        fflush ( outfile );
#endif
    }

    /* this while loop is not too efficient at finding the next useful edge */
    /* for instance, if degree[new_i]==2, then all edges (i,j) for any j
       can be eliminated -- we should have a way to bump to the next i value */
    while ( ( level < numedges ) &&
            ( ( degree[new_i] == 2 ) ||
              ( degree[new_j] == 2 ) ||
              ( ( otherEnd[new_i] == new_j ) && ( picked != ncount - 1 ) ) ) )
    {                           /* edge is not available, get a new one */
#ifdef VERBOSE
        fprintf ( outfile,
                  "new edge (%3d,%3d), degrees (%3d,%3d), weight %1d, otherEnd[%3d]=%3d\n",
                  new_i, new_j, degree[new_i], degree[new_j],
                  edges[level].edge1->weight, new_i, otherEnd[new_i] );
        fflush ( outfile );
#endif
        level++;
        newedge++;
        if ( level < numedges )
        {
            new_i = newedge->I;
            new_j = newedge->J;
        }
    }
    if ( level == numedges )
    {                           /* could not find any available edge of cost <= 2
                                   so we only have edges of cost 3 left */
        /* Check if current partial solution, when completed, improves on
           best so far; if so, copy it */
#ifdef VERBOSE
        fprintf ( outfile, "found a partial solution\n" );
        fflush ( outfile );
#endif
        if ( score + ( 3 * ( ncount - picked ) ) < *best )
        {
            /* yes, so copy stack to outcycle */
#ifdef VERBOSE
            fprintf ( outfile, "found a better partial solution\n" );
            fflush ( outfile );
#endif
            for ( i = 0; i < ncount; i++ )
            {
                outcycle[i] = stack[i];
#ifdef VERBOSE
                fprintf ( outfile, "outcycle[%3d]=%3d\n", i, outcycle[i] );
                fflush ( outfile );
#endif
            }
            *best = score + 3 * ( ncount - picked );
        }
        return;
    }

    /* we found an edge to include */
#ifdef VERYVERBOSE
    fprintf ( outfile,
              "level %3d, new edge (%3d,%3d), degrees (%3d,%3d), weight %1d, status %3d, otherEnd[%3d]=%3d, otherEnd[%3d]=%3d\n",
              level, new_i, new_j, degree[new_i], degree[new_j],
              edges[level].edge1->weight, edges[level].edge1->status, new_i,
              otherEnd[new_i], new_j, otherEnd[new_j] );
    fflush ( outfile );
#endif

    /* save original status to restore before returning from recursion */
    orig_status = newedge->edge1->status;

    /* prepare tentative inclusion of next edge */

    /* save otherEnd values to restore after call */
    otherEndI = otherEnd[new_i];
    otherEndJ = otherEnd[new_j];

    /* recurse with edge included */
    stack[picked] = level;
    otherEnd[otherEndI] = otherEndJ;
    otherEnd[otherEndJ] = otherEndI;
    degree[new_i]++;
    degree[new_j]++;
    newedge->edge1->status = STAT_INCLUDED;
    newedge->edge2->status = STAT_INCLUDED;

#ifdef VERYVERBOSE
    fprintf ( outfile, "\ninclude: " );
    fflush ( outfile );
#endif
    /* should guard call rather than entry */
    rec_dfs ( ncount, degree, adj_list, stack, outcycle, best,
              score + edges[level].edge1->weight, picked + 1,
              edges, numedges, otherEnd, level + 1 );

    /* restore otherEnd and degree info */
    otherEnd[otherEndI] = new_i;
    otherEnd[otherEndJ] = new_j;
    degree[new_i]--;
    degree[new_j]--;

    /* remove edge from stack */
    stack[picked] = NOEDGE;

    /* set edge to be excluded */
    newedge->edge1->status = STAT_EXCLUDED;
    newedge->edge2->status = STAT_EXCLUDED;

    /* recurse with edge excluded */
#ifdef VERYVERBOSE
    fprintf ( outfile, "\nexclude: " );
    fflush ( outfile );
#endif
    rec_dfs ( ncount, degree, adj_list, stack, outcycle, best,
              score, picked, edges, numedges, otherEnd, level + 1 );

    /* restore edge status */
    newedge->edge1->status = orig_status;
    newedge->edge2->status = orig_status;
    return;
}


void
tsp_dfs ( int ncount, int mincost, int *found_cost, int *degree,
          struct adj_struct *adj_list, intpair_t * neighbors,
          int *stack, int *outcycle, int *otherEnd, edge_t * edges )
{
    /* this is the parent routine, which takes care of initialization,
       then calls the recursive dfs */
    /* ncount is effectively a constant */
    /* so is mincost, which is the tour cost of one of the three genomes */

    /* outcycle is the returned tour */

    /* pointers to arrays adj_list, degree, neighbors, and otherEnd are
       shifted to the middle to allow negative indexing */
    /* pointers to arrays stack, outcycle, and edges are not shifted */

    int num_genes;
    int i, j, ind, top;
    int count0, count1, count2, countL;
    edge_t *edges0, *edges1, *edges2, *edgesL, *newedge;
    int score, best, picked, level;
    int numedges;
    int negEdges;
    struct adj_struct *node;
    int curcity, nextcity;
    int icity, jcity;
    int *visited;

    num_genes = ncount / 2;

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
    /* not done yet: the .edge2 field needs to be initialized too */
    for ( i = 0; i < numedges; i++ )
    {
        ind = edges[i].I;
        node = adj_list[edges[i].J].next;
        while ( node != NULL )
        {
            if ( node->vertex == ind )
            {
                break;
                /* goto got_it;  break */
            }
            else
            {
                node = node->next;
            }
        }
        /* got_it: */
        edges[i].edge2 = node;
    }
    /* initialize degree to all 1s, to reflect the fact that
       the (g,-g) edges are forcibly included */
    for ( i = -num_genes; i < 0; i++ )
        degree[i] = 1;
    degree[0] = 0;              /* thus will never be used */
    for ( i = 1; i <= num_genes; i++ )
        degree[i] = 1;

#ifdef DEBUG
    if ( negEdges != ncount / 2 )
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
#ifdef VERYVERBOSE
    for ( i = 0; i < numedges; i++ )
        fprintf ( outfile,
                  "EDGE (%3d,%3d)=(%3d,%3d) has weight %5d=%5d, status %1d=%1d\n",
                  edges[i].I, edges[i].J, edges[i].edge1->vertex,
                  edges[i].edge2->vertex, edges[i].edge1->weight,
                  edges[i].edge2->weight, edges[i].edge1->status,
                  edges[i].edge2->status );
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

    /* initialize global variables and structures */
    best = 0;                   /* value of best solution found so far */

    score = 0;                  /* total value of edges picked so far */
    picked = 0;                 /* number of edges picked so far */
    level = 0;
    newedge = edges;
    /* Initialize otherEnd[city] to itself */
    for ( i = -num_genes; i <= num_genes; i++ )
        otherEnd[i] = i;
    /* initially include -Z weight edges (g,-g) */
    while ( newedge->edge1->weight < 0 )
    {
        stack[level] = level;
        i = newedge->I;
        j = newedge->J;
        otherEnd[i] = j;
        otherEnd[j] = i;
        score += newedge->edge1->weight;
        picked++;
        level++;
        newedge++;
    }
#ifdef DEBUG
    if ( level != negEdges )
        fprintf ( outfile, "ERROR: level: %d  negEdges: %d\n", level,
                  negEdges );
    fflush ( outfile );
#endif

    best = mincost;             /* if we make the median equal to one of the three
                                   leaves, we get a cost of at most 2 on each edge */


    /* initial best solution is actually empty (beyond g/-g edges */
    for ( i = negEdges; i < ncount; i++ )
        stack[i] = NOEDGE;      /* no such edge */

    rec_dfs ( ncount, degree, adj_list, stack, outcycle, &best, score, picked,
              edges, numedges, otherEnd, level );

    *found_cost = best;

    if ( best >= mincost )
        return;

    /* outcycle at this point is a stack of edges of of cost 0, 1, or 2
       in the optimal solution.  Complete it into a full tour by using
       whatever cost-3 edges are needed and store results in outcycle. */

    visited = otherEnd;         /* reuse existing space */
    for ( i = -num_genes; i < 0; i++ )
        visited[i] = FALSE;
    visited[0] = TRUE;
    for ( i = 1; i <= num_genes; i++ )
        visited[i] = FALSE;

    /* Set up degree array */
    for ( i = negEdges; i < ncount; i++ )
    {
        ind = outcycle[i];
#ifdef DEBUG
        fprintf ( outfile, "converting tour; outcycle[%3d]=%3d\n", i, ind );
        fflush ( outfile );
#endif
        if ( ind == NOEDGE )
            break;
        else
        {
            icity = edges[ind].I;
            jcity = edges[ind].J;
            degree[icity]++;
            degree[jcity]++;
        }
    }
#ifdef DEBUG
    fprintf ( outfile, "converting tour: degrees computed are:\n" );
    for ( i = -num_genes; i <= num_genes; i++ )
        fprintf ( outfile, "degree[%3d]=%3d\n", i, degree[i] );
    fflush ( outfile );
#endif

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

    /* For each city, set nextcity[city] to adjacent cities */
    for ( i = -num_genes; i <= num_genes; i++ )
    {
        neighbors[i].A = UNUSED;
        neighbors[i].B = UNUSED;
    }
#ifdef DEBUG
    fprintf ( outfile, "converting tour: neighbors computed are:\n" );
    for ( i = -num_genes; i <= num_genes; i++ )
        fprintf ( outfile, "neighbors[%3d].A=%3d, neighbors[%3d].B=%3d\n", i,
                  neighbors[i].A, i, neighbors[i].B );
    fflush ( outfile );
#endif
    for ( i = 0; i < ncount; i++ )
    {
        ind = outcycle[i];
        if ( ind == NOEDGE )
            break;
        else
        {
            icity = edges[ind].I;
            jcity = edges[ind].J;
            if ( neighbors[icity].A == UNUSED )
                neighbors[icity].A = jcity;
            else
                neighbors[icity].B = jcity;
            if ( neighbors[jcity].A == UNUSED )
                neighbors[jcity].A = icity;
            else
                neighbors[jcity].B = icity;
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

#ifdef DEBUG
    fprintf ( outfile, "converting; start city is curcity=%3d\n", curcity );
    fflush ( outfile );
#endif
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
    return;
}


int
bbtsp ( int ncount, int *tour, int use_median, int *g1, int *g2, int *g3,
        struct adj_struct *adj_list, intpair_t * neighbors,
        int *stack, int *outcycle, int *degree, int *otherEnd, edge_t * edges,
        int CIRCULAR )
{
    int found_cost;
    int num_genes;
    int found;
    int i, i1, i2, j1, j2, k1, k2, i1first, j1first, k1first;
    int m1 = 0, m2 = 0, m1first = 0;
    int cost1, cost2, cost3, costm, mincost;
    int *ming;
    struct adj_struct *node;

    num_genes = ncount / 2;

#ifdef VERBOSE
    fprintf ( outfile, "Running bbtsp on %4d cities... \n", ncount );
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
    fprintf ( outfile, "median genome is: " );
    for ( i = 0; i < num_genes; i++ )
        fprintf ( outfile, " %3d", tour[i] );
    fprintf ( outfile, "\n" );
    fflush ( outfile );
#endif
    /* uses positive/negative indexing and so position 0 in arrays
       is always wasted */

    /* shift adj_list index to middle to improve efficiency of loops
       below */
    adj_list += num_genes;

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
    /* compute cost of each of the three genomes as a first solution */
    cost1 = cost2 = cost3 = costm = num_genes * ( -LARGENUM );

    /* now go through all gene pairs in turn, in each genome */
    /* but do this in the usual 3 parts, to gain time */

    /* first adjacency */
    i1 = i1first = g1[0];
    i2 = -g1[1];
    node = adj_list[i1].next;
    /* cost1 += weights[i1][i2], done by traversing adj. list */
    while ( node != NULL && node->vertex != i2 )
    {                           /* i2 must be in the adj. list */
        node = node->next;
    }
    cost1 += node->weight;

    j1 = j1first = g2[0];
    j2 = -g2[1];
    node = adj_list[j1].next;
    while ( node->vertex != j2 )
    {                           /* j2 must be in the adj. list */
        node = node->next;
    }
    cost2 += node->weight;

    k1 = k1first = g3[0];
    k2 = -g3[1];
    node = adj_list[k1].next;
    while ( node->vertex != k2 )
    {                           /* k2 must be in the adj. list */
        node = node->next;
    }
    cost3 += node->weight;

    if ( use_median )
    {
        m1 = m1first = tour[0];
        m2 = -tour[1];
        node = adj_list[m1].next;
        found = FALSE;
        while ( ( !found ) && ( node != NULL ) )
        {
            if ( node->vertex == m2 )
            {
                found = TRUE;
            }
            else
            {
                node = node->next;
            }
        }
        if ( found )
        {
            costm += node->weight;
        }
        else
        {
            costm += 3;         /* no suitable edge in adj. list -- so max cost */
        }
    }

    /* middle adjacencies */
    for ( i = 2; i < num_genes; i++ )
    {
        i1 = -i2;
        i2 = -g1[i];
        /* cost1 += weights[i1][i2], done by traversing adj. list */
        node = adj_list[i1].next;
        while ( node->vertex != i2 )
        {                       /* i2 must be in the adj. list */
            node = node->next;
        }
        cost1 += node->weight;

        j1 = -j2;
        j2 = -g2[i];
        /* cost2 += weights[j11][j2], done by traversing adj. list */
        node = adj_list[j1].next;
        while ( node->vertex != j2 )
        {                       /* j2 must be in the adj. list */
            node = node->next;
        }
        cost2 += node->weight;

        k1 = -k2;
        k2 = -g3[i];
        /* cost3 += weights[k1][k2], done by traversing adj. list */
        node = adj_list[k1].next;
        while ( node->vertex != k2 )
        {                       /* k2 must be in the adj. list */
            node = node->next;
        }
        cost3 += node->weight;

        if ( use_median )
        {
            m1 = -m2;
            m2 = -tour[i];
            /* costm += weights[m1][m2], done by traversing adj. list */
            node = adj_list[m1].next;
            found = FALSE;
            while ( ( !found ) && ( node != NULL ) )
            {
                if ( node->vertex == m2 )
                {
                    found = TRUE;
                }
                else
                {
                    node = node->next;
                }
            }
            if ( found )
            {
                costm += node->weight;
            }
            else
            {
                costm += 3;     /* no suitable edge in adj. list -- so max cost */
            }
        }
    }

    if ( CIRCULAR )
    {
        /* last run from loop -- all to avoid mod operations... */
        i1 = -i2;
        i2 = -i1first;
        /* cost1 += weights[i1][i2], done by traversing adj. list */
        node = adj_list[i1].next;
        while ( node->vertex != i2 )
        {                       /* i2-1 must be in the adj. list */
            node = node->next;
        }
        cost1 += node->weight;

        j1 = -j2;
        j2 = -j1first;
        /* cost2 += weights[j11][j2], done by traversing adj. list */
        node = adj_list[j1].next;
        while ( node->vertex != j2 )
        {                       /* i2-1 must be in the adj. list */
            node = node->next;
        }
        cost2 += node->weight;

        k1 = -k2;
        k2 = -k1first;
        /* cost3 += weights[k1][k2], done by traversing adj. list */
        node = adj_list[k1].next;
        while ( node->vertex != k2 )
        {                       /* i2-1 must be in the adj. list */
            node = node->next;
        }
        cost3 += node->weight;

        if ( use_median )
        {
            m1 = -m2;
            m2 = -m1first;
            /* costm += weights[m1][m2], done by traversing adj. list */
            node = adj_list[m1].next;
            found = FALSE;
            while ( ( !found ) && ( node != NULL ) )
            {
                if ( node->vertex == m2 )
                {
                    found = TRUE;
                }
                else
                {
                    node = node->next;
                }
            }
            if ( found )
            {
                costm += node->weight;
            }
            else
            {
                costm += 3;     /* no suitable edge in adj. list -- so max cost */
            }
        }
    }

#ifdef VERYVERBOSE
    fprintf ( outfile,
              "done checking, cost1=%3d, cost2=%3d, cost3=%3d, costm=%3d,\n",
              cost1, cost2, cost3, costm );
    fprintf ( outfile, "               g1=%p, g2=%p, g3=%p, tour=%p,\n", g1,
              g2, g3, tour );
    fflush ( outfile );
#endif
    /* retain the smallest and pass it down to the bbtsp solver as
       initial solution */
    if ( use_median )
    {                           /* consider 3 neighbors and old value */
        if ( ( cost1 <= cost2 ) && ( cost1 <= cost3 ) && ( cost1 <= costm ) )
        {
            mincost = cost1;
            ming = g1;
        }
        else
        {
            if ( ( cost2 <= cost3 ) && ( cost2 <= costm ) )
            {
                mincost = cost2;
                ming = g2;
            }
            else
            {
                if ( cost3 <= costm )
                {
                    mincost = cost3;
                    ming = g3;
                }
                else
                {
                    mincost = costm;
                    ming = tour;
                }
            }
        }
    }
    else
    {                           /* only 3 neighbors to consider */
        if ( ( cost1 <= cost2 ) && ( cost1 <= cost3 ) )
        {
            mincost = cost1;
            ming = g1;
        }
        else
        {
            if ( cost2 <= cost3 )
            {
                mincost = cost2;
                ming = g2;
            }
            else
            {
                mincost = cost3;
                ming = g3;
            }
        }
    }
#ifdef VERYVERBOSE
    fprintf ( outfile, "found cheapest: mincost=%3d, ptr=%p\n", mincost,
              ming );
    fflush ( outfile );
#endif

    /* Fill in initial solution of cost mincost */
    for ( i = 0; i < num_genes; i++ )
    {
        tour[i] = ming[i];
#ifdef VERBOSE
        fprintf ( outfile, "init tour[%3d]=%3d\n", i, tour[i] );
        fflush ( outfile );
#endif
    }


    tsp_dfs ( ncount, mincost, &found_cost, degree + num_genes, adj_list,
              neighbors + num_genes, stack, outcycle, otherEnd + num_genes,
              edges );

    /* Fill tour only if better than initial */
    if ( found_cost < mincost )
    {
#ifdef VERBOSE
        fprintf ( outfile, "better tour -- found_cost=%5d, mincost=%5d\n",
                  found_cost, mincost );
        fflush ( outfile );
#endif
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
        mincost = found_cost;
    }
    return mincost;
}
