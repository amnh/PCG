#include "structs.h"
#include "specialtsp.h"
#include "math.h"

/* Solves TSP exactly by dfs with pruning, using the lower bound
   suggested by Sankoff and Blanchette */
void
ap_rec_dfs ( int ncount, int *degree, int **weights, int **status,
             int *stack, int *outcycle, int *best, int score,
             int picked, smalledge_t * edges, int numedges, int *otherEnd,
             int level )
{
    /* use ordering of edges; left branch is include,
       right branch is exclude; do a dfs of the tree (so that include
       always gets explored first), keeping the best so far and using
       that and the lower bound to prune branches */

    int num_genes;
    int lb_new;
    int i, j, inc;
    int new_i = -1, new_j = -1;
    int orig_status;
    int otherEndI, otherEndJ;
    smalledge_t *newedge = NULL;

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
    for ( i = 0; i < ncount; i++ )
    {
        if ( i < num_genes )
            new_i = -( i + 1 );
        else
            new_i = i + 1 - num_genes;
        if ( degree[new_i] == 1 )
        {                       /* degree 2 is done; degrees 0 & >2 impossible */
            inc = 3;            /* if we cannot find an edge, that's the cost */
            for ( j = i + 1; j < ncount; j++ )
            {
                if ( j < num_genes )
                    new_j = -( j + 1 );
                else
                    new_j = j + 1 - num_genes;
                if ( ( status[i][j] == STAT_AVAIL ) && ( degree[new_j] == 1 )
                     && ( ( otherEnd[new_i] != new_j )
                          || ( picked == ncount - 1 ) )
                     && ( weights[i][j] < inc ) )
                    inc = weights[i][j];
            }
            lb_new += inc;      /* no usable edge of lower cost */
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
        if ( new_i < num_genes )
            new_i = -( new_i + 1 );
        else
            new_i = new_i + 1 - num_genes;
        new_j = newedge->J;
        if ( new_j < num_genes )
            new_j = -( new_j + 1 );
        else
            new_j = new_j + 1 - num_genes;
#ifdef VERBOSE
        fprintf ( outfile,
                  "new edge (%3d,%3d), degrees (%3d,%3d), weight %1d, otherEnd[%3d]=%3d\n",
                  new_i, new_j, degree[new_i], degree[new_j],
                  weights[newedge->I][newedge->J], new_i, otherEnd[new_i] );
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
        level++;
        newedge++;
        if ( level < numedges )
        {
            new_i = newedge->I;
            if ( new_i < num_genes )
                new_i = -( new_i + 1 );
            else
                new_i = new_i + 1 - num_genes;
            new_j = newedge->J;
            if ( new_j < num_genes )
                new_j = -( new_j + 1 );
            else
                new_j = new_j + 1 - num_genes;
#ifdef VERBOSE
            fprintf ( outfile,
                      "new edge (%3d,%3d), degrees (%3d,%3d), weight %1d, otherEnd[%3d]=%3d\n",
                      new_i, new_j, degree[new_i], degree[new_j],
                      weights[newedge->I][newedge->J], new_i,
                      otherEnd[new_i] );
            fflush ( outfile );
#endif
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
              weights[newedge->I][newedge->J], status[newedge->I][newedge->J],
              new_i, otherEnd[new_i], new_j, otherEnd[new_j] );
    fflush ( outfile );
#endif

    /* save original status to restore before returning from recursion */
    orig_status = status[newedge->I][newedge->J];

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
    status[newedge->I][newedge->J] = STAT_INCLUDED;

#ifdef VERYVERBOSE
    fprintf ( outfile, "\ninclude: " );
    fflush ( outfile );
#endif
    /* should guard call rather than entry */
    ap_rec_dfs ( ncount, degree, weights, status, stack, outcycle, best,
                 score + weights[newedge->I][newedge->J], picked + 1,
                 edges, numedges, otherEnd, level + 1 );

    /* restore otherEnd and degree info */
    otherEnd[otherEndI] = new_i;
    otherEnd[otherEndJ] = new_j;
    degree[new_i]--;
    degree[new_j]--;

    /* remove edge from stack */
    stack[picked] = NOEDGE;

    /* set edge to be excluded */
    status[newedge->I][newedge->J] = STAT_EXCLUDED;

    /* recurse with edge excluded */
#ifdef VERYVERBOSE
    fprintf ( outfile, "\nexclude: " );
    fflush ( outfile );
#endif
    ap_rec_dfs ( ncount, degree, weights, status, stack, outcycle, best,
                 score, picked, edges, numedges, otherEnd, level + 1 );

    /* restore edge status */
    status[newedge->I][newedge->J] = orig_status;
    return;
}


void
ap_tsp_dfs ( int ncount, int mincost, int *found_cost, int *degree,
             int **weights, int **status, intpair_t * neighbors,
             int *stack, int *outcycle, int *otherEnd, smalledge_t * edges )
{
    /* this is the parent routine, which takes care of initialization,
       then calls the recursive dfs */
    /* ncount is effectively a constant */
    /* so is mincost, which is the tour cost of one of the three genomes */

    /* outcycle is the returned tour */

    /* pointers to arrays degree, neighbors, and otherEnd are
       shifted to the middle to allow negative indexing */
    /* pointers to arrays stack, outcycle, and edges are not shifted */

    int num_genes;
    int i, j, ind, top;
    int countm3, countm2, countm1, count0, count1, count2, countL;
    smalledge_t *edgesm3, *edgesm2, *edgesm1, *edges0, *edges1, *edges2,
        *edgesL;
    smalledge_t *newedge;
    int score, best, picked, level;
    int numedges;
    int negEdges;
    int curcity, nextcity;
    int icity, jcity;
    int *visited;

    num_genes = ncount / 2;

    /* prepare a sorted list of edges (in increasing order of cost) */

    /* use distribution sort -- here ad hoc, since we only have 7 values:
       -3, -2, -1, 0, 1, 2, and L = -largevalue */
    /* could just use an array of pointers since adj. lists are sorted... */

    /* tally values */
    countL = countm3 = countm2 = countm1 = count0 = count1 = count2 = 0;
    for ( i = 0; i < ncount; i++ )
        for ( j = i + 1; j < ncount; j++ )
        {
            switch ( weights[i][j] )
            {
                case -3:
                    countm3++;
                    break;
                case -2:
                    countm2++;
                    break;
                case -1:
                    countm1++;
                    break;
                case 0:
                    count0++;
                    break;
                case 1:
                    count1++;
                    break;
                case 2:
                    count2++;
                    break;
                case 3:
                    break;      /* don't care about these, don't count them */
                default:
                    countL++;
                    break;
            }
        }
    numedges =
        countm3 + countm2 + countm1 + count0 + count1 + count2 + countL;
    negEdges = countL;
    edgesL = edges;
    edgesm3 = edgesL + countL;
    edgesm2 = edgesm3 + countm3;
    edgesm1 = edgesm2 + countm2;
    edges0 = edgesm1 + countm1;
    edges1 = edges0 + count0;
    edges2 = edges1 + count1;
    for ( i = 0; i < ncount; i++ )
        for ( j = i + 1; j < ncount; j++ )
        {
            switch ( weights[i][j] )
            {
                case -3:
                    edgesm3->I = i;
                    edgesm3->J = j;
                    edgesm3++;
                    break;
                case -2:
                    edgesm2->I = i;
                    edgesm2->J = j;
                    edgesm2++;
                    break;
                case -1:
                    edgesm1->I = i;
                    edgesm1->J = j;
                    edgesm1++;
                    break;
                case 0:
                    edges0->I = i;
                    edges0->J = j;
                    edges0++;
                    break;
                case 1:
                    edges1->I = i;
                    edges1->J = j;
                    edges1++;
                    break;
                case 2:
                    edges2->I = i;
                    edges2->J = j;
                    edges2++;
                    break;
                case 3:
                    break;      /* don't register */
                default:
                    edgesL->I = i;
                    edgesL->J = j;
                    edgesL++;
                    break;
            }
        }
#ifdef DEBUG
    for ( i = 0; i < numedges; i++ )
        fprintf ( outfile, "Edge[%3d]: (%4d, %4d), wt: %7d\n",
                  i, edges[i].I, edges[i].J,
                  weights[edges[i].I][edges[i].J] );
    fprintf ( outfile, "\n" );
    fflush ( outfile );
#endif

    /* initialize degree to all 1s, to reflect the fact that
       the (g,-g) edges are forcibly included */
    for ( i = -num_genes; i < 0; i++ )
        degree[i] = 1;
    degree[0] = 0;              /* thus will never be used */
    for ( i = 1; i <= num_genes; i++ )
        degree[i] = 1;

    /* initialize global variables and structures */
    best = 0;                   /* value of best solution found so far */
    *found_cost = best;

    score = 0;                  /* total value of edges picked so far */
    picked = 0;                 /* number of edges picked so far */
    level = 0;
    newedge = edges;
    /* Initialize otherEnd[city] to itself */
    for ( i = -num_genes; i <= num_genes; i++ )
        otherEnd[i] = i;
    /* initially include -Z weight edges (g,-g) */
    while ( weights[newedge->I][newedge->J] < -3 )
    {
        stack[level] = level;
        i = newedge->I;
        if ( i < num_genes )
            i = -( i + 1 );
        else
            i = i + 1 - num_genes;
        j = newedge->J;
        if ( j < num_genes )
            j = -( j + 1 );
        else
            j = j + 1 - num_genes;
        otherEnd[i] = j;
        otherEnd[j] = i;
        score += weights[newedge->I][newedge->J];
        picked++;
        level++;
        newedge++;
    }
#ifdef DEBUG
    if ( level != negEdges )
        fprintf ( outfile, "ERROR: level: %d  negEdges: %d\n", level,
                  negEdges );
    fprintf ( outfile,
              "done initializing, picked=%3d, level=%3d, score=%5d\n", picked,
              level, score );
    fflush ( outfile );
#endif

    /* initial best solution is actually empty (beyond g/-g edges */
    for ( i = negEdges; i < ncount; i++ )
        stack[i] = NOEDGE;      /* no such edge */

    ap_rec_dfs ( ncount, degree, weights, status, stack, outcycle, &best,
                 score, picked, edges, numedges, otherEnd, level );


    if ( best >= *found_cost )
        return;

    *found_cost = best;

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
            if ( icity < num_genes )
                icity = -( icity + 1 );
            else
                icity = icity + 1 - num_genes;
            jcity = edges[ind].J;
            if ( jcity < num_genes )
                jcity = -( jcity + 1 );
            else
                jcity = jcity + 1 - num_genes;
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
            if ( icity < num_genes )
                icity = -( icity + 1 );
            else
                icity = icity + 1 - num_genes;
            jcity = edges[ind].J;
            if ( jcity < num_genes )
                jcity = -( jcity + 1 );
            else
                jcity = jcity + 1 - num_genes;
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
ap_bbtsp ( int ncount, int *tour, int **weights, int **status,
           intpair_t * neighbors, int *stack,
           int *outcycle, int *degree, int *otherEnd, smalledge_t * edges )
{
    int mincost, found_cost;
    int num_genes;
    int i;

    num_genes = ncount / 2;
    mincost = LARGENUM * ncount;

#ifdef VERBOSE
    fprintf ( outfile, "Running bbtsp on %4d cities... \n", ncount );
    fflush ( outfile );
#endif
    /* uses positive/negative indexing and so position 0 in arrays
       is always wasted */

    ap_tsp_dfs ( ncount, mincost, &found_cost, degree, weights, status,
                 neighbors, stack, outcycle, otherEnd, edges );

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

int
ap_coalestsp ( int ncount, int *tour, int **weights,
               intpair_t * neighbors, int *stack,
               int *outcycle, int *degree, int *otherEnd,
               smalledge_t * edges )
{
    /* uses coalesced simple path heuristic to build tour */

    /* ncount is effectively a constant */
    /* outcycle is the returned tour */
    /* pointers to arrays adj_list, degree, neighbors, and otherEnd are
       passed in shifted to the middle to allow negative indexing */
    /* pointers to arrays outcycle, and edges are not shifted */

    int num_genes;
    int i, j, top;
    int countm3, countm2, countm1, count0, count1, count2, countL;
    smalledge_t *edgesm3, *edgesm2, *edgesm1, *edges0, *edges1, *edges2,
        *edgesL;
    smalledge_t *newedge;
    int score, picked;
    int numedges, negEdges;
    int otherEndI, otherEndJ;
    int *visited;
    int curcity, nextcity;

    num_genes = ncount / 2;

#ifdef VERBOSE
    fprintf ( outfile, "Running ap_coalestsp on %4d cities... \n", ncount );
    fflush ( outfile );
#endif
    /* uses positive/negative indexing and so position 0 in arrays
       is always wasted */

    /* prepare a sorted list of edges (in increasing order of cost) */

    /* use distribution sort -- here ad hoc, since we only have 7 values:
       -3, -2, -1, 0, 1, 2, and L = -largevalue */
    /* could just use an array of pointers since adj. lists are sorted... */

    /* tally values */
    countL = countm3 = countm2 = countm1 = count0 = count1 = count2 = 0;
    for ( i = 0; i < ncount; i++ )
        for ( j = i + 1; j < ncount; j++ )
        {
            switch ( weights[i][j] )
            {
                case -3:
                    countm3++;
                    break;
                case -2:
                    countm2++;
                    break;
                case -1:
                    countm1++;
                    break;
                case 0:
                    count0++;
                    break;
                case 1:
                    count1++;
                    break;
                case 2:
                    count2++;
                    break;
                case 3:
                    break;      /* don't care about these, don't count them */
                default:
                    countL++;
                    break;
            }
        }
    numedges =
        countm3 + countm2 + countm1 + count0 + count1 + count2 + countL;
    negEdges = countL;
    edgesL = edges;
    edgesm3 = edgesL + countL;
    edgesm2 = edgesm3 + countm3;
    edgesm1 = edgesm2 + countm2;
    edges0 = edgesm1 + countm1;
    edges1 = edges0 + count0;
    edges2 = edges1 + count1;
    for ( i = 0; i < ncount; i++ )
        for ( j = i + 1; j < ncount; j++ )
        {
            switch ( weights[i][j] )
            {
                case -3:
                    edgesm3->I = i;
                    edgesm3->J = j;
                    edgesm3++;
                    break;
                case -2:
                    edgesm2->I = i;
                    edgesm2->J = j;
                    edgesm2++;
                    break;
                case -1:
                    edgesm1->I = i;
                    edgesm1->J = j;
                    edgesm1++;
                    break;
                case 0:
                    edges0->I = i;
                    edges0->J = j;
                    edges0++;
                    break;
                case 1:
                    edges1->I = i;
                    edges1->J = j;
                    edges1++;
                    break;
                case 2:
                    edges2->I = i;
                    edges2->J = j;
                    edges2++;
                    break;
                case 3:
                    break;      /* don't register */
                default:
                    edgesL->I = i;
                    edgesL->J = j;
                    edgesL++;
                    break;
            }
        }
#ifdef DEBUG
    for ( i = 0; i < numedges; i++ )
        fprintf ( outfile, "Edge[%3d]: (%4d, %4d), wt: %7d\n",
                  i, edges[i].I, edges[i].J,
                  weights[edges[i].I][edges[i].J] );
    fprintf ( outfile, "\n" );
    fflush ( outfile );
#endif

    /* initialize degree to all 1s, to reflect the fact that
       the (g,-g) edges are forcibly included */
    for ( i = -num_genes; i < 0; i++ )
        degree[i] = 1;
    degree[0] = 0;              /* thus will never be used */
    for ( i = 1; i <= num_genes; i++ )
        degree[i] = 1;

    /* initialize global variables and structures */
    score = 0;                  /* total value of edges picked so far */
    picked = 0;                 /* number of edges picked so far */
    newedge = edges;
    /* Initialize otherEnd[city] to itself */
    for ( i = -num_genes; i <= num_genes; i++ )
        otherEnd[i] = i;
    /* initially include -Z weight edges (g,-g) */
    while ( weights[newedge->I][newedge->J] < -3 )
    {
        stack[picked] = picked;
        /* decode values of indices */
        i = newedge->I;
        if ( i < num_genes )
            i = -( i + 1 );
        else
            i = i + 1 - num_genes;
        j = newedge->J;
        if ( j < num_genes )
            j = -( j + 1 );
        else
            j = j + 1 - num_genes;
        otherEnd[i] = j;
        otherEnd[j] = i;
        neighbors[i].A = j;
        neighbors[j].A = i;
        score += weights[newedge->I][newedge->J];
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
        /* decode values of indices */
        i = newedge->I;
        if ( i < num_genes )
            i = -( i + 1 );
        else
            i = i + 1 - num_genes;
        j = newedge->J;
        if ( j < num_genes )
            j = -( j + 1 );
        else
            j = j + 1 - num_genes;
#ifdef DEBUG
        fprintf ( outfile, "checking edge (%3d,%3d) for inclusion\n",
                  newedge->I, newedge->J );
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
            score += weights[newedge->I][newedge->J];
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
    stack[picked] = -1;
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

int
av_bbtsp ( int ncount, int *tour, int **weights, int **status,
           intpair_t * neighbors, int *stack,
           int *outcycle, int *degree, int *otherEnd, smalledge_t * edges )
{
    return 0;
}

int
av_coalestsp ( int ncount, int *tour, int **weights,
               intpair_t * neighbors, int *stack,
               int *outcycle, int *degree, int *otherEnd,
               smalledge_t * edges )
{
    return 0;
}
