/* this is the inversion median solver implemented by Alberto
   and integrated into GRAPPA by Tao Liu.
   There are many global variables used and are initialized in
   the main function
*/

#include "inversion_median_alberto.h"
#include <string.h>

void
alloc_error ( char *s )
{
    printf ( "\n Warning: Not enough memory to allocate %s\n", s );
    printf ( "\n Cannot proceed with MSBR\n" );
    exit ( 0 );
}

void
stop_error ( char *s )
{
    printf ( "%s", s );
    printf ( " Cannot proceed with MSBR execution" );
    exit ( 0 );
}

void
genome_init ( int N )
{
    int i, k, l;
    Num_Genes = N;

    /* graph sizes */
    vc = 2 * Num_Genes + 2;
    mc = vc / 2;
    ec = vc * ( vc - 1 ) / 2 - vc / 2;

    for ( k = 1; k <= 3; k++ )
    {
        /*     gen[k] = (struct genome_struct*)malloc(sizeof(struct genome_struct));
           gen[k]->genes = (int *)calloc(Num_Genes, sizeof(int)); */
        for ( i = 0; i < Num_Genes; i++ )
            gen[k]->genes[i] = pi[k][i];
/*      gen[k]->encoding = NULL;
      gen[k]->gnamePtr = NULL;*/
    }
    for ( k = 1; k <= 3; k++ )
    {
        rdist[k][k] = 0;
        for ( l = k + 1; l <= 3; l++ )
            rdist[k][l] = rdist[l][k] =
                invdist_noncircular ( gen[k], gen[l], 0, Num_Genes,
                                      localDistmem );
    }
}

void
median_init (  )
{
/*  genupd = (struct genome_struct *) calloc(1,sizeof(struct genome_struct));
  genupd->genes = (int *) calloc(Num_Genes, sizeof(int));
  genupd->encoding = NULL;
  genupd->gnamePtr = NULL;*/
}

int
median_distance ( int *med )
{
    /* find the median reversal distance for a given permutation */
    int i, k, ldist;

    for ( i = 0; i < Num_Genes; i++ )
        genupd->genes[i] = med[i];
    ldist = 0;
    for ( k = 1; k <= 3; k++ )
        ldist +=
            invdist_noncircular ( gen[k], genupd, 0, Num_Genes,
                                  localDistmem );

    return ( ldist );
}

int *
Find_circular_identity ( int *perm, int k )
{
    int i, j;                   /* *id; */
    int sign = 1;

    /*id = (int*)calloc(Num_Genes, sizeof(int)); */
    for ( i = 0; i < Num_Genes; i++ )
    {
        if ( perm[i] == 1 )
            break;
        else if ( perm[i] == -1 )
        {
            sign = -1;
            break;
        }
    }
    if ( i == Num_Genes )
    {
        fprintf ( stderr,
                  "FATAL ERROR: permutation has no element labeled '1'.\n" );
        exit ( 1 );
    }
    if ( sign == 1 )
    {
        for ( j = 0; j < Num_Genes; j++ )
        {
            fid[k][j] = perm[i];
            if ( ++i == Num_Genes )
                i = 0;
        }
    }
    else if ( sign == -1 )
    {
        for ( j = 0; j < Num_Genes; j++ )
        {
            fid[k][j] = -perm[i];
            if ( --i < 0 )
                i = Num_Genes - 1;
        }
    }
    return fid[k];
}

void
mat_2_per ( int ngenes, int vc, int *mate, int *per )
{
    int i, h;
    i = h = 0;
    do
    {
        i = mate[i];
        if ( i == vc - 1 )
            return;
        if ( ( i % 2 ) == 1 )
        {
            per[h] = ( i + 1 ) / 2;
            i++;
        }
        else
        {
            per[h] = -( i ) / 2;
            i--;
        }
        h++;
    }
    while ( TRUE );
}

void
find_half_cycles ( int *pmi, int k, int mc, int *ncyc )
{
    /* returns the half cycles w.r.t. permutation k associated with the
       cycles formed by pmi and the permutation matching of k */

#define PM 0
#define K 1

    int e, i, j, nunexp, first, curr, pm_k, pos;


    /* find the edges incident with each node for pm */

    for ( e = 0; e < mc; e++ )
    {
        i = i_ind[pmi[e]];
        j = j_ind[pmi[e]];
        pme[i] = pme[j] = pmi[e];
        pmmate[i] = j;
        pmmate[j] = i;
        i = i_ind[pm[k][e]];
        j = j_ind[pm[k][e]];
        ke[i] = ke[j] = pm[k][e];
        kmate[i] = j;
        kmate[j] = i;
    }

    /* find the cycles */

    ( *ncyc ) = 0;
    nunexp = vc;
    for ( i = 0; i < vc; i++ )
    {
        unexp[i] = i;
        inv_unexp[i] = i;
    }
    do
    {
        first = unexp[0];
        curr = first;
        pm_k = PM;
        do
        {
            pos = inv_unexp[curr];
            nunexp--;
            unexp[pos] = unexp[nunexp];
            inv_unexp[unexp[nunexp]] = pos;
            if ( pm_k == PM )
            {
                curr = pmmate[curr];
                pm_k = K;
            }
            else
            {
                curr = kmate[curr];
                pm_k = PM;
            }
        }
        while ( curr != first );

        /* sort the edges of new_cyc in increasing order */
        ( *ncyc )++;
    }
    while ( nunexp > 0 );

}

void
termin ( float elaps, int optimal )
{
    int qmc;

    qmc = 3 * ( Num_Genes + 1 );
/*  if ( optimal == TRUE ) 
    printf(":-) optimal solution found (value: %d):\n", qmc - realbest);  
  else 
    printf(":-( TIME LIMIT: best solution found (value: %d, best LB: %d)\n",
           qmc - realbest, qmc - realub);

  for ( i = 0; i < Num_Genes; i++ ) printf("%d ",pibest[i]);
  printf("\n");
  printf("%d %d %d %f %15.0f\n",
    qmc - metricub, qmc - realub, qmc - realbest, elaps,
    (double) MILL * (double) millnodes + (double) treenodes);*/
}

void
msbrbb ( int lncyc, int ln, int lub, int lcurr )
{
    int type;
    int k, l, i, f, c, u, v, lnext, newcurr, pos = 0;
    int totcycles, newcyc, newub;
    int **dcycles;
    int ldist, qmc;
    int stop_flag = FALSE;

    /* main branch-and-bound recursion */

    treenodes++;
    if ( treenodes % MILL == 0 )
    {
        millnodes++;
        treenodes = 0;
        if ( tf - ti > TLIM )
            termin ( tf - ti, FALSE );
        stop_flag = TRUE;
    }

    /* possible updating of best solution */
    if ( !stop_flag && ( ln < 0 && lncyc > best_now ) )
    {
        /* define candidate best permutation */
        mat_2_per ( Num_Genes, vc, solmate, piupd );

        /* compute reversal distance for this permutation */
        ldist = median_distance ( piupd );

        qmc = 3 * ( Num_Genes + 1 );
        if ( qmc - ldist > best_now )
        {
            realbest = realub = best_now = qmc - ldist;
            for ( i = 0; i < Num_Genes; i++ )
                pibest[i] = piupd[i];
            stop_flag = TRUE;
        }
        /*
           else
           printf("Warning: CMP (optimal) value not corresp. to RMP value\n");
         */
    }


    /* recursion exit */
    if ( nunreach == 0 )
        return;
    /* upper bound test */
    if ( lub <= best_now )
        return;

    /* branching, by choosing one vertex and considering all possible
       edges incident to it for matching */
    /* fancy branching rule, which extends the current partial Hamiltonian 
       matching (starting from node 0) - in general, we start from a given
       node lcurr */

    if ( !stop_flag )
    {

        dcycles = ( int ** ) calloc ( 4, sizeof ( int * ) );
        for ( i = 0; i < 4; i++ )
            dcycles[i] = ( int * ) calloc ( 4, sizeof ( int ) );
        for ( i = 0; i < nunreach; i++ )
        {
            lnext = unreach[i];
            /* consider adding edge (lcurr,lnext) - from now on we have a
               generic branching on this edge, whatsoever way it is found */
            if ( hamate[lcurr] == lnext && nunreach > 1 )
                continue;

            solmate[lcurr] = lnext;
            solmate[lnext] = lcurr;
            /* for each pair of permutations, compute how many cycles in the 
               associated permutation matchings go through nodes lcurr, lnext,
               before and after modifying the permutation matchings */
            for ( k = 1; k <= 3; k++ )
                for ( l = k + 1; l <= 3; l++ )
                {
                    if ( permate[k][lcurr] == lnext
                         && permate[l][lcurr] == lnext )
                        /* 1 cycle before contraction and 0 after */
                        dcycles[k][l] = -1;
                    else if ( permate[k][lcurr] == lnext
                              || permate[l][lcurr] == lnext )
                        /* 1 cycle before and after contraction */
                        dcycles[k][l] = 0;
                    else
                    {
                        /* start from node lcurr */
                        f = c = lcurr;
                        type = TWO;
                        do
                        {
                            /* k edge */
                            c = permate[k][c];
                            if ( c == lnext )
                            {
                                type = ONE_GOOD;
                                break;
                            }
                            /* l edge */
                            c = permate[l][c];
                            if ( c == lnext )
                            {
                                type = ONE_BAD;
                                break;
                            }
                        }
                        while ( c != f );
                        if ( type == ONE_BAD )
                        {
                            /* 1 cycle before and after contraction */
                            dcycles[k][l] = 0;
                        }
                        else if ( type == ONE_GOOD )
                        {
                            /* 1 cycle before contraction, 2 after */
                            dcycles[k][l] = 1;
                        }
                        else    /* type == TWO */
                            /* 2 cycles before contraction, 1 after */
                            dcycles[k][l] = -1;
                    }
                }

            /* update the permutation matching edges, also counting the number of 
               cycles formed by edge (lcurr,lnext) with the current permutation 
               matchings */
            newcyc = 0;
            for ( k = 1; k <= 3; k++ )
            {
                if ( permate[k][lcurr] == lnext )
                {
                    /* new cycle formed by partial solution */
                    newcyc++;
                    /* the permutation matching for k is unchanged */
                }
                else
                {
                    /* change permutation matching */
                    u = permate[k][lcurr];
                    v = permate[k][lnext];
                    permate[k][u] = v;
                    permate[k][v] = u;
                }
            }
            /* update the current Hamiltonian matching */
            u = hamate[lcurr];
            newcurr = v = hamate[lnext];
            hamate[u] = v;
            hamate[v] = u;

            /* update the number of cycles formed by every pair of 
               permutation matchings and compute upper bound */
            totcycles = 0;
            mc -= 1;
            for ( k = 1; k <= 3; k++ )
            {
                ncycles[k][k] -= 1;
                for ( l = k + 1; l <= 3; l++ )
                {
                    ncycles[k][l] += dcycles[k][l];
                    ncycles[l][k] += dcycles[k][l];
                    totcycles += ncycles[k][l];
                }
            }
            newub = lncyc + newcyc +
                ( int ) ( totcycles / ( double ) ( 3 - 1 ) +
                          ( double ) ( 3 * mc ) / 2.0 );

            /* update the unreached node data structure */
            nunreach--;
            if ( nunreach > 0 )
            {
                unreach[i] = unreach[nunreach];
                inv_unreach[unreach[i]] = i;
                pos = inv_unreach[newcurr];
                nunreach--;
                unreach[pos] = unreach[nunreach];
                inv_unreach[unreach[pos]] = pos;
            }

            /* recursively call msbrbb */
            msbrbb ( lncyc + newcyc, ln - 1, newub, newcurr );

            /* restore global data structures */
            if ( nunreach > 0 )
            {
                inv_unreach[unreach[pos]] = nunreach;
                unreach[nunreach] = unreach[pos];
                nunreach++;
                unreach[pos] = newcurr;
                inv_unreach[unreach[i]] = nunreach;
                unreach[nunreach] = unreach[i];
            }
            nunreach++;
            unreach[i] = lnext;
            mc += 1;
            for ( k = 1; k <= 3; k++ )
            {
                ncycles[k][k] += 1;
                for ( l = k + 1; l <= 3; l++ )
                {
                    ncycles[k][l] -= dcycles[k][l];
                    ncycles[l][k] -= dcycles[k][l];
                }
            }
            for ( k = 1; k <= 3; k++ )
            {
                if ( permate[k][lcurr] != lnext )
                {
                    u = permate[k][lcurr];
                    v = permate[k][lnext];
                    permate[k][u] = lcurr;
                    permate[k][v] = lnext;
                }
            }
            /* update the current Hamiltonian matching */
            u = hamate[lcurr];
            v = hamate[lnext];
            hamate[u] = lcurr;
            hamate[v] = lnext;
            solmate[lcurr] = solmate[lnext] = NONE;
        }
        for ( i = 0; i < 4; i++ )
            free ( dcycles[i] );
        free ( dcycles );
    }
}

void
init_global_variables ( int N, distmem_t * distmem )
{
    int i;

    Num_Genes = N;
    /* variable in 'glob.h */
    MAXNOD = 2 * Num_Genes + 2;
    MAXM = Num_Genes + 1;
    MAXEDG = MAXNOD * MAXNOD / 2 - MAXM;

    localDistmem = distmem;

    genupd =
        ( struct genome_struct * ) calloc ( 1,
                                            sizeof ( struct genome_struct ) );
    genupd->genes = ( int * ) calloc ( Num_Genes, sizeof ( int ) );

    id = ( struct genome_struct ** ) calloc ( MAXQ,
                                              sizeof ( struct genome_struct
                                                       * ) );
    fid = ( int ** ) calloc ( MAXQ, sizeof ( int * ) );
    for ( i = 0; i < MAXQ; i++ )
    {
        id[i] =
            ( struct genome_struct * )
            malloc ( sizeof ( struct genome_struct ) );
        fid[i] = ( int * ) malloc ( sizeof ( int ) * MAX_NUM_GENES );
    }

    pi = ( int ** ) calloc ( MAXQ + 1, sizeof ( int * ) );
    for ( i = 0; i < MAXQ + 1; i++ )
        pi[i] = ( int * ) calloc ( Num_Genes, sizeof ( int ) );
    pm = ( int ** ) calloc ( MAXQ + 1, sizeof ( int * ) );
    for ( i = 0; i < MAXQ + 1; i++ )
        pm[i] = ( int * ) calloc ( MAXM, sizeof ( int ) );
/*  pibest = (int*)calloc(Num_Genes, sizeof(int));*/

    e_ind = ( int ** ) calloc ( MAXNOD, sizeof ( int * ) );
    for ( i = 0; i < MAXNOD; i++ )
        e_ind[i] = ( int * ) calloc ( MAXNOD, sizeof ( int ) );
    i_ind = ( int * ) calloc ( MAXEDG, sizeof ( int ) );
    j_ind = ( int * ) calloc ( MAXEDG, sizeof ( int ) );

    /* graph sizes */
    vc = 2 * Num_Genes + 2;
    mc = vc / 2;
    ec = vc * ( vc - 1 ) / 2 - vc / 2;

    /* variable in 'msbr.h */

    unreach = ( int * ) calloc ( MAXNOD, sizeof ( int ) );
    inv_unreach = ( int * ) calloc ( MAXNOD, sizeof ( int ) );
    ncycles = ( int ** ) calloc ( MAXQ + 1, sizeof ( int * ) );
    rdist = ( int ** ) calloc ( MAXQ + 1, sizeof ( int * ) );
    permate = ( int ** ) calloc ( MAXQ + 1, sizeof ( int * ) );
    for ( i = 0; i < MAXQ + 1; i++ )
    {
        ncycles[i] = ( int * ) calloc ( MAXQ + 1, sizeof ( int ) );
        rdist[i] = ( int * ) calloc ( MAXQ + 1, sizeof ( int ) );
        permate[i] = ( int * ) calloc ( MAXNOD, sizeof ( int ) );
    }
    hamilmate = ( int * ) calloc ( MAXNOD, sizeof ( int ) );
    hamate = ( int * ) calloc ( MAXNOD, sizeof ( int ) );
    solmate = ( int * ) calloc ( MAXNOD, sizeof ( int ) );
    piupd = ( int * ) calloc ( Num_Genes, sizeof ( int ) );

    gen =
        ( struct genome_struct ** ) calloc ( MAXQ + 1,
                                             sizeof ( struct genome_struct
                                                      * ) );
    for ( i = 0; i < MAXQ + 1; i++ )
    {
        gen[i] =
            ( struct genome_struct * )
            malloc ( sizeof ( struct genome_struct ) );
        gen[i]->genes = ( int * ) malloc ( Num_Genes * sizeof ( int ) );
    }

    pme = ( int * ) calloc ( MAXNOD, sizeof ( int ) );
    ke = ( int * ) calloc ( MAXNOD, sizeof ( int ) );
    pmmate = ( int * ) calloc ( MAXNOD, sizeof ( int ) );
    kmate = ( int * ) calloc ( MAXNOD, sizeof ( int ) );
    unexp = ( int * ) calloc ( MAXNOD, sizeof ( int ) );
    inv_unexp = ( int * ) calloc ( MAXNOD, sizeof ( int ) );

}

void
free_variables (  )
{
    int i;
    for ( i = 0; i < MAXQ + 1; i++ )
        free ( pi[i] );
    free ( pi );
    for ( i = 0; i < MAXQ + 1; i++ )
        free ( pm[i] );
    free ( pm );

    free ( i_ind );
    free ( j_ind );
    for ( i = 0; i < MAXNOD; i++ )
        free ( e_ind[i] );
    free ( e_ind );

    free ( unreach );
    free ( inv_unreach );
    for ( i = 0; i < MAXQ + 1; i++ )
    {
        free ( ncycles[i] );
        free ( rdist[i] );
        free ( permate[i] );
    }
    free ( ncycles );
    free ( rdist );
    free ( permate );
    free ( hamilmate );
    free ( hamate );
    free ( solmate );
    free ( piupd );

    for ( i = 0; i < MAXQ + 1; i++ )
    {
        free ( gen[i]->genes );
        free ( gen[i] );
    }
    free ( gen );
    free ( genupd->genes );
    free ( genupd );
    free ( pme );
    free ( ke );
    free ( pmmate );
    free ( kmate );
    free ( unexp );
    free ( inv_unexp );

    for ( i = 0; i < MAXQ; i++ )
    {
        free ( id[i] );
        free ( fid[i] );
    }
    free ( id );
    free ( fid );
}

int *
albert_inversion_median_circular ( struct genome_struct **passgenome,
                                   int ngenes, int *genes )
{
    int *med, i;

    pibest = genes;

    Num_Genes = ngenes;
    for ( i = 0; i < MAXQ; i++ )
    {
        id[i]->genes = Find_circular_identity ( passgenome[i]->genes, i );
    }

    med = albert_inversion_median_noncircular ( id, ngenes, genes );

    return med;
}

int *
albert_inversion_median_noncircular ( struct genome_struct **passgenome,
                                      int ngenes, int *genes )
{
    int h, i, j, k, l, e, prev, curr;
    int ncyc, ncyclesk, maxcyclesk, totcycles, inibest, oldbest;
    int mindistk, totdist, kbest;

    pibest = genes;
    Num_Genes = ngenes;

    for ( j = 1; j < MAXQ + 1; j++ )
        for ( i = 0; i < Num_Genes; i++ )
            pi[j][i] = passgenome[j - 1]->genes[i];

    /* compute distances between all pairs and find best "local" median */
    genome_init ( Num_Genes );
    median_init (  );

    mindistk = 3 * mc;
    for ( k = 1; k <= 3; k++ )
    {
        totdist = 0;
        for ( l = 1; l <= 3; l++ )
            totdist += rdist[k][l];
        if ( totdist < mindistk )
        {
            mindistk = totdist;
            kbest = k;
            /* initialize best solution */
            for ( i = 0; i < Num_Genes; i++ )
                pibest[i] = pi[k][i];
        }
    }

    /* definition of the edge variables */

    e = 0;
    for ( i = 0; i < vc; i++ )
        for ( j = i + 1; j < vc; j++ )
        {
            if ( ( i % 2 == 1 && j == i + 1 ) || ( i == 0 && j == vc - 1 ) )
            {
                /* exclude edges in H */
                e_ind[i][j] = e_ind[j][i] = NONE;
                continue;
            }
            i_ind[e] = i;
            j_ind[e] = j;
            e_ind[i][j] = e_ind[j][i] = e;
            e++;
        }

    /* compute permutation matchings */

    /* definition of the matching edges */

    for ( k = 1; k <= 3; k++ )
    {
        /* prev is the predecessor of current element curr */
        prev = 0;
        l = 0;
        for ( h = 0; h <= Num_Genes; h++ )
        {
            if ( h < Num_Genes )
                curr = pi[k][h];
            else
                curr = Num_Genes + 1;
            if ( curr >= 0 && prev >= 0 )
            {
                /* edge from (2 * prev) to (2 * curr - 1) */
                i = 2 * prev;
                j = ( 2 * curr - 1 );
            }
            else if ( curr < 0 && prev >= 0 )
            {
                /* edge from (2 * prev) to (2 * -curr) */
                i = 2 * prev;
                j = ( 2 * -curr );
            }
            else if ( curr >= 0 && prev < 0 )
            {
                /* edge from (2 * -prev - 1) to (2 * curr - 1) */
                i = 2 * -prev - 1;
                j = ( 2 * curr - 1 );
            }
            else if ( curr < 0 && prev < 0 )
            {
                /* edge from (2 * -prev - 1) to (2 * -curr) */
                i = 2 * -prev - 1;
                j = ( 2 * -curr );
            }
            pm[k][l] = e_ind[i][j];
            permate[k][i] = j;
            permate[k][j] = i;
            l++;
            prev = curr;
        }
    }
    /* Hamiltonian matching edges (dynamically updated in branch-and-bound) */
    for ( i = 1; i <= Num_Genes; i++ )
    {
        hamate[2 * i - 1] = 2 * i;
        hamate[2 * i] = 2 * i - 1;
    }
    hamate[0] = 2 * Num_Genes + 1;
    hamate[2 * Num_Genes + 1] = 0;
    /* Hamiltonian matching edges (permanent copy) */
    for ( i = 0; i < vc; i++ )
        hamilmate[i] = hamate[i];
    /* solution edges */
    for ( i = 0; i < vc; i++ )
        solmate[i] = NONE;

    /* compute initial number of cycles between every pair of 
       permutation matchings */
    maxcyclesk = 0;
    for ( k = 1; k <= 3; k++ )
    {
        ncyclesk = 0;
        /* consider the cycles formed by the matchings of other permutations
           and the matching of permutation k */
        for ( l = 1; l <= 3; l++ )
        {
            find_half_cycles ( pm[k], l, mc, &ncyc );
            ncycles[k][l] = ncyc;
            ncyclesk += ncyc;
        }
        if ( ncyclesk > maxcyclesk )
        {
            maxcyclesk = ncyclesk;
            /* optimal solution is not yet stored */
        }
    }

    /* compute metric upper bound */
    totcycles = 0;
    for ( k = 1; k <= 3; k++ )
        for ( l = k + 1; l <= 3; l++ )
            totcycles += ncycles[k][l];

    UB = ( int ) ( totcycles / ( double ) ( 3 - 1 ) +
                   ( double ) ( 3 * mc ) / 2.0 );

    /* best feasible solution is initially the trivial one */
    realbest = 3 * mc - mindistk;

    /*  printf("... initial solution value = %d, initial LB = %d\n", 
       mindistk, 3 * mc - UB);   */

#ifdef LOCAL_SEARCH
    /* apply local search to current best solution to improve it */
    local_search ( n, vc, solmate );
#endif

    /* call main branch-and-bound recursion */
    /* this version has one call for each possible value of best */
    inibest = realbest;

    realub = metricub = UB;
    millnodes = treenodes = 0;
    if ( realbest == UB )
    {
        termin ( tf - ti, TRUE );
    }
    for ( best_now = UB - 1; best_now >= inibest; best_now-- )
    {
        nunreach = vc - 1;      /* initial (reached) node is 0 */
        for ( i = 0; i < nunreach; i++ )
        {
            unreach[i] = i + 1;
            inv_unreach[i + 1] = i;
        }
        oldbest = best_now;
        msbrbb ( 0, Num_Genes, UB, 0 );
        if ( oldbest < best_now )
            break;
        else
            realub = oldbest;
    }

    /* enumeration complete */
    i = median_distance ( pibest );
/*  fprintf(fout, "   LB=%d,   UB=%d,   MS=%d,       non-ms=%d\n", 3*mc-UB, mindistk, 3*(Num_Genes+1) - realbest, i);  */

    return pibest;
}
