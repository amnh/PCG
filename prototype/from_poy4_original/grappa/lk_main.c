#ifdef CONCORDE

#include "machdefs.h"
#include "linkern.h"
#include "util.h"
#include "kdtree.h"
#include "edgegen.h"
#include "macrorus.h"
#include "tsp.h"

void
randcycle ( int ncount, int *cyc, CCrandstate * rstate )
{
    int i, k, temp;

    for ( i = 0; i < ncount; i++ )
        cyc[i] = i;

    for ( i = ncount; i > 1; i-- )
    {
        k = CCutil_lprand ( rstate ) % i;
        CC_SWAP ( cyc[i - 1], cyc[k], temp );
    }
}

void
fill_dat ( int ncount, CCdatagroup * dat, int **adj )
{
    int i, j;

    CCutil_init_datagroup ( dat );
    CCutil_dat_setnorm ( dat, CC_MATRIXNORM );

    dat->adj = CC_SAFE_MALLOC ( ncount, int * );
    dat->adjspace = CC_SAFE_MALLOC ( ncount * ( ncount + 1 ) / 2, int );
    if ( dat->adj == ( int ** ) NULL || dat->adjspace == ( int * ) NULL )
    {
        CC_IFFREE ( dat->adj, int * );
        CC_IFFREE ( dat->adjspace, int );
    }
    for ( i = 0, j = 0; i < ncount; i++ )
    {
        dat->adj[i] = dat->adjspace + j;
        j += ( i + 1 );
    }
    for ( i = 0; i < ncount; i++ )
    {
        for ( j = 0; j <= i; j++ )
        {
            dat->adj[i][j] = adj[i][j];
        }
    }
    return;
}

int
chlinkern ( int ncount, int **adj, int *tour, int *incycle, int *outcycle )
{
    double val;
    int tempcount, *templist;
#if 0
    int *incycle = ( int * ) NULL, *outcycle = ( int * ) NULL;
#endif
    CCdatagroup dat;
    CCrandstate rstate;
    int attempt = 1;
    int norm;
    int seed;
    int nearnum;
    int quadtry = 2;
    int run_silently = 1;
    int in_repeater;
    double time_bound = -1.0;
    double length_bound = -1.0;
    int i, genes;
    int kick_type = CC_LK_RANDOM_KICK;


    fill_dat ( ncount, &dat, adj );

    seed = ( int ) CCutil_real_zeit (  );
    CCutil_sprand ( seed, &rstate );

#if TSPOUT
    printf ( "Chained Lin-Kernighan with seed %d\n", seed );
    fflush ( outfile );
#endif

    CCutil_dat_getnorm ( &dat, &norm );

    in_repeater = ncount;

    nearnum = 4 * quadtry;

    if ( CCedgegen_junk_k_nearest ( ncount, nearnum, &dat,
                                    ( double * ) NULL, 1,
                                    &tempcount, &templist, run_silently ) )
    {
        fprintf ( stderr, "CCedgegen_junk_k_nearest failed\n" );
        val = -1.0;
        goto CLEANUP;
    }

#if 0
    randcycle ( ncount, incycle, &rstate );
#else
    if ( CCedgegen_junk_nearest_neighbor_tour
         ( ncount, CCutil_lprand ( &rstate ) % ncount, &dat, incycle,
           &val, run_silently ) )
    {
        fprintf ( stderr, "CCedgegen_junk_nearest_neighbor_tour failed\n" );
        val = -1.0;
        goto CLEANUP;
    }
#endif

    do
    {
        if ( CClinkern_tour ( ncount, &dat, tempcount, templist, 10000000,
                              in_repeater, incycle, outcycle, &val,
                              run_silently, time_bound, length_bound,
                              /*saveit_name */ ( char * ) NULL, kick_type,
                              &rstate ) )
        {
            fprintf ( stderr, "CClinkern_tour failed\n" );
            val = -1.0;
            goto CLEANUP;
        }
        if ( length_bound != -1 && val > length_bound )
        {
            printf ( "Cycle of value %.0f  -  did not reach %.0f\n",
                     val, length_bound );
            printf ( "Try again. Number of attempts: %d\n", ++attempt );
        }
    }
    while ( length_bound != -1 && val > length_bound );

#if 1
    /* Fill tour */
    genes = ncount / 2;
    if ( ( outcycle[0] == outcycle[1] - genes ) ||
         ( outcycle[1] == outcycle[0] - genes ) )
    {
        for ( i = 0; i < genes; i++ )
        {
            tour[i] = 1 + outcycle[2 * i];
            if ( tour[i] > genes )
            {
                tour[i] = -( tour[i] - genes );
            }
        }
    }
    else
    {
        for ( i = 0; i < genes; i++ )
        {
            tour[i] = 1 + outcycle[( 2 * i + 1 ) % ncount];
            if ( tour[i] > genes )
                tour[i] = -( tour[i] - genes );
        }
    }
#endif

#if TSPOUT
    printf ( "Final Cycle: %.0f\n", val );
    fflush ( outfile );
#endif

  CLEANUP:

#ifndef BIG_PROBLEM
    CC_IFFREE ( templist, int );
#endif
    CCutil_freedatagroup ( &dat );

    return ( ( int ) val );
}


int
greedylk ( int ncount, int **adj, int *tour, int *incycle, int *outcycle )
{
    double val;
    int tempcount, *templist;
#if 0
    int *incycle = ( int * ) NULL, *outcycle = ( int * ) NULL;
#endif
    CCdatagroup dat;
    int rval = 0;
    CCrandstate rstate;
    int run_silently = 1;
    double time_bound = 0.0;
    double length_bound = 0.0;
    int i, genes;
    int kick_type = CC_LK_RANDOM_KICK;
    CCedgegengroup plan;
    int ecount;
    int *elist = ( int * ) NULL;
    int *elen = ( int * ) NULL;
    double *x;
    int seed;

    fill_dat ( ncount, &dat, adj );

#if TSPOUT
    printf ( "Greedy + Lin-Kernighan\n" );
    fflush ( outfile );
#endif

    ecount = ( ncount * ( ncount - 1 ) ) / 2;
    seed = ( int ) CCutil_real_zeit (  );
    CCutil_sprand ( seed, &rstate );

    CCutil_genedgelist ( ncount, ecount, &elist, &elen, &dat, -1, &rstate );

    x = CC_SAFE_MALLOC ( ecount, double );
    for ( i = 0; i < ecount; i++ )
        x[i] = ( double ) elen[i];

    rval = CCtsp_x_greedy_tour ( &dat, ncount, ecount, elist, x, incycle,
                                 &val, run_silently );
    if ( rval )
    {
        fprintf ( stderr, "CCtsp_x_greedy_tour failed\n" );
        goto CLEANUP;
    }

    CCedgegen_init_edgegengroup ( &plan );
    plan.quadnearest = 2;

    rval =
        CCedgegen_edges ( &plan, ncount, &dat, ( double * ) NULL, &tempcount,
                          &templist, run_silently, &rstate );
    if ( rval )
    {
        fprintf ( stderr, "CCedgegen_edges failed\n" );
        goto CLEANUP;
    }

    rval = CClinkern_tour ( ncount, &dat, tempcount, templist, /* ncount */ 0,
                            /* ncount > 1000 ? 500 : ncount/2 */ 0,
                            incycle, outcycle, &val,
                            run_silently, time_bound, length_bound,
                            ( char * ) NULL, kick_type, &rstate );
    if ( rval )
    {
        fprintf ( stderr, "CClinkern_tour failed\n" );
        goto CLEANUP;
    }

#if 1
    /* Fill tour */
    genes = ncount / 2;
    if ( ( outcycle[0] == outcycle[1] - genes ) ||
         ( outcycle[1] == outcycle[0] - genes ) )
    {
        for ( i = 0; i < genes; i++ )
        {
            tour[i] = 1 + outcycle[2 * i];
            if ( tour[i] > genes )
            {
                tour[i] = -( tour[i] - genes );
            }
        }
    }
    else
    {
        for ( i = 0; i < genes; i++ )
        {
            tour[i] = 1 + outcycle[( 2 * i + 1 ) % ncount];
            if ( tour[i] > genes )
                tour[i] = -( tour[i] - genes );
        }
    }
#endif

#if TSPOUT
    printf ( "Final Cycle: %.0f\n", val );
    fflush ( outfile );
#endif

  CLEANUP:
#ifndef BIG_PROBLEM
    CC_IFFREE ( templist, int );
#endif
    CCutil_freedatagroup ( &dat );
    CC_IFFREE ( elist, int );
    CC_IFFREE ( elen, int );
    CC_IFFREE ( x, double );

    return ( ( int ) val );
}

#endif
