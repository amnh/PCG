/* $Id: calc_medians.c 48 2005-04-13 15:56:25Z ron $
   Written by Adam Siepel, Spring 2001
   Copyright 2001, Adam Siepel */

/* Simple driver for median code.  Supports computation of breakpoint
   and trivial medians as well as of reversal medians.  Also allows
   _all_ medians to be found, and can report time required to find
   each median.  Expects input in form of a file of permutations; will
   compute median of each three successive permutations.  Some of this
   code was borrowed from GRAPPA. */

#include "structs.h"
#include "circ_order.h"
#include "inittree.h"
#include "bbtsp.h"
#include "convert.h"
#include "condense3.h"
#include "const_tree.h"
#include "condense.h"
#ifndef WINNT
#include <unistd.h>
#endif
#include <string.h>
#include <errno.h>
#include "uf.h"
#include "invdist.h"
#include <sys/types.h>
#include <sys/stat.h>
#ifndef WINNT
#include <sys/time.h>
#else
#include <time.h>
#endif
#include "getopt3.h"
#include "read_input.h"
#include "simpleio.h"
#include "inversion_median.h"
#include "all_inversion_medians.h"
#include "med_util.h"
#include "sorting_reversal_median.h"
#include "lists.h"


void
print_usage ( char *progname )
{
    fprintf ( stderr,
              "\nUSAGE: %s -f <datafile> -n <ngenes> [-o outfile] [-b|-v|-m|-a|-T]\n",
              progname );
    fprintf ( stderr,
              "\tb: compute breakpoint medians instead of reversal medians\n\tv: compute \"trivial\" medians\n\tm: compute reversal medians using \"metric\" algorithm\n\ta: compute ALL reversal medians (uses metric algorithm)\n\tT: report only the time required to find each median (microseconds)\n\nBy default, reversal medians are computed using the \"sorting\" algorithm.\n\n" );

    return;
}

int
main ( int argc, char *argv[] )
{
    FILE *input;
    FILE *outfile = stdout;
    char c;
    char *inputfname = NULL, *outputfname = NULL;
    struct stat statbuf;

    struct genome_struct **genomes;
    struct genome_struct medianstruct;

    int NUM_GENES = 0;          /* number of genes in input */
    int CIRCULAR = 0, TRIVIAL = 0;

    distmem_t *distmem;

    struct adj_struct *adj_list = ( struct adj_struct * ) NULL;
    struct adj_struct *adj_pool = ( struct adj_struct * ) NULL;
    int *genes = ( int * ) NULL;
    int *outcycle = ( int * ) NULL;
    int *stack = ( int * ) NULL;
    int *degree = ( int * ) NULL;
    int *otherEnd = ( int * ) NULL;
    edge_t *edges = ( edge_t * ) NULL;

    intpair_t *neighbors = ( intpair_t * ) NULL;
    int i, d01, d12, d02;
    int median[1000];
    char name[20];
    int score = 0;
    int ALLMEDS = 0;
    int TIMING_MODE = 0;
    int BREAKPTMED = 0;
    int METRICMED = 0;
    List medianlist;
    struct timeval tm1, tm2;
    MedianMemory *medmem = NULL;

    while ( ( c = getopt ( argc, argv, "f:o:n:bmvaT" ) ) != -1 )
    {
        switch ( c )
        {
            case 'f':
                inputfname = optarg;
                break;
            case 'o':
                outputfname = optarg;
                break;
            case 'n':
                NUM_GENES = atoi ( optarg );
                break;
            case 'v':
                TRIVIAL = 1;
                break;
            case 'a':
                ALLMEDS = 1;
                break;
            case 'T':
                TIMING_MODE = 1;
                break;
            case 'b':
                BREAKPTMED = 1;
                break;
            case 'm':
                METRICMED = 1;
                break;
            case '?':
                fprintf ( stderr, "Unrecognized option: -%c\n", optopt );
                exit ( -1 );
        }
    }

    /* check we have usable parameters */
    if ( NUM_GENES == 0 )
    {
        print_usage ( argv[0] );
        exit ( -1 );
    }

    if ( inputfname == ( char * ) NULL )
    {
        fprintf ( stderr, "ERROR: input filename required\n" );
        print_usage ( argv[0] );
        exit ( -1 );
    }
    else if ( stat ( inputfname, &statbuf ) ||
              ( input = fopen ( inputfname, "r" ) ) == NULL )
    {
        fprintf ( stderr, "ERROR: Could not open input file (%s): ",
                  inputfname );
        perror ( "" );
        print_usage ( argv[0] );
        exit ( -1 );
    }

    if ( outputfname != NULL )
    {
        outfile = fopen ( outputfname, "w+" );
        if ( outfile == NULL )
        {
            fprintf ( stderr, "ERROR: Could not open output file (%s): ",
                      outputfname );
            perror ( "" );
            print_usage ( argv[0] );
            exit ( -1 );
        }
    }


    if ( BREAKPTMED )
    {
        /* special alloc for breakpt med */
        outcycle =
            ( int * ) malloc ( ( 2 * NUM_GENES + 1 ) * sizeof ( int ) );
        genes = ( int * ) malloc ( NUM_GENES * sizeof ( int ) );
        degree = ( int * ) malloc ( ( 2 * NUM_GENES + 1 ) * sizeof ( int ) );
        otherEnd =
            ( int * ) malloc ( ( 2 * NUM_GENES + 1 ) * sizeof ( int ) );
        stack = ( int * ) malloc ( ( 2 * NUM_GENES + 1 ) * sizeof ( int ) );
        edges = ( edge_t * ) malloc ( ( 7 * NUM_GENES ) * sizeof ( edge_t ) );
        adj_pool =
            ( struct adj_struct * ) malloc ( ( 14 * NUM_GENES ) *
                                             sizeof ( struct adj_struct ) );
        adj_list =
            ( struct adj_struct * ) malloc ( ( 2 * NUM_GENES + 1 ) *
                                             sizeof ( struct adj_struct ) );
        neighbors =
            ( intpair_t * ) malloc ( ( 2 * NUM_GENES + 1 ) *
                                     sizeof ( intpair_t ) );
    }


    /* allocate memory for distance computations */
    distmem = new_distmem ( NUM_GENES );

    /* allocate memory for sorting median alg */
    if ( !TRIVIAL && !BREAKPTMED && !METRICMED && !ALLMEDS )
        medmem = new_median_memory ( NUM_GENES, 0, ( NUM_GENES + 1 ) * 2 );

    /* allocate genome structs */
    genomes =
        ( struct genome_struct ** ) malloc ( 3 *
                                             sizeof ( struct genome_struct
                                                      * ) );
    for ( i = 0; i < 3; i++ )
        genomes[i] =
            ( struct genome_struct * )
            malloc ( sizeof ( struct genome_struct ) );

    /* read sets of three until EOF */
    i = 0;
    while ( read3 ( input, genomes[0], genomes[1], genomes[2], NUM_GENES ) !=
            EOF )
    {
        i++;
        if ( TIMING_MODE )
            gettimeofday ( &tm1, NULL );

        if ( BREAKPTMED )
        {

            /* this borrowed wholesale from GRAPPA */
            convert2_to_tsp ( genomes[0], genomes[1], genomes[2],
                              adj_list, adj_pool, NUM_GENES, CIRCULAR );

            bbtsp ( 2 * NUM_GENES, median, FALSE,   /* cannot use median that does not exist */
                    genomes[0]->genes, genomes[1]->genes, genomes[2]->genes,
                    adj_list, neighbors, stack, outcycle, degree, otherEnd,
                    edges, CIRCULAR );

            medianstruct.genes = median;
        }
        else if ( TRIVIAL == 1 )
        {
            d01 =
                invdist_noncircular ( genomes[0], genomes[1], 0, NUM_GENES,
                                      distmem );
            d12 =
                invdist_noncircular ( genomes[1], genomes[2], 0, NUM_GENES,
                                      distmem );
            d02 =
                invdist_noncircular ( genomes[0], genomes[2], 0, NUM_GENES,
                                      distmem );

            /* find a smallest sum of two */
            if ( d01 >= d12 && d01 >= d02 )
            {
                medianstruct.genes = genomes[2]->genes;
            }
            else if ( d12 >= d01 && d12 >= d02 )
            {
                medianstruct.genes = genomes[0]->genes;
            }
            else
            {
                medianstruct.genes = genomes[1]->genes;
            }
        }
        else if ( ALLMEDS == 1 )
            find_all_inversion_medians ( &medianlist, genomes, NUM_GENES,
                                         distmem );
        else if ( METRICMED == 1 )
            find_inversion_median ( &medianstruct, genomes,
                                    NUM_GENES, distmem );
        else                    /* sorting median algorithm */
            find_reversal_median ( &medianstruct, genomes, NUM_GENES,
                                   medmem );

        if ( TIMING_MODE )
        {
            gettimeofday ( &tm2, NULL );
            fprintf ( outfile, "%ld\n",
                      ( tm2.tv_sec - tm1.tv_sec ) * 1000000 + tm2.tv_usec -
                      tm1.tv_usec );
        }
        else if ( ALLMEDS == 0 )
        {
            score = median_score ( genomes, &medianstruct,
                                   NUM_GENES, distmem );

            sprintf ( name, "median %d (%d)", i, score );
            print_genome ( outfile, name, medianstruct.genes, NUM_GENES );
        }
        else
        {                       /* ALLMEDS == 1 */
            while ( ( medianstruct.genes =
                      ( int * ) pop_queue ( &medianlist ) ) != NULL )
            {

                score = median_score ( genomes, &medianstruct,
                                       NUM_GENES, distmem );

                sprintf ( name, "median %d (%d)", i, score );
                print_genome ( outfile, name, medianstruct.genes, NUM_GENES );
                free ( medianstruct.genes );
            }
            free_list ( &medianlist );
        }
    }

    return ( 0 );
}
