#include <stdio.h>
#include <stdlib.h>
#ifndef CYGWINNT
#include <string.h>
#endif
#include <math.h>
#include "structs.h"
#include "binencode.h"
FILE *outfile;
extern int NUM_GENOMES;

int
mapToPolarity ( int gene, int num_genes )
{
    if ( gene > num_genes )
        gene -= ( 2 * num_genes + 1 );
    return ( gene );
}

int
mapToLinear ( int gene, int num_genes )
{
    if ( gene < 0 )
        gene += ( 2 * num_genes + 1 );
    return ( gene );
}

void
fillGenomes ( struct genome_struct *g, int num_genomes, int num_genes )
{
    int i, j;
    int r, tmp;

    g[0].genome_num = 1;
    sprintf ( g[0].gnamePtr, "Genome %3d", g[0].genome_num );
    for ( j = 0; j < num_genes; j++ )
        g[0].genes[j] = ( j + 1 );

    for ( i = 1; i < num_genomes; i++ )
    {
        g[i].genome_num = i + 1;
        sprintf ( g[i].gnamePtr, "Genome %3d", g[i].genome_num );
        for ( j = 0; j < num_genes; j++ )
        {
            if ( random (  ) <= MAX_RAND / 2 )
                g[i].genes[j] = ( j + 1 );
            else
                g[i].genes[j] = -( j + 1 );
        }
        for ( j = num_genes - 1; j >= 1; j-- )
        {
            r = ( int ) floor ( ( double ) ( j - 1 ) *
                                ( ( double ) random (  ) /
                                  ( double ) MAX_RAND ) );
            tmp = g[i].genes[r];
            g[i].genes[r] = g[i].genes[j];
            g[i].genes[j] = tmp;
        }
    }
    return;
}

void
fillGenomeRand ( int *genes, int num_genes )
{
    int j;
    int r, tmp;

    for ( j = 0; j < num_genes; j++ )
    {
        if ( random (  ) <= MAX_RAND / 2 )
            genes[j] = ( j + 1 );
        else
            genes[j] = -( j + 1 );
    }
    for ( j = num_genes - 1; j >= 1; j-- )
    {
        r = ( int ) floor ( ( double ) ( j - 1 ) *
                            ( ( double ) random (  ) /
                              ( double ) MAX_RAND ) );
        tmp = genes[r];
        genes[r] = genes[j];
        genes[j] = tmp;
    }
    return;
}

void
printGenomes ( struct genome_struct *g, int num_genomes, int num_genes )
{
    int i, j;

    fprintf ( outfile, "Genomes:\n\n" );
    for ( i = 0; i < num_genomes; i++ )
    {
        fprintf ( outfile, "(%3d) %16s:", g[i].genome_num, g[i].gnamePtr );
        for ( j = 0; j < num_genes; j++ )
        {
            fprintf ( outfile, "%3d ", g[i].genes[j] );
        }
        fprintf ( outfile, "\n" );
    }
    fprintf ( outfile, "\n" );
    return;
}

void
createGeneAdjMatrix ( int ***adj, int num_genes )
{
    int i, j, num;
    num = 2 * num_genes + 1;
    *adj = ( int ** ) malloc ( num * sizeof ( int * ) );
    if ( *adj == ( int ** ) NULL )
        fprintf ( stderr, "ERROR: adj NULL\n" );
    for ( i = 0; i < num; i++ )
    {
        ( *adj )[i] = ( int * ) malloc ( num * sizeof ( int ) );
        if ( ( *adj )[i] == ( int * ) NULL )
            fprintf ( stderr, "ERROR: adj[%d] NULL\n", i );
        for ( j = 0; j < num; j++ )
            ( *adj )[i][j] = 0;
    }
    return;
}

void
destroyGeneAdjMatrix ( int **adj, int num_genes )
{
    int i, num;
    num = 2 * num_genes;
    for ( i = num - 1; i >= 0; i-- )
        free ( adj[i] );
    free ( adj );
    return;
}

void
setAdj ( int **adj, int num_genes, int g0, int g1 )
{
    int index0, index1;

    index0 = mapToLinear ( g0, num_genes );
    index1 = mapToLinear ( g1, num_genes );
    adj[index0][index1] = 1;

    index0 = mapToLinear ( -g1, num_genes );
    index1 = mapToLinear ( -g0, num_genes );
    adj[index0][index1] = 1;

    return;
}

void
fillGeneAdjMatrix ( int **adj, struct genome_struct *mygenomes,
                    int num_genomes, int num_genes )
{
    int i, j;
    int g0, g1;

    for ( i = 0; i < num_genomes; i++ )
    {
        for ( j = 0; j < num_genes; j++ )
        {
            g0 = mygenomes[i].genes[j];
            g1 = mygenomes[i].genes[( j + 1 ) % num_genes];
            setAdj ( adj, num_genes, g0, g1 );
        }
    }
    return;
}

void
printGeneAdjMatrix ( int **adj, int num_genes )
{
    int i, j;
    int num;

    num = 2 * num_genes;

    fprintf ( outfile, "    \t" );
    for ( j = 1; j <= num; j++ )
    {
        fprintf ( outfile, "%3d \t", mapToPolarity ( j, num_genes ) );
    }
    fprintf ( outfile, "\n" );

    for ( i = 1; i <= num; i++ )
    {
        fprintf ( outfile, "%3d: \t", mapToPolarity ( i, num_genes ) );
        for ( j = 1; j <= num; j++ )
        {
            fprintf ( outfile, "%3d \t",
                      mapToPolarity ( adj[i][j], num_genes ) );
        }
        fprintf ( outfile, "\n" );
    }
    fprintf ( outfile, "\n" );
    return;
}


int
getAdjNum ( int **adj, int num_genes )
{
    int i, j;
    int num;
    int k;

    num = 2 * num_genes;
    k = 0;

    for ( i = 1; i <= num; i++ )
        for ( j = 1; j <= num; j++ )
            k += adj[i][j];

    k /= 2;

    return ( k );
}

int
containsPair ( struct genome_struct *g, int G0, int G1, int num_genes )
{
    int found;
    int i, p0, p1;

    found = FALSE;
    i = 0;
    while ( ( i < num_genes ) && ( !found ) )
    {
        p0 = g->genes[i];
        p1 = g->genes[( i + 1 ) % num_genes];
        found = ( ( p0 == G0 && p1 == G1 ) || ( p0 == -G1 && p1 == -G0 ) );
        i++;
    }

    return ( found );
}

void
encodeAdjGenomes ( int **adj, struct genome_struct *mygenomes,
                   int num_genomes, int num_genes )
{
    int i, j, v, num;
    int width;
    int *geneVectorG0;
    int *geneVectorG1;

    width = getAdjNum ( adj, num_genes );

    geneVectorG0 = ( int * ) malloc ( width * sizeof ( int ) );
    if ( geneVectorG0 == ( int * ) NULL )
        fprintf ( stderr, "ERROR: geneVectorG0 NULL\n" );
    geneVectorG1 = ( int * ) malloc ( width * sizeof ( int ) );
    if ( geneVectorG1 == ( int * ) NULL )
        fprintf ( stderr, "ERROR: geneVectorG1 NULL\n" );

    v = 0;
    for ( i = 1; i < num_genes; i++ )
    {
        geneVectorG0[v] = i;
        geneVectorG1[v] = i + 1;
        adj[mapToLinear ( i, num_genes )][mapToLinear ( i + 1, num_genes )] =
            0;
        adj[mapToLinear ( -( i + 1 ), num_genes )][mapToLinear
                                                   ( -i, num_genes )] = 0;
        v++;
    }
    geneVectorG0[v] = num_genes;
    geneVectorG1[v] = 1;
    adj[mapToLinear ( num_genes, num_genes )][mapToLinear ( 1, num_genes )] =
        0;
    adj[mapToLinear ( -1, num_genes )][mapToLinear ( -num_genes, num_genes )]
        = 0;
    v++;

    num = 2 * num_genes;
    for ( i = 1; i <= num; i++ )
        for ( j = 1; j <= num; j++ )
            if ( adj[i][j] )
            {
                geneVectorG0[v] = mapToPolarity ( i, num_genes );
                geneVectorG1[v] = mapToPolarity ( j, num_genes );
                adj[mapToLinear ( i, num_genes )][mapToLinear
                                                  ( j, num_genes )] = 0;
                adj[mapToLinear ( -j, num_genes )][mapToLinear
                                                   ( -i, num_genes )] = 0;
                v++;
            }


#if DEBUG
    fprintf ( outfile, "VECTOR GENES:\n" );
    fprintf ( outfile, "width: %d    v: %d\n", width, v );
    for ( i = 0; i < v; i++ )
        fprintf ( outfile, "V[%3d]: ( %4d, %4d)\n", i, geneVectorG0[i],
                  geneVectorG1[i] );
    fprintf ( outfile, "\n" );
#endif

    for ( i = 0; i < num_genomes; i++ )
    {
        mygenomes[i].encoding =
            ( char * ) malloc ( ( width + 1 ) * sizeof ( char ) );
        if ( mygenomes[i].encoding == ( char * ) NULL )
            fprintf ( stderr, "ERROR: encoding NULL\n" );
        for ( j = 0; j < width; j++ )
        {
            if ( containsPair ( mygenomes + i,
                                geneVectorG0[j], geneVectorG1[j],
                                num_genes ) )
            {
                mygenomes[i].encoding[j] = '1';
            }
            else
            {
                mygenomes[i].encoding[j] = '0';
            }
        }
        mygenomes[i].encoding[width] = '\0';
    }


    free ( geneVectorG1 );
    free ( geneVectorG0 );
    return;
}

void
freeGenomes ( struct genome_struct *mygenomes, int num_genomes )
{
    int i;
    for ( i = 0; i < num_genomes; i++ )
        free ( mygenomes[i].encoding );
    free ( mygenomes );
    return;
}


void
printGenomeEncodings ( struct genome_struct *g, int num_genomes )
{
    int i;
    fprintf ( outfile, "GENOME ENCODINGS:\n" );
    for ( i = 0; i < num_genomes; i++ )
    {
        fprintf ( outfile, "%s: %s\n", g[i].gnamePtr, g[i].encoding );
    }
    fprintf ( outfile, "\n" );

    return;
}

void
encodeGenomes ( struct genome_struct *mygenomes,
                int num_genomes, int num_genes )
{
    int **adj;

    createGeneAdjMatrix ( &adj, num_genes );

    fillGeneAdjMatrix ( adj, mygenomes, num_genomes, num_genes );

#if DEBUG
    printGeneAdjMatrix ( adj, num_genes );
#endif

    encodeAdjGenomes ( adj, mygenomes, num_genomes, num_genes );

    destroyGeneAdjMatrix ( adj, num_genes );

    return;
}

int
hamming_distance_from_encoding ( struct genome_struct *g1,
                                 struct genome_struct *g2 )
{
    int i, num;
    int width;
    num = 0;

    width = strlen ( g1->encoding );

    if ( width != strlen ( g2->encoding ) )
        fprintf ( stderr, "ERROR: hamming_distance_from_encoding\n" );

    for ( i = 0; i < width; i++ )
    {
        num += ( g1->encoding[i] ^ g2->encoding[i] );
    }
    return ( num );
}

void
findAllHammingDistancesFromEncoding ( struct genome_struct *mygenomes,
                                      int num_genomes, int num_genes )
{
    int i, j, dist;

    fprintf ( outfile, "HAMMING DISTANCE From ENCODING:\n" );
    for ( i = 0; i < num_genomes; i++ )
    {
        for ( j = i + 1; j < num_genomes; j++ )
        {
            dist =
                hamming_distance_from_encoding ( mygenomes + i,
                                                 mygenomes + j );
            fprintf ( outfile,
                      "The Hamming Distance between %s and %s is %3d (BP=%3d)\n",
                      mygenomes[i].gnamePtr, mygenomes[j].gnamePtr, dist,
                      dist / 2 );
        }
    }
    fprintf ( outfile, "\n" );
    return;
}

int
hamming_distance_quad ( struct genome_struct *g1, struct genome_struct *g2,
                        int num_genes )
{
    int i, num;
    int gene0, gene1;
    num = 0;

    for ( i = 0; i < num_genes; i++ )
    {
        gene0 = g1->genes[i];
        gene1 = g1->genes[( i + 1 ) % num_genes];
        num += containsPair ( g2, gene0, gene1, num_genes );
    }

    num = num_genes - num;
    return ( num );
}


#define setAdj(g0, g1, gAdj, n) \
    if ((g0)>0) \
      gAdj[(g0)] = (g1); \
    else \
      gAdj[(n) - (g0)] = (g1); \
    if ((g1)>0) \
      gAdj[(n) + (g1)] = -(g0); \
    else \
      gAdj[-(g1)] = -(g0);

#define checkAdj(g0, g1, gAdj, n) \
    if ((g0)>0) { \
      if ((g1)>0) { \
        if ((gAdj[(g0)] != (g1)) && (gAdj[(n) + (g1)] != -(g0))) \
          num++; \
      } \
      else { \
        if ((gAdj[(g0)] != (g1)) && (gAdj[-(g1)] != -(g0))) \
          num++; \
      } \
    } \
    else { \
      if ((g1)>0) { \
        if ((gAdj[(n) - (g0)] != (g1)) && (gAdj[(n) + (g1)] != -(g0))) \
          num++; \
      } \
      else { \
        if ((gAdj[(n) - (g0)] != (g1)) && (gAdj[-(g1)] != -(g0))) \
          num++; \
      } \
    }

int
hamming_distance ( struct genome_struct *g1, struct genome_struct *g2,
                   int num_genes, int CIRCULAR, int *geneAdj )
{
    register int i;
    register int gene0, gene1, firstgene;
    int num;

    num = 0;

    firstgene = gene0 = g1->genes[0];
    gene1 = g1->genes[1];
    setAdj ( gene0, gene1, geneAdj, num_genes );
    for ( i = 1; i < num_genes - 1; i++ )
    {
        gene0 = gene1;
        gene1 = g1->genes[i + 1];
        setAdj ( gene0, gene1, geneAdj, num_genes );
    }
    gene0 = gene1;
    gene1 = firstgene;
    setAdj ( gene0, gene1, geneAdj, num_genes );

    firstgene = gene0 = g2->genes[0];
    gene1 = g2->genes[1];
    checkAdj ( gene0, gene1, geneAdj, num_genes );
    for ( i = 1; i < num_genes - 1; i++ )
    {
        gene0 = gene1;
        gene1 = g2->genes[i + 1];
        checkAdj ( gene0, gene1, geneAdj, num_genes );
    }

    if ( CIRCULAR )
    {
        gene0 = gene1;
        gene1 = firstgene;
        checkAdj ( gene0, gene1, geneAdj, num_genes );
    }

    return ( num );
}

int
hamming_distance_nomem ( struct genome_struct *g1, struct genome_struct *g2,
                         int num_genes, int CIRCULAR )
{
    int dist;
    int *geneAdj;
    geneAdj = ( int * ) malloc ( ( num_genes + 1 ) * 2 * sizeof ( int ) );

    
    if ( geneAdj == ( int * ) NULL )
        fprintf ( stderr, "ERROR: geneAdj NULL\n" );

    dist = hamming_distance ( g1, g2, num_genes, CIRCULAR, geneAdj );

    free ( geneAdj );
    return ( dist );
}

void
setBPmatrix ( int **distmatrix, struct genome_struct *genomes,
              int num_genes, int num_genomes, distmem_t * distmem,
              int CIRCULAR )
{
    int i, j;

    for ( i = 0; i < num_genomes; i++ )
    {
        distmatrix[i][i] = 0;
        for ( j = i + 1; j < num_genomes; j++ )
        {
            distmatrix[i][j] = distmatrix[j][i] =
                hamming_distance ( genomes + i, genomes + j, num_genes,
                                   CIRCULAR, distmem->hammingArr );
        }
    }
}

void
findAllHammingDistancesFromScratch ( struct genome_struct *mygenomes,
                                     int num_genomes, int num_genes,
                                     int CIRCULAR )
{
    int i, j, dist;
    int *geneAdj;

    geneAdj = ( int * ) malloc ( ( num_genes + 1 ) * 2 * sizeof ( int ) );

    fprintf ( outfile, "HAMMING DISTANCE From SCRATCH:\n" );
    for ( i = 0; i < num_genomes; i++ )
    {
        for ( j = i + 1; j < num_genomes; j++ )
        {
            dist = hamming_distance ( mygenomes + i, mygenomes + j, num_genes,
                                      CIRCULAR, geneAdj );
            fprintf ( outfile,
                      "The Hamming Distance between %s and %s is %3d (BP=%3d)\n",
                      mygenomes[i].gnamePtr, mygenomes[j].gnamePtr, dist,
                      dist / 2 );
        }
    }
    fprintf ( outfile, "\n" );

    free ( geneAdj );

    return;
}

void
main_binencode (  )
{
    int NUM_GENES = 0;
    int NUM_GENOMES = 0;
    int CIRCULAR = TRUE;

    struct genome_struct *mygenomes;

    /* Allocate the genomes */
    mygenomes = ( struct genome_struct * )
        malloc ( NUM_GENOMES * sizeof ( struct genome_struct ) );
    if ( mygenomes == ( struct genome_struct * ) NULL )
        fprintf ( stderr, "ERROR: mygenomes is NULL\n" );

    /* Fill the genomes with sample data */
    fillGenomes ( mygenomes, NUM_GENOMES, NUM_GENES );

    /* Print out the original genomes */
    printGenomes ( mygenomes, NUM_GENOMES, NUM_GENES );

    /* Encode the genomes */
    encodeGenomes ( mygenomes, NUM_GENOMES, NUM_GENES );

    /* Print out the genome encodings */
    printGenomeEncodings ( mygenomes, NUM_GENOMES );

    /* Calculate Hamming Distances */
    findAllHammingDistancesFromEncoding ( mygenomes, NUM_GENOMES, NUM_GENES );

    /* Calculate Hamming Distances */
    findAllHammingDistancesFromScratch ( mygenomes, NUM_GENOMES, NUM_GENES,
                                         CIRCULAR );

    /* Free the genomes */
    freeGenomes ( mygenomes, NUM_GENOMES );

    return;
}
