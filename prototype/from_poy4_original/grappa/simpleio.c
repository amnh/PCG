/* $Id: simpleio.c 48 2005-04-13 15:56:25Z ron $
   Written by Adam Siepel, Spring 2001 
   Copyright 2001, Adam Siepel */

/* Simple utilities to read and write permutations */

#include "simpleio.h"

void
print_genome ( FILE * outf, char *name, int *genome, int ngenes )
{
    int i;
    fprintf ( outf, "> %s\n", name );
    for ( i = 0; i < ngenes; i++ )
        fprintf ( outf, "%d ", genome[i] );
    fprintf ( outf, "\n" );
}

int
read3 ( FILE * in, struct genome_struct *gen1, struct genome_struct *gen2,
        struct genome_struct *gen3, int ngenes )
{
    if ( read_genome ( in, gen1, ngenes ) == EOF )
        return EOF;
    if ( read_genome ( in, gen2, ngenes ) == EOF )
        return EOF;
    if ( read_genome ( in, gen3, ngenes ) == EOF )
        return EOF;

    return 0;
}

int
read_genome ( FILE * in, struct genome_struct *genome, int ngenes )
{
    char tmp[100];
    int i;

    if ( fgets ( tmp, 100, in ) == NULL )
        return EOF;

    genome->genes = ( int * ) malloc ( ngenes * sizeof ( int ) );

    for ( i = 0; i < ngenes; i++ )
        if ( fscanf ( in, "%d ", &( genome->genes[i] ) ) <= 0 )
        {
            fprintf ( stderr, "ERROR reading gene %d for genome %s.\n", i,
                      tmp );
            exit ( 1 );
        }


    return 0;
}
