#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "read_input.h"
#include "structs.h"
#include "binencode.h"
#include "invdist.h"
#include "condense.h"
#ifdef MPBPA
#include "mpi.h"
#endif

void
check_genes ( int *genes, int num_genes, int at_genome )
{
    int i, g0, g1;

    int *checkArr;

    checkArr = ( int * ) malloc ( ( num_genes + 1 ) * sizeof ( int ) );
    if ( checkArr == ( int * ) NULL )
        fprintf ( stderr, "ERROR: checkArr NULL\n" );

    for ( i = 1; i <= num_genes; i++ )
        checkArr[i] = 0;

    for ( i = 0; i < num_genes; i++ )
    {
        g0 = genes[i];
        g1 = ( g0 > 0 ? g0 : -g0 );
        if ( g1 == 0 )
        {
            fflush ( outfile );
            fclose ( outfile );
            fprintf ( stderr, "ERROR: Reading Input Genome %d\n", at_genome );
            fprintf ( stderr, "ERROR: Input gene == 0\n" );
            exit ( -1 );
        }
        if ( g1 > num_genes )
        {
            fflush ( outfile );
            fclose ( outfile );
            fprintf ( stderr, "ERROR: Reading Input Genome %d\n", at_genome );
            fprintf ( stderr,
                      "ERROR: Input gene (%d) larger than number of genes %d\n",
                      g0, num_genes );
            exit ( -1 );
        }
        if ( checkArr[g1] != 0 )
        {
            fflush ( outfile );
            fclose ( outfile );
            fprintf ( stderr, "ERROR: Reading Input Genome %d\n", at_genome );
            fprintf ( stderr, "ERROR: Duplicate gene (%d)\n", g1 );
            exit ( -1 );
        }

        checkArr[g1] = 1;
    }

    for ( i = 1; i <= num_genes; i++ )
    {
        if ( checkArr[i] != 1 )
        {
            fflush ( outfile );
            fclose ( outfile );
            fprintf ( stderr, "ERROR: Reading Input Genome %d\n", at_genome );
            fprintf ( stderr, "ERROR: Missing gene (%d)\n", i );
            exit ( -1 );
        }
    }

    free ( checkArr );
    return;
}

/* Read data file of gene orders in format: 
   >genome name 1 2 4 3 5 -6 -10 -9 -8 7 
   genes 1..n.  */
void
read_data ( FILE * input, struct genome_struct **genome_list,
            int *num_genes, int *num_genomes, int INVDIST, int CIRCULAR,
            int *condense_succ, int *condense_decode, int *orig_num_genes )
{
    int genome_num;
    int i, j, score;
    int a;
    int g;
    char buf[MAX_STR_LEN];
    char genebuf[MAX_STR_LEN];
    int readingName;
    int gnum;
    int NumGenomes, NumGenes;

    NumGenomes = 0;
    NumGenes = -1;
  /************/
    readingName = 1;
    j = 0;
    a = fgetc ( input );
    while ( a != EOF )
    {

        if ( j == MAX_STR_LEN )
        {
            fflush ( outfile );
            fclose ( outfile );
            fprintf ( stderr, "ERROR: Buffer overflow reading input\n" );
            exit ( -1 );
        }
        switch ( a )
        {
            case '\n':
                if ( j > 1 )
                {
                    buf[j] = '\0';
                    j = 0;
                    if ( readingName )
                    {
                        if ( buf[0] != '>' )
                        {
                            fflush ( outfile );
                            fclose ( outfile );
                            fprintf ( stderr,
                                      "\nERROR: reading genome name from input. " );
                            fprintf ( stderr,
                                      "Expecting format: \">Name\" \n" );
                            exit ( -1 );
                        }
                        NumGenomes++;
                    }
                    else
                    {
                        i = 0;
                        gnum = 0;
                        while ( i < strlen ( buf ) )
                        {
                            while ( buf[i] == ' ' )
                                i++;
                            while ( ( buf[i] != ' ' ) && ( buf[i] != '\0' ) )
                                i++;
                            gnum++;
                            i++;
                        }

                        if ( NumGenes < 0 )
                            NumGenes = gnum;
                        else
                        {
                            if ( NumGenes != gnum )
                            {
                                fflush ( outfile );
                                fclose ( outfile );
                                fprintf ( stderr,
                                          "ERROR: read_data() inconsistent num of genes\n" );
                                fprintf ( stderr,
                                          "ERROR: Expecting %d genes, but ",
                                          NumGenes );
                                fprintf ( stderr, "Genome %d has %d genes\n",
                                          NumGenomes, gnum );
                                exit ( -1 );
                            }
                        }
                    }

                    readingName = 1 - readingName;
                }
                break;
            case EOF:
                fprintf ( outfile, "EOF\n" );
                break;
            default:
                buf[j] = a;
                j++;
        }
        a = fgetc ( input );
    }

    if ( NumGenomes > MAX_GENOMES )
    {
        fprintf ( stderr,
                  "ERROR: \tNumber of genomes in the input is %d, but\n",
                  NumGenomes );
        fprintf ( stderr,
                  "\tGRAPPA is compiled with a maximum of %d genomes.\n",
                  MAX_GENOMES );
        fprintf ( stderr, "\tPlease edit the header file \"structs.h\", " );
        fprintf ( stderr, "increase the value\n" );
        fprintf ( stderr, "\tof MAX_GENOMES (currently set to %d), ",
                  MAX_GENOMES );
        fprintf ( stderr, "and recompile.\n" );
        exit ( -1 );
    }

    if ( NumGenes > MAX_NUM_GENES )
    {
        fprintf ( stderr,
                  "ERROR: \tNumber of genes in the input is %d, but\n",
                  NumGenes );
        fprintf ( stderr,
                  "\tGRAPPA is compiled with a maximum of %d genes.\n",
                  MAX_NUM_GENES );
        fprintf ( stderr, "\tPlease edit the header file \"structs.h\", " );
        fprintf ( stderr, "increase the value\n" );
        fprintf ( stderr, "\tof MAX_NUM_GENES (currently set to %d), ",
                  MAX_NUM_GENES );
        fprintf ( stderr, "and recompile.\n" );
        exit ( -1 );
    }

    rewind ( input );

#ifdef MPBPA
    if ( MYPROC == 0 )
    {
#endif
        fprintf ( outfile, "Number of Genomes:\t%d\n", NumGenomes );
        fprintf ( outfile, "Number of Genes:  \t%d\n", NumGenes );
        fflush ( outfile );
#ifdef MPBPA
    }
#endif

    *genome_list =
        ( struct genome_struct * ) malloc ( NumGenomes *
                                            sizeof ( struct genome_struct ) );
    if ( *genome_list == ( struct genome_struct * ) NULL )
        fprintf ( stderr, "ERROR: genome_list NULL\n" );

    for ( i = 0; i < NumGenomes; i++ )
    {
        ( *genome_list )[i].gnamePtr =
            ( char * ) malloc ( MAX_NAME * sizeof ( char ) );
        if ( ( *genome_list )[i].gnamePtr == ( char * ) NULL )
        {
            fprintf ( stderr, "ERROR: gname NULL\n" );
        }
    }

  /************/
    genome_num = 0;
    readingName = 1;
    j = 0;
    a = fgetc ( input );
    while ( a != EOF )
    {

        if ( j == MAX_STR_LEN )
        {
            fprintf ( stderr, "ERROR: Buffer overflow reading input\n" );
            exit ( -1 );
        }
        switch ( a )
        {
            case '\n':
                if ( j > 1 )
                {
                    buf[j] = '\0';
                    j = 0;
                    if ( readingName )
                    {
                        if ( buf[0] != '>' )
                        {
                            fprintf ( stderr,
                                      "\nERROR: reading genome name from input. " );
                            fprintf ( stderr,
                                      "Expecting format: \">Name\" \n" );
                            exit ( -1 );
                        }
                        if ( strlen ( buf + 1 ) > MAX_STR_LEN )
                            fprintf ( stderr,
                                      "ERROR: Buffer overflow in genome name\n" );
                        strcpy ( ( *genome_list )[genome_num].gnamePtr,
                                 buf + 1 );
                        ( *genome_list )[genome_num].genome_num =
                            genome_num + 1;
                        ( *genome_list )[genome_num].genes =
                            ( int * ) malloc ( NumGenes * sizeof ( int ) );
                        if ( ( *genome_list )[genome_num].genes == NULL )
                            fprintf ( stderr,
                                      "ERROR: cannot allocate genes\n" );

                    }
                    else
                    {
                        i = 0;
                        gnum = 0;
                        while ( i < strlen ( buf ) )
                        {
                            g = 0;
                            while ( buf[i] == ' ' )
                                i++;
                            while ( ( buf[i] != ' ' ) && ( buf[i] != '\0' ) )
                                genebuf[g++] = buf[i++];
                            genebuf[g] = '\0';
                            ( *genome_list )[genome_num].genes[gnum] =
                                atoi ( genebuf );
                            gnum++;
                            g = 0;
                            i++;
                        }

                        check_genes ( ( *genome_list )[genome_num].genes,
                                      NumGenes, genome_num + 1 );

                        genome_num++;
                    }
                    readingName = 1 - readingName;
                }
                break;
            case EOF:
                fprintf ( outfile, "EOF\n" );
                break;
            default:
                buf[j] = a;
                j++;
        }
        a = fgetc ( input );
    }

    fclose ( input );

#ifdef MPBPA
    if ( MYPROC == 0 )
    {
#endif
/* STACIA REDUCING OUTPUT 
    printGenomes(*genome_list, NumGenomes, NumGenes);
    fprintf(outfile,"\n");
*/
#ifdef MPBPA
    }
#endif

    if ( INVDIST )
    {
#ifdef MPBPA
        if ( MYPROC == 0 )
        {
#endif
            if ( NumGenomes != 2 )
            {
                fprintf ( stderr, "WARNING: NUM_GENOMES == %3d ",
                          NumGenomes );
                fprintf ( stderr,
                          "  Only the first two genomes will be used\n\n" );
            }
            score =
                invdist_circular_nomem ( *genome_list, *genome_list + 1,
                                         NumGenes );
            printf ( "score = %d\n", score );

            fprintf ( outfile, "Inversion Distance: %12d\n", score );

            fprintf ( outfile, "Breakpoint Distance: %12d\n",
                      hamming_distance_nomem ( *genome_list, *genome_list + 1,
                                               NumGenes, CIRCULAR ) );
#ifdef MPBPA
        }
        MPI_Barrier ( MPI_COMM_WORLD );
        MPI_Finalize (  );
#endif
        fflush ( outfile );
        fclose ( outfile );
        exit ( 1 );
    }

    *orig_num_genes = NumGenes;

    if ( NumGenomes >= 3 )
        condense_genes ( *genome_list, NumGenomes, &NumGenes, CIRCULAR,
                         condense_succ + NumGenes,
                         condense_decode + NumGenes );

#ifdef VERYVERBOSE
#ifdef MPBPA
    if ( MYPROC == 0 )
    {
#endif
        fprintf ( outfile, "Condensed Genomes:\n" );
        printGenomes ( *genome_list, NumGenomes, NumGenes );
        fprintf ( outfile, "\n" );
#ifdef MPBPA
    }
    MPI_Barrier ( MPI_COMM_WORLD );
#endif
#endif

    *num_genes = NumGenes;
    *num_genomes = NumGenomes;

    return;
}
