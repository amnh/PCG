#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "condense.h"

#define NEWCODE

typedef struct chain_s
{
    int adj;
    struct chain_s *next;
} chain_t;

void
condense_genes ( struct genome_struct *genome_list,
                 int num_genomes, int *num_genes, int CIRCULAR,
                 int *succ, int *decode )
{
    int *genes_map;
    int *genes_in_chain;
    int **adj;
    int maxAdj;
    int i, j, k, g0, g1, gnum;
#ifndef NEWCODE
    int gmap;
#endif
    int *g;
    int n;
    chain_t **chain;
    chain_t *newchain;
    chain_t *chainptr, *nextptr, *lastchain;
    int *endAdj;
    int new_n;
    int maxpos;
    int *code_m, *code;

    n = *num_genes;
    maxAdj = 2 * n + 1;
    adj = ( int ** ) malloc ( maxAdj * sizeof ( int * ) );
    if ( adj == ( int ** ) NULL )
        fprintf ( stderr, "ERROR: adj NULL\n" );
    for ( i = 1; i < maxAdj; i++ )
    {
        adj[i] = ( int * ) malloc ( maxAdj * sizeof ( int ) );
        if ( adj[i] == ( int * ) NULL )
            fprintf ( stderr, "ERROR: adj[i] NULL\n" );
    }

    endAdj = ( int * ) malloc ( maxAdj * sizeof ( int ) );
    if ( endAdj == ( int * ) NULL )
        fprintf ( stderr, "ERROR: edjAdj NULL\n" );

    genes_map = ( int * ) malloc ( ( n + 1 ) * sizeof ( int ) );
    if ( genes_map == ( int * ) NULL )
        fprintf ( stderr, "ERROR: genes_map NULL\n" );

    genes_in_chain = ( int * ) malloc ( ( n + 1 ) * sizeof ( int ) );
    if ( genes_in_chain == ( int * ) NULL )
        fprintf ( stderr, "ERROR: genes_in_chain NULL\n" );

    code_m = ( int * ) malloc ( maxAdj * sizeof ( int ) );
    if ( code_m == ( int * ) NULL )
        fprintf ( stderr, "ERROR: code_m NULL\n" );
    code = code_m + n;


    for ( i = 1; i < maxAdj; i++ )
        for ( j = 1; j < maxAdj; j++ )
            adj[i][j] = 0;

    for ( i = 0; i < num_genomes; i++ )
    {
        g = genome_list[i].genes;
        if ( CIRCULAR )
            maxpos = n;
        else
            maxpos = n - 1;
        for ( j = 0; j < maxpos; j++ )
        {
            g0 = g[j];
            g1 = g[( j + 1 ) % n];

            if ( g0 < 0 )
                g0 = n - g0;
            if ( g1 < 0 )
                g1 = n - g1;
            adj[g0][g1]++;

            if ( g0 <= n )
                g0 = g0 + n;
            else
                g0 = g0 - n;
            if ( g1 <= n )
                g1 = g1 + n;
            else
                g1 = g1 - n;
            adj[g1][g0]++;

        }
    }

    chain = ( chain_t ** ) malloc ( maxAdj * sizeof ( chain_t * ) );
    if ( chain == ( chain_t ** ) NULL )
        fprintf ( stderr, "ERROR: chain NULL\n" );

    for ( i = 1; i < maxAdj; i++ )
        chain[i] = ( chain_t * ) NULL;

    for ( i = 1; i < maxAdj; i++ )
        for ( j = 1; j < maxAdj; j++ )
            if ( adj[i][j] == num_genomes )
            {

#ifdef DEBUG
                fprintf ( outfile, "Genes %3d and %3d are adj\n",
                          i <= n ? i : -( i - n ), j <= n ? j : -( j - n ) );
#endif

                newchain = ( chain_t * ) malloc ( sizeof ( chain_t ) );
                if ( newchain == ( chain_t * ) NULL )
                    fprintf ( stderr, "ERROR: newchain NULL\n" );

                newchain->adj = j;
                newchain->next = ( chain_t * ) NULL;
                chain[i] = newchain;
            }

#ifdef DEBUG
    for ( i = 1; i < maxAdj; i++ )
    {
        chainptr = chain[i];
        while ( chainptr != NULL )
        {
            fprintf ( outfile, "Chain[%3d] -> %3d\n", i, chainptr->adj );
            chainptr = chainptr->next;
        }
    }
#endif

    for ( i = 1; i < maxAdj; i++ )
    {
        if ( chain[i] == NULL )
            endAdj[i] = -1;
        else
            endAdj[i] = chain[i]->adj;
    }

    for ( i = 1; i < maxAdj; i++ )
    {
        if ( chain[i] != NULL )
        {

            while ( ( chain[j = endAdj[i]] != NULL ) && ( endAdj[i] != i ) )
            {

                lastchain = chain[i];
                while ( lastchain->next != NULL )
                    lastchain = lastchain->next;

                lastchain->next = chain[j];

                chain[j] = NULL;
                endAdj[i] = endAdj[j];
                endAdj[j] = -1;

            }
        }
    }


#ifdef DEBUG
    for ( i = 1; i < maxAdj; i++ )
    {
        chainptr = chain[i];
        if ( chainptr != NULL )
        {
            fprintf ( outfile, "MChain[%3d] -> ", i );
            while ( chainptr != NULL )
            {
                fprintf ( outfile, "%3d ", chainptr->adj );
                chainptr = chainptr->next;
            }
            fprintf ( outfile, "\n" );
        }
    }
#endif

    /* Count number of chains */
    j = 0;
    for ( i = 1; i < maxAdj; i++ )
    {
        if ( chain[i] != ( chain_t * ) NULL )
        {
            chainptr = chain[i];
            while ( chainptr->next != NULL )
                chainptr = chainptr->next;
            if ( chainptr->adj == i )
            {
                fprintf ( stderr, "ERROR: All genomes are identical\n" );
                exit ( -1 );
            }
        }
    }


    new_n = 0;

    for ( i = 1; i <= n; i++ )
        genes_in_chain[i] = FALSE;

    for ( i = 1; i < maxAdj; i++ )
    {
        chainptr = chain[i];
        if ( chainptr != NULL )
        {
            gnum = ( i <= n ? i : i - n );
            genes_in_chain[gnum] = TRUE;
            while ( chainptr != NULL )
            {
                j = chainptr->adj;
                gnum = ( j <= n ? j : j - n );
                genes_in_chain[gnum] = TRUE;
                chainptr = chainptr->next;
            }
        }
    }

#ifdef DEBUG
    for ( i = 1; i <= n; i++ )
    {
        fprintf ( outfile, "genes_in_chain[%3d]: %1d\n", i,
                  genes_in_chain[i] );
    }
#endif

    for ( i = 1; i <= n; i++ )
        genes_map[i] = 0;

#ifdef NEWCODE
    for ( i = -n; i <= n; i++ )
    {
        succ[i] = 0;
        decode[i] = 0;
        code[i] = 0;
    }
#endif

    for ( i = 1; i <= n; i++ )
    {
        if ( ( chain[i] != NULL ) || ( chain[i + n] != NULL ) ||
             ( genes_in_chain[i] == FALSE ) )
        {
            new_n++;
            genes_map[i] = new_n;

#ifdef NEWCODE
            succ[i] = BREAK;
            succ[-i] = BREAK;
#endif

            if ( genes_in_chain[i] == FALSE )
            {
#ifdef NEWCODE
                decode[new_n] = i;
                decode[-new_n] = -i;
                code[i] = new_n;
                code[-i] = -new_n;
#endif
                continue;
            }

            if ( ( chain[i] == NULL ) && ( chain[i + n] == NULL ) )
                fprintf ( stderr,
                          "ERROR: both chains %d and %d cannot be null\n", i,
                          i + n );

            if ( chain[i] != NULL )
                gnum = i;
            else
            {
                gnum = i + n;
            }

            j = gnum;
#ifdef NEWCODE
            decode[new_n] = ( j <= n ? j : -( j - n ) );
            code[( j <= n ? j : -( j - n ) )] = new_n;
#ifdef DEBUG
            fprintf ( outfile, "Setting Decode[%4d]: %4d\n", new_n,
                      ( j <= n ? j : -( j - n ) ) );
#endif
#endif
            chainptr = chain[j];
            while ( chainptr != ( chain_t * ) NULL )
            {
#ifdef NEWCODE
                g0 = ( j <= n ? j : -( j - n ) );
                g1 = ( chainptr->adj <=
                       n ? chainptr->adj : -( chainptr->adj - n ) );
#ifdef DEBUG
                fprintf ( outfile, "About to assign succ[%4d]: %4d\n", g0,
                          g1 );
#endif
                succ[g0] = g1;

                if ( chainptr->next == ( chain_t * ) NULL )
                {
#ifdef DEBUG
                    fprintf ( outfile, "About to assign succ[%4d]: BREAK\n",
                              g1 );
#endif
                    succ[g1] = BREAK;
                }
#endif
                j = chainptr->adj;
                chainptr = chainptr->next;
            }
            if ( j <= n )
                j = j + n;
            else
                j = j - n;

#ifdef DEBUG
            fprintf ( outfile, "At chain %3d, removing chain %3d\n", gnum,
                      j );
#endif

            chainptr = chain[j];
#ifdef NEWCODE
            k = j;
            decode[-new_n] = ( k <= n ? k : -( k - n ) );
            code[( k <= n ? k : -( k - n ) )] = -new_n;
#ifdef DEBUG
            fprintf ( outfile, "Setting Decode[%4d]: %4d\n", -new_n,
                      ( k <= n ? k : -( k - n ) ) );
#endif
#endif
            if ( chainptr == ( chain_t * ) NULL )
                fprintf ( stderr, "ERROR: opposite chain is NULL\n" );
            else
            {
                while ( chainptr != ( chain_t * ) NULL )
                {
#ifdef NEWCODE
                    g0 = ( j <= n ? j : -( j - n ) );
                    g1 = ( chainptr->adj <=
                           n ? chainptr->adj : -( chainptr->adj - n ) );
#ifdef DEBUG
                    fprintf ( outfile, "-About to assign succ[%4d]: %4d\n",
                              g0, g1 );
#endif
                    succ[g0] = g1;

                    if ( chainptr->next == ( chain_t * ) NULL )
                    {
#ifdef DEBUG
                        fprintf ( outfile,
                                  "-About to assign succ[%4d]: BREAK\n", g1 );
#endif
                        succ[g1] = BREAK;
                    }
                    j = chainptr->adj;
#endif
                    nextptr = chainptr->next;
                    free ( chainptr );
                    chainptr = nextptr;
                }
            }
            chain[k] = ( chain_t * ) NULL;
        }
    }

#ifdef NEWCODE
#ifdef DEBUG
    for ( i = -n; i <= n; i++ )
    {
        fprintf ( outfile,
                  "decode[%3d]: %4d  code[%4d]: %4d  succ[%3d]: %4d\n", i,
                  decode[i], i, code[i], i, succ[i] );
    }
#endif
#endif

#ifdef DEBUG
    for ( i = 1; i <= n; i++ )
    {
        fprintf ( outfile, "Genemap[%3d]: %3d of %3d\n", i, genes_map[i],
                  new_n );
    }
#endif

    for ( i = 0; i < num_genomes; i++ )
    {
        g = genome_list[i].genes;
        k = 0;
        for ( j = 0; j < n; j++ )
        {
            g0 = g[j];
#ifdef NEWCODE
            if ( code[g0] != 0 )
            {
                g[k] = code[g0];
                k++;
            }
#else
            gnum = ( g0 > 0 ? g0 : -g0 );
            gmap = genes_map[gnum];
            if ( gmap > 0 )
            {
                if ( g0 > 0 )
                {
                    g[k] = gmap;
                    k++;
                }
                else
                {
                    if ( g0 < 0 )
                    {
                        g[k] = -gmap;
                        k++;
                    }
                }
            }
#endif
        }
    }

    *num_genes = new_n;

    for ( i = 1; i < maxAdj; i++ )
    {
        chainptr = chain[i];
        while ( chainptr != NULL )
        {
            nextptr = chainptr->next;
            free ( chainptr );
            chainptr = nextptr;
        }
    }
    free ( chain );

    free ( code_m );
    free ( genes_in_chain );
    free ( genes_map );
    free ( endAdj );
    for ( i = 1; i < maxAdj; i++ )
        free ( adj[i] );
    free ( adj );

    return;
}

void
print_tree_genomes_uncondensed ( struct tNode *tree, int num_genes,
                                 int *condense_succ,
                                 int *condense_decode, int orig_num_genes )
{
    int i, j;

    int *succ, *decode;

    succ = condense_succ + orig_num_genes;
    decode = condense_decode + orig_num_genes;

    if ( tree == NULL )
        return;
    fprintf ( outfile, "Genome %4d:", tree->tag );
    for ( i = 0; i < num_genes; i++ )
    {
#ifdef NEWCODE
        j = decode[tree->genome->genes[i]];
        while ( j != BREAK )
        {
            fprintf ( outfile, " %4d", j );
            j = succ[j];
        }
#else
        fprintf ( outfile, " %4d", tree->genome->genes[i] );
#endif
    }
    fprintf ( outfile, "\n" );
    print_tree_genomes_uncondensed ( tree->lChild, num_genes,
                                     condense_succ, condense_decode,
                                     orig_num_genes );
    print_tree_genomes_uncondensed ( tree->rChild, num_genes,
                                     condense_succ, condense_decode,
                                     orig_num_genes );
    return;
}


void
print_tree_nexus ( struct tNode *tree )
{

    if ( tree == NULL )
        return;

    if ( tree->parent == NULL )
    {
        fprintf ( outfile, "\n" );
        if ( tree->lChild != NULL )
        {

            fprintf ( outfile, "(%s:%d,", tree->genome->gnamePtr,
                      *( tree->sc_lChild ) );

            print_tree_nexus ( tree->lChild );
            fprintf ( outfile, ":0);" );
        }
        fprintf ( outfile, "\n\n" );
        return;
    }

    if ( tree->lChild != NULL )
    {
        fprintf ( outfile, "(" );
        print_tree_nexus ( tree->lChild );
        fprintf ( outfile, ":%d,", *( tree->sc_lChild ) );
        print_tree_nexus ( tree->rChild );
        fprintf ( outfile, ":%d)", *( tree->sc_rChild ) );
    }
    else
    {
        fprintf ( outfile, "%s", tree->genome->gnamePtr );
    }

    return;
}

void
print_tree_nexus_noscore ( char *TreeString, struct tNode *tree )
{
    if ( tree == NULL )
        return;

    if ( tree->parent == NULL )
    {
        if ( tree->lChild != NULL )
        {

            sprintf ( TreeString, "(%s,", tree->genome->gnamePtr );

            print_tree_nexus_noscore ( TreeString, tree->lChild );
        }
        strcat ( TreeString, ")\0" );
        return;
    }

    if ( tree->lChild != NULL )
    {
        strcat ( TreeString, "(" );
        print_tree_nexus_noscore ( TreeString, tree->lChild );
        strcat ( TreeString, "," );
        print_tree_nexus_noscore ( TreeString, tree->rChild );
        strcat ( TreeString, ")" );

    }
    else
    {
        strcat ( TreeString, tree->genome->gnamePtr );
    }

    return;
}

void
print_tree_uncondensed ( struct tNode *tree, int num_genes,
                         int *condense_succ,
                         int *condense_decode, int orig_num_genes )
{

    print_tree_nexus ( tree );

    print_tree_genomes_uncondensed ( tree, num_genes,
                                     condense_succ, condense_decode,
                                     orig_num_genes );
    return;
}
