#include <stdio.h>
#include <stdlib.h>
#include "uf.h"

UFelem *
UFalloc ( int n )
{
    UFelem *uf;
    uf = ( UFelem * ) malloc ( n * sizeof ( UFelem ) );
    if ( uf == ( UFelem * ) NULL )
        fprintf ( stderr, "ERROR: uf NULL\n" );
    return ( uf );
}

void
UFfree ( UFelem * uf )
{
    free ( uf );
    return;
}

void
UFcreate ( UFelem * uf, int n )
{
    int i;
    for ( i = 0; i < n; i++ )
    {
        uf[i].rank = 0;
        uf[i].parent = i;       /* roots point to themselves */
    }
    return;
}

int
UFunion ( UFelem * uf, int i, int j )
{
    /* union by rank; assumes that i, j are roots */
    int root;
    if ( uf[i].rank >= uf[j].rank )
    {
        uf[j].parent = i;
        if ( uf[i].rank == uf[j].rank )
            uf[i].rank++;
        if ( uf[i].handleE < uf[j].handleE )
        {
            uf[i].handleB = uf[j].handleB;
            uf[i].handleE = uf[j].handleE;
        }
        root = i;
    }
    else
    {
        /* uf[i].rank < uf[j].rank */
        uf[i].parent = j;
        if ( uf[j].handleE < uf[i].handleE )
        {
            uf[j].handleB = uf[i].handleB;
            uf[j].handleE = uf[i].handleE;
        }
        root = j;
    }
    return ( root );
}

int
UFfind ( UFelem * uf, int i )
{
    /* uses halving rather than full compression ; thus only one pass */
    int above;

    above = uf[i].parent;
    while ( above != uf[above].parent )
    {                           /* parent is not the root */
        uf[i].parent = uf[above].parent;    /* point to my grandparent */
        i = uf[above].parent;   /* move up two levels */
        above = uf[i].parent;
    }
    return ( above );
}
