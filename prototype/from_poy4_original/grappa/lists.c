/* $Id: lists.c 48 2005-04-13 15:56:25Z ron $
   Written by Adam Siepel, Spring 2001
   Copyright 2001, Adam Siepel */

/* Simple list-handling functions, allowing indexing or either FIFO or
   LIFO behavior. */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "lists.h"

void
push ( List * q, void *v )
{
    int i;
    if ( q->ridx >= q->CAPACITY )
    {
        if ( q->lidx > 0 )
        {
            for ( i = q->lidx; i < q->ridx; i++ )
                q->array[i - q->lidx] = q->array[i];
            q->ridx -= q->lidx;
            q->lidx = 0;
        }

        else
        {
            fprintf ( stderr, "ERROR: Exceeded list capacity\n" );
            assert ( 0 );
        }
    }
    q->array[q->ridx++] = v;
}

int
list_size ( List * l )
{
    return ( l->ridx - l->lidx );
}

void *
list_get ( List * l, int i )
{
    if ( i >= list_size ( l ) )
        return NULL;
    return ( l->array[l->lidx + i] );
}

void *
pop_queue ( List * q )
{
    if ( q->lidx >= q->ridx )
        return NULL;
    return q->array[q->lidx++];
}

void *
peek_queue ( List * q )
{
    if ( q->lidx >= q->ridx )
        return NULL;
    return q->array[q->lidx];
}

void *
pop_stack ( List * q )
{
    if ( q->ridx <= q->lidx )
        return NULL;
    return q->array[--q->ridx];
}

void *
peek_stack ( List * s )
{
    if ( s->ridx <= s->lidx )
        return NULL;
    return s->array[s->ridx - 1];
}

int
empty ( List * q )
{
    return ( q->lidx >= q->ridx );
}

/* Must be executed on a new list before it is usable!!! */
void
init_list ( List * q, int nelements, int elementsz )
{
    q->ridx = q->lidx = 0;
    q->CAPACITY = nelements;
    q->elementsz = elementsz;
    q->array = ( void ** ) malloc ( nelements * elementsz );
}

void
copy_list ( List * new, List * old )
{
    int i;
    init_list ( new, old->CAPACITY, old->elementsz );
    for ( i = 0; i < list_size ( old ); i++ )
        push ( new, list_get ( old, i ) );
}

void
free_list ( List * q )
{
    free ( q->array );
}

void
clear_list ( List * l )
{
    l->ridx = l->lidx = 0;
}

void
list_delete ( List * l, int idx )
{
    int i;
    if ( idx >= list_size ( l ) )
        return;
    for ( i = idx + 1; i < list_size ( l ); i++ )
        l->array[l->lidx + i - 1] = l->array[l->lidx + i];
    l->ridx--;
}

/* note: pointer-based */
int
list_contains ( List * l, void *ptr )
{
    int i;
    for ( i = 0; i < list_size ( l ); i++ )
        if ( list_get ( l, i ) == ptr )
            return 1;
    return 0;
}
