/* $Id: priority_stack.c 48 2005-04-13 15:56:25Z ron $
   Written by Adam Siepel, Spring 2001 
   Copyright 2001, Adam Siepel */

/* Defines a data structure that is a hybrid of a priority queue and a
   stack: an item of highest priority is always returned, but within
   classes of equal priority, items are handled in LIFO fashion.
   Implementation uses an array of stacks. */

/* Use -DTHREADSAFE for concurrent access by multiple threads.  Note
   that the priority stack is threadsafe, but the underlying stacks
   are not. */

#include <stdlib.h>
#include "priority_stack.h"
#include <stdio.h>
#include <assert.h>

/* returns NULL if empty */
void *
ps_pop ( PriorityStack * ps )
{
    void *i;

#ifdef THREADSAFE
    pthread_mutex_lock ( &ps->mutex );
#endif

    if ( ps->count == 0 )
        i = NULL;

    else
    {
        while ( empty ( &ps->stacks[ps->idx] ) )
        {
            ps->idx++;
            assert ( ps->idx <= ps->max - ps->min );
        }

        i = pop_stack ( &ps->stacks[ps->idx] );
        ps->count--;
    }

#ifdef THREADSAFE
    pthread_mutex_unlock ( &ps->mutex );
#endif

    return i;
}

PriorityStack *
new_ps ( int min, int max, int stack_nelements, int stack_elementsz )
{
    int i;
    PriorityStack *ps =
        ( PriorityStack * ) malloc ( sizeof ( PriorityStack ) );
    ps->min = min;
    ps->max = max;
    ps->stacks = ( List * ) calloc ( max - min + 1, sizeof ( List ) );
    ps->elementsz = stack_elementsz;
    for ( i = 0; i <= ( max - min ); i++ )
        init_list ( &ps->stacks[i], stack_nelements, stack_elementsz );
    ps->count = 0;
    ps->idx = ps->max - ps->min;

#ifdef THREADSAFE
    pthread_mutex_init ( &ps->mutex, NULL );
#endif

    return ps;
}

void
ps_free ( PriorityStack * ps )
{
    int i;
    for ( i = 0; i <= ( ps->max - ps->min ); i++ )
        free_list ( &ps->stacks[i] );
    free ( ps->stacks );
    free ( ps );
}

void
ps_clear ( PriorityStack * ps )
{
    int i;
    for ( i = 0; i <= ( ps->max - ps->min ); i++ )
        clear_list ( &ps->stacks[i] );
    ps->count = 0;
    ps->idx = ps->max - ps->min;
}

void
ps_push ( PriorityStack * ps, void *v, int priority )
{

#ifdef THREADSAFE
    pthread_mutex_lock ( &ps->mutex );
#endif

    push ( &ps->stacks[priority - ps->min], v );
    ps->count++;
    if ( priority - ps->min < ps->idx )
        ps->idx = priority - ps->min;

#ifdef THREADSAFE
    pthread_mutex_unlock ( &ps->mutex );
#endif
}

/* Removes and stores in a newly allocated List l all items with
   priority lower (i.e., value higher) than specified threshold;
   useful for keeping memory usage under control */
void
ps_flush ( PriorityStack * ps, List * l, int threshold )
{
    int i;
    int count = 0;

#ifdef THREADSAFE
    pthread_mutex_lock ( &ps->mutex );
#endif

    for ( i = threshold - ps->min + 1; i <= ( ps->max - ps->min ); i++ )
        count += list_size ( &ps->stacks[i] );

    init_list ( l, count, ps->elementsz );
/*   fprintf(stderr, "Flushing %d from priority stack\n", count);   */
    if ( count == 0 )
        return;

    for ( i = threshold - ps->min + 1; i <= ( ps->max - ps->min ); i++ )
    {
        while ( !empty ( &ps->stacks[i] ) )
        {
            push ( l, pop_stack ( &ps->stacks[i] ) );
            ps->count--;
        }
    }

    if ( ps->idx >= threshold - ps->min + 1 )
        ps->idx = ps->max - ps->min;

#ifdef THREADSAFE
    pthread_mutex_unlock ( &ps->mutex );
#endif
}
