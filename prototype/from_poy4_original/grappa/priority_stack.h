/* $Id: priority_stack.h 48 2005-04-13 15:56:25Z ron $
   Written by Adam Siepel, Spring 2001 
   Copyright 2001, Adam Siepel */

/* Defines a data structure that is a hybrid of a priority queue and a
   stack: an item of highest priority is always returned, but within
   classes of equal priority, items are handled in LIFO fashion.
   Implementation uses an array of stacks. */

/* Use -DTHREADSAFE for concurrent access by multiple threads.  Note
   that the priority stack is threadsafe, but the underlying stacks
   are not. */

#ifndef PSTACK_H
#define PSTACK_H

#include <stdlib.h>
#ifndef WINNT
#include <pthread.h>
#endif
#include "lists.h"

typedef struct priority_stack PriorityStack;
struct priority_stack
{
    List *stacks;               /* array of stacks */
    int min, max;               /* values of min and max priorities */
    int idx;                    /* index of current min list */
    int count;                  /* current number of items */
    int elementsz;              /* size of each element in pstack */
#ifdef THREADSAFE
    pthread_mutex_t mutex;
#endif
};

void *ps_pop ( PriorityStack * ps );
int ps_empty ( PriorityStack * ps );
PriorityStack *new_ps ( int min, int max, int stack_nelements,
                        int stack_elementsz );
void ps_push ( PriorityStack * ps, void *v, int priority );
void ps_free ( PriorityStack * ps );
void ps_clear ( PriorityStack * ps );
void ps_flush ( PriorityStack * ps, List * l, int threshold );

#endif
