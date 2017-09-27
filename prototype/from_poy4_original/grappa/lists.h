/* $Id: lists.h 48 2005-04-13 15:56:25Z ron $
   Written by Adam Siepel, Spring 2001
   Copyright 2001, Adam Siepel */

/* Simple list-handling functions, allowing indexing or either FIFO or
   LIFO behavior. */

#ifndef LISTS_H
#define LISTS_H

typedef struct list List;
struct list
{
    void **array;
    int lidx;
    int ridx;
    int CAPACITY;
    int elementsz;
};

void init_list ( List * q, int nelements, int elementsz );
void free_list ( List * q );
void push ( List * q, void *v );
void *pop_stack ( List * q );
void *pop_queue ( List * q );
void *peek_queue ( List * q );
void *peek_stack ( List * q );
int empty ( List * q );
int list_size ( List * l );
void copy_list ( List * old, List * new );
void *list_get ( List * l, int i );
void clear_list ( List * l );
void list_delete ( List * l, int idx );
int list_contains ( List * l, void *ptr );

#endif
