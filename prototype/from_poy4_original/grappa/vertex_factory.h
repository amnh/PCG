/* $Id: vertex_factory.h 48 2005-04-13 15:56:25Z ron $
   Written by Adam Siepel, Spring 2001 
   Copyright 2001, Adam Siepel */

/* "Factory" for vertices, allowing more efficient memory allocation
   and deallocation.  Compile with -DTHREADSAFE for concurrent access
   from multiple threads. */

#ifndef VFACT_H
#define VFACT_H

#ifndef WINNT
#include <pthread.h>
#endif
#define VFSIZE 10000
typedef struct vertex Vertex;
struct vertex
{
    int *perm;
    unsigned int distance;      /* distance from home */
    unsigned short best_possible_score; /* score of best possible median */
    unsigned short worst_possible_score;    /* score of worst possible median */
    int d1, d2;
    Vertex *next;
};

typedef struct vertex_factory VertexFactory;
struct vertex_factory
{
    Vertex **vertices;
    int **perms;
    int nalloc;
    int ngenes;
    int capacity;
    int length;
    int width;
    void ( *clear_mem ) ( void * );
    void *clear_mem_arg;
    int medianNGenes;

    Vertex *head;
    Vertex *tail;
    int uncondensedNumGenes;
#ifdef THREADSAFE
    pthread_mutex_t mutex;
#endif
};

VertexFactory *new_vf ( int startsize, int ngenes,
                        void ( *clear_mem ) ( void * ), void *arg );
Vertex *get_vertex ( VertexFactory * vf );
void return_vertex ( VertexFactory * vf, Vertex * v );
void vf_free ( VertexFactory * vf );

void clean_vf ( VertexFactory * vf, int ngenes,
                void ( *clear_mem ) ( void * ), void *arg );
#endif
