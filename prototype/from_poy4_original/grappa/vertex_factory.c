/* $Id: vertex_factory.c 48 2005-04-13 15:56:25Z ron $
   Written by Adam Siepel, Spring 2001 
   Copyright 2001, Adam Siepel */

/* "Factory" for vertices, allowing more efficient memory allocation
   and deallocation.  Compile with -DTHREADSAFE for concurrent access
   from multiple threads. */

#include <stdlib.h>
#include "vertex_factory.h"
#include <stdio.h>

#define VFWIDTH 1024
void
clean_vf ( VertexFactory * vf, int ngenes, void ( *clear_mem ) ( void * ),
           void *arg )
{
    int i, j;
    int length1;
    vf->nalloc = 0;
    vf->ngenes = ngenes;
    vf->clear_mem = clear_mem;
    vf->clear_mem_arg = arg;
    length1 = vf->length - 1;
    for ( i = 0; i < vf->width; i++ )
    {
        for ( j = 0; j < length1; j++ )
        {
            vf->vertices[i][j].perm = &( vf->perms[i][j * ngenes] );
            vf->vertices[i][j].next = &( vf->vertices[i][j + 1] );
        }
        if ( i < vf->width - 1 )
        {
            vf->vertices[i][j].perm = &( vf->perms[i][j * ngenes] );
            vf->vertices[i][j].next = &( vf->vertices[i + 1][0] );
        }
        else
        {
            vf->vertices[i][j].perm = &( vf->perms[i][j * ngenes] );
            vf->vertices[i][j].next = NULL;
        }
    }
    vf->head = &( vf->vertices[0][0] );
    vf->tail = &( vf->vertices[vf->width - 1][length1] );
#ifdef THREADSAFE
    pthread_mutex_init ( &vf->mutex, NULL );
#endif

    return;

}

/* Last two arguments pertain to a function to be called when capacity
   has been reached, before reallocating more memory.  Pass NULL for
   both if you do not wish to specify such a function */
VertexFactory *
new_vf ( int startsize, int ngenes, void ( *clear_mem ) ( void * ),
         void *arg )
{
    int i;
    VertexFactory *vf;
    vf = ( VertexFactory * ) malloc ( sizeof ( VertexFactory ) );
    vf->vertices = ( Vertex ** ) malloc ( VFWIDTH * sizeof ( Vertex * ) );
    vf->vertices[0] = ( Vertex * ) malloc ( startsize * sizeof ( Vertex ) );
    vf->width = 1;
    vf->perms = ( int ** ) malloc ( VFWIDTH * sizeof ( int * ) );
    vf->perms[0] = ( int * ) malloc ( startsize * ngenes * sizeof ( int ) );
    vf->nalloc = 0;
    vf->ngenes = ngenes;
    vf->uncondensedNumGenes = ngenes;
    vf->capacity = startsize;
    vf->length = startsize;
    vf->clear_mem = clear_mem;
    vf->clear_mem_arg = arg;
    for ( i = 0; i < startsize - 1; i++ )
    {
        vf->vertices[0][i].perm = &( vf->perms[0][i * ngenes] );
        vf->vertices[0][i].next = &( vf->vertices[0][i + 1] );
    }
    vf->vertices[0][i].perm = &( vf->perms[0][i * ngenes] );
    vf->vertices[0][i].next = NULL; /* to avoid the if block in the 
                                       for block */
    vf->tail = &( vf->vertices[0][i] );
    vf->head = &( vf->vertices[0][0] );

#ifdef THREADSAFE
    pthread_mutex_init ( &vf->mutex, NULL );
#endif

    return vf;
}

Vertex *
get_vertex ( VertexFactory * vf )
{
    int oldcap, i;
    Vertex *retval;
    int width1;
    int length;
#ifdef THREADSAFE
    pthread_mutex_lock ( &vf->mutex );
#endif

    if ( vf->head == NULL )
    {                           /*vf->nalloc == vf->capacity) { */
        length = vf->length;
        width1 = vf->width - 1;
        if ( vf->clear_mem != NULL )
        {
#ifdef THREADSAFE
            pthread_mutex_unlock ( &vf->mutex );
#endif
            fprintf ( stderr, "Invoking clear_mem\n" );
            vf->clear_mem ( vf->clear_mem_arg );
#ifdef THREADSAFE
            pthread_mutex_lock ( &vf->mutex );
#endif
        }

        oldcap = vf->capacity;
        vf->width++;
        width1 = vf->width - 1;
        vf->vertices[width1] =
            ( Vertex * ) malloc ( length * sizeof ( Vertex ) );
        vf->perms[width1] =
            ( int * ) malloc ( length * vf->uncondensedNumGenes *
                               sizeof ( int ) );
        vf->capacity += length;
        for ( i = 0; i < length - 1; i++ )
        {
            vf->vertices[width1][i].perm =
                &( vf->perms[width1][i * vf->ngenes] );
            vf->vertices[width1][i].next = &( vf->vertices[width1][i + 1] );
        }
        vf->vertices[width1][i].perm = &( vf->perms[width1][i * vf->ngenes] );
        vf->vertices[width1][i].next = NULL;
        vf->tail = &( vf->vertices[width1][i] );
        vf->head = &( vf->vertices[width1][0] );
    }


    vf->nalloc++;

    retval = vf->head;
    vf->head = retval->next;

#ifdef THREADSAFE
    pthread_mutex_unlock ( &vf->mutex );
#endif

    return retval;
}

void
return_vertex ( VertexFactory * vf, Vertex * v )
{
#ifdef THREADSAFE
    pthread_mutex_lock ( &vf->mutex );
#endif

/*   printf("%d\n", v->memidx); */
    vf->nalloc--;
    ( vf->tail )->next = v;
    vf->tail = v;
    v->next = NULL;

#ifdef THREADSAFE
    pthread_mutex_unlock ( &vf->mutex );
#endif
}

void
vf_free ( VertexFactory * vf )
{
    int i;
    int width;
    width = vf->width;
    for ( i = 0; i < width; i++ )
        free ( vf->vertices[i] );
    for ( i = 0; i < width; i++ )
        free ( vf->perms[i] );

    free ( vf->vertices );
    free ( vf->perms );
    free ( vf );
}
