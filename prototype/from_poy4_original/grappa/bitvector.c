/* $Id: bitvector.c 48 2005-04-13 15:56:25Z ron $
   Written by Adam Siepel, Spring 2001 
   Copyright 2001, Adam Siepel */

/* Routines to construct, query, and perform operations on bit
   vectors.  Bitwise AND, OR, NOT, and XOR are supported.  Functions
   have been tuned somewhat for performance.  */

#include "bitvector.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

void
init_bitvector ( BitVector * bv, int capacity )
{
    bv->capacity = bv->length = capacity;
    bv->nbytes = bv->length / ( sizeof ( char ) * BYTESIZE ) + 1;
    bv->v = ( char * ) malloc ( bv->nbytes );
    bitvector_clear ( bv );
}

void
free_bitvector ( BitVector * bv )
{
    free ( bv->v );
}

void
copy_bitvector ( BitVector * dest, BitVector * src )
{
    int i;
    assert ( dest->length == src->length );
    for ( i = 0; i < src->nbytes; i++ )
        dest->v[i] = src->v[i];
}

void
bitwise_and ( BitVector * op1, BitVector * op2, BitVector * result )
{
    int i;
    assert ( op1->length == op2->length && op1->length == result->length );
    for ( i = 0; i < op1->nbytes; i++ )
        result->v[i] = op1->v[i] & op2->v[i];
}

void
bitwise_or ( BitVector * op1, BitVector * op2, BitVector * result )
{
    int i;
    assert ( op1->length == op2->length && op1->length == result->length );
    for ( i = 0; i < op1->nbytes; i++ )
        result->v[i] = op1->v[i] | op2->v[i];
}

void
bitwise_not ( BitVector * op, BitVector * result )
{
    int i;
    assert ( op->length == result->length );
    for ( i = 0; i < op->nbytes; i++ )
        result->v[i] = ~( op->v[i] );
}

void
bitwise_xor ( BitVector * op1, BitVector * op2, BitVector * result )
{
    int i;
    assert ( op1->length == op2->length && op1->length == result->length );
    for ( i = 0; i < op1->nbytes; i++ )
        result->v[i] = ( op1->v[i] | op2->v[i] ) & ~( op1->v[i] & op2->v[i] );
}

void
bitvector_set ( BitVector * bv, int pos, int val )
{
    int i, j;
    assert ( pos < bv->length && ( val == 0 || val == 1 ) );
    i = pos / SZ;
    j = SZ - ( pos % SZ ) - 1;
    if ( val == 1 )
        bv->v[i] |= ( 1 << j );
    else
        bv->v[i] &= ( MAXVAL - ( 1 << j ) );
}

void
bitvector_set_func ( BitVector * bv, int func ( void *, int ), void *data )
{
    int i, j, l;
    for ( i = 0, l = 0; l < bv->length; i++ )
    {
        for ( j = SZ - 1; j >= 0; j--, l++ )
        {
            if ( func ( data, l ) )
                bv->v[i] |= ( 1 << j );
            else
                bv->v[i] &= ( MAXVAL - ( 1 << j ) );
        }
    }
}

int
bitvector_get ( BitVector * bv, int pos )
{
    int i, j;
    assert ( pos < bv->length );
    i = pos / SZ;
    j = SZ - ( pos % SZ ) - 1;
    return ( ( bv->v[i] & ( 1 << j ) ) >> j );
}

void
print_bits ( FILE * f, BitVector * v )
{
    int i;
    for ( i = 0; i < v->length; i++ )
        fprintf ( f, "%d", bitvector_get ( v, i ) );
    fprintf ( f, "\n" );
}

/* allows operations to be performed on subset of entire capacity
   (i.e., length can shrink and expand within the bounds of capacity) */
void
set_bitvector_length ( BitVector * bv, int len )
{
    assert ( len <= bv->capacity );
    bv->length = len;
    bv->nbytes = bv->length / ( sizeof ( char ) * BYTESIZE ) + 1;
}

void
bitvector_clear ( BitVector * bv )
{
    int i;
    for ( i = 0; i < bv->nbytes; i++ )
        bv->v[i] = 0;
}

/* intended to be more efficient than repeated calls to bitvector_get */
void
bitvector_get_positions_of_ones ( BitVector * bv, List * l )
{
    int i, j, k, idx;
    clear_list ( l );
    idx = 0;
    for ( i = 0; i < bv->nbytes; i++ )
    {
        k = SZ - 1;
        for ( j = 0; j < SZ; j++ )
        {
            if ( bv->v[i] & ( 1 << k ) )
                push ( l, ( void * ) idx );
            k--;
            idx++;
        }
    }
}

/* similarly, a replacement for repeated calls to bitvector_set */
void
bitvector_set_positions_of_ones ( BitVector * bv, List * l )
{
    int i, j, k, pos;
    bitvector_clear ( bv );
    for ( i = 0; i < list_size ( l ); i++ )
    {
        pos = ( int ) list_get ( l, i );
        j = pos / SZ;
        k = SZ - ( pos % SZ ) - 1;
        bv->v[j] |= ( 1 << k );
    }
}
