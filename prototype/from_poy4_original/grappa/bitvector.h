/* $Id: bitvector.h 48 2005-04-13 15:56:25Z ron $
   Written by Adam Siepel, Spring 2001 
   Copyright 2001, Adam Siepel */

/* Routines to construct, query, and perform operations on bit
   vectors.  Bitwise AND, OR, NOT, and XOR are supported.  Functions
   have been tuned somewhat for performance.  */

#ifndef BITVECTOR_H
#define BITVECTOR_H

#include <stdlib.h>
#include <stdio.h>
#include "priority_stack.h"

#define BYTESIZE 8

enum
{
    SZ = sizeof ( char ) * BYTESIZE,
    MAXVAL = ( 1 << SZ ) - 1
};

typedef struct bitvector BitVector;
struct bitvector
{
    int length;
    int nbytes;
    char *v;
    int capacity;
};

void init_bitvector ( BitVector * bv, int capacity );
void free_bitvector ( BitVector * bv );
void copy_bitvector ( BitVector * dest, BitVector * src );
void bitwise_and ( BitVector * op1, BitVector * op2, BitVector * result );
void bitwise_or ( BitVector * op1, BitVector * op2, BitVector * result );
void bitwise_not ( BitVector * op, BitVector * result );
void bitwise_xor ( BitVector * op1, BitVector * op2, BitVector * result );
void print_bits ( FILE * f, BitVector * v );
void bitvector_set ( BitVector * bv, int pos, int val );
void bitvector_set_func ( BitVector * bv, int func ( void *, int ),
                          void *data );
int bitvector_get ( BitVector * bv, int pos );
void set_bitvector_length ( BitVector * bv, int len );
void bitvector_clear ( BitVector * bv );
void bitvector_get_positions_of_ones ( BitVector * bv, List * l );
void bitvector_set_positions_of_ones ( BitVector * bv, List * l );

#endif
