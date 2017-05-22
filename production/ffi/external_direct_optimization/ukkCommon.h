/*
 * Copyright (c) David Powell <david@drp.id.au>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 59 Temple
 * Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 */


// Name:         ukkCommon.h
// FileType:     C Header
// Created:      Tue May 11 13:23:45 1999
// Author:       David Powell
//
// Contains the common routines for ukk.alloc, ukk.noalign, ukk.checkp and ukk.dpa
// Has the mem allocation routines, in header files because they are all inline funcs
// Use the NO_ALLOC_ROUTINES flag to compile without alloc routines.
// Use the FIXED_NUM_PLANES flag to make routines allocate on d^2 memory.

#ifndef UKKCOMMON_H
#define UKKCOMMON_H

#include <limits.h>
#include <string.h>

#include "dyn_character.h"
#include "ukkCheckPoint.h"

#define MAX_STR    100000

#define MAX_STATES 27                 // Maximum number of possible states, 3^3

#define MAX_COST   (2 * MAX_STR)
#define INFINITY   INT_MAX / 2

#define FULL_ALLOC_INFO  0

#define FIXED_NUM_PLANES 1

#define CELLS_PER_BLOCK  10

typedef enum { MATCH
             , DEL
             , INS
             } Trans;  // The 3 possible state-machine states

typedef struct alloc_info_t {
    size_t elemSize;
    size_t abSize;
    size_t acSize;
    size_t abOffset;
    size_t acOffset;
    size_t abBlocks;
    size_t acBlocks;

    #ifdef FIXED_NUM_PLANES
        size_t costSize;
    #endif

    size_t baseAlloc;
    void **basePtr;     // void because may point at U_cell_type or CPTye

    size_t memAllocated;
} alloc_info_t;


#ifndef UKKCOMMON_C

    extern unsigned int mismatchCost;
    extern unsigned int gapOpenCost;
    extern unsigned int gapExtendCost;
    extern unsigned int deleteOpenCost;
    extern unsigned int deleteExtendCost;

    extern int neighbours[MAX_STATES];
    extern int contCost  [MAX_STATES];
    extern int secondCost[MAX_STATES];
    extern int transCost [MAX_STATES][MAX_STATES];
    extern int stateNum  [MAX_STATES];

    extern size_t numStates;
    extern size_t maxSingleStep;

    extern char lesserStr[MAX_STR];
    extern char longerStr[MAX_STR];
    extern char middleStr[MAX_STR];
    extern size_t  lesserLen, longerLen, middleLen;

#endif

#define maxSingleCost (maxSingleStep * 2)


int whichCharCost( char a
                 , char b
                 , char c
                 );


/** Make sure that index a is valid for a given set of array indices */
int okIndex( int a
           , int da
           , int end
           );


// Setup routines
int  stateTransitionCost( int from
                        , int to
                        );


void step( int  n
         , int *a
         , int *b
         , int *c
         );


int neighbourNum( int i
                , int j
                , int k
                );


void transitions( Trans  stateTransition[3]
                , size_t state
                );


char *state2str( size_t state );


/** Count number of times whichTransition appears in stateTransitions */
size_t countThisTransition( Trans stateTransitions[3]
                          , Trans whichTransition
                          );

/** Set up the Ukkonnen and check point matrices before running alignment. */
void setup();


// Alignment checking routines
void checkAlign( char   *al
               , size_t  alLen
               , char   *str
               , size_t  strLen
               );

/** Reverses an array of ints. */
void revIntArray( int    *arr
                , size_t  start
                , size_t  end
                );

/** Reverses an array of chars. */
void revCharArray( char   *arr
                 , size_t  start
                 , size_t  end
                 );

unsigned int alignmentCost( int     states[]
                          , char   *al1
                          , char   *al2
                          , char   *al3
                          , size_t  len
                          );

void *getPtr( alloc_info_t *a
            , int    ab
            , int    ac
            , size_t d
            , int    s
            );

// TODO: unsigned ints for costs:
// IMPORTANT!!! Order of input characters is short, long, middle.
int powell_3D_align ( dyn_character_t *charA
                    , dyn_character_t *charB
                    , dyn_character_t *charC
                    , dyn_character_t *retCharA
                    , dyn_character_t *retCharB
                    , dyn_character_t *retCharC
                    , unsigned int     mismatch
                    , unsigned int     gapOpen
                    , unsigned int     gapExtend
                    );

// allocation routines. Were previously commented out.
void allocFinal( alloc_info_t *a
               , void         *flag
               , void         *top
               );

#ifdef FIXED_NUM_PLANES
    alloc_info_t allocInit( size_t elemSize
                          , size_t costSize
                          );
#else
    alloc_info_t allocInit( size_t elemSize );
#endif


#endif // UKKCOMMON_H
