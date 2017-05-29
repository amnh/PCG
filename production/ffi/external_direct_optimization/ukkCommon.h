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

#include "costMatrix.h"
#include "dyn_character.h"
// #include "ukkCheckPoint.h"

#define MAX_STR    100000   // TODO: can I get rid of this?

#define MAX_STATES 27      // Maximum number of possible fsm_states where fsm_states are {insert, delete, substitute}, so 3^3. See enum below

#define MAX_COST   (2 * MAX_STR)
#define INFINITY   INT_MAX / 2

#define FULL_ALLOC_INFO  0

#define FIXED_NUM_PLANES 1

#define CELLS_PER_BLOCK  10

typedef enum { MATCH_SUB
             , DEL
             , INS
             } Trans;  // The 3 possible finite fsm_state machine fsm_states

/** How much space was allocated for the characters. */
typedef struct alloc_info_t {
    size_t elemSize;
    size_t abSize;
    size_t acSize;
    size_t abOffset;
    size_t acOffset;
    size_t abBlocks;
    size_t acBlocks;

    #ifdef FIXED_NUM_PLANES // See above
        size_t costSize;
    #endif

    size_t baseAlloc;
    void **baseArrays;     // void because may point at U_cell_type or CPTye

    size_t memAllocated;
} alloc_info_t;


// This is a persistent set of costs needed throughout the code.
// I moved these costs all into this struct so I could remove global variables.
// TODO: eventually a 3d cost matrix needs to replace this.
typedef struct global_costs_t {
    unsigned int       mismatchCost;
    unsigned int       gapOpenCost;
    unsigned int       gapExtendCost;
    cost_matrices_3d_t costMatrix;
    // unsigned int deleteOpenCost;   // This was the cost for reverting a gap opening. It was only ever used
                                      // assert that it was the same as gapOpenCost, neither of those values ever mutate.
    // unsigned int deleteExtendCost; // Likewise, this was used where gapExtendCost could have been, as a delete is actually
                                      // a gap insertion, and an insertion in one character is a gap intertion in another.
} global_costs_t;


typedef struct characters_t {
    size_t numStates;           // how many possible FSM fsm_states there are, if each of three FSMs is in one of {INS, DEL, MATCH_SUB}
                                // and given that one can't have all gap or more than one insertion. This is, btw, always 15.
    size_t maxSingleStep;

    char *lesserStr;            // string representation of shortest character
    char *longerStr;            // string representation of longest character
    char *middleStr;            // string representation of middle character

    size_t lesserIdx;           // current index into shortest character
    size_t longerIdx;           // current index into longest character
    size_t middleIdx;           // current index into middle character

    size_t lesserLen;           // length of respective string representation
    size_t longerLen;           // length of respective string representation
    size_t middleLen;           // length of respective string representation
} characters_t;


typedef struct fsm_arrays_t {
    int *neighbours;                // array of neighbor fsm_states for each possible fsm_state in fsm_stateNum
    int *fsmState_continuationCost; // as with transition cost, the cost to extend one or two gaps. Moot once a tcm is used.
    int *secondCost;                //
    int *transitionCost;            // cost to transition from one fsm_state to another (i.e. start a gap)
    int *fsmState_num;              // number that corresponds to a given fsm_state, i.e. 0 is all match/subs and 1 is [m/s m/s del]
} fsm_arrays_t;

// #ifndef UKKCOMMON_C

//     extern int neighbours[MAX_STATES];
//     extern int fsm_stateContinuationCost  [MAX_STATES];
//     extern int secondCost[MAX_STATES];
//     extern int transitionCost [MAX_STATES][MAX_STATES];
//     extern int fsm_stateNum  [MAX_STATES];

//     extern size_t numStates;
//     extern size_t maxSingleStep;

//     extern char lesserStr[MAX_STR];
//     extern char longerStr[MAX_STR];
//     extern char middleStr[MAX_STR];
//     extern size_t  lesserLen, longerLen, middleLen;

// #endif

// #define MAX_SINGLE_COST (maxSingleStep * 2)


#ifdef FIXED_NUM_PLANES // see above
    alloc_info_t allocInit( size_t elemSize
                          , size_t costSize
                          , characters_t *globalCharacters
                          );
#else
    alloc_info_t allocInit( size_t elemSize
                          , characters_t *globalCharacters
                          );
#endif


int whichCharCost( char a
                 , char b
                 , char c
                 );


/** Make sure that index a is valid for a given set of array indices */
int okIndex( int a
           , int da
           , int end
           );


/******** Setup routines ********/

/*
int fsm_stateTransitionCost( int from
                       , int to
                       , int *transitionCost
                       );
*/

void copyCharacter ( char            *str
                   , dyn_character_t *inChar
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


/** Resets a transitionn array that holds the transitions fsm_states for three fsm_state-transition FSMs. There are 27 possible fsm_states.
 *  For instance, if the fsm_state is 1, then the FSMs are in the cumulative fsm_state [DEL, MATCH_SUB, MATCH_SUB], whereas if the fsm_state were
 *  22, the cumulative fsm_state would be [DEL, DEL, INS] (which is actually not possible, as it signifies a gap in all three dynamic
 *  characters, which is meaningless).
 */
void transitions( Trans  fsm_stateTransition[3]
                , size_t fsm_state
                );


/** Takes an array of fsm_states by value and returns a string of those fsm_states as match, delete, insert.
 *  Used only in printing of fsm_state array if DEBUG_3D is set.
 */
char *fsm_state2str( size_t  fsm_state
               , int    *fsm_stateNum
               );


/** Count number of times whichTransition appears in fsm_stateTransitions, where MATCH_SUB = 0, DEL = 1, INS = 2. */
size_t countThisTransition( Trans fsm_stateTransitions[3]
                          , Trans whichTransition
                          );

/** Set up the Ukkonnen and check point matrices before running alignment.
 *  Finish setup of characters.
 */
void setup( global_costs_t  *globalCosts
          , characters_t    *inputChars
          , characters_t    *resultChars
          , fsm_arrays_t    *fsmArrays
          , dyn_character_t *in_lesserChar
          , dyn_character_t *in_middleChar
          , dyn_character_t *in_longerChar
          , unsigned int     mismatch_cost
          , unsigned int     gapOpen
          , unsigned int     gapExtend
          );


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


unsigned int alignmentCost( int             fsm_states[]
                          , char           *al1
                          , char           *al2
                          , char           *al3
                          , size_t          len
                          , global_costs_t *globalCosts
                          , fsm_arrays_t   *fsmArrays
                          );


/** Return a pointer into either Ukkonnen matrix or distance matrix. */
void *getPtr( alloc_info_t *alloc_info_t
            , int           ab_idx_diff
            , int           ac_idx_diff
            , size_t        editDist
            , int           fsm_state
            , size_t        numStates
            );


/************* allocation routines. Were previously commented out. ***************/
void allocFinal( alloc_info_t *allocInfo
               , void         *flag
               , void         *top
               , size_t        numStates
               );


#endif // UKKCOMMON_H
