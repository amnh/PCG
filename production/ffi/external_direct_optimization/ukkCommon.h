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

#define FULL_ALLOC_INFO  0  // Use the NO_ALLOC_ROUTINES flag to compile without alloc routines.

#define FIXED_NUM_PLANES 1  // Use the FIXED_NUM_PLANES flag to make routines allocate on d^2 memory.

#define CELLS_PER_BLOCK  10

typedef enum { MATCH_SUB
             , DEL
             , INS
             } Trans;  // The 3 possible finite fsm_state machine fsm_states

/** Matrix of either Ukkonnen or CheckPoint cells.
 *  Also includes information on element size and amount of space currently allocated for the cells.
 */
typedef struct alignment_mtx_t {
    size_t elemSize;
    size_t lessLong_size;
    size_t lessMidd_size;
    size_t lessLong_offset;
    size_t lessMidd_offset;
    size_t lessLong_blocks;
    size_t lessMidd_blocks;

    #ifdef FIXED_NUM_PLANES // See above
        size_t costSize;
    #endif

    size_t baseAlloc;
    void **matrix;         // 2D arrays of either Ukk or CheckP cells. Void because may point at U_cell_type or CPType.

    size_t memAllocated;   // total amount of memory allocated to `matrix`
} alignment_mtx_t;


// This is a persistent set of costs needed throughout the code.
// I moved these costs all into this struct so I could remove global variables.
typedef struct affine_costs_t {
    unsigned int mismatchCost;        // Note that this is the substitution cost, so forces sub cost to be constant.
    unsigned int gapOpenCost;
    unsigned int gapExtendCost;

    // unsigned int deleteOpenCost;   // This was the cost for reverting a gap opening. It was only ever used to
                                      // assert that it was the same as gapOpenCost, and neither of those values ever mutate.
    // unsigned int deleteExtendCost; // Likewise, this was used where gapExtendCost could have been, as a delete is actually
                                      // a gap insertion, and an insertion in one character is a gap intertion in another.
} affine_costs_t;


/** Struct to hold persistant dynamic characters used throughout the code. Generally, it's the original inputs and the
 *  eventual outputs.
 */
typedef struct characters_t {
    size_t numStates;           // How many possible FSM fsm_states there are. You would expect this to be 27 (3^3), but
                                // if each of three FSMs is in one of {INS, DEL, MATCH_SUB}
                                // and given that one can't have all gap or more than one insertion it is---always---16.

    size_t maxSingleStep;       // The largest

    char *lesserStr;            // string representation of shortest character
    char *middleStr;            // string representation of middle character
    char *longerStr;            // string representation of longest character

    int lesserIdx;              // current index into shortest character; signed to avoid compiler warnings
    int middleIdx;              // current index into middle character; signed to avoid compiler warnings
    int longerIdx;              // current index into longest character; signed to avoid compiler warnings

    int lesserLen;              // length of respective string representation  <-- Used to define where to end on the three strings in the
    int middleLen;              // length of respective string representation  <-- check-point recursion.
    int longerLen;              // length of respective string representation  <-- So inputChars->lesserLen contains the edit distance the
                                                                                // recursion must finish on + 1.
                                                                                // int because their negatives are used in ukkCheckPoint
} characters_t;


/** Holds arrays of fsm states. For each state it stores the previous possible state as well as
 *  the cost to transition from the previous to the current state and the cost to stay in that state.
 *
 *  In this context, and fsm state is actually the combined states of three fsms, one for each character to be aligned,
 *  where the possible fsm states are Insert, Delete, and Match/Sub, i.e. insert a gap in some sequence or don't insert, where
 *  "some" is determined by the value: MDM, IMM, etc. Note that some states are not possible.
 *
 *  This is all calculated ahead of time, which begs the question, why is there only one previous state?
 */
typedef struct fsm_arrays_t {
    int *neighbours;                // array of neighbor fsm_states for each possible fsm state
    int *fsmState_continuationCost; // as with transition cost, the cost to extend one or two gaps. (You can't extend three gaps.)
    int *secondCost;                //
    int *transitionCost;            // cost to transition from one fsm_state to another (i.e. start or end a gap)
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
    alignment_mtx_t allocInit( size_t        elemSize
                             , size_t        costSize
                             , characters_t *globalCharacters
                             );
#else
    alignment_mtx_t allocInit( size_t        elemSize
                             , characters_t *globalCharacters
                             );
#endif


/** Is this a match insertion insertion (MII), etc.?
 *  Matches return 0, subs (which are also coded as M) return 1, various transitions add gap open or gap continuation costs.
 */
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


/** Mutates a, b, and c such that each is true or false if the least significant first, second or third digit, respectively,
 *  of neighbour is 1.
 *  I.e., if a given fsm state of `neighbour` is delete, set that state to true; otherwise, set to 0.
 */
void step( int  neighbour
         , int *a
         , int *b
         , int *c
         );


/** Returns the value of the neighbor fsm state where the current fsm state is ijk, and each of i, j, k can be 1 or 0.
 *  Creates a binary number where i is the least significant bit and k is the most.
 */
int neighbourNum( int i
                , int j
                , int k
                );


/** Resets a transition array that holds the transition fsm_states for three fsm state-transition FSMs. There are 27 possible fsm states.
 *  For instance, if the fsm state is 1, then the FSMs are in the cumulative fsm state [DEL, MATCH_SUB, MATCH_SUB],
 *  whereas if the fsm state were 22, the cumulative fsm state would be [DEL, DEL, INS] (which is actually not possible,
 *  as it signifies a gap in all three dynamic characters, which is meaningless).
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
 *
 *  Finish setup of characters.
 */
void setup( affine_costs_t  *globalCosts
          , characters_t    *inputChars
          , characters_t    *resultChars
          , fsm_arrays_t    *fsmStateArrays
          , dyn_character_t *in_lesserChar
          , dyn_character_t *in_middleChar
          , dyn_character_t *in_longerChar
          , unsigned int     mismatch_cost
          , unsigned int     gapOpen
          , unsigned int     gapExtend
          );


/* Ensure that output characters are aligned. */
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


/** Once three dynamic characters are aligned, step down the arrays and see when to
 *  add affine (deleted and instert) and mismatch costs.
 */
unsigned int alignmentCost( int             fsm_states[]
                          , char           *al1
                          , char           *al2
                          , char           *al3
                          , size_t          len
                          , affine_costs_t *globalCosts
                          , fsm_arrays_t   *fsmArrays
                          );


/** Checks to see if Ukkonnen or CheckPoint matrix needs to be reallocated. If so, continues to double it in size until
 *  the width is less than current edit distance. Returns pointer to cell indicated by `ab_idx_diff`, `ac_idx_diff` and `editDist`.
 *
 *  May call functions to alloc new plane, then returns pointer to first cell in that plane.
 */
void *getPtr( alignment_mtx_t *alignment_mtx_t
            , int           ab_idx_diff
            , int           ac_idx_diff
            , size_t        editDist
            , int           fsm_state
            , size_t        numStates
            );


/** Deallocate either Ukkonnen or Check Point matrix. */
void deallocate_MtxCell( alignment_mtx_t *inputMtx
                       // , void            *flag
                       // , void            *top
                       // , size_t           numStates
                       );


#endif // UKKCOMMON_H
