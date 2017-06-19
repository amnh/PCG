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

// TODO: Is this true?
          // A given fsm_state can have come from 1 and only 1 neighbour
          // MMM, MMD, MDD, IMM, etc.  all have exactly one neighbour
          // Not possible to have more than 1 'I' fsm_state (eg. MII IMI)

// Name:         ukkCheckp.h
// FileType:     C Header
// Created:      Thu 10/20/16
// Author:       Eric Ford, based on .c file by David Powell

// TODO: document all of this?


/**** NOTE: All edit distances and costs are signed, as often initialized to -INFINITY ****/

#ifndef UKKCHECKP_H
#define UKKCHECKP_H

#include "costMatrix.h"
#include "ukkCommon.h"

// AllocInfo myUAllocInfo;
// AllocInfo myCPAllocInfo;

// TODO: /** How many cells there are, and how wide ukk band is? */
typedef struct counts_t {
    long cells;
    long innerLoop;
} counts_t;


/** The previous finite state machine state. */
typedef struct from_t {
    int lessLong_idx_diff;  // This and next are used to map into Ukkonnen matrix, where each cell, (ab, d) is (_idx_diff, edit distance).
    int lessMidd_idx_diff;  // From Powell, et al. 2000: that's the length of a (top character) for a given edit distance.
                            // So row in Ukk is diagonal in distance matrix and column is a cost "contour".
    int cost;               // Must be signed, as is sometimes initialized as negative
    int fsm_state;
} from_t;


/**  */
typedef struct ukk_cell_t {
    int    editDist;        // must be int because comparing to -INFINITY
    long   computed;
    from_t from;
} ukk_cell_t;


/**  */
typedef struct checkPoint_cell_t {
    int editDist;   // must be int because comparing to -INFINITY
    int cost;
} checkPoint_cell_t;


// TODO: unsigned ints for costs? Probably shouldn't be, actually.
/** This is the interface function to the alignment code. It takes in three characters, as well as a mismatch cost, a gap open cost and
 *  a gap extention cost (all of which should be replaced by a 3d cost matrix).
 *
 *  IMPORTANT!!! Order of input characters is short, long, middle, or at least short must be first.
 */
int powell_3D_align( dyn_character_t *lesserChar
                   , dyn_character_t *middleChar
                   , dyn_character_t *longerChar
                   , dyn_character_t *retLesserChar
                   , dyn_character_t *retMiddleChar
                   , dyn_character_t *retLongerChar
                   , unsigned int     mismatchCost
                   , unsigned int     gapOpenCost
                   , unsigned int     gapExtendCost
                   );


/** For Ukkonen check point between to specified points in the U matrix... TODO: ...?
 *  All distances and costs are signed, as often initialized to -INFINITY
 */
// int doUkkInLimits( int              start_lessLong_idx_diff
//                  , int              start_lessMidd_idx_diff
//                  , int              startCost
//                  , int              startState
//                  , int              start_editDist
//                  , int              final_lessLong_idx_diff
//                  , int              final_lessMidd_idx_diff
//                  , int              finalCost
//                  , int              finalState
//                  , int              finalDist
//                  , affine_costs_t  *affineCosts
//                  , characters_t    *inputChars
//                  , characters_t    *resultChars
//                  , fsm_arrays_t *fsmArrays
//                  );

int doUkkInLimits( int             start_lessLong_idx_diff
                 , int             start_lessMidd_idx_diff
                 , int             startCost
                 , int             startState
                 , int             start_editDist
                 , int             final_lessLong_idx_diff
                 , int             final_lessMidd_idx_diff
                 , int             finalCost
                 , int             finalState
                 , int             finalDist
                 , affine_costs_t *affineCosts
                 , characters_t   *inputChars
                 , characters_t   *resultChars
                 , fsm_arrays_t   *globalCostArrays
                 );


/** Extracts info from the 'from' and CP info then recurses with doUkkInLimits for the two subparts.
 *  All distances and costs are signed, as often initialized to -INFINITY
 */
// int getSplitRecurse( size_t           start_lessLong_idx_diff
//                    , size_t           start_lessMidd_idx_diff
//                    , int              startCost
//                    , int              startState
//                    , int              start_editDist
//                    , size_t           final_lessLong_idx_diff
//                    , size_t           final_lessMidd_idx_diff
//                    , int              finalCost
//                    , int              finalState
//                    , int              finalDist
//                    , affine_costs_t  *affineCosts
//                    , characters_t    *inputChars
//                    , characters_t    *resultChars
//                    , fsm_arrays_t *fsmArrays
//                    );
int getSplitRecurse( int             start_lessLong_idx_diff
                   , int             start_lessMidd_idx_diff
                   , int             startCost
                   , int             startState
                   , int             start_editDist
                   , size_t          final_lessLong_idx_diff
                   , size_t          final_lessMidd_idx_diff
                   , int             finalCost
                   , int             finalState
                   , int             finalDist
                   , affine_costs_t *affineCosts
                   , characters_t   *inputChars
                   , characters_t   *resultChars
                   , fsm_arrays_t   *globalCostArrays
                   );


/** Recovers an alignment directly from the Ukkonnen matrix.
 *  Used for the base case of the check point recursion.
 */
// void traceBack( int           start_lessLong_idx_diff
//               , int           start_lessMidd_idx_diff
//               , int           startCost
//               , int           startState
//               , int           final_lessLong_idx_diff
//               , int           final_lessMidd_idx_diff
//               , int           finalCost
//               , unsigned int  finalState
//               , characters_t *inputChars
//               , characters_t *resultChars
//               );

void traceBack( int           start_lessLong_idx_diff
              , int           start_lessMidd_idx_diff
              , int           startCost
              , int           startState
              , int           final_lessLong_idx_diff
              , int           final_lessMidd_idx_diff
              , int           finalCost
              , unsigned int  finalState
              , characters_t *inputChars
              , characters_t *resultChars
              );

/**  */
int Ukk( int             lessLong_idx_diff
       , int             lessMidd_idx_diff
       , int             editDistance
       , unsigned int    fsm_state
       , affine_costs_t *affineCosts
       , characters_t   *inputChars
       , fsm_arrays_t   *fsmArrays
       );


/** For clarity, calls findBest with return_the_fsm_state = 0 */
int find_bestDist( int    lessLong_idx_diff
                 , int    lessMidd_idx_diff
                 , int    input_editDist
                 , size_t numStates
                 );


/** For clarity, calls findBest with return_the_fsm_state = 1 */
int find_bestState( int    lessLong_idx_diff
                  , int    lessMidd_idx_diff
                  , int    input_editDist
                  , size_t numStates
                  );


/** Find the furthest distance at lessLong_idx_diff, lessMidd_idx_diff, input_editDistance. return_the_fsm_state selects whether the
 *  best distance is returned, or the best final fsm_state (needed for ukk.alloc traceback)
 */
int findBest( int    lessLong_idx_diff
            , int    lessMidd_idx_diff
            , int    input_editDist
            , int    return_the_fsm_state
            , size_t numStates
            );


/** Set up and then call functions that do actual affine Ukkonnen calculations.
 *
 *  IMPORTANT!!! Order of input characters is short, long, middle.
 */
int align3d_ukk( dyn_character_t *retLesserChar
               , dyn_character_t *retMiddleChar
               , dyn_character_t *retLongerChar
               , dyn_character_t *original_lesserChar
               , dyn_character_t *original_middleChar
               , dyn_character_t *original_longerChar
               , affine_costs_t  *affineCosts
               , characters_t    *inputChars
               , characters_t    *resultChars
               , fsm_arrays_t    *globalCostArrays
               );


/** Converts a character input, {A, C, G, T} to an int. Problem: on ambiguous inputs biases toward A.
 *  TODO: Also, disallows larger alphabets.
 */
int char_to_base (char v);


/**  */
// void printTraceBack( affine_costs_t *affineCosts
//                    , characters_t   *inputChars
//                    , characters_t   *resultChars
//                    , fsm_arrays_t   *fsmArrays
//                    );

void printTraceBack( dyn_character_t *retLesserChar
                   , dyn_character_t *retMiddleChar
                   , dyn_character_t *retLongerChar
                   , dyn_character_t *original_lesserChar
                   , dyn_character_t *original_middleChar
                   , dyn_character_t *original_longerChar
                   , affine_costs_t  *affineCosts
                   , characters_t    *inputChars
                   , characters_t    *resultChars
                   , fsm_arrays_t    *globalCostArrays
                   );


/**  */
int calcUkk( int             lessLong_idx_diff
           , int             lessMidd_idx_diff
           , int             input_editDist
           , int             toState
           , affine_costs_t *affineCosts
           , characters_t   *inputChars
           , fsm_arrays_t   *fsmArrays
           );

#endif // UKKCHECKP_H
