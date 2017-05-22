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
          // A given state can have come from 1 and only 1 neighbour
          // MMM, MMD, MDD, IMM, etc.  all have exactly one neighbour
          // Not possible to have more than 1 'I' state (eg. MII IMI)

// Name:         ukkCheckp.h
// FileType:     C Header
// Created:      Thu 10/20/16
// Author:       Eric Ford, based on .c file by David Powell

// TODO: document all of this?


/**** NOTE: All distances and costs are signed, as often initialized to -INFINITY ****/

#include "ukkCommon.h"

#ifndef UKKCHECKP_H
#define UKKCHECKP_H

// AllocInfo myUAllocInfo;
// AllocInfo myCPAllocInfo;

typedef struct counts_t {
    long cells;
    long innerLoop;
} counts_t;


/**  */
typedef struct from_t {
    int ab_idx_diff;
    int ac_idx_diff;
    int cost;           // Must be signed, as is sometimes initialized as negative
    int state;
} from_t;


/**  */
typedef struct ukk_cell_t {
    int    dist;        // must be int because comparing to -INFINITY
    long   computed;
    from_t from;
} ukk_cell_t;


/**  */
typedef struct check_point_t {
    int dist;   // must be int because comparing to -INFINITY
    int cost;
} check_point_t;


/** For Ukkonen check point between to specified points in the U matrix... TODO: ...?
 *  All distances and costs are signed, as often initialized to -INFINITY
 */
int doUkkInLimits( int startAB
                 , int startAC
                 , int startCost
                 , int startState
                 , int startDist
                 , int finalAB
                 , int finalAC
                 , int finalCost
                 , int finalState
                 , int finalDist
                 );

/** Extracts info from the 'from' and CP info then recurses with doUkkInLimits for the two subparts.
 *  All distances and costs are signed, as often initialized to -INFINITY
 */
int getSplitRecurse( size_t startAB
                   , size_t startAC
                   , int    startCost
                   , int    startState
                   , int    startDist
                   , size_t finalAB
                   , size_t finalAC
                   , int    finalCost
                   , int    finalState
                   , int    finalDist
                   );

/** Recovers an alignment directly from the Ukkonnen matrix.
 *  Used for the base case of the check point recursion.
 */
void traceBack( int startAB
              , int startAC
              , int startCost
              , int startState
              , int finalAB
              , int finalAC
              , int finalCost
              , unsigned int finalState
              );

/**  */
int Ukk( int          ab_idx_diff
       , int          ac_idx_diff
       , int          distance
       , unsigned int state
       );

// Find the furthest distance at ab_idx_diff, ac_idx_diff, input_distance. wantState selects whether the
// best distance is returned, or the best final state (needed for ukk.alloc traceback)
int findBest_DistState( int ab_idx_diff
                      , int ac_idx_diff
                      , int input_dist
                      , int return_the_state
                      );


/**  */
int whichCharCost(char a, char b, char c);

// IMPORTANT!!! Order of input characters is short, long, middle.
int doUkk( dyn_character_t *retCharA
         , dyn_character_t *retCharB
         , dyn_character_t *retCharC
         );


/**  */
int char_to_base (char v);


/**  */
void printTraceBack( dyn_character_t *retCharA
                   , dyn_character_t *retCharB
                   , dyn_character_t *retCharC
                   );


/**  */
int calcUkk( int ab_idx_diff
           , int ac_idx_diff
           , int input_dist
           , int toState
           );

#endif
