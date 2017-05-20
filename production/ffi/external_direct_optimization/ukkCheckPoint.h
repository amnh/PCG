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

#include "ukkCommon.h"

#ifndef UKKCHECKP_H
#define UKKCHECKP_H

// AllocInfo myUAllocInfo;
// AllocInfo myCPAllocInfo;

typedef struct counts_t {
    long cells;
    long innerLoop;
} counts_t;

//typedef struct {int dist; long computed;} U_cell_type;
typedef struct from_t {
    int          ab;
    int          ac;
    unsigned int cost;
    int          state;
} from_t;

typedef struct ukk_cell_t {
    int    dist;
    long   computed;
    from_t from;
} ukk_cell_t;

typedef struct check_point_t {
    size_t       dist;
    unsigned int cost;
} check_point_t;

//typedef struct {int from_ab,from_ac,from_cost,from_state;} From_type;


//U_cell_type *U(int ab, int ac, int d, int s);

// doUkkInLimits - for Ukkonen check point between to specified points in the U matrix
size_t doUkkInLimits( size_t       startAB
                    , size_t       startAC
                    , unsigned int startCost
                    , int          startState
                    , size_t       startDist
                    , size_t       finalAB
                    , size_t       finalAC
                    , unsigned int finalCost
                    , int          finalState
                    , size_t       finalDist
                    );

// getSplitRecurse - extracts info from the 'from' and CP info then recurses with doUkkInLimits
//                   for the two subparts
int getSplitRecurse( size_t       startAB
                   , size_t       startAC
                   , unsigned int startCost
                   , int          startState
                   , size_t       startDist
                   , size_t       finalAB
                   , size_t       finalAC
                   , unsigned int finalCost
                   , int          finalState
                   , size_t       finalDist
                   );

// traceBack - recovers an alignment from the U matrix directly.  Used for the base case
//             of the check point recursion
void traceBack( int startAB
              , int startAC
              , int startCost
              , int startState
              , int finalAB
              , int finalAC
              , int finalCost
              , int finalState
              );


int Ukk( int ab
       , int ac
       , int d
       , int state
       );

// Find the furthest distance at ab, ac, d. wantState selects whether the
// best distance is returned, or the best final state (needed for ukk.alloc traceback)
int best(int ab, int ac, int d, int wantState);

int calcUkk(int ab, int ac, int d, int toState);

int whichCharCost(char a, char b, char c);

// IMPORTANT!!! Order of input characters is short, long, middle.
int doUkk( dyn_character_t *retCharA
         , dyn_character_t *retCharB
         , dyn_character_t *retCharC
         );

int char_to_base (char v);

void printTraceBack( dyn_character_t *retCharA
                   , dyn_character_t *retCharB
                   , dyn_character_t *retCharC
                   );

int calcUkk( int ab
           , int ac
           , int d
           , int toState
           );

#endif
