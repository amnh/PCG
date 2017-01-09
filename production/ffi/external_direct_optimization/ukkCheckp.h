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

typedef struct {
    long cells,
    innerLoop;
} Counts;

//typedef struct {int dist; long computed;} U_cell_type;
typedef struct {
    int ab;
    int ac;
    int cost;
    int state;
} fromType;

typedef struct {
    int dist;
    long computed;
    fromType from;
} U_cell_type;

typedef struct {
    int dist;
    int cost;
} CPType;

//typedef struct {int from_ab,from_ac,from_cost,from_state;} From_type;


U_cell_type *U(int ab, int ac, int d, int s);

static inline CPType *CP(int ab, int ac, int d, int s);

// doUkkInLimits - for Ukkonen check point between to specified points in the U matrix
int doUkkInLimits(int sab, int sac, int sCost, int sState, int sDist,
                  int fab, int fac, int fCost, int fState, int fDist);

// getSplitRecurse - extracts info from the 'from' and CP info then recurses with doUkkInLimits
//                   for the two subparts
int getSplitRecurse(int sab, int sac, int sCost, int sState, int sDist,
                    int fab, int fac, int fCost, int fState, int fDist);

// traceBack - recovers an alignment from the U matrix directly.  Used for the base case
//             of the check point recursion
void traceBack(int sab, int sac, int sCost, int sState,
               int fab, int fac, int fCost, int fState);


int Ukk(int ab,int ac,int d,int state);

// Find the furthest distance at ab,ac,d.   wantState selects whether the
// best distance is returned, or the best final state (needed for ukk.alloc traceback)
int best(int ab, int ac, int d, int wantState);

static inline void sort(int aval[], int len);

int calcUkk(int ab, int ac, int d, int toState);

int okIndex(int a, int da, int end);

int whichCharCost(char a, char b, char c);

int doUkk(seq_p retSeqA, seq_p retSeqB, seq_p retSeqC);

int char_to_base (char v);

void printTraceBack(seq_p retSeqA, seq_p retSeqB, seq_p retSeqC);

static inline int withinMatrix(int ab, int ac, int d);

int calcUkk(int ab, int ac, int d, int toState);

#endif