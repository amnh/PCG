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


// Similar to ukk.alloc.new but uses % trick to use less memory, by
// not retrieving the alignment.  Note also, the 'computed' field is
// used to store which cost (actually d+costOffset) the cell contains
// instead of simply whether the cell has been computed or not.

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#define NDEBUG 1
#include <assert.h>

#define MAXINT INT_MAX
#define FIXED_NUM_PLANES 0
// #include "seq.h"
#include "ukkCommon.h"

          // A given state can have come from 1 and only 1 neighbour
          // MMM, MMD, MDD, IMM, etc.  all have exactly one neighbour
          // Not possible to have more than 1 'I' state (eg. MII IMI)

typedef struct {long cells, innerLoop;} Counts;

//typedef struct {int dist; long computed;} U_cell_type;
typedef struct {int ab,ac,cost,state;} fromType;
typedef struct {int dist; long computed; fromType from;} U_cell_type;
typedef struct {int dist; int cost;} CPType;
//typedef struct {int from_ab,from_ac,from_cost,from_state;} From_type;

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
void printTraceBack();



 int Ukk(int ab,int ac,int d,int state);
int best(int ab, int ac, int d, int wantState);
int calcUkk(int ab, int ac, int d, int toState);
int okIndex(int a, int da, int end);
int whichCharCost(char a, char b, char c);




int doUkk(struct seq *ra, struct seq *rb, struct seq *rc);

int doUkkInLimits(int sab, int sac, int sCost, int sState, int sDist,
		  int fab, int fac, int fCost, int fState, int fDist);

int getSplitRecurse(int sab, int sac, int sCost, int sState, int sDist,
		    int fab, int fac, int fCost, int fState, int fDist);

// -- Traceback routines --------------------------------------------------------------
void traceBack(int sab, int sac, int sCost, int sState,
	       int fab, int fac, int fCost, int fState);

int
char_to_base (char v);

void printTraceBack(struct seq *ra, struct seq *rb, struct seq *rc);

// Find the furthest distance at ab,ac,d.   wantState selects whether the
// best distance is returned, or the best final state (needed for ukk.alloc traceback)
int best(int ab, int ac, int d, int wantState);

 int withinMatrix(int ab, int ac, int d);

 int Ukk(int ab,int ac,int d,int state);

int calcUkk(int ab, int ac, int d, int toState);

void progDesc(char *prog);
