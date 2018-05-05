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


// Name:         ukk.dpa.c
// Type:         C Source
// Created:      Fri May 14 11:41:29 1999
// Author:       David Powell
// Project:      
// Description:  This program implements 3 sequence, linear
//               gap alignment using a generalisation of the
//               standard DPA.  Despite its name it has
//               nothing to do with Ukkonen's algorithm
//
// Notes:

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <values.h>
#include "ukkCommon.h"

typedef struct
{
  int cost;
  int from_s;
} D_cell_type;

int whichCharCost(char a, char b, char c);
void traceBack(int i, int j, int k, int s);

#define D(i,j,k,s) DPAmatrix[ ((((i)*(Blen+1)+(j))*(Clen+1)+(k))*numStates+(s)) ]
D_cell_type *DPAmatrix;

int finalCost;

int doUkk()
{
  long i,j,k;
  int toState,fromState;
  long matrixSize = 0;

  //matrixSize = ((Alen*(Blen+1)+Blen)*(Clen+1)+Clen)*numStates+numStates;
  matrixSize = (Alen+1) * (Blen+1) * (Clen+1) * numStates;

  fprintf(stderr, "Going to allocate %ld elements\n", matrixSize);
  DPAmatrix = (D_cell_type *)calloc(matrixSize, sizeof(D_cell_type));

  if (!DPAmatrix) {
    fprintf(stderr, "Unable to alloc memory\n");
    exit(-1);
  }

  for (toState=1; toState<numStates; toState++)
    D(0,0,0,toState).cost = INFINITY;
  D(0,0,0,0).cost = 0;
  
    
  for (i=0; i<=Alen; i++) {
    for (j=0; j<=Blen; j++) {
      for (k=0; k<=Clen; k++) {
	if (i==0 && j==0 && k==0) continue;
	
	for (toState=0; toState<numStates; toState++) {
	  int i2,j2,k2;
	  int da,db,dc;
	  int bestCost = INFINITY, bestState=-1;
	  int neighbour = neighbours[toState];
	  
	  step(neighbour,&da,&db,&dc);
	  i2 = i - da;
	  j2 = j - db;
	  k2 = k - dc;

	  if (i2<0 || j2<0 || k2<0) {
	    D(i,j,k,toState).cost = INFINITY;
	    continue;
	  }

	  for (fromState=0; fromState<numStates; fromState++) {
	    int mCost, thisCost;
	    int w = whichCharCost(da ? Astr[i-1] : '-',
				  db ? Bstr[j-1] : '-', 
				  dc ? Cstr[k-1] : '-');

	    if (w==0) 
	      mCost = -misCost; // Hack to get around fact that we are using UKK setup routines
	    else if (w==1)
	      mCost = 0;
	    else if (w==2)
	      mCost = misCost;
	    else
	      assert(0);

	    thisCost = D(i2,j2,k2,fromState).cost
	      + stateTransitionCost(fromState,toState)
	      + contCost[toState]
	      + mCost;

	    if (thisCost < bestCost) {
	      bestCost  = thisCost;
	      bestState = fromState;
	    }

/* #ifdef DEBUGTRACE */
/* 	    fprintf(stderr, "(%ld,%ld,%ld,%d) fromState=%d (%d,%d,%d) this=%d best=%d\n", */
/* 		    i,j,k,toState, */
/* 		    fromState, da,db,dc, */
/* 		    thisCost,bestCost); */
/* #endif */
	    
	  } // fromState

	  D(i,j,k,toState).cost = bestCost;
	  D(i,j,k,toState).from_s = bestState;
	  
#ifdef DEBUGTRACE
	  //	  fprintf(stderr,"D(%ld,%ld,%ld,%d) = %d\n", i, j, k, toState, bestCost);
	  fprintf(stderr,"U(%ld,%ld,%d,%d) = %ld\n", i-j, i-k, bestCost, toState, i);
#endif
	  
	} // toState
      } // k
    } // j
  } // i
  
  {
    int toState, bestState=-1, bestCost = INFINITY;
    
    for (toState=0; toState<numStates; toState++)
      if (bestCost > D(Alen,Blen,Clen,toState).cost) {
	bestCost = D(Alen,Blen,Clen,toState).cost;
	bestState = toState;
      }

    finalCost = bestCost;
    traceBack(Alen, Blen, Clen, bestState);

    printf("Final Cost=%d (final State=%d)\n",bestCost,bestState);

  }

  return 0;
}

void traceBack(int i, int j, int k, int s)
{
  int ai=0;
  char Ares[MAX_STR*2], Bres[MAX_STR*2], Cres[MAX_STR*2];
  int states[MAX_STR*2],cost[MAX_STR*2];

  while (i!=0 || j!=0 || k!=0) {
    int da,db,dc;
    int neighbour = neighbours[s];
    
    step(neighbour,&da,&db,&dc);
    Ares[ai] = (da ? Astr[i-1] : '-');
    Bres[ai] = (db ? Bstr[j-1] : '-');
    Cres[ai] = (dc ? Cstr[k-1] : '-');
    states[ai] = s;
    cost[ai] = D(i,j,k,s).cost;
    ai++;

    s = D(i,j,k,s).from_s;
    i = i - da;
    j = j - db;
    k = k - dc;
  }
  assert(i==0 && j==0 && k==0 && s==0);

  printf("ALIGNMENT\n");
  for (i=ai-1; i>=0; i--) printf("%c",Ares[i]); printf("\n");
  for (i=ai-1; i>=0; i--) printf("%c",Bres[i]); printf("\n");
  for (i=ai-1; i>=0; i--) printf("%c",Cres[i]); printf("\n");

  for (i=ai-1; i>=0; i--)
    printf("%s ",state2str(states[i]));
  printf("\n");

  for (i=ai-1; i>=0; i--)  printf("%-2d  ",cost[i]); printf("\n");

  revCharArray(Ares,0, ai);
  revCharArray(Bres,0, ai);
  revCharArray(Cres,0, ai);
  revIntArray(states, 0, ai);

  checkAlign(Ares,ai,Astr,Alen);
  checkAlign(Bres,ai,Bstr,Blen);
  checkAlign(Cres,ai,Cstr,Clen);

  assert(alignmentCost(states, Ares,Bres,Cres, ai) == finalCost);
}

void progDesc(char *prog) {
  printf(
  "This program calculates the edit cost for optimally aligning three sequences\n"
  "under linear gap costs.  It also determines an optimal alignment.\n"
  "A generalisation of the standard DPA is used.\n"
  "Time complexity O(n^3), space complexity O(n^3).\n"

  "\n\n"
  );
}



// End of ukk.dpa.c

