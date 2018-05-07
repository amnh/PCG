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



// ukk.alloc.c - compiles to ukk.alloc or ukk.noalign 
// Note the 'computed' field is used to store which cost (actually d+1)
// this cell was computed for.
// When compiling for ukk.noalign compile with '-DEDITCOST_ONLY'
// This uses the % trick to reduce mem usage to d^2.
// the % trick is actually implemented in ukkCommon.h

#include <stdio.h>
#include <stdlib.h>
#include <values.h>
#include <assert.h>

#ifdef  EDITCOST_ONLY
#define FIXED_NUM_PLANES
#endif

#include "ukkCommon.h"

          // A given state can have come from 1 and only 1 neighbour
          // MMM, MMD, MDD, IMM, etc.  all have exactly one neighbour
          // Not possible to have more than 1 'I' state (eg. MII IMI)

typedef struct {long cells, innerLoop;} Counts;

#ifdef EDITCOST_ONLY
typedef struct {int dist; int computed;} U_cell_type;
#else
typedef struct {int ab,ac,cost,state;} fromType;
typedef struct {int dist; long computed; fromType from;} U_cell_type;
#endif

// Functions to appear.
int doUkk();
inline int withinMatrix(int ab, int ac, int d);
inline int Ukk(int ab,int ac,int d,int state);
int best(int ab, int ac, int d, int wantState);
int calcUkk(int ab, int ac, int d, int toState);
int okIndex(int a, int da, int end);
int whichCharCost(char a, char b, char c);
#ifndef EDITCOST_ONLY
void traceBack(int ab, int ac, int d, int s);
#endif

AllocInfo myAllocInfo;
U_cell_type dummyCell;

inline U_cell_type *U(int ab, int ac, int d, int s) { return getPtr(&myAllocInfo,ab,ac,d,s); };

Counts counts;
int furthestReached=-1;

int doUkk()
{
  int d=-1;
  int finalab,finalac;

  finalab = Alen-Blen;
  finalac = Alen-Clen;

#ifdef EDITCOST_ONLY
  myAllocInfo = allocInit(sizeof(U_cell_type), maxSingleCost);
#else
  myAllocInfo = allocInit(sizeof(U_cell_type));
#endif

  counts.cells=0;
  counts.innerLoop=0;

  { // Calculate starting position
    int d=0;
    while (d<Alen && (Astr[d]==Bstr[d] && Astr[d]==Cstr[d])) {
      d++;
      counts.innerLoop++;
    }
    U(0,0,0,0)->dist = d;
    U(0,0,0,0)->computed = 1;
  }

  do {
    d++;
    fprintf(stderr,"About to do cost %d\n",d);

    Ukk(finalab,finalac,d,0);

#ifdef DEBUG
    fprintf(stderr,"Furthest reached for cost %d is %d.\n",
	    d,furthestReached);
#endif

  } while (best(finalab,finalac,d,0)<Alen);

#ifndef EDITCOST_ONLY
  traceBack(finalab, finalac, d, best(finalab,finalac,d,1));
#endif

  printf("Final cost = %d\n",d);
  printf("Number of cells calculated = %ld.  Inner Loop = %ld\n",
          counts.cells,counts.innerLoop);
  printf("numStates=%d\n",numStates);
  printf("DPA(N^3) would calculate %d (or %d)\n",
          (Alen+1)*(Blen+1)*(Clen+1)*numStates,
          (Alen+1)*(Blen+1)*(Clen+1)*(MAX_STATES-1));

  allocFinal(&myAllocInfo, (&dummyCell.computed),(&dummyCell));

  return d;
}

// Find the furthest distance at ab,ac,d.   wantState selects whether the
// best distance is returned, or the best final state (needed for ukk.alloc traceback)
int best(int ab, int ac, int d, int wantState)
{
  int s;

  int best = -INFINITY;
  int bestState = -1;
  for (s=0; s<numStates; s++) {
    if (withinMatrix(ab,ac,d) &&
       U(ab,ac,d,s)->computed && U(ab,ac,d,s)->dist>best) {
      best = U(ab,ac,d,s)->dist;
      bestState = s;
    }
  }

//  fprintf(stderr,"best(%d,%d,%d,(%d))=%d\n",ab,ac,d,bestState,best);

  if (wantState)
    return bestState;
  else
    return best;
}

#ifndef EDITCOST_ONLY
void traceBack(int ab, int ac, int d, int s)
{
  int finalCost = d;
  int ai=0,bi=0,ci=0,si=0,costi=0;
  char Ares[MAX_STR*2], Bres[MAX_STR*2], Cres[MAX_STR*2];
  int states[MAX_STR*2],cost[MAX_STR*2];
  
  while(ab!=0 || ac!=0 || d!=0) {
    int a=U(ab,ac,d,s)->dist;
    int nab=U(ab,ac,d,s)->from.ab;
    int nac=U(ab,ac,d,s)->from.ac;
    int nd=U(ab,ac,d,s)->from.cost;
    int ns=U(ab,ac,d,s)->from.state;

    int b=a-ab,c=a-ac;
    int a1 = U(nab,nac,nd,ns)->dist;
    int b1=a1-nab,c1=a1-nac;

    assert(U(ab,ac,d,s)->computed == d+1);
    assert(U(nab,nac,nd,ns)->computed == nd+1);

#ifdef DEBUG
    fprintf(stderr, "ab=%3d ac=%3d d=%3d s=%2d dist=%3d   nab=%3d nac=%3d nd=%3d ns=%2d ndist=%3d\n",
	    ab,ac,d,s,a,
	    nab,nac,nd,ns,a1);
#endif

    while (a>a1 && b>b1 && c>c1) {
      a--; b--; c--;
      Ares[ai++] = Astr[a];
      Bres[bi++] = Bstr[b];
      Cres[ci++] = Cstr[c];
      states[si++] = 0;		/* The match state */
      cost[costi++] = d;
    }

    if (a!=a1 || b!=b1 || c!=c1) {
      if (a>a1) Ares[ai++]=Astr[--a]; else Ares[ai++]='-';
      if (b>b1) Bres[bi++]=Bstr[--b]; else Bres[bi++]='-';
      if (c>c1) Cres[ci++]=Cstr[--c]; else Cres[ci++]='-';
      states[si++] = s;
      cost[costi++] = d;
    }

    assert(a==a1 && b==b1 && c==c1);

    ab=nab; ac=nac; d=nd; s=ns;
  }

  // Add the first run of matches to the alignment
  { int i;
    for (i=U(0,0,0,0)->dist-1; i>=0; i--) {
      Ares[ai++] = Astr[i];
      Bres[bi++] = Bstr[i];
      Cres[ci++] = Cstr[i];
      states[si++] = 0;		/* The match state */
      cost[costi++] = 0;
    }
  }

  {
    // Reverse the alignments
    revCharArray(Ares,0,ai);
    revCharArray(Bres,0,bi);
    revCharArray(Cres,0,ci);
    revIntArray(states,0,si);
    revIntArray(cost,0,costi);
  }

  // Print out the alignment
  printf("ALIGNMENT\n");
  { int i; 
    for (i=0; i<ai; i++) printf("%c",Ares[i]); printf("\n");
    for (i=0; i<bi; i++) printf("%c",Bres[i]); printf("\n");
    for (i=0; i<ci; i++) printf("%c",Cres[i]); printf("\n");

    // Print state information
    for (i=0; i<si; i++)
      printf("%s ",state2str(states[i]));
    printf("\n");

    // Print cost stuff
    for (i=0; i<costi; i++)  printf("%-2d  ",cost[i]); printf("\n");
  }
  
  assert(ai==bi && ai==ci && ai==si && ai==costi);

  checkAlign(Ares,ai,Astr,Alen);
  checkAlign(Bres,bi,Bstr,Blen);
  checkAlign(Cres,ci,Cstr,Clen);

  assert(alignmentCost(states, Ares,Bres,Cres, ai) == finalCost);
}
#endif

inline void sort(int aval[], int len)
{
  int i,j;

  for (i=0; i<len; i++) {
    int minI=i, t;
    for (j=i+1; j<len; j++) {
      if (aval[j] < aval[minI])
	minI = j;
    }
    t = aval[i];
    aval[i] = aval[minI];
    aval[minI] = t;
  }
}

inline int withinMatrix(int ab, int ac, int d)
{
  // The new method for checking the boundary condition.  Much tighter ~20%(?)  -- 28/02/1999
  int bc=ac-ab;
  int aval[3];
  int g,h,cheapest;

  if (d<0) return 0;
  
  aval[0]=abs(ab);
  aval[1]=abs(ac);
  aval[2]=abs(bc);

  // Set g and h to the smallest and second smallest of aval[] respectively
  sort(aval,3);
  g = aval[0];
  h = aval[1];

  cheapest = (g==0 ? 0 : startInsert+g*continueInsert) + (h==0 ? 0 : startInsert+h*continueInsert);

  if (cheapest>d)
    return 0;
  else
    return 1;
}

inline int Ukk(int ab,int ac,int d,int state) 
{
  
  if (!withinMatrix(ab,ac,d)) return -INFINITY;
#ifdef EDITCOST_ONLY
  if (U(ab,ac,d,state)->computed == d+1) {
#else
  if (U(ab,ac,d,state)->computed) {
#endif
    assert(U(ab,ac,d,state)->computed == d+1);
    return U(ab,ac,d,state)->dist;
  }

/*
  fprintf(stderr,"Calculating U(%d,%d,%d,%d)",ab,ac,d,state);
*/
  counts.cells++;

  calcUkk(ab,ac,d,state);

  if (U(ab,ac,d,state)->dist>furthestReached)
    furthestReached = U(ab,ac,d,state)->dist;

  return U(ab,ac,d,state)->dist;
}

#ifdef DEBUGTRACE  
int indenti = 0;
char indent[1000];
#endif

int calcUkk(int ab, int ac, int d, int toState)
{
  int neighbour = neighbours[toState];
  int da,db,dc,ab1,ac1;

#ifndef EDITCOST_ONLY
  fromType from;
#endif
  int bestDist;

#ifdef DEBUGTRACE
  indent[indenti]=0;
  fprintf(stderr,"%sCalcUKK(ab=%d,ac=%d,d=%d,toState=%d)\n",
	  indent,
	  ab,ac,d,toState);
  indent[indenti++]=' ';
  indent[indenti++]=' ';
  indent[indenti]=0;
#endif

#ifdef EDITCOST_ONLY
  assert(U(ab,ac,d,toState)->computed < d+1);
#else
  assert(U(ab,ac,d,toState)->computed == 0);
#endif
  bestDist = -INFINITY;

  step(neighbour,&da,&db,&dc);     
  ab1 = ab-da+db;
  ac1 = ac-da+dc;
    
  // calculate if its a valid diagonal
  if (ab1>=-Blen && ab1<=Alen && ac1>=-Clen && ac1<=Alen) {
    int fromState;

    // Loop over possible state we are moving from
    //   May be possible to limit this?
    for (fromState=0; fromState<numStates; fromState++) {
      int transCost=stateTransitionCost(fromState,toState);
      int fromCost = -INFINITY;
      int dist = -INFINITY;
      int cost = d-transCost-contCost[toState];
      int a1 = Ukk(ab1,ac1,cost,fromState);
      int a2 = -1;

      if (okIndex(a1,da,Alen) &&
	  okIndex(a1-ab1,db,Blen) &&
	  okIndex(a1-ac1,dc,Clen) &&
	  (whichCharCost(da ? Astr[a1] : '-',
			 db ? Bstr[a1-ab1] : '-', 
			 dc ? Cstr[a1-ac1] : '-')==1) ) {
	fromCost = cost;
	dist = a1+da;
      } else {
	if (!secondCost[toState]) continue;
	
	a2 = Ukk(ab1,ac1,cost-misCost,fromState);

	if (okIndex(a2,da,Alen) &&
	    okIndex(a2-ab1,db,Blen) &&
	    okIndex(a2-ac1,dc,Clen)) {
	  fromCost = cost-misCost;
	  dist = a2+da;
	}
      }

      // Check if this is an improvment
      if (bestDist<dist) {
        bestDist = dist;
#ifndef EDITCOST_ONLY
        from.ab = ab1;
        from.ac = ac1;
        from.cost = fromCost;
        from.state = fromState;
#endif
      }
    } // End loop over from states 
  } // End if valid neighbour

  // Insure that we have how we can reach for AT MOST cost d
  {
    int dist = Ukk(ab,ac,d-1,toState);
    // Check if this is an improvment
    if (bestDist<dist) {
      bestDist = dist;
#ifndef EDITCOST_ONLY
      from.ab = ab;
      from.ac = ac;
      from.cost = d-1;
      from.state = toState;
#endif
    }
  }

  if (toState == 0) {  // Is the toState == MMM
    // May be possible to extend the diagonal along a run of matches.

    /* Note: In the past have used 'extended' to only update this cell if
       we actually extend a diagonal.  This is WRONG.  The reason is that
       if we pick the furthest along and try to extend that only, it may 
       not extend, and thus this cell will not be updated.  Whereas a 
       cell less far along may have been able to extend as further than
       the current value.

       Note:  This current method of updating regardless of whether there
       is actually a run of matches, causes some descrepencies between the
       U matrix and the D matrix.
    */
       

    // Get furthest of states for this cost 
    int dist=-INFINITY;
    int from_state=-1,s;

    // Find furthest state
    for (s=0; s<numStates; s++) {
      int thisdist;
      thisdist = (s==0) ? bestDist : Ukk(ab,ac,d,s);
      if (thisdist>dist) {
	dist = thisdist;
	from_state = s;
      }
    }

    // Try to extend to diagonal
    while (dist>=0 && dist<Alen && 
	   (Astr[dist]==Bstr[dist-ab] && Astr[dist]==Cstr[dist-ac])) {
      dist++;
      counts.innerLoop++;
    }

    // Was there an improvement?
    if (dist > bestDist) {
      bestDist = dist;  // Note: toState=MMM

#ifndef EDITCOST_ONLY
      // Update from information if a diagonal was extended, and the
      // diagonal we extended from was not the MMM state.
      if (from_state!=0) {
        from.ab = ab;
        from.ac = ac;
        from.cost = d;
        from.state = from_state;
      }
#endif
    }
  } // End attempt to extend diagonal on a run of matches

#ifdef EDITCOST_ONLY
  assert(U(ab,ac,d,toState)->computed < d+1);
#else
  assert(!U(ab,ac,d,toState)->computed);
#endif
  U(ab,ac,d,toState)->dist = bestDist;
  U(ab,ac,d,toState)->computed = d+1; // Store cost computed +1.  The +1 is so 0 can be
                                      // used to represent unused
#ifndef EDITCOST_ONLY
  U(ab,ac,d,toState)->from = from;
#endif

#ifdef DEBUGTRACE
  indenti-=2;
  indent[indenti]=0;  
  fprintf(stderr,"%sCalcUKK(ab=%d,ac=%d,d=%d,toState=%d)=%d\n",
	  indent,
	  ab,ac,d,toState,U(ab,ac,d,toState)->dist);
#ifndef EDITCOST_ONLY
  fprintf(stderr,"%sFrom: ab=%d ac=%d cost=%d state=%d\n",
	  indent,
	  U(ab,ac,d,toState)->from.ab,   U(ab,ac,d,toState)->from.ac,
	  U(ab,ac,d,toState)->from.cost, U(ab,ac,d,toState)->from.state);
#endif
#endif  
  
  return U(ab,ac,d,toState)->dist;
}

void progDesc(char *prog) {
#ifdef EDITCOST_ONLY
  printf(
  "This program calculates the edit cost for optimally aligning three sequences\n"
  "under linear gap costs.  A generalisation of Ukkonen's algorithm to three\n"
  "sequences is used.  Average time complexity O(n + d^3), space complexity O(d^2).\n"
  "For more details, see:\n\n");

#else

  printf(
  "This program calculates the edit cost for optimally aligning three sequences\n"
  "under linear gap costs.  It also determines an optimal alignment.\n"
  "A generalisation of Ukkonen's algorithm to three sequence is used.\n"
  "Average time complexity O(n + d^3), space complexity O(d^3).\n"
  "For more details, see:\n\n");
#endif
  printf(
  "    D. R. Powell, L. Allison and T. I. Dix,\n"
  "    \"Fast, Optimal Alignment of Three Sequences Using Linear Gap Costs\",\n"
  "    Journal of Theoretical Biology, 207:3, pp 325-336\n"
  "\n\n"
  );
}
