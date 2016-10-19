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
 * Copyright (C) David Powell <david@drp.id.au>
 * This program comes with ABSOLUTELY NO WARRANTY, and is provided
 * under the GNU Public License v2.
 *
 */


// Name:         ukkCommon.c
// Type:         C Source
// Created:      Tue May 11 13:22:07 1999
// Author:       David Powell

// Contains the common routines for ukk.alloc, ukk.noalign, ukk.checkp and ukk.dpa
// Also see ukkCommon.h
// Compile with -DSYSTEM_INFO to print system information of
// every run. Useful to timing runs where cpu info is important.

/**
 *  Usage: [m a b]
 *  where m is the cost of a mismatch,
 *      a is the cost to start a gap,
 *      b is the cost to extend a gap.
 */

// #define UKKCOMMON_C
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#define NO_ALLOC_ROUTINES
#include "debug.h"
#include "seq.h"
#include "ukkCommon.h"

// GLOBAL VARIABLES
// TODO: Can we make these variables non-global?
int neighbours[MAX_STATES];
int contCost  [MAX_STATES];
int secondCost[MAX_STATES];
int transCost [MAX_STATES] [MAX_STATES];
int stateNum  [MAX_STATES];

int mismatchCost     = 1;
int gapOpenCost      = 3;             // a: w(k)=a+b*k
int gapExtendCost    = 1;             // b:
int deleteOpenCost   = 3;
int deleteExtendCost = 1;

int numStates;
int maxSingleStep;

char aStr[MAX_STR];
char bStr[MAX_STR];
char cStr[MAX_STR];
int  aLen, bLen, cLen;

AllocInfo myUAllocInfo;
AllocInfo myCPAllocInfo;

CPType CPdummyCell;
U_cell_type UdummyCell;
Counts counts;

long costOffset     = 1;
int furthestReached = -1;
int finalCost;

int sabG = 0, sacG = 0, sCostG = 0, sStateG = 0;
int aIdx = 0, bIdx = 0, cIdx = 0, si = 0, costi = 0;

#ifdef DEBUG_CALL_ORDER
int indenti = 0;
char indent[1000];
#endif

int endA, endB, endC;     // Used to define where to end on the three strings in the 
                          // checkp recursion.  So endA contains the distance the recursion
                          // must finish on + 1.

int completeFromInfo = 0; // Set to 1 for base cases, so 'from' info that alignment
                          //  can be retrieved from is set.

int CPcost;
int CPwidth;
int completeFromInfo;

int endA, endB, endC;   // Used to define where to end on the three strings in the 
                        // checkp recursion.  So endA contains the distance the recursion
                        // must finish on + 1.

int CPonDist;   // Flag for whether to use distance of cost as the CP criteria
                // CP on dist is only done for first iteration when then final
                // cost is unknown

char resultA[MAX_STR * 2],   resultB[MAX_STR * 2], resultC[MAX_STR * 2];
int  states[MAX_STR * 2], cost[MAX_STR * 2];

static inline U_cell_type *U(int ab, int ac, int d, int s) { 
    return getPtr(&myUAllocInfo, ab, ac, d, s); 
};

static inline CPType *CP(int ab, int ac, int d, int s)     { 
    return getPtr(&myCPAllocInfo, ab, ac, d, s); 
};

// doUkkInLimits - for Ukkonen check point between to specIdxfied points in the U matrix
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

static inline int Ukk(int ab,int ac,int d,int state);
int best(int ab, int ac, int d, int wantState);
int calcUkk(int ab, int ac, int d, int toState);
int okIndex(int a, int da, int end);
int whichCharCost(char a, char b, char c);



// Main driver function
int doUkk(seq_p retSeqA, seq_p retSeqB, seq_p retSeqC) {

    CPdummyCell.dist      = 0;
    CPdummyCell.cost      = 0;

    UdummyCell.dist       = 0;
    UdummyCell.computed   = 0;
    UdummyCell.from.ab    = 0;
    UdummyCell.from.ac    = 0;
    UdummyCell.from.cost  = 0;
    UdummyCell.from.state = 0;

    counts.cells     = 0;
    counts.innerLoop = 0;

    finalCost         = 0;
    CPcost            = 0;
    CPwidth           = 0;
    completeFromInfo  = 0;
    
    aIdx    = 0;
    bIdx    = 0; 
    cIdx     = 0;
    si    = 0;
    costi = 0;



    sabG    = 0;
    sacG     = 0;
    sCostG  = 0;
    sStateG = 0;

    int d = -1;
    int finalab, finalac;
    int startDist;


    CPwidth = maxSingleStep;
    // Concern: what is the correct value to use for Umatrix depth.
    // Would think that (maxSingleCost = maxSingleStep * 2) would be enough
    // but doesn't seem to be.  No idea why. *shrug*
    myUAllocInfo  = allocInit (sizeof(U_cell_type), maxSingleCost);
    myCPAllocInfo = allocInit (sizeof(CPType), CPwidth);

    

    // Calculate starting position
    d = 0;
    while (d < aLen && (aStr[d] == bStr[d] && aStr[d] == cStr[d])) {
        d++;
        counts.innerLoop++;
    }
    U(0, 0, 0, 0)->dist = d;
    U(0, 0, 0, 0)->computed = 0 + costOffset;
    startDist = d;


    finalab = aLen - bLen;
    finalac = aLen - cLen;
    endA    = aLen;
    endB    = bLen;
    endC    = cLen;

    CPonDist = 1;
    CPcost = INFINITY;
    do {
        d++;
        Ukk(finalab, finalac, d, 0);

        if (DEBUG_3D) {
            fprintf(stderr, "Furthest reached for cost %d is %d.\n",
                    d, furthestReached);
        }

        if (CPonDist && furthestReached >= aLen / 2) {
            CPcost = d + 1;
            CPonDist = 0;

            if (DEBUG_3D) {
                  fprintf(stderr, "Setting CPcost: %d\n", CPcost);
            }
        }
    
    
    } while (best(finalab, finalac, d, 0) < aLen);

    assert(best(finalab, finalac, d, 0) == aLen);

    CPonDist  = 0;
    long finalCost = d;
  
    
    // Recurse for alignment
    int fState = best(finalab, finalac, finalCost, 1);
    int dist;

    if ( U(finalab, finalac, finalCost, fState)->from.cost <= 0) {
        // We check pointed too late on this first pass.
        // So we got no useful information.  Oh well, have to do it all over again
        assert( U(finalab, finalac, finalCost, fState)->computed == finalCost + costOffset);
        dist = doUkkInLimits(0, 0, 0, 0, startDist, finalab, finalac, finalCost,
                             fState, aLen);
    } else {
        // Use the 'from' info and do the two sub parts.
        dist = getSplitRecurse(0, 0, 0, 0, startDist, finalab, finalac, finalCost,
                               fState, aLen);
    }
    
    assert(dist == aLen);
    printTraceBack(retSeqA, retSeqB, retSeqC);
    
    allocFinal(&myUAllocInfo,  (&UdummyCell.computed), (&UdummyCell));
    allocFinal(&myCPAllocInfo, (&CPdummyCell.cost),    (&CPdummyCell));

    return d;
}

// int progDesc(char *);   // Description of program unique to each prog

void copySequence (seq_p s, char *str) {
    int len, i;
    SEQT *begin;
    len   = seq_get_len (s);
    begin = seq_get_begin(s);
    
    for (i = 1; i < len; i++) {
        if (begin[i] & 1) {
            str[i - 1] = 'A';
        } else if (begin[i] & 2) {
            str[i - 1] = 'C';
        } else if (begin[i] & 4) {
            str[i - 1] = 'G';
        } else if (begin[i] & 8) {
            str[i - 1] = 'T';
        } else {
            printf ("This is impossible!");
            fflush(stdout);
            exit(1);
        }
    }
    str[len - 1] = 0;
    return;
}

int powell_3D_align (seq_p seqA,    seq_p seqB,    seq_p seqC, 
                     seq_p retSeqA, seq_p retSeqB, seq_p retSeqC, 
                     int mismatchCost, int gapOpenCost, int gapExtendCost) {
    printf("powell_3D_align\n");


    int deleteOpenCost = gapOpenCost;
    int deleteExtendCost   = gapExtendCost;
    /* Seq_custom_val(seqA,sa);
    Seq_custom_val(seqB,sb);
    Seq_custom_val(seqC,sc);
    Seq_custom_val(retSeqA,retSeqA);
    Seq_custom_val(retSeqB,retSeqB);
    Seq_custom_val(retSeqC,retSeqC);
    */
    assert (mismatchCost != 0 && gapOpenCost >= 0 && gapExtendCost > 0);


    copySequence (seqA, aStr);
    copySequence (seqB, bStr);
    copySequence (seqC, cStr);

    aLen = seq_get_len (seqA);
    bLen = seq_get_len (seqB);
    cLen = seq_get_len (seqC);

    setup(gapOpenCost, gapExtendCost, mismatchCost, deleteOpenCost, deleteExtendCost);

    return doUkk (retSeqA, retSeqB, retSeqC);
}

/*
value
powell_3D_align_bc (value *argv, int argn) {
    return (powell_3D_align (argv[0], argv[1], argv[2], argv[3], argv[4], 
            argv[5], argv[6], argv[7], argv[8]));
}
*/

/* ---------------------------------------------------------------------- */

int whichCharCost(char a, char b, char c)
{
    assert(a!=0 && b!=0 && c!=0); 
    /* 
      When running as a ukk algorithm (ie. not the DPA), then
      a=b=c only when there is a run of matches after a state other that MMM,
      and since we are moving to a MMM state, this cost will NEVER be used,
      so it doesn't matter what we return   
      When running as the DPA, it can occur at a=b=c, return 0 in this case
    */
    if (a==b && a==c) {
      return 0; 
    }
    /* return 1 for the following
         x-- -x- --x
         xx- x-x -xx
         xxy xyx yxx
    
       return 2 for the following
         xy- x-y -xy
         xyz
    */  
    // Take care of any 2 the same
    if (a==b || a==c || b==c) {
        return 1;
    }
    return 2;
}


int okIndex(int a, int da, int end) {
    if (a < 0)           return 0;
    if (da  && a <  end)   return 1;
    if (!da && a <= end) return 1;
    return 0;
    //  return (a<0 ? 0 : (da==0 ? 1 : a<end));
}




/* ---------------------------------------------------------------------- */
/* Common setup routines */

int stateTransitionCost(int from, int to) {
    return transCost[from][to];
}

// --------------------------------------------------
void step(int n, int *a, int *b, int *c) {
    assert(n>0 && n<=7);
    *a=(n>>0)&1;
    *b=(n>>1)&1;
    *c=(n>>2)&1;
}

int neighbourNum(int i, int j, int k) {
    return (i*1)+(j*2)+(k*4);
}

// --------------------------------------------------

void transitions(int s, Trans st[3]) {
    st[0] = (s/1)%3;
    st[1] = (s/3)%3;
    st[2] = (s/9)%3;
}

char *state2str(int s)  {
    static char str[4];
    Trans st[3];
    int i;
    transitions(stateNum[s], st);
    for (i = 0;i < 3;i++) {
        str[i] = (st[i]==match ? 'M' : (st[i]==del ? 'D' : 'I'));
    }
    return str;
}

int countTrans(Trans st[3], Trans t) {
    int i, n = 0;
    for (i = 0; i < 3; i++) {
        if (st[i] == t) n++;
    }
    return n;
}

void setup(int gapOpenCost, int gapExtendCost, int mismatchCost, int deleteOpenCost, int deleteExtendCost) {
    maxSingleStep = numStates = 0;
    int i, j;
    for (i = 0; i < MAX_STATES - 1; i++) {
        neighbours[i] = 0;
        contCost[i]   = 0;
        secondCost[i] = 0;
        stateNum[i]   = 0;
        for (j = 0; j < MAX_STATES - 1; j++) {
            transCost[i][j] = 0;
        }
    }
    int s, ns = 0;

    assert(gapOpenCost == deleteOpenCost && "Need to rewrite setup routine");
    assert(gapExtendCost == deleteExtendCost && "Need to rewrite setup routine");

    for (s = 0; s < MAX_STATES; s++) {
        Trans st[3];
        transitions(s,st);

        if (countTrans(st, match) == 0) {
          continue;     // Must be at least one match
        }

        if (countTrans(st, ins) > 1) {
          continue;     // Can't be more than 1 insert state!  (7/7/1998)
        }

        #ifdef LIMIT_TO_GOTOH
            // Gotoh86 only allowed states that had a least 2 match states. (Total of 7 possible)
            if (countTrans(st, ins) + countTrans(st, del) > 1) {
              continue;
            }
        #endif

        stateNum[ns] = s;

        // Setup possible neighbours for states (neighbours[])
        int numInserts = countTrans(st, ins);
        if (numInserts==0) {
            neighbours[ns] = neighbourNum(st[0]==match ? 1 : 0,
                                          st[1]==match ? 1 : 0,
                                          st[2]==match ? 1 : 0);
        } else { // (numInserts==1)
          neighbours[ns] = neighbourNum(st[0]==ins ? 1 : 0,
                                        st[1]==ins ? 1 : 0,
                                        st[2]==ins ? 1 : 0);
        }
        // End setting up neighbours


        // Setup cost for continuing a state (contCost[])
        int cost, cont2;
        if (countTrans(st, ins) > 0) {
            cost  = gapExtendCost;    /* Can only continue 1 insert at a time */
            cont2 = 0;
        } else if (countTrans(st, match) == 3) {
            cost  = mismatchCost;        /* All match states */
            cont2 = 1;
        } else if (countTrans(st, del) == 1) {
            cost  = deleteExtendCost;    /* Continuing a delete */
            cont2 = 1;
        } else {
            cost  = 2 * deleteExtendCost;    /* Continuing 2 deletes */
            cont2 = 0;
        }
        contCost[ns]   = cost;
        secondCost[ns] = cont2;
        // End setup of contCost[]

        ns++;
    }

    numStates = ns;

    // Setup state transition costs (transCost[][])
    int s1, s2;
    int maxCost = 0;
    
    assert(gapOpenCost==deleteOpenCost && "Need to rewrite setup routine");
    for (s1 = 0; s1 < numStates; s1++) {
        for (s2 = 0; s2 < numStates; s2++) {
            Trans from[3], to[3];
            int cost = 0, i;
            transitions(stateNum[s1], from);
            transitions(stateNum[s2], to);
    
            for (i = 0; i < 3; i++) {
                if ((to[i]==ins || to[i]==del) && (to[i] != from[i])){
                    cost += gapOpenCost;
                }
            }
            transCost[s1][s2] = cost;
    
            // Determine biggest single step cost
            int thisCost = cost + contCost[s2];
            Trans st[3];
            transitions(stateNum[s2],st);
            thisCost += mismatchCost*(countTrans(st,match)-1);
            maxCost = (maxCost<thisCost ? thisCost : maxCost);
    
        }
    }
    
    maxSingleStep = maxCost;
    // End setup of transition costs
}




/* ---------------------------------------------------------------------- */
/* Some alignment checking routines */
void checkAlign(char *al, int alLen, char *str, int strLen)  {
    int i,j=0;
    for (i=0; i<alLen; i++) {
        if (al[i] == '-') continue;
        assert(al[i]==str[j] && "Output alignment not equal to input string");
        j++;
    }
    assert(j==strLen && "Output alignment not equal length to input string");
}

void revIntArray(int *arr, int start, int end) {
    int i;
    if (end<=start) {
        return;
    }
    for (i=start; i<(end+start)/2; i++) {
        int t = arr[i];
        arr[i] = arr[end-i+start-1];
        arr[end-i+start-1] = t;
    }
}

void revCharArray(char *arr, int start, int end) {
    int i;
    if (end<=start) {
        return;
    }
    for (i=start; i<(end+start)/2; i++) {
        char t = arr[i];
        arr[i] = arr[end-i+start-1];
        arr[end-i+start-1] = t;
    }
}

int alignmentCost(int states[], char *al1, char *al2, char *al3, int len) {
    int i;
    int cost=0;
    Trans last_st[3] = {match, match, match};

    assert(gapOpenCost == deleteOpenCost);

    for (i=0; i<len; i++) {
        int s;
        Trans st[3];
        transitions(stateNum[states[i]], st);

    //    if (i>0) fprintf(stderr,"%-2d  ",cost);
        
        // Pay for begining a gap.
        for (s = 0; s < 3; s++) {
            if (st[s] != match && st[s] != last_st[s]) {
                cost += gapOpenCost;
            }
        }

        for (s = 0; s < 3; s++) {
          last_st[s] = st[s];
        }
        
        // Pay for continuing an insert
        if (countTrans(st, ins)>0) {
            assert(countTrans(st,ins) == 1);
            cost += gapExtendCost;
            continue;
        }

        // Pay for continuing deletes
        cost += deleteExtendCost * countTrans(st, del);

        // Pay for mismatches
        char ch[3];
        int cIdx = 0;
        if (st[0] == match) { 
            assert(al1[i] != '-'); 
            ch[cIdx++] = al1[i]; 
        }
        if (st[1] == match) { 
            assert(al2[i] != '-'); 
            ch[cIdx++] = al2[i]; 
        }
        if (st[2] == match) { 
            assert(al3[i] != '-'); 
            ch[cIdx++] = al3[i]; 
        }
        cIdx--;
        for (; cIdx > 0; cIdx--) {
            if (ch[cIdx-1] != ch[cIdx]) {
                cost += mismatchCost;
            }
        }
        if (countTrans(st, match)==3 && ch[0]==ch[2] && ch[0]!=ch[1]) {
            cost -= mismatchCost;
        }
        // end pay for mismatches
    }

    printf ("The recomputed cost is %d\n", cost);

    return cost;
}

// Find the furthest distance at ab,ac,d.   wantState selects whether the
// best distance is returned, or the best final state (needed for ukk.alloc traceback)
int best(int ab, int ac, int d, int wantState) {
    int s;
    int best = -INFINITY;
    int bestState = -1;
    for (s = 0; s < numStates; s++) {
        if (U(ab, ac, d, s)->computed == d + costOffset && U(ab, ac, d, s)->dist > best) {
            best = U(ab,ac,d,s)->dist;
            bestState = s;
        }
    }

//  fprintf(stderr,"best(%d,%d,%d,(%d))=%d\n",ab,ac,d,bestState,best);
    if (wantState) {
        return bestState;
    } else {
        return best;
    }
}

int calcUkk(int ab, int ac, int d, int toState) {
    if (DEBUG_CALL_ORDER) {
        printf("--ukk.checkp: calcUkk\n" );
        fflush(stdout);
    }
    int neighbour = neighbours[toState];
    int da, db, dc, ab1, ac1;
  
    fromType from;
    int bestDist;

    from.cost = -1;

    if (DEBUG_CALL_ORDER) {
        indent[indenti]=0;
        fprintf(stderr, "%s CalcUKK(ab = %d, ac = %d, d = %d, toState = %d)\n",
                indent,
                ab, ac, d, toState);
        indent[indenti++] = ' ';
        indent[indenti++] = ' ';
        indent[indenti] = 0;
    }

    assert( U(ab, ac, d, toState)->computed < d + costOffset);
    bestDist = -INFINITY;

    // Initialise CP from info if necessary
    if (d >= CPcost && d < CPcost + CPwidth) {
        from.ab    = ab;
        from.ac    = ac;
        from.cost  = d;
        from.state = toState;
    }

    step(neighbour, &da, &db, &dc);
    ab1 = ab - da + db;
    ac1 = ac - da + dc;
    
    // calculate if its a valid diagonal
    if (ab1 >= -endB && ab1 <= endA && ac1 >= -endC && ac1 <= endA) {
        int fromState;
        
        // Loop over possible state we are moving from
        //   May be possible to limit this?
        for (fromState = 0; fromState < numStates; fromState++) {
            int transCost=stateTransitionCost(fromState,toState);
            int fromCost = -INFINITY;
            int dist = -INFINITY;
            int cost = d-transCost-contCost[toState];
            int a1 = Ukk(ab1,ac1,cost,fromState);
            int a2 = -1;

            if ( okIndex(a1, da, endA) &&
                 okIndex(a1 - ab1, db, endB) &&
                 okIndex(a1 - ac1, dc, endC) &&
                 (whichCharCost(da ? aStr[a1] : '-',
                                db ? bStr[a1-ab1] : '-', 
                                dc ? cStr[a1-ac1] : '-') == 1) ) 
            {
                fromCost = cost;
                dist = a1 + da;
            } else {
                if (!secondCost[toState]) {
                    continue;
                }
                
                a2 = Ukk(ab1, ac1, cost - mismatchCost, fromState);
                
                if (okIndex(a2, da, endA) &&
                    okIndex(a2 - ab1, db, endB) &&
                    okIndex(a2 - ac1, dc, endC)) 
                {
                        fromCost = cost - mismatchCost;
                        dist = a2 + da;
                }
            }
          
            // Check if this is an improvment
            if (bestDist < dist) {
                bestDist = dist;
        
                if (completeFromInfo) {        // Do we need to store complete from information for a base case?
                  from.ab = ab1;
                  from.ac = ac1;
                  from.cost = fromCost;
                  from.state = fromState;
                } else if (d >= CPcost + CPwidth) { // Store from info for CP
                      from = U(ab1,ac1,fromCost,fromState)->from;
                }
            }
        } // End loop over from states 
    } // End if valid neighbour

    // Insure that we have how we can reach for AT MOST cost d
    int dist = Ukk(ab,ac,d-1,toState);
    // Check if this is an improvment
    if (okIndex(dist,0,endA) &&
        okIndex(dist-ab,0,endB) &&
        okIndex(dist-ac,0,endC) &&
        bestDist<dist) 
    {
        bestDist = dist;

        if (completeFromInfo) {        // Do we need to store complete from information for a base case?
            from.ab    = ab;
            from.ac    = ac;
            from.cost  = d - 1;
            from.state = toState;
        } else if (d >= CPcost + CPwidth) { // Store from info for CP
            from = U(ab, ac, d - 1, toState)->from;
        }
    }
    // End Insure that we have how we can reach for AT MOST cost d


    if (toState == 0) {  // Is the toState == MMM
        // May be possible to extend the diagonal along a run of matches.

        /* Note: In the past have used 'extended' to only update this cell if
           we actually extend a diagonal.  This is WRONG.  The reason is that
           if we pick the furthest along and try to extend that only, it may 
           not extend, and thus this cell will not be updated.  Whereas a 
           cell less far along may have been able to extend as further than
           the current cell value.

           Note:  This current method of updating regardless of whether there
           is actually a run of matches, causes some descrepencies between the
           U matrix and the D matrix.
        */
           
        // Get furthest of states for this cost
        int dist = -INFINITY;
        int from_state = -1, s;

        for (s = 0; s < numStates; s++) {
            int thisdist;
            thisdist = (s==0) ? bestDist : Ukk(ab,ac,d,s);
            if (thisdist>dist) {
                dist = thisdist;
                from_state = s;
            }
        }

        // Try to extend to diagonal
        while (okIndex(dist, 1, endA) && 
               okIndex(dist - ab, 1, endB) && 
               okIndex(dist - ac, 1, endC) &&
               (aStr[dist] == bStr[dist - ab] && 
                aStr[dist] == cStr[dist - ac])
              ) {
          dist++;
          counts.innerLoop++;
        }
          
        // Was there an improvement?
        if (dist > bestDist) {
            bestDist = dist;  // Note: toState = MMM
            
            // Update 'from' information if the state we extended from was
            // not the same state we are in (the MMM state).
            if (from_state != 0) {
                if (completeFromInfo) {        // Do we need to store complete 'from' information for a base case?
                    from.ab    = ab;
                    from.ac    = ac;
                    from.cost  = d;
                    from.state = from_state;
                } else if (d >= CPcost + CPwidth) { // Store from info for CP
                    from = U(ab, ac, d, from_state)->from;
                }
          }
        }
    } // End attempt to extend diagonal on a run of matches

    assert( U(ab, ac, d, toState)->computed < d + costOffset);
    U(ab, ac, d, toState)->dist     = bestDist;
    U(ab, ac, d, toState)->computed = d + costOffset;
    U(ab, ac, d, toState)->from     = from;
  
    if (DEBUG_CALL_ORDER) {
        indenti -= 2;
        indent[indenti] = 0;  
        fprintf(stderr, "%sCalcUKK(ab = %d, ac = %d, d = %d, toState = %d) = %d\n",
                indent, ab, ac, d, toState, U(ab, ac, d, toState)->dist);
        fprintf(stderr,"%sFrom: ab = %d, ac = %d, cost = %d, state = %d\n",
                indent,
                U(ab, ac, d, toState)->from.ab,   U(ab, ac, d, toState)->from.ac,
                U(ab, ac, d, toState)->from.cost, U(ab, ac, d, toState)->from.state);
    }

    return U(ab, ac, d, toState)->dist;
}

int doUkkInLimits(int sab, int sac, int sCost, int sState, int sDist,
                  int fab, int fac, int fCost, int fState, int fDist) {

    assert(sCost >= 0 && fCost >= 0);
  
    sabG    = sab; 
    sacG    = sac; 
    sCostG  = sCost; 
    sStateG = sState;
    endA    = fDist; 
    endB    = fDist - fab; 
    endC    = fDist - fac;

    if (DEBUG_3D) {
        fprintf(stderr, "Doing(sab = %d, sac = %d, sCost = %d, sState = %d, sDist = %d,\n", sab, sac, sCost, sState, sDist);
        fprintf(stderr, "      fab = %d, fac = %d, fCost = %d, fState = %d, fDist = %d\n",  fab, fac, fCost, fState, fDist);
        
        int i;
        fprintf(stderr, "Sequence to align at this step:\n");
        for (i = sDist; i < fDist; i++) {
            fprintf(stderr,"%c",aStr[i]); 
            fprintf(stderr,"\n");
        }
        for (i = sDist - sab; i < fDist - fab; i++) {
            fprintf(stderr,"%c",bStr[i]); 
            fprintf(stderr,"\n");
        }
        for (i = sDist - sac; i < fDist - fac; i++) {
            fprintf(stderr,"%c",cStr[i]); 
            fprintf(stderr,"\n");
        }
    }
  
    completeFromInfo = 0;
  
    costOffset += finalCost + 1;
    assert(costOffset > 0 && "Oops, overflow in costOffset");

    U(sab, sac, sCost, sState)->dist     = sDist;
    U(sab, sac, sCost, sState)->computed = sCost+costOffset;

    if (fCost - sCost <= CPwidth) { // Is it the base case?
        int i;
        completeFromInfo = 1;

        if (DEBUG_3D) {
            fprintf(stderr, "Base case.\n");
        }

        // if (0) {
        //     for (i = sCost; i <= fCost; i++)
        //       Ukk(fab,fac,i,0);

        //     assert( U(fab, fac, fCost, State)->dist == fDist );
        // } else {
            int dist;
            i = sCost - 1;
            do {
                i++;
                dist = Ukk(fab, fac, i, fState);
            } while (dist < fDist);

            assert(dist == fDist);
            if (i != fCost) {
                fprintf(stderr, "Dist reached for cost %d (old cost %d)\n",i,fCost);
                fCost = i;
                assert(0);
            }
        // }
          
        
        if (DEBUG_3D) {
            fprintf(stderr,"Tracing back in base case.\n");
        }
        traceBack(sab, sac, sCost, sState,
                  fab, fac, fCost, fState);

        completeFromInfo = 0;
        return best(fab, fac, fCost, 0);
    }


    CPcost = (fCost + sCost - CPwidth + 1) / 2;

    #if 0
        // Do the loop up to the desired cost.  Can't do Ukk(fab,fac,fCost,fState) directly (without
        // the loop) because the Umatrix is written to before it is actually needed.
        // Could be fixed, but this is also fine
        {
        int i;
        for (i=sCost; i<=fCost; i++) {
          Ukk(fab,fac,i,0);
          //      Ukk(fab,fac,i,fState);
        }
        assert(U(fab,fac,fCost,fState)->dist==fDist);
        }
    #else
    {
        int dist, i;
        i = sCost - 1;
        do {
            i++;
            dist = Ukk(fab, fac, i, 0);      // Need this (?) otherwise if fState!=0 we may need larger than expected slice size.
            dist = Ukk(fab, fac, i, fState);
        } while (dist < fDist);
    
        assert(dist == fDist);
        if (i != fCost) {
            fprintf(stderr, "Dist reached for cost %d (old cost %d)\n", i, fCost);
            fCost = i;
            assert(0);
        }
    }
    #endif

    return getSplitRecurse(sab, sac, sCost, sState, sDist,
                           fab, fac, fCost, fState, fDist);
}

int getSplitRecurse(int sab, int sac, int sCost, int sState, int sDist,
                    int fab, int fac, int fCost, int fState, int fDist) {
    // Get 'from' and CP data.  Then recurse
    int finalLen;
    int CPdist;
    fromType f;

    assert(sCost >= 0 && fCost >= 0);
    assert( U(fab, fac, fCost, fState)->computed == fCost + costOffset);
    f = U(fab, fac, fCost, fState)->from;

    assert(f.cost >= 0);

    if (CP(f.ab, f.ac, f.cost, f.state)->cost == 0) {
        CP(f.ab, f.ac, f.cost, f.state)->cost = 1;
    }
  
    assert( CP(f.ab, f.ac, f.cost, f.state)->cost == f.cost + 1);   // Use cost+1 so can tell if not used (cost==0)
    CPdist = CP(f.ab, f.ac, f.cost, f.state)->dist;
    assert(CPdist >= 0);

    if (DEBUG_3D) {
        fprintf(stderr, "CPcost = %d, CPwidth = %d\n", CPcost, CPwidth);
        fprintf(stderr, "From: ab = %d, ac = %d, d = %d, s = %d\n",f.ab, f.ac, f.cost, f.state);
        fprintf(stderr, "CP dist = %d\n", CPdist);
    }


    // Note: Doing second half of alignment first.  Only reason
    // for this is so the alignment is retrieved in exactly reverse order
    // making it easy to print out.
    finalLen = doUkkInLimits(f.ab, f.ac, f.cost, f.state, CPdist,
                             fab, fac, fCost, fState, fDist);
  
    doUkkInLimits(sab, sac, sCost, sState, sDist,
                  f.ab, f.ac, f.cost, f.state, CPdist);
  
    if (DEBUG_3D) {
        fprintf(stderr,"Done.\n");
    }
  
    //  return best(fab,fac,fCost,0);
    return finalLen;
}



static inline void sort(int aval[], int len)
{
    int i, j;
    for (i = 0; i < len; i++) {
        int minI = i, t;
        for (j = i + 1; j < len; j++) {
            if (aval[j] < aval[minI]) {
                minI = j;
            }
        }
        t = aval[i];
        aval[i] = aval[minI];
        aval[minI] = t;
    }
}

static inline int withinMatrix(int ab, int ac, int d) {
    // The new method for checking the boundary condition.  Much tighter ~20%(?)  -- 28/02/1999
    int bc = ac - ab;
    int aval[3];
    int g, h, cheapest;
    
    if (d < 0) {
        return 0;
    }
    
    aval[0] = abs(sabG - ab);
    aval[1] = abs(sacG - ac);
    aval[2] = abs((sacG - sabG) - bc);
  
    // Set g and h to the smallest and second smallest of aval[] respectively
    sort (aval, 3);
    g = aval[0];
    h = aval[1];
  
    if (sStateG == 0) {
        // We know a good boudary check if the start state is MMM
        cheapest = (g==0 ? 0 : gapOpenCost + g * gapExtendCost) + (h==0 ? 0 : gapOpenCost + h * gapExtendCost);
    } else {
        // If start state is something else.  Can't charge for start of gaps unless we
        // do something more clever,
        cheapest = (g==0 ? 0 : g * gapExtendCost) + (h==0 ? 0 : h * gapExtendCost);
    }
  
    if (cheapest + sCostG > d) {
        return 0;
    } else {
        return 1;
    }
}

static inline int Ukk(int ab, int ac, int d, int state) {
  
    if (!withinMatrix(ab, ac, d)) {
        return -INFINITY;
    }
    if ( U(ab, ac, d, state)->computed == d + costOffset) {
        return  U(ab, ac, d, state)->dist;
    }

/*
    fprintf(stderr,"Calculating U(%d,%d,%d,%d)",ab,ac,d,state);
*/
    counts.cells++;
 
    calcUkk(ab, ac, d, state);

    // Store away CP from info in necessary
    if (d >= CPcost && (d < CPcost + CPwidth) ) {
        CP(ab, ac, d, state)->dist = U(ab, ac, d, state)->dist;
        CP(ab, ac, d, state)->cost = d + 1;          // Note adding 1 so cost==0 signifies unused cell
    }

    if ( U(ab, ac, d, state)->dist > furthestReached) {
        furthestReached = U(ab, ac, d, state)->dist;
    }

    return U(ab, ac, d, state)->dist;
}

void traceBack(int sab, int sac, int sCost, int sState, 
               int fab, int fac, int fCost, int fState)
{
    int ab, ac, d, s;
    ab = fab;
    ac = fac;
    d  = fCost;
    s  = fState;
  
    while (ab!=sab || ac!=sac || d!=sCost || s!=sState) {
        int a   = U(ab, ac, d, s)->dist;
        int nab = U(ab, ac, d, s)->from.ab;
        int nac = U(ab, ac, d, s)->from.ac;
        int nd  = U(ab, ac, d, s)->from.cost;
        int ns  = U(ab, ac, d, s)->from.state;

        int b = a - ab, c = a - ac;
        int a1 = U(nab, nac, nd, ns)->dist;
        int b1 = a1 - nab, c1 = a1 - nac;

        assert( U(ab, ac, d, s)->computed == d + costOffset);
        assert( U(nab, nac, nd, ns)->computed == nd + costOffset);

        if (DEBUG_3D) {
            fprintf(stderr, "ab = %3d, ac = %3d, d = %3d, s = %2d, dist = %3d,   nab = %3d, nac = %3d, nd = %3d, ns = %2d, ndist = %3d,\n",
                ab, ac, d, s, a,
                nab, nac, nd, ns, a1);
        } 
        // Run of matches
        while (a>a1 && b>b1 && c>c1) {
            a--; b--; c--;
            resultA[aIdx++] = aStr[a];
            resultB[bIdx++] = bStr[b];
            resultC[cIdx++]   = cStr[c];
            states[si++]    = 0;        /* The match state */
            cost[costi++]   = d;
        }

        // The step for (nab,nac,nd,ns) -> (ab,ac,d,s)
        if (a!=a1 || b!=b1 || c!=c1) {
            if (a > a1) {
                resultA[aIdx++] = aStr[--a]; 
            } else {
                resultA[aIdx++] = '-';
            }
            if (b > b1) {
                resultB[bIdx++] = bStr[--b]; 
            } else {
                resultB[bIdx++] = '-';
            }
            if (c>c1) {
                resultC[cIdx++] = cStr[--c]; 
            }
            else {
                resultC[cIdx++] = '-';
            }
            states[si++]  = s;
            cost[costi++] = d;
        }

        assert(a==a1 && b==b1 && c==c1);

        ab = nab; 
        ac = nac; 
        d = nd; 
        s = ns;
    }

    if (DEBUG_3D) {
        int i;

        fprintf(stderr,"Alignment so far\n");
        for (i = aIdx - 1; i >= 0; i--) {
            fprintf(stderr, "%c", resultA[i]); 
            fprintf(stderr, "\n");
        }
        for (i = bIdx - 1; i >= 0; i--) {
            fprintf(stderr, "%c", resultB[i]); 
            fprintf(stderr, "\n");
        }
        for (i = cIdx - 1; i >= 0; i--) {
            fprintf(stderr, "%c", resultC[i]); 
            fprintf(stderr, "\n");
        }

        // Print state information
        printf("States:\n");
        for (i = si - 1; i >= 0; i--) {
          fprintf(stderr, "%s ", state2str(states[i]));
        }
        fprintf(stderr, "\n");
        printf("Costs:\n");
        // Print cost stuff
        for (i = costi - 1; i >= 0; i--) {
            fprintf(stderr, "%-2d  ", cost[i]); 
            fprintf(stderr, "\n");
        }
    }


    assert(ab==sab);
    assert(ac==sac);
    assert(d==sCost);
    assert(s==sState);
}

int char_to_base (char v) {
    if ('A' == v) return 1;
    else if ('C' == v) return 2;
    else if ('G' == v) return 4;
    else if ('T' == v) return 8;
    else if ('-' == v) return 16;
    else return -1;
}

void printTraceBack(struct seq *ra, struct seq *rb, struct seq *rc)
{
    // Print out the alignment

    // Add the first run of matches to the alignment
    // NB. The first run of matches must be added in reverse order.

    int endRun, i = 0;
    while ( i<aLen && (aStr[i]==bStr[i] && aStr[i]==cStr[i]) ) {
      i++;
    }
    endRun = i;

    for (i = endRun - 1; i >= 0; i--)  {
      resultA[aIdx++]    = aStr[i];
      resultB[bIdx++]    = bStr[i];
      resultC[cIdx++]    = cStr[i];
      states[si++]  = 0;        /* The match state */
      cost[costi++] = 0;
    }
    // end print alignment


    // Reverse the alignments
    revCharArray(resultA,0,aIdx);
    revCharArray(resultB,0,bIdx);
    revCharArray(resultC,0,cIdx);
    revIntArray(states,0,si);
    revIntArray(cost,0,costi);
    // end reverse alignments

    // Print out the alignment
    for (int i = aIdx - 1; i >= 0; i--) {
        seq_prepend (ra, char_to_base (resultA[i]));
        seq_prepend (rb, char_to_base (resultB[i]));
        seq_prepend (rc, char_to_base (resultC[i]));
    }
    seq_prepend (ra, 16);
    seq_prepend (rb, 16);
    seq_prepend (rc, 16);
  
    assert(aIdx==bIdx && aIdx==cIdx && aIdx==si && aIdx==costi);

    checkAlign(resultA, aIdx, aStr, aLen);
    checkAlign(resultB, bIdx, bStr, bLen);
    checkAlign(resultC, cIdx, cStr, cLen);

    assert(alignmentCost(states, resultA, resultB, resultC, aIdx) == finalCost);
}

/* ---------------------------------------------------------------------- */

// End of ukkCommon.c

