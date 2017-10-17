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
 *
 * This program calculates the edit cost for optimally aligning three sequences
 * under linear gap costs. It also determines an optimal alignment.
 * A generalisation of Ukkonen's algorithm to three sequence is used.
 * Check-pointing is used to recover the alignment.
 * Average time complexity O(n*log(d) + d^3), space complexity O(d^2).
 * For more details, see
 *
 *  D. R. Powell, L. Allison and T. I. Dix,
 *  "Fast, Optimal Alignment of Three Sequences Using Linear Gap Costs"
 *  Journal of Theoretical Biology, 207:3, pp 325-336.
 *
 *  D. R. Powell, L. Allison and T. I. Dix,
 *  "A Versatile Divide and Conquer Technique for Optimal String Alignment
 *  Information Processing Letters, 1999, 70:3, pp 127-139.
 *
 *  D. R. Powell, "Algorithms for Sequence Alignment",
 *  PhD Thesis, Monash University, 2001, Chapter 4.
 */


// Similar to ukk.alloc.new but uses % trick to use less memory, by
// not retrieving the alignment. Note also, the 'computed' field is
// used to store which cost (actually d + costOffset) the cell contains
// instead of simply whether the cell has been computed or not.

#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "debug_constants.h"
#include "seq.h"
#include "ukkCheckp.h"
// #include "ukkCommon.h"

#define MAXINT INT_MAX
// #define FIXED_NUM_PLANES TODO: this is also defined in ukkCommon.h; it was commented out, but then allocInit() failed to compile

AllocInfo myUAllocInfo;
AllocInfo myCPAllocInfo;

// these three are also extern in ukkCommon.h
extern int mismatchCost;
extern int gapOpenCost;
extern int gapExtendCost;

// globals--it'd be nice to clean these up a little


// costOffset - added to the 'computed' field of each cell.  'costOffset' is
// recursive step of the checlpoint algorithm.  'Tis really a hack so I don't
// have to reinitialize the memory structures.
long costOffset = 1;
long finalCost;

int furthestReached = -1;
int CPonDist;   // Flag for whether to use distance of cost as the CP criteria
                // CP on dist is only done for first iteration when then final
                // cost is unknown


// Use these globals cause don't want to pass 'em around, and they're needed
// by the withinMatrix func.  Be nice to have closures :-)
int sabG = 0, sacG = 0, sCostG = 0, sStateG = 0;

int endA, endB, endC;   // Used to define where to end on the three strings in the
                        // checkp recursion.  So endA contains the distance the recursion
                        // must finish on + 1.

int completeFromInfo = 0; // Set to 1 for base cases, so 'from' info that alignment
                          //  can be retrieved from is set.

int CPwidth;
int CPcost;

Counts counts;

int  aSeqIdx = 0, bSeqIdx = 0, cSeqIdx = 0, stateIdx = 0, costIdx = 0;

char resultA[MAX_STR * 2],   resultB[MAX_STR * 2], resultC[MAX_STR * 2];
int  states[MAX_STR * 2],    cost[MAX_STR * 2];

U_cell_type *U(int ab, int ac, int d, int s) {
    return getPtr(&myUAllocInfo, ab, ac, d, s);
}

CPType *CP(int ab, int ac, int d, int s)     {
    return getPtr(&myCPAllocInfo, ab, ac, d, s);
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
        fprintf(stderr, "Doing (sab = %2d, sac = %2d, sCost = %2d, sState = %2d, sDist = %2d\n", sab, sac, sCost, sState, sDist);
        fprintf(stderr, "       fab = %2d, fac = %2d, fCost = %2d, fState = %2d, fDist = %2d\n",  fab, fac, fCost, fState, fDist);

        int i;
        fprintf(stderr, "Sequence to align at this step:\n");
        for (i = sDist; i < fDist; i++) {
            fprintf(stderr, "%c", aStr[i]);
            fprintf(stderr, "\n");
        }
        for (i = sDist - sab; i < fDist - fab; i++) {
            fprintf(stderr, "%c", bStr[i]);
            fprintf(stderr, "\n");
        }
        for (i = sDist - sac; i < fDist - fac; i++) {
            fprintf(stderr, "%c", cStr[i]);
            fprintf(stderr, "\n");
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

        #if 0
            for (i=sCost; i<=fCost; i++)
              Ukk(fab,fac,i,0);

            assert(U(fab,fac,fCost,fState)->dist==fDist);
        #else
        {
            int dist;
            i = sCost - 1;
            do {
                i++;
                dist = Ukk(fab, fac, i, fState);
            } while (dist < fDist);

            assert(dist == fDist);
            if (i != fCost) {
                fprintf(stderr, "Dist reached for cost %2d (old cost %2d)\n",i,fCost);
                fCost = i;
                assert(0);
            }
        }
        #endif


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
            fprintf(stderr, "Dist reached for cost %2d (old cost %2d)\n", i, fCost);
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
        fprintf(stderr, "CPcost = %2d CPwidth = %2d\n", CPcost, CPwidth);
        fprintf(stderr, "From: ab = %2d ac = %2d d = %2d s = %2d\n", f.ab, f.ac, f.cost, f.state);
        fprintf(stderr, "CP dist = %2d\n", CPdist);
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

// -- Traceback routines --------------------------------------------------------------
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
            fprintf(stderr, "ab = %3d, ac = %3d, d = %3d, s = %2d, dist = %3d, \n nab = %3d, nac = %3d, nd = %3d, ns = %2d, ndist = %3d\n",
                    ab, ac, d, s, a,
                    nab, nac, nd, ns, a1);
        }

        // Run of matches
        while (a>a1 && b>b1 && c>c1) {
          a--; b--; c--;
          resultA[aSeqIdx++] = aStr[a];
          resultB[bSeqIdx++] = bStr[b];
          resultC[cSeqIdx++] = cStr[c];
          states[stateIdx++] = 0;        /* The match state */
          cost[costIdx++] = d;
        }

        // The step for (nab,nac,nd,ns) -> (ab,ac,d,s)
        if (a!=a1 || b!=b1 || c!=c1) {
            if (a > a1) {
                resultA[aSeqIdx++] = aStr[--a];
            } else {
                resultA[aSeqIdx++] = '-';
            }
            if (b > b1) {
                resultB[bSeqIdx++] = bStr[--b];
            } else {
                resultB[bSeqIdx++] = '-';
            }
            if (c>c1) {
                resultC[cSeqIdx++] = cStr[--c];
            }
            else {
                resultC[cSeqIdx++] = '-';
            }
            states[stateIdx++]  = s;
            cost[costIdx++] = d;
        }

        assert(a==a1 && b==b1 && c==c1);

        ab = nab;
        ac = nac;
        d = nd;
        s = ns;
    }

    if (DEBUG_3D) {
        {
            int i;

            fprintf(stderr,"Alignment so far\n");
            for (i = aSeqIdx - 1; i >= 0; i--) {
                fprintf(stderr, "%c",resultA[i]);
                fprintf(stderr, "\n");
            }
            for (i = bSeqIdx - 1; i >= 0; i--) {
                fprintf(stderr, "%c",resultB[i]);
                fprintf(stderr, "\n");
            }
            for (i = cSeqIdx - 1; i >= 0; i--) {
                fprintf(stderr, "%c",resultC[i]);
                fprintf(stderr, "\n");
            }
            // Print state information
            for (i = stateIdx - 1; i >= 0; i--) {
              fprintf(stderr,"%s ",state2str(states[i]));
            }
            fprintf(stderr,"\n");
            // Print cost stuff
            for (i = costIdx - 1; i >= 0; i--) {
                fprintf(stderr,"%-2d  ",cost[i]);
                fprintf(stderr,"\n");
            }
        }
    }

    assert(ab == sab);
    assert(ac == sac);
    assert(d  == sCost);
    assert(s  == sState);
}

int char_to_base (char v) {
    if      ('A' == v) return 1;
    else if ('C' == v) return 2;
    else if ('G' == v) return 4;
    else if ('T' == v) return 8;
    else if ('-' == v) return 16;
    else return -1;
}

void printTraceBack(seq_p retSeqA, seq_p retSeqB, seq_p retSeqC) {
    // Print out the alignment

    // Add the first run of matches to the alignment
    // NB. The first run of matches must be added in reverse order.

    int endRun, i = 0;
    while ( i<aLen && (aStr[i]==bStr[i] && aStr[i]==cStr[i]) ) {
      i++;
    }
    endRun = i;

    for (i = endRun - 1; i >= 0; i--)  {
      resultA[aSeqIdx++]    = aStr[i];
      resultB[bSeqIdx++]    = bStr[i];
      resultC[cSeqIdx++]    = cStr[i];
      states[stateIdx++]  = 0;        /* The match state */
      cost[costIdx++] = 0;
    }
    // end print alignment


    // Reverse the alignments
    revCharArray(resultA, 0, aSeqIdx);
    revCharArray(resultB, 0, bSeqIdx);
    revCharArray(resultC, 0, cSeqIdx);
    revIntArray(states,   0, stateIdx);
    revIntArray(cost,     0, costIdx);
    // end reverse alignments

    // Print out the alignment
    for (int i = aSeqIdx - 1; i >= 0; i--) {
      seq_prepend (retSeqA, char_to_base (resultA[i]));
      seq_prepend (retSeqB, char_to_base (resultB[i]));
      seq_prepend (retSeqC, char_to_base (resultC[i]));
    }
    seq_prepend (retSeqA, 16);
    seq_prepend (retSeqB, 16);
    seq_prepend (retSeqC, 16);

    assert(aSeqIdx==bSeqIdx && aSeqIdx==cSeqIdx && aSeqIdx==stateIdx && aSeqIdx==costIdx);

    checkAlign(resultA, aSeqIdx, aStr, aLen);
    checkAlign(resultB, bSeqIdx, bStr, bLen);
    checkAlign(resultC, cSeqIdx, cStr, cLen);

    assert(alignmentCost(states, resultA, resultB, resultC, aSeqIdx) == finalCost);
}

// Find the furthest distance at ab, ac, d. wantState selects whether the
// best distance is returned, or the best final state (needed for ukk.alloc traceback)
int best(int ab, int ac, int d, int wantState) {

    int s;
    int best = -INFINITY;
    int bestState = -1;
    for (s = 0; s < numStates; s++) {
    if ( (U(ab, ac, d, s)->computed == d + costOffset) && (U(ab, ac, d, s)->dist > best) ) {
      best = U(ab, ac, d, s)->dist;
      bestState = s;
    }
    }

//  fprintf(stderr,"best(%2d,%2d,%2d,(%2d))=%2d\n",ab,ac,d,bestState,best);

    if (wantState) {
        return bestState;
    } else {
        return best;
    }
}

static inline void sort(int aval[], int len) {
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

 /*
    printf("\nwithinMatrix\n");
    printf("lessMidd_idx_diff %2d\n", lessMidd_idx_diff);
    printf("lessLong_idx_diff %2d\n", lessLong_idx_diff);
*/
    int aval[3];


    // The new method for checking the boundary condition.  Much tighter ~20%(?)  -- 28/02/1999
    int bc = ac - ab;
    int g, h, cheapest;

    if (d < 0) {
        return 0;
    }

    aval[0] = abs(sabG - ab);
    aval[1] = abs(sacG - ac);
    aval[2] = abs((sacG - sabG) - bc);

    // Set g and h to the smallest and second smallest of aval[] respectively
    sort(aval, 3);
    g = aval[0];
    h = aval[1];

    if (sStateG == 0) {
        // We know a good boudary check if the start state is MMM
        cheapest = (g==0 ? 0 : gapOpenCost + g * gapExtendCost) + (h==0 ? 0 : gapOpenCost + h * gapExtendCost);
    } else {
        // If start state is something else.  Can't charge for start of gaps unless we
        // do something more clever,
        cheapest = (g==0 ? 0 : g*gapExtendCost) + (h==0 ? 0 : h * gapExtendCost);
    }

    if (cheapest + sCostG > d) {
        return 0;
    } else {
        return 1;
    }
}

int Ukk(int ab,int ac,int d,int state) {

    if (!withinMatrix(ab, ac, d)) {
        return -INFINITY;
    }
    if ( U(ab, ac, d, state)->computed == d + costOffset) {
        return  U(ab, ac, d, state)->dist;
    }

/*
    fprintf(stderr,"Calculating U(%2d,%2d,%2d,%2d)",ab,ac,d,state);
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

// TODO: these two for debug call order. Find a way to delete them
    int indenti = 0;
    char indent[1000];


int doUkk(seq_p retSeqA, seq_p retSeqB, seq_p retSeqC) {
    CPdummyCell.dist      = 0;
    CPdummyCell.cost      = 0;
    UdummyCell.dist       = 0;
    UdummyCell.computed   = 0;
    UdummyCell.from.ab    = 0;
    UdummyCell.from.ac    = 0;
    UdummyCell.from.cost  = 0;
    UdummyCell.from.state = 0;
    finalCost             = 0;
    CPcost                = 0;
    CPwidth               = 0;
    completeFromInfo      = 0;

    aSeqIdx  = 0;
    bSeqIdx  = 0;
    cSeqIdx  = 0;
    stateIdx = 0;
    costIdx  = 0;

    costOffset      = 1;
    furthestReached = -1;

    sabG    = 0;
    sacG    = 0;
    sCostG  = 0;
    sStateG = 0;

    int curDist = -1;
    int finalab, finalac;
    int startDist;

    CPwidth = maxSingleStep;
    // Concern: what is the correct value to use for Umatrix depth.
    // Would think that maxSingleCost=maxSingleStep*2 would be enough
    // but doesn't seem to be.  No idea why. *shrug*
    myUAllocInfo  = allocInit(sizeof(U_cell_type), maxSingleCost);
    myCPAllocInfo = allocInit(sizeof(CPType), CPwidth);

    counts.cells     = 0;
    counts.innerLoop = 0;

    // Calculate starting position
    curDist = 0;
    while (curDist < aLen && (aStr[curDist] == bStr[curDist] && aStr[curDist] == cStr[curDist])) {
        curDist++;
        counts.innerLoop++;
    }
    U(0, 0, 0, 0)->dist = curDist;
    U(0, 0, 0, 0)->computed = 0 + costOffset;
    startDist = curDist;


    finalab = aLen - bLen;
    finalac = aLen - cLen;
    endA    = aLen;
    endB    = bLen;
    endC    = cLen;

    CPonDist = 1;
    CPcost = INFINITY;
    do {
        curDist++;
        Ukk(finalab, finalac, curDist, 0);

        if (DEBUG_3D) {
            fprintf(stderr, "Furthest reached for cost %2d is %2d.\n",
                    curDist, furthestReached);
        }

        if (CPonDist && furthestReached >= aLen / 2) {
            CPcost   = curDist + 1;
            CPonDist = 0;

            if (DEBUG_3D) {
                fprintf(stderr, "Setting CPcost: %2d\n", CPcost);
            }
        }


    } while (best(finalab, finalac, curDist, 0) < aLen);

    assert(best(finalab, finalac, curDist, 0) == aLen);

    CPonDist  = 0;
    finalCost = curDist;


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

    printf("doUkk: dist: = %2d\n", curDist);
    return curDist;
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
        indent[indenti] = 0;
        fprintf(stderr, "%s CalcUKK(ab = %2d, ac = %2d, d = %2d, toState = %2d)\n",
                indent, ab, ac, d, toState);
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
            int transCost = stateTransitionCost(fromState,toState);
            int fromCost  = -INFINITY;
            int dist      = -INFINITY;
            int cost      = d-transCost-contCost[toState];
            int a1        = Ukk(ab1,ac1,cost,fromState);
            int a2        = -1;

            if ( okIndex(a1, da, endA) &&
                 okIndex(a1 - ab1, db, endB) &&
                 okIndex(a1 - ac1, dc, endC) &&
                 (whichCharCost(da ? aStr[a1] : '-',
                                db ? bStr[a1-ab1] : '-',
                                dc ? cStr[a1-ac1] : '-') == 1)
                 ) {
                fromCost = cost;
                dist = a1 + da;
            } else {
                if (!secondCost[toState]) {
                    continue;
                }

                a2 = Ukk(ab1, ac1, cost - mismatchCost, fromState);

                if (okIndex(a2, da, endA) &&
                    okIndex(a2 - ab1, db, endB) &&
                    okIndex(a2 - ac1, dc, endC)) {
                        fromCost = cost - mismatchCost;
                        dist = a2 + da;
                }
            }

            // Check if this is an improvment
            if (bestDist < dist) {
                bestDist = dist;

                if (completeFromInfo) {        // Do we need to store complete from information for a base case?
                    from.ab    = ab1;
                    from.ac    = ac1;
                    from.cost  = fromCost;
                    from.state = fromState;
                } else if (d >= CPcost + CPwidth) { // Store from info for CP
                      from = U(ab1, ac1, fromCost, fromState)->from;
                }
            }
        } // End loop over from states
    } // End if valid neighbour

    // Insure that we have how we can reach for AT MOST cost d

    int dist = Ukk(ab, ac, d - 1, toState);
    // Check if this is an improvment
    if (okIndex(dist,      0, endA) &&
        okIndex(dist - ab, 0, endB) &&
        okIndex(dist - ac, 0, endC) &&
        bestDist < dist)
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
    } // end insure that we have how we can reach for AT MOST cost d

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
            thisdist = (s == 0) ? bestDist : Ukk(ab,ac,d,s);
            if (thisdist > dist) {
                dist       = thisdist;
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
            bestDist = dist;  // Note: toState=MMM

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
        indenti        -= 2;
        indent[indenti] = 0;
        fprintf(stderr, "%sCalcUKK(ab = %2d, ac = %2d, d = %2d,    toState = %2d) = %2d\n",
                indent, ab, ac, d, toState, U(ab, ac, d, toState)->dist);
        fprintf(stderr, "%sFrom:   ab = %2d, ac = %2d, cost = %2d, state = %2d\n",
                indent,
                U(ab, ac, d, toState)->from.ab,   U(ab, ac, d, toState)->from.ac,
                U(ab, ac, d, toState)->from.cost, U(ab, ac, d, toState)->from.state);
    }

    return U(ab, ac, d, toState)->dist;
}

