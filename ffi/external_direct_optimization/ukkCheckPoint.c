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
// used to store which cost (actually d + costOffset) the cell contains
// instead of simply whether the cell has been computed or not.

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <assert.h>

#define DEBUGTRACE 0

#include "ukkCheckPoint.h"
// #include "ukkCommon.h"

          // A given state can have come from 1 and only 1 neighbour
          // MMM, MMD, MDD, IMM, etc.  all have exactly one neighbour
          // Not possible to have more than 1 'I' state (eg. MII IMI)

typedef struct {
    long cells;
    long innerLoop;
} Counts;

//typedef struct {int dist; long computed;} U_cell_type;
typedef struct {
    int ab,
        ac,
        cost,
        state;
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

//typedef struct {int from_ab, from_ac, from_cost, from_state;} From_type;

// Functions to appear.
AllocInfo_t myUAllocInfo;
AllocInfo_t myCPAllocInfo;

U_cell_type UdummyCell;
CPType      CPdummyCell;

U_cell_type *U(int ab, int ac, int d, int s)
{
    return getPtr(&myUAllocInfo, ab, ac, d, s);
}

CPType *CP(int ab, int ac, int d, int s)
{
    return getPtr(&myCPAllocInfo, ab, ac, d, s);
}


void printTraceBack();



int best( int ab, int ac, int d, int wantState );

int calcUkk( int ab, int ac, int d, int toState, characters_t *inputs );

int okIndex( int a, int da, int end );

int whichCharCost( int a, int b, int c );


Counts counts;

// costOffset---added to the `computed` field of each cell. `costOffset` is
// recursive step of the checlpoint algorithm. It's really a hack so I don't
// have to reinitialize the memory structures.
long costOffset = 1;
long finalCost;

int furthestReached_g = -1;
int CPonDist;   // Flag for whether to use distance of cost as the CP criteria
                // CP on dist is only done for first iteration when then final
                // cost is unknown


// Use these globals cause don't want to pass them around, and they're needed
// by the withinMatrix func. Be nice to have closures :-)
int sabG    = 0,
    sacG    = 0,
    sCostG  = 0,
    sStateG = 0;

int endA,     // Used to define where to end on the three strings in the
    endB,     // checkp recursion. So endA contains the distance the recursion
    endC;     // must finish on + 1.

int completeFromInfo = 0;    // Set to 1 for base cases, so 'from' info that alignment
                             // can be retrieved from is set.

int CPwidth;
int CPcost;


int doUkk( characters_t *inputs
         , characters_t *outputs
         )
{
    int d = -1,
        finalab,
        finalac,
        startDist;

    CPwidth = maxSingleStep_g;
    // Concern: what is the correct value to use for Umatrix depth.
    // Would think that maxSingleCost = maxSingleStep_g * 2 would be enough
    // but doesn't seem to be.  No idea why. *shrug*
    myUAllocInfo  = allocInit(sizeof(U_cell_type), maxSingleCost, inputs);
    myCPAllocInfo = allocInit(sizeof(CPType),      CPwidth,       inputs);

    counts.cells     = 0;
    counts.innerLoop = 0;

    // Calculate starting position
    startDist = 0;
    while (   startDist < inputs->lenSeq1
           && (   inputs->seq1[startDist] == inputs->seq2[startDist]
               && inputs->seq1[startDist] == inputs->seq3[startDist]) ) {
        startDist++;
        counts.innerLoop++;
    }
    U(0, 0, 0, 0)->dist     = startDist;
    U(0, 0, 0, 0)->computed = 0 + costOffset;
    // startDist = startDist;

    finalab = inputs->lenSeq1 - inputs->lenSeq2;
    finalac = inputs->lenSeq1 - inputs->lenSeq3;
    endA    = inputs->lenSeq1;
    endB    = inputs->lenSeq2;
    endC    = inputs->lenSeq3;

    CPonDist = 1;
    CPcost   = INFINITY;
    do {
        d++;
        if (DEBUG_3D)     fprintf(stderr, "About to do cost %d\n", d);
        Ukk(finalab, finalac, d, 0, inputs);

        if (DEBUG_3D) fprintf(stderr, "Furthest reached for cost %d is %d.\n", d, furthestReached_g);

        if (CPonDist && furthestReached_g >= inputs->lenSeq1 / 2) {
            CPcost   = d + 1;
            CPonDist = 0;
            if (DEBUG_3D) fprintf(stderr, "Setting CPcost = %d\n", CPcost);
        }
    } while (best(finalab, finalac, d, 0) < inputs->lenSeq1);

    assert( best(finalab, finalac, d, 0) == inputs->lenSeq1 );

    CPonDist  = 0;
    finalCost = d;

    {
        // Recurse for alignment
        int fState = best(finalab, finalac, finalCost, 1);
        int dist;

        if (U( finalab, finalac, finalCost, fState )->from.cost <= 0) {
            // We check pointed too late on this first pass.
            // So we got no useful information. Oh well, have to do it all over again
            assert( U(finalab, finalac, finalCost, fState)->computed == finalCost + costOffset );

            dist = doUkkInLimits( 0
                                , 0
                                , 0
                                , 0
                                , startDist
                                , finalab
                                , finalac
                                , finalCost
                                , fState
                                , inputs->lenSeq1  // TODO: Remove inputs->lenSeq1
                                , inputs
                                , outputs
                                );
        } else {
            // Use the 'from' info and do the two sub parts.
            dist = getSplitRecurse( 0
                                  , 0
                                  , 0
                                  , 0
                                  , startDist
                                  , finalab
                                  , finalac
                                  , finalCost
                                  , fState
                                  , inputs->lenSeq1  // TODO: Remove inputs->lenSeq1
                                  , inputs
                                  , outputs
                                  );
        }

        assert(dist == inputs->lenSeq1);
        if (DEBUG_3D)    printTraceBack(inputs, outputs);
    }
    if (DEBUG_3D) {
        printf("Final cost = %ld\n", finalCost);

        printf("Number of cells calculated = %ld.  Inner Loop = %ld\n",
                counts.cells, counts.innerLoop);
        printf("DPA(N^3) would calculate %ld (or %ld)\n",
                (inputs->lenSeq1 + 1L) * (inputs->lenSeq2 + 1) * (inputs->lenSeq3 + 1) * numStates_g,
                (inputs->lenSeq1 + 1L) * (inputs->lenSeq2 + 1) * (inputs->lenSeq3 + 1) * (MAX_STATES - 1));

        printf("\nU matrix Alloc info\n");
    }
    allocFinal(&myUAllocInfo, (&UdummyCell.computed), (&UdummyCell));

    if (DEBUG_3D)    printf("\nCP matrix Alloc info\n");

    allocFinal(&myCPAllocInfo, (&CPdummyCell.cost), (&CPdummyCell));

    // Because lengths are never actually changed in output, but it should be clear to outside fns that the values can be used.
    outputs->lenSeq1 = outputs->idxSeq1;
    outputs->lenSeq2 = outputs->idxSeq2;
    outputs->lenSeq3 = outputs->idxSeq3;

    return finalCost;
}


int doUkkInLimits(int sab, int sac, int sCost, int sState, int sDist,
          int fab, int fac, int fCost, int fState, int fDist, characters_t *inputs, characters_t *outputs)
{

    assert( sCost >= 0 && fCost >= 0 );

    sabG    = sab;
    sacG    = sac;
    sCostG  = sCost;
    sStateG = sState;
    endA    = fDist;
    endB    = fDist - fab;
    endC    = fDist - fac;

    if (DEBUG_3D) {
        fprintf(stderr, "\nDoing(sab = %d, sac = %d, sCost = %d, sState = %d, sDist = %d,\n", sab, sac, sCost, sState, sDist);
        fprintf(stderr, "      fab = %d, fac = %d, fCost = %d, fState = %d, fDist = %d\n",  fab, fac, fCost, fState, fDist);

        int i;
        fprintf(stderr, "\n\nSequence to align at this step:\n");
        for (i = sDist; i < fDist; i++)                fprintf(stderr, "%3d ", inputs->seq1[i]); fprintf(stderr, "\n");
        for (i = sDist - sab; i < fDist - fab; i++)    fprintf(stderr, "%3d ", inputs->seq2[i]); fprintf(stderr, "\n");
        for (i = sDist - sac; i < fDist - fac; i++)    fprintf(stderr, "%3d ", inputs->seq3[i]); fprintf(stderr, "\n");
    }

    completeFromInfo = 0;

    costOffset += finalCost + 1;
    assert(costOffset > 0 && "Oops, overflow in costOffset");

    U(sab, sac, sCost, sState)->dist = sDist;
    U(sab, sac, sCost, sState)->computed = sCost + costOffset;

    if (fCost - sCost <= CPwidth) { // Is it the base case?
        int i;
        completeFromInfo = 1;

        if (DEBUG_3D)    fprintf(stderr, "Base case.\n");

  // #if 0
  //     for (i = sCost; i <= fCost; i++)
  //       Ukk(fab, fac, i, 0);

  //     assert(U(fab, fac, fCost, fState)->dist == fDist);
  // #else
      {
          int dist;
          i = sCost - 1;
          do {
            i++;
            dist = Ukk( fab, fac, i, fState, inputs );
          } while (dist < fDist);

          assert( dist == fDist );
          if (i != fCost) {
              fprintf(stderr, "Dist reached for cost %d (old cost %d)\n", i, fCost);
              fCost = i;  // TODO: Why is this here?
              assert(0);
          }
      }
    // #endif


        if (DEBUG_3D)    fprintf(stderr, "\n\nTracing back in base case.\n");

        traceBack( sab
                 , sac
                 , sCost
                 , sState
                 , fab
                 , fac
                 , fCost
                 , fState
                 , inputs
                 , outputs
                 );

        completeFromInfo = 0;
        return best( fab, fac, fCost, 0 );
    } // end (fCost - sCost <= CPwidth)


    CPcost = (fCost + sCost - CPwidth + 1) / 2;

    {
      int dist,
          i = sCost - 1;

      do {
          i++;
          if (DEBUG_3D)    fprintf(stderr, "About to do cost %d\n", i);
          dist = Ukk(fab, fac, i, 0, inputs);      // Need this (?) otherwise if fState != 0 we may need larger than expected slice size.
          dist = Ukk(fab, fac, i, fState, inputs);
      } while (dist < fDist);

      assert(dist == fDist);
      if (i != fCost) {
          if (DEBUG_3D)    fprintf(stderr, "Dist reached for cost %d (old cost %d)\n", i, fCost);
          fCost = i;
          assert(0);
      }
    }

    return getSplitRecurse(sab, sac, sCost, sState, sDist,
                           fab, fac, fCost, fState, fDist, inputs, outputs);
}

int getSplitRecurse( int           sab
                   , int           sac
                   , int           sCost
                   , int           sState
                   , int           sDist
                   , int           fab
                   , int           fac
                   , int           fCost
                   , int           fState
                   , int           fDist
                   , characters_t *inputs
                   , characters_t *outputs
                   )
{
    // Get 'from' and CP data.  Then recurse
    int finalLen;
    int CPdist;
    fromType f;

    assert(sCost >= 0 && fCost >= 0);
    assert(U(fab, fac, fCost, fState)->computed == fCost + costOffset);
    f = U(fab, fac, fCost, fState)->from;

    assert(f.cost >= 0);

    if (CP(f.ab, f.ac, f.cost, f.state)->cost == 0)    CP(f.ab, f.ac, f.cost, f.state)->cost = 1;

    assert(CP(f.ab, f.ac, f.cost, f.state)->cost == f.cost + 1);   // Use cost + 1 so can tell if not used (cost == 0)
    CPdist = CP(f.ab, f.ac, f.cost, f.state)->dist;
    assert(CPdist >= 0);

    if (DEBUG_3D) {
        fprintf(stderr, "CPcost = %d CPwidth = %d\n", CPcost, CPwidth);
        fprintf(stderr, "From: ab = %d ac = %d d = %d s = %d\n", f.ab, f.ac, f.cost, f.state);
        fprintf(stderr, "CP dist = %d\n", CPdist);
    }


    // Note: Doing second half of alignment first.  Only reason
    // for this is so the alignment is retrieved in exactly reverse order
    // making it easy to print out.
    finalLen = doUkkInLimits(f.ab, f.ac, f.cost, f.state, CPdist,
                             fab, fac, fCost, fState, fDist, inputs, outputs);

    doUkkInLimits(sab, sac, sCost, sState, sDist,
                  f.ab, f.ac, f.cost, f.state, CPdist, inputs, outputs);

    if (DEBUG_3D) {
        fprintf(stderr, "Done.\n");
    }

    //  return best(fab, fac, fCost, 0);
    return finalLen;
}

// -- Traceback routines --------------------------------------------------------------
int si    = 0,
    costi = 0;

int state_vector[MAX_STR * 2],
    cost_vector[MAX_STR * 2];


void traceBack( int           sab
              , int           sac
              , int           sCost
              , int           sState
              , int           fab
              , int           fac
              , int           fCost
              , int           fState
              , characters_t *inputs
              , characters_t *outputs
              )
{
    int ab = fab,
        ac = fac,
        d  = fCost,
        s  = fState;

    while (   ab != sab
           || ac != sac
           || d  != sCost
           || s  != sState) {
        int a   = U(ab, ac, d, s)->dist;
        int nab = U(ab, ac, d, s)->from.ab;
        int nac = U(ab, ac, d, s)->from.ac;
        int nd  = U(ab, ac, d, s)->from.cost;
        int ns  = U(ab, ac, d, s)->from.state;

        int b = a - ab,
            c = a - ac;

        int a1 = U(nab, nac, nd, ns)->dist,
            b1 = a1 - nab,
            c1 = a1 - nac;

        assert( U( ab,  ac,  d,  s)->computed ==  d + costOffset );
        assert( U(nab, nac, nd, ns)->computed == nd + costOffset );

        if (DEBUG_3D) {
            fprintf(stderr, " ab = %3d  ac = %3d  d = %3d  s = %2d  dist = %3d\nnab = %3d nac = %3d nd = %3d ns = %2d ndist = %3d\n",
                     ab,  ac,  d,  s, a,
                    nab, nac, nd, ns, a1);
        }

        // Run of matches
        while (   a > a1
               && b > b1
               && c > c1) {

            a--;
            b--;
            c--;
            outputs->seq1[outputs->idxSeq1++] = inputs->seq1[a];
            outputs->seq2[outputs->idxSeq2++] = inputs->seq2[b];
            outputs->seq3[outputs->idxSeq3++] = inputs->seq3[c];

            if (DEBUG_3D) {
                printf("idxSeq1: %d\n", outputs->idxSeq1);
                printf("idxSeq2: %d\n", outputs->idxSeq2);
                printf("idxSeq3: %d\n", outputs->idxSeq3);
                printf( "%d", inputs->seq1[a] );
                printf( "  %d\n", outputs->seq1[outputs->idxSeq1 - 1] );

                printf( "%d", inputs->seq2[b] );
                printf( "  %d\n", outputs->seq2[outputs->idxSeq2 - 1] );

                printf( "%d", inputs->seq3[c] );
                printf( "  %d\n", outputs->seq3[outputs->idxSeq3 - 1] );
            }

            state_vector[si++]   = 0;        /* The match state */
            cost_vector[costi++] = d;
        } // while a > a1, etc.

        // The step for (nab, nac, nd, ns) -> (ab, ac, d, s)
        if (   a != a1
            || b != b1
            || c != c1 ) {

            if (a > a1)   outputs->seq1[outputs->idxSeq1++] = inputs->seq1[--a];
            else          outputs->seq1[outputs->idxSeq1++] = gap_char_g;

            if (b > b1)   outputs->seq2[outputs->idxSeq2++] = inputs->seq2[--b];
            else          outputs->seq2[outputs->idxSeq2++] = gap_char_g;

            if (c > c1)   outputs->seq3[outputs->idxSeq3++] = inputs->seq3[--c];
            else          outputs->seq3[outputs->idxSeq3++] = gap_char_g;

            state_vector[si++]   = s;
            cost_vector[costi++] = d;
        }

        assert(   a == a1
               && b == b1
               && c == c1 );

        ab = nab;
        ac = nac;
        d  = nd;
        s  = ns;
    }  // while ab != sab, etc.

    if (DEBUG_3D) {
        int i;

        fprintf(stderr, "\n\nAlignment so far\n");
        printf("Aidx: %d\n", outputs->idxSeq1);
        for (i = outputs->idxSeq1 - 1; i >= 0; i--)    fprintf( stderr, "%6d", outputs->seq1[i] );
        fprintf(stderr, "\n" );

        printf("Bidx: %d\n", outputs->idxSeq2);
        for (i = outputs->idxSeq2 - 1; i >= 0; i--)    fprintf( stderr, "%6d", outputs->seq2[i] );
        fprintf(stderr, "\n" );

        printf("Cidx: %d\n", outputs->idxSeq3);
        for (i = outputs->idxSeq3 - 1; i >= 0; i--)    fprintf( stderr, "%6d", outputs->seq3[i] );
        fprintf(stderr, "\n\n" );

        // Print state information
        for (i = si - 1; i >= 0; i--)               fprintf( stderr, "%s ", state2str(state_vector[i]) );
        fprintf(stderr, "\n");

        // Print cost stuff
        for (i = costi - 1; i >= 0; i--)             fprintf( stderr, "%-2d  ", cost_vector[i]);
        fprintf( stderr, "\n\n" );
    }

    assert( ab == sab );
    assert( ac == sac );
    assert( d  == sCost );
    assert( s  == sState );
}

void printTraceBack( characters_t *inputs, characters_t *outputs )
{
    printf("Yes, we actually do traceback!");
    // Print out the alignment
    {
        // Add the first run of matches to the alignment
        // NB. The first run of matches must be added in reverse order.

        int endRun,
            i = 0;

        // TODO: Does this mean that A has to be the shortest?
        // printf("index: %d, Alen: %d\n", i, Alen);
        if (DEBUG_3D) {
            printf("Traceback:\n");
            printf( "%u\n", inputs->seq1[0] );
            printf( "%u\n", inputs->seq2[0] );
            printf( "%u\n", inputs->seq3[0] );
        }
        while (   i < inputs->lenSeq1
               && (   inputs->seq1[i] == inputs->seq2[i]
                   && inputs->seq1[i] == inputs->seq3[i])) {
            if (DEBUG_3D)    printf("i (< Alen): %d\n", i);
            i++;
        }
        endRun = i;

        for (i = endRun - 1; i >= 0; i--)  {
            if (DEBUG_3D) {
                printf("Aidx: %6d", outputs->idxSeq1);
                printf("\n");
            }
            outputs->seq1[outputs->idxSeq1++] = inputs->seq1[i];
            outputs->seq2[outputs->idxSeq2++] = inputs->seq2[i];
            outputs->seq3[outputs->idxSeq3++] = inputs->seq3[i];
            state_vector[si++]             = 0;               /* The match state */
            cost_vector[costi++]           = 0;
        }
    }

    {
        // Reverse the alignments
        //revElem_tArray( outputs->seq1, 0, outputs->idxSeq1 );
        //revElem_tArray( outputs->seq2, 0, outputs->idxSeq2 );
        //revElem_tArray( outputs->seq3, 0, outputs->idxSeq3 );

        revIntArray( state_vector,  0, si    );
        revIntArray( cost_vector,   0, costi );
    }
    if (DEBUG_3D) {
        // Print out the alignment
        printf( "ALIGNMENT\n" );

        int print_idx;

        for (print_idx = 0; print_idx < outputs->idxSeq1; print_idx++)   printf( "%6u", outputs->seq1[print_idx] );
        printf( "\n" );

        for (print_idx = 0; print_idx < outputs->idxSeq2; print_idx++)   printf( "%6u", outputs->seq2[print_idx] );
        printf( "\n" );

        for (print_idx = 0; print_idx < outputs->idxSeq3; print_idx++)   printf( "%6u", outputs->seq3[print_idx] );
        printf( "\n\n" );

        // Print state information
        for (print_idx = 0; print_idx < si; print_idx++)    printf( "%s ", state2str(state_vector[print_idx]) );
        printf( "\n" );

        // Print cost stuff
        for (print_idx = 0; print_idx < costi; print_idx++) printf( "%-2d  ", cost_vector[print_idx] );
        printf( "\n" );
    }

    assert( outputs->idxSeq1 == outputs->idxSeq2 );
    assert( outputs->idxSeq1 == outputs->idxSeq3 );
    assert( outputs->idxSeq1 == si );
    assert( outputs->idxSeq1 == costi );

    checkAlign( outputs->seq1, outputs->idxSeq1, inputs->seq1, inputs->lenSeq1 );
    checkAlign( outputs->seq2, outputs->idxSeq2, inputs->seq2, inputs->lenSeq2 );
    checkAlign( outputs->seq3, outputs->idxSeq3, inputs->seq3, inputs->lenSeq3 );

    assert( alignmentCost( state_vector
                         , outputs->seq1
                         , outputs->seq2
                         , outputs->seq3
                         , outputs->idxSeq1 ) == finalCost
          );
}


// Find the furthest distance at ab, ac, d.   wantState selects whether the
// best distance is returned, or the best final state (needed for ukk.alloc traceback)
int best( int ab, int ac, int d, int wantState )
{

    int s;
    int best = -INFINITY;
    int bestState = -1;
    for (s = 0; s < numStates_g; s++) {
        if (      U(ab, ac, d, s)->computed == d + costOffset
               && U(ab, ac, d, s)->dist > best) {
            best = U(ab, ac, d, s)->dist;
            bestState = s;
        }
    }

//  fprintf(stderr, "best(%d, %d, %d, (%d)) = %d\n", ab, ac, d, bestState, best);

    if (wantState)    return bestState;
    else              return best;
}


void sort( int aval[], int len )
{
    int i, j;

    for (i = 0; i < len; i++) {
        int minI = i,
            t;
        for (j = i + 1; j < len; j++) {
            if (aval[j] < aval[minI])    minI = j;
        }
        t          = aval[i];
        aval[i]    = aval[minI];
        aval[minI] = t;
    }
}


int withinMatrix( int ab, int ac, int d )
{
  // The new method for checking the boundary condition.  Much tighter ~20%(?)  -- 28 / 02 / 1999
  int bc = ac - ab;
  int aval[3];
  int g, h, cheapest;

  if (d < 0) return 0;

  aval[0] = abs(sabG - ab);
  aval[1] = abs(sacG - ac);
  aval[2] = abs((sacG - sabG) - bc);

  // Set g and h to the smallest and second smallest of aval[] respectively
  sort(aval, 3);
  g = aval[0];
  h = aval[1];

  if (sStateG == 0) {
    // We know a good boudary check if the start state is MMM
    cheapest = ( g == 0 ? 0
                        : startInsert_g + g * continueInsert_g )
             + ( h == 0 ? 0
                        : startInsert_g + h * continueInsert_g );
  } else {
    // If start state is something else.  Can't charge for start of gaps unless we
    // do something more clever,
    cheapest = (g == 0 ? 0 : g * continueInsert_g) + (h == 0 ? 0 : h * continueInsert_g);
  }

  if (cheapest + sCostG > d)    return 0;
  else                          return 1;
}


int Ukk( int           ab
       , int           ac
       , int           d
       , unsigned int  state
       , characters_t *inputs
       )
{
    if (DEBUG_CALL_ORDER)    printf("Ukk: %d\n", *inputs->seq1);

    if (!withinMatrix(ab, ac, d))                           return -INFINITY;
    if (U(ab, ac, d, state)->computed == d + costOffset)    return U(ab, ac, d, state)->dist;

  /*
    fprintf(stderr, "Calculating U(%d, %d, %d, %d)", ab, ac, d, state);
  */
    counts.cells++;

    calcUkk(ab, ac, d, state, inputs);

    // Store away CP from info in necessary
    if (d >= CPcost && d < CPcost + CPwidth) {
        CP(ab, ac, d, state)->dist = U(ab, ac, d, state)->dist;
        CP(ab, ac, d, state)->cost = d + 1;          // Note adding 1 so cost == 0 signifies unused cell
    }

    if (U(ab, ac, d, state)->dist > furthestReached_g)    furthestReached_g = U(ab, ac, d, state)->dist;

    return U(ab, ac, d, state)->dist;
}


int calcUkk(int ab, int ac, int d, int toState, characters_t *inputs)
{
    int neighbour = neighbours[toState];
    int da, db, dc, ab1, ac1;

    fromType from;
    int bestDist;

    from.cost = -1;

    // for use in DEBUGTRACEing
    int  indenti = 0;
    char indent[1000];


    if (DEBUGTRACE) {
        indent[indenti] = 0;
        fprintf(stderr, "%sCalcUKK(ab = %d, ac = %d, d = %d, toState = %d)\n",
                indent,
                ab, ac, d, toState);

        indent[indenti++] = ' ';
        indent[indenti++] = ' ';
        indent[indenti]   = 0;
    }

    assert( U(ab, ac, d, toState)->computed < d + costOffset );
    bestDist = -INFINITY;

    // Initialise CP from info if necessary
    if (d >= CPcost && d < CPcost + CPwidth) {
        from.ab    = ab;
        from.ac    = ac;
        from.cost  = d;
        from.state = toState;
    }

    exists_neighbor_in_delete_state( neighbour, &da, &db, &dc );
    ab1 = ab - da + db;
    ac1 = ac - da + dc;

    // calculate if its a valid diagonal
    if (      ab1 >= -endB
           && ab1 <= endA
           && ac1 >= -endC
           && ac1 <= endA) {
        int fromState;

        // Loop over possible state we are moving from
        //   May be possible to limit this?
        for (fromState = 0; fromState < numStates_g; fromState++) {

            int transCost = stateTransitionCost(fromState, toState);
            int fromCost  = -INFINITY;
            int dist      = -INFINITY;
            int cost      = d - transCost - contCost[toState];
            int a1        = Ukk( ab1, ac1, cost, fromState, inputs );
            int a2        = -1;

            if (     okIndex(a1,       da, endA)
                  && okIndex(a1 - ab1, db, endB)
                  && okIndex(a1 - ac1, dc, endC)
                  && whichCharCost( da ? inputs->seq1[a1]       : gap_char_g
                                  , db ? inputs->seq2[a1 - ab1] : gap_char_g
                                  , dc ? inputs->seq3[a1 - ac1] : gap_char_g
                                  ) == 1 ) {
                fromCost = cost;
                dist     = a1 + da;

            } else {

                if (!secondCost[toState])    continue;

                a2 = Ukk(ab1, ac1, cost - misCost_g, fromState, inputs);

                if (     okIndex( a2,       da, endA )
                      && okIndex( a2 - ab1, db, endB )
                      && okIndex( a2 - ac1, dc, endC ) ) {
                    fromCost = cost - misCost_g;
                    dist     = a2 + da;
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
                    from = U(ab1, ac1, fromCost, fromState)->from;
                }
            }
        } // End loop over from state_vector
    } // End if valid neighbour

    // Insure that we have how we can reach for AT MOST cost d
    {
        int dist = Ukk(ab, ac, d - 1, toState, inputs);
        // Check if this is an improvment
        if (     okIndex(dist, 0, endA)
              && okIndex(dist - ab, 0, endB)
              && okIndex(dist - ac, 0, endC)
              && bestDist < dist) {
            bestDist = dist;

            if (completeFromInfo) {        // Do we need to store complete from information for a base case?
                from.ab = ab;
                from.ac = ac;
                from.cost = d - 1;
                from.state = toState;
            } else if (d >= CPcost + CPwidth) { // Store from info for CP
                from = U(ab, ac, d - 1, toState)->from;
            }
        }
    }

    if (toState == 0) {  // Is the toState == MMM
    // May be possible to extend the diagonal along a run of matches.

    /* Note: In the past have used 'extended' to only update this cell if
       we actually extend a diagonal.  This is WRONG.  The reason is that
       if we pick the furthest along and try to extend that only, it may
       not extend, and thus this cell will not be updated.  Whereas a
       cell less far along may have been able to extend as further than
       the current cell value.

       Note:  This current method of updating regardless of whether there
       is actually a run of matches, causes some discrepancies between the
       U matrix and the D matrix.
    */

        // Get furthest of state_vector for this cost
        int dist = -INFINITY;
        int from_state = -1, s;

        for (s = 0; s < numStates_g; s++) {
            int thisdist;
            thisdist = (s == 0) ? bestDist : Ukk(ab, ac, d, s, inputs);
            if (thisdist > dist) {
                dist = thisdist;
                from_state = s;
            }
        }

       // Try to extend to diagonal
       while (  okIndex(dist, 1, endA)
              && okIndex(dist - ab, 1, endB)
              && okIndex(dist - ac, 1, endC)
              && (   inputs->seq1[dist] == inputs->seq2[dist - ab]
                  && inputs->seq1[dist] == inputs->seq3[dist - ac])) {
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
                    from.ab = ab;
                    from.ac = ac;
                    from.cost = d;
                    from.state = from_state;
                } else if (d >= CPcost + CPwidth) { // Store from info for CP
                   from = U(ab, ac, d, from_state)->from;
                }
            }
        }
    } // End attempt to extend diagonal on a run of matches

    assert( U(ab, ac, d, toState)->computed < d + costOffset );

    U( ab, ac, d, toState )->dist     = bestDist;
    U( ab, ac, d, toState )->computed = d + costOffset;
    U( ab, ac, d, toState )->from     = from;

    if (DEBUGTRACE) {
        indent[indenti] = 0;
        fprintf(stderr, "%sCalcUKK(ab = %d, ac = %d, d = %d, toState = %d) = %d\n",
                indent,
                ab, ac, d, toState, U(ab, ac, d, toState)->dist);
        fprintf(stderr, "%sFrom: ab = %d ac = %d cost = %d state = %d\n",
                indent,
                U(ab, ac, d, toState)->from.ab, U(ab, ac, d, toState)->from.ac,
                U(ab, ac, d, toState)->from.cost, U(ab, ac, d, toState)->from.state);
    }

    return U(ab, ac, d, toState)->dist;
}
