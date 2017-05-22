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
 * This program calculates the edit cost for optimally aligning three characters
 * under linear gap costs. It also determines an optimal alignment.
 * A generalisation of Ukkonen's algorithm to three character is used.
 * Check-pointing is used to recover the alignment.
 * Average time complexity O(n*log(d) + d^3), space complexity O(d^2).
 * For more details, see
 *
 *  D. R. Powell, L. Allison and T. I. Dix,
 *  "Fast, Optimal Alignment of Three Characters Using Linear Gap Costs"
 *  Journal of Theoretical Biology, 207:3, pp 325-336.
 *
 *  D. R. Powell, L. Allison and T. I. Dix,
 *  "A Versatile Divide and Conquer Technique for Optimal String Alignment
 *  Information Processing Letters, 1999, 70:3, pp 127-139.
 *
 *  D. R. Powell, "Algorithms for Character Alignment",
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
#include "dyn_character.h"
#include "ukkCheckPoint.h"
//#include "ukkCommon.h"

#define MAXINT INT_MAX
// #define FIXED_NUM_PLANES TODO: this is also defined in ukkCommon.h; it was commented out, but then allocInit() failed to compile

alloc_info_t myUkk_allocInfo;
alloc_info_t myCheckPt_allocInfo;

// these three are also extern in ukkCommon.h
extern unsigned int mismatchCost;
extern unsigned int gapOpenCost;
extern unsigned int gapExtendCost;


// TODO: globals--it'd be nice to clean these up a little


// costOffset - added to the 'computed' field of each cell. 'costOffset' is
// recursive step of the check point algorithm. 'Tis really a hack so I don't
// have to reinitialize the memory structures.
long costOffset = 1;    // must be signed for future comparisons
unsigned int finalCost;

int furthestReached = -1;
int checkPoint_dist;       // Flag for whether to use distance of cost as the check pointing criterion.
                           // Check pointing on the distance is only done for first iteration, when the final
                           // cost is unknown.


// Use these globals cause don't want to pass 'em around, and they're needed
// by the withinMatrix func.  Be nice to have closures :-)
// must be signed; later used as possibly negative
int startABG    = 0,
    startACG    = 0,
    startCostG  = 0,
    startStateG = 0;

// Used to define where to end on the three strings in the
// check-point recursion. So endA contains the distance the recursion
// must finish on + 1.
int endA,
    endB,
    endC;

// Set to 1 for base cases, so 'from' info that alignment
//  can be retrieved from is set.
int completeFromInfo = 0;

int checkPoint_width;
int checkPoint_cost;

counts_t counts;

size_t aCharIdx = 0,
       bCharIdx = 0,
       cCharIdx = 0,
       stateIdx = 0,
       costIdx  = 0;

char resultA[MAX_STR * 2],
     resultB[MAX_STR * 2],
     resultC[MAX_STR * 2];

int  states[MAX_STR * 2],
     cost[MAX_STR * 2];

ukk_cell_t    UdummyCell;
check_point_t checkPoint_dummyCell;

// TODO: these two for debug call order. Find a way to delete them
    int indenti = 0;
    char indent[1000];

static inline ukk_cell_t *get_ukk_cell( int ab_idx_diff
                                      , int ac_idx_diff
                                      , int distance
                                      , int s
                                      )
{
    return getPtr( &myUkk_allocInfo, ab_idx_diff, ac_idx_diff, distance, s );
}


/************* next three functions are static inline, so not in .h file. ******************/
static inline check_point_t *get_checkPoint_cell( int ab_idx_diff
                                                , int ac_idx_diff
                                                , int distance
                                                , int s
                                                )
{
    return getPtr( &myCheckPt_allocInfo, ab_idx_diff, ac_idx_diff, distance, s );
}


static inline void sort( int    aval[]
                       , size_t len
                       )
{
    size_t i, j;

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

static inline int withinMatrix( int ab_idx_diff
                              , int ac_idx_diff
                              , int d
                              )
{
    // The new method for checking the boundary condition.  Much tighter ~20%(?)  -- 28/02/1999
    int bc = ac_idx_diff - ab_idx_diff;
    int aval[3];
    int g,
        h,
        cheapest;

    if (d < 0) return 0;

    aval[0] = abs(startABG - ab_idx_diff);
    aval[1] = abs(startACG - ac_idx_diff);
    aval[2] = abs(startACG - startABG - bc);

    // Set g and h to the smallest and second smallest of aval[] respectively
    sort( aval, 3 );
    g = aval[0];
    h = aval[1];

    if (startStateG == 0) {
        // We know a good boudary check if the start state is MMM
        cheapest = (g==0 ? 0 : gapOpenCost + g * gapExtendCost) + (h==0 ? 0 : gapOpenCost + h * gapExtendCost);
    } else {
        // If start state is something else. Can't charge for start of gaps unless we
        // do something more clever.
        cheapest = (g==0 ? 0 : g*gapExtendCost) + (h==0 ? 0 : h * gapExtendCost);
    }

    if (cheapest + startCostG > d) return 0;
    else                           return 1;

}

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
                 )
{
    assert( startCost >= 0 && finalCost >= 0 );

    startABG    = startAB;
    startACG    = startAC;
    startCostG  = startCost;
    startStateG = startState;
    endA        = finalDist;
    endB        = finalDist - finalAB;
    endC        = finalDist - finalAC;

    int curCost;

    if (DEBUG_3D) {
        fprintf(stderr
               , "Doing (startAB = %2d, startAC = %2d, startCost = %d, startState = %2d, startDist = %2d\n"
               , startAB
               , startAC
               , startCost
               , startState
               , startDist
               );
        fprintf(stderr
               , "       finalAB = %2d, finalAC = %2d, finalCost = %2d, finalState = %2d, finalDist = %2d\n"
               , finalAB
               , finalAC
               , finalCost
               , finalState
               , finalDist
               );

        fprintf(stderr, "Character to align at this step:\n");
        for (curCost = startDist; curCost < finalDist; curCost++) {
            fprintf(stderr, "%3c", lesserStr[curCost]);
        }
        fprintf(stderr, "\n");
        for (curCost = startDist - startAB; curCost < finalDist - finalAB; curCost++) {
            fprintf(stderr, "%3c", longerStr[curCost]);
        }
        fprintf(stderr, "\n");
        for (curCost = startDist - startAC; curCost < finalDist - finalAC; curCost++) {
            fprintf(stderr, "%3c", middleStr[curCost]);
        }
        fprintf(stderr, "\n");
    }

    completeFromInfo = 0;

    costOffset += finalCost + 1;
    assert(costOffset > 0 && "Oops, overflow in costOffset");

    get_ukk_cell(startAB, startAC, startCost, startState)->dist     = startDist;
    get_ukk_cell(startAB, startAC, startCost, startState)->computed = startCost + costOffset;

    if (finalCost - startCost <= checkPoint_width) { // Is it the base case?
        int curCost;
        completeFromInfo = 1;

        if (DEBUG_3D) {
            fprintf(stderr, "Base case.\n");
        }

        #if 0
            for (curCost = startCost; curCost <= finalCost; curCost++) {
                Ukk(finalAB, finalAC, curCost, 0);
            }

            assert( get_ukk_cell( finalAB, finalAC, finalCost, finalState)->dist == finalDist );
        #else
        {
            int dist;

            curCost = startCost - 1;

            do {
                curCost++;
                dist = Ukk(finalAB, finalAC, curCost, finalState);
            } while (dist < finalDist);

            assert(dist == finalDist);
            if (curCost != finalCost) {
                fprintf(stderr, "Dist reached for cost %2d (old cost %2d)\n", curCost, finalCost);
                finalCost = curCost;
                assert(0);
            }
        }
        #endif


        if (DEBUG_3D) {
            fprintf(stderr,"Tracing back in base case.\n");
        }
        traceBack( startAB
                 , startAC
                 , startCost
                 , startState
                 , finalAB
                 , finalAC
                 , finalCost
                 , finalState
                 );

        completeFromInfo = 0;
        return findBest_DistState(finalAB, finalAC, finalCost, 0);
    }


    checkPoint_cost = (finalCost + startCost - checkPoint_width + 1) / 2;

    #if 0
        // Do the loop up to the desired cost.  Can't do Ukk(finalAB,finalAC,finalCost,finalState) directly (without
        // the loop) because the Umatrix is written to before it is actually needed.
        // Could be fixed, but this is also fine
        {
        int i;
        for (i=startCost; i<=finalCost; i++) {
          Ukk(finalAB,finalAC,i,0);
          //      Ukk(finalAB,finalAC,i,finalState);
        }
        assert(get_ukk_cell(finalAB,finalAC,finalCost,finalState)->dist==finalDist);
        }
    #else
    {
        int dist;

        curCost = startCost - 1;

        do {
            curCost++;
            dist = Ukk(finalAB, finalAC, curCost, 0);  // Need this (?) otherwise if finalState != 0 we may need larger than expected slice size.
            dist = Ukk(finalAB, finalAC, curCost, finalState);
        } while (dist < finalDist);

        assert(dist == finalDist);
        if (curCost != finalCost) {
            fprintf(stderr, "Dist reached for cost %2d (old cost %2d)\n", curCost, finalCost);
            finalCost = curCost;
            assert(0);
        }
    }
    #endif

    return getSplitRecurse( startAB
                          , startAC
                          , startCost
                          , startState
                          , startDist
                          , finalAB
                          , finalAC
                          , finalCost
                          , finalState
                          , finalDist
                          );
}

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
                   )
{
    // Get 'from' and checkPoint_ data.  Then recurse
    size_t finalLen;
    int    checkPoint_dist;
    from_t finalCell;

    assert(    startCost >= 0
            && finalCost >= 0 );

    assert( get_ukk_cell(finalAB, finalAC, finalCost, finalState)->computed == finalCost + costOffset);

    finalCell = get_ukk_cell(finalAB, finalAC, finalCost, finalState)->from;

    assert( finalCell.cost >= 0) ;

    if (get_checkPoint_cell( finalCell.ab_idx_diff
                           , finalCell.ac_idx_diff
                           , finalCell.cost
                           , finalCell.state
                           )->cost == 0) {

        get_checkPoint_cell( finalCell.ab_idx_diff
                           , finalCell.ac_idx_diff
                           , finalCell.cost
                           , finalCell.state
                           )->cost = 1;
    }

    assert( get_checkPoint_cell( finalCell.ab_idx_diff
                               , finalCell.ac_idx_diff
                               , finalCell.cost
                               , finalCell.state
                               )->cost == finalCell.cost + 1
          );   // Use cost + 1 so can tell if not used (cost == 0)

    checkPoint_dist = get_checkPoint_cell( finalCell.ab_idx_diff
                                         , finalCell.ac_idx_diff
                                         , finalCell.cost
                                         , finalCell.state
                                         )->dist;

    assert(checkPoint_dist >= 0);

    if (DEBUG_3D) {
        fprintf( stderr
               , "checkPoint_cost   = %2d checkPoint_width = %2d\n"
               , checkPoint_cost
               , checkPoint_width
               );
        fprintf( stderr
               , "From: ab_idx_diff = %2d ac_idx_diff = %2d d = %2d s = %2d\n"
               , finalCell.ab_idx_diff
               , finalCell.ac_idx_diff
               , finalCell.cost
               , finalCell.state
               );
        fprintf( stderr
               , "checkPoint_ dist  = %2d\n"
               , checkPoint_dist
               );
    }


    // Note: Doing second half of alignment first.  Only reason
    // for this is so the alignment is retrieved in exactly reverse order
    // making it easy to print out.
    finalLen = doUkkInLimits( finalCell.ab_idx_diff
                            , finalCell.ac_idx_diff
                            , finalCell.cost
                            , finalCell.state
                            , checkPoint_dist
                            , finalAB
                            , finalAC
                            , finalCost
                            , finalState
                            , finalDist
                            );

    doUkkInLimits( startAB
                 , startAC
                 , startCost
                 , startState
                 , startDist
                 , finalCell.ab_idx_diff
                 , finalCell.ac_idx_diff
                 , finalCell.cost
                 , finalCell.state
                 , checkPoint_dist
                 );

    if (DEBUG_3D) {
        fprintf( stderr, "Done.\n" );
    }

    //  return findBest_DistState(finalAB,finalAC,finalCost,0);
    return finalLen;
}

// -- Traceback routines --------------------------------------------------------------
void traceBack( int startAB
              , int startAC
              , int startCost
              , int startState
              , int finalAB
              , int finalAC
              , int finalCost
              , unsigned int finalState
              )
{
    int ab_idx_diff       = finalAB,
        ac_idx_diff       = finalAC,
        distance = finalCost,
        state    = finalState;

    while (   ab_idx_diff       != startAB
           || ac_idx_diff       != startAC
           || distance != startCost
           || state    != startState
          ) {

        int a         = get_ukk_cell(ab_idx_diff, ac_idx_diff, distance, state)->dist;
        int nab_idx_diff       = get_ukk_cell(ab_idx_diff, ac_idx_diff, distance, state)->from.ab_idx_diff;
        int nac_idx_diff       = get_ukk_cell(ab_idx_diff, ac_idx_diff, distance, state)->from.ac_idx_diff;
        int nDistance = get_ukk_cell(ab_idx_diff, ac_idx_diff, distance, state)->from.cost;
        int nState    = get_ukk_cell(ab_idx_diff, ac_idx_diff, distance, state)->from.state;

        int b  = a - ab_idx_diff,
            c  = a - ac_idx_diff,
            a1 = get_ukk_cell( nab_idx_diff, nac_idx_diff, nDistance, nState )->dist,
            b1 = a1 - nab_idx_diff,
            c1 = a1 - nac_idx_diff;

        assert( get_ukk_cell( ab_idx_diff , ac_idx_diff , distance , state )->computed == distance  + costOffset );
        assert( get_ukk_cell( nab_idx_diff, nac_idx_diff, nDistance, nState)->computed == nDistance + costOffset );

        if (DEBUG_3D) {
            fprintf( stderr
                   , " ab_idx_diff = %3d,  ac_idx_diff = %3d,  distance = %3d,  state = %2d,  dist = %3d, \n\
nab_idx_diff = %3d, nac_idx_diff = %3d, nDistance= %3d, nState = %2d, ndist = %3d\n\n"
                   , ab_idx_diff
                   , ac_idx_diff
                   , distance
                   , state
                   , a
                   , nab_idx_diff
                   , nac_idx_diff
                   , nDistance
                   , nState
                   , a1
                   );
        }

        // Run of matches
        while (    a > a1
                && b > b1
                && c > c1
              ) {
          a--;
          b--;
          c--;
          resultA[aCharIdx++] = lesserStr[a];
          resultB[bCharIdx++] = longerStr[b];
          resultC[cCharIdx++] = middleStr[c];
          states[stateIdx++]  = 0;        /* The match state */
          cost[costIdx++]     = distance;
        }

        // The step for (nab_idx_diff, nac_idx_diff, nDistance, nState) -> (ab_idx_diff, ac_idx_diff, distance, state)
        if (   a != a1
            || b != b1
            || c != c1
           ) {
            if (a > a1) resultA[aCharIdx++] = lesserStr[--a];
            else        resultA[aCharIdx++] = '-';

            if (b > b1) resultB[bCharIdx++] = longerStr[--b];
            else        resultB[bCharIdx++] = '-';

            if (c > c1) resultC[cCharIdx++] = middleStr[--c];
            else        resultC[cCharIdx++] = '-';

            states[stateIdx++] = state;
            cost[costIdx++]    = distance;
        }

        assert(    a == a1
                && b == b1
                && c == c1);

        ab_idx_diff = nab_idx_diff;
        ac_idx_diff = nac_idx_diff;
        distance = nDistance;
        state    = nState;
    }

    if (DEBUG_3D) {
        {
            int i;        // counts down to 0, must be signed

            fprintf(stderr,"Alignment so far\n");
            for (i = aCharIdx - 1; i >= 0; i--) {
                fprintf(stderr, "%c",resultA[i]);
            }
            fprintf(stderr, "\n");
            for (i = bCharIdx - 1; i >= 0; i--) {
                fprintf(stderr, "%c",resultB[i]);
            }
            fprintf(stderr, "\n");
            for (i = cCharIdx - 1; i >= 0; i--) {
                fprintf(stderr, "%c",resultC[i]);
            }
            // Print state information
            for (i = stateIdx - 1; i >= 0; i--) {
              fprintf(stderr,"%s ",state2str(states[i]));
            }
            fprintf(stderr,"\n");
            // Print cost stuff
            for (i = costIdx - 1; i >= 0; i--) {
                fprintf(stderr,"%-2d  ",cost[i]);
            }
            fprintf(stderr, "\n");
        }
    }

    assert(ab_idx_diff       == startAB);
    assert(ac_idx_diff       == startAC);
    assert(distance == startCost);
    assert(state    == startState);
}

int char_to_base (char v) {
    if      ('A' == v) return 1;
    else if ('C' == v) return 2;
    else if ('G' == v) return 4;
    else if ('T' == v) return 8;
    else if ('-' == v) return 16;
    else return -1;
}

void printTraceBack( dyn_character_t *retCharA
                   , dyn_character_t *retCharB
                   , dyn_character_t *retCharC
                   )
{
    // Print out the alignment

    // Add the first run of matches to the alignment
    // NB. The first run of matches must be added in reverse order.

    size_t endRun,
           i = 0;

    int j; // countdown to 0 later, so much be signed

    while (   i < lesserLen
           && (   lesserStr[i] == longerStr[i]
               && lesserStr[i] == middleStr[i])
           ) {
      i++;
    }
    endRun = i;

    for (j = endRun - 1; j >= 0; j--)  {
      resultA[aCharIdx++] = lesserStr[j];
      resultB[bCharIdx++] = longerStr[j];
      resultC[cCharIdx++] = middleStr[j];
      states[stateIdx++] = 0;        /* The match state */
      cost[costIdx++]    = 0;
    }
    // end print alignment


    // Reverse the alignments
    revCharArray(resultA, 0, aCharIdx);
    revCharArray(resultB, 0, bCharIdx);
    revCharArray(resultC, 0, cCharIdx);
    revIntArray(states,   0, stateIdx);
    revIntArray(cost,     0, costIdx);
    // end reverse alignments

    // Print out the alignment
    for (j = aCharIdx - 1; j >= 0; j--) {
      dyn_char_prepend (retCharA, char_to_base (resultA[j]));
      dyn_char_prepend (retCharB, char_to_base (resultB[j]));
      dyn_char_prepend (retCharC, char_to_base (resultC[j]));
    }
    dyn_char_prepend (retCharA, 16);
    dyn_char_prepend (retCharB, 16);
    dyn_char_prepend (retCharC, 16);

    assert(   aCharIdx == bCharIdx
           && aCharIdx == cCharIdx
           && aCharIdx == stateIdx
           && aCharIdx == costIdx);

    checkAlign(resultA, aCharIdx, lesserStr, lesserLen);
    checkAlign(resultB, bCharIdx, longerStr, longerLen);
    checkAlign(resultC, cCharIdx, middleStr, middleLen);

    assert( alignmentCost(states, resultA, resultB, resultC, aCharIdx) == finalCost );
}

// Find the furthest distance at ab_idx_diff, ac_idx_diff, d. wantState selects whether the
// best distance is returned, or the best final state (needed for ukk.alloc traceback)
int findBest_DistState( int ab_idx_diff
                      , int ac_idx_diff
                      , int input_dist
                      , int return_the_state
                      )
{
    int bestDist  = -INFINITY;
    int bestState = -1;

    for (size_t s = 0; s < numStates; s++) {
        if (    ( get_ukk_cell(ab_idx_diff, ac_idx_diff, input_dist, s)->computed == input_dist + costOffset )
             && ( get_ukk_cell(ab_idx_diff, ac_idx_diff, input_dist, s)->dist      > bestDist )
           ) {
          bestDist  = get_ukk_cell(ab_idx_diff, ac_idx_diff, input_dist, s)->dist;
          bestState = s;
        }
    }

/*
    fprintf( stderr
           , "findBest_DistState(%2d, %2d, %2d, (%2d)) = %2d\n"
           , ab_idx_diff
           , ac_idx_diff
           , input_dist
           , bestState
           , bestDist
           );
*/
    if (return_the_state) {
        return bestState;
    } else {
        return bestDist;
    }
}


int Ukk( int          ab_idx_diff
       , int          ac_idx_diff
       , int          distance
       , unsigned int state
       )
{
    if ( !withinMatrix(ab_idx_diff, ac_idx_diff, distance) )                                          return -INFINITY;
    if (  get_ukk_cell(ab_idx_diff, ac_idx_diff, distance, state)->computed == distance + costOffset) return  get_ukk_cell(ab_idx_diff, ac_idx_diff, distance, state)->dist;

/*
    fprintf( stderr
           ,"Calculating get_ukk_cell(%2d, %2d, %2d, %2d)\n"
           , ab_idx_diff
           , ac_idx_diff
           , distance
           , state)
           ;
*/
    counts.cells++;

    calcUkk( ab_idx_diff, ac_idx_diff, distance, state );

    // Store away check point from info in necessary
    if (     distance >= checkPoint_cost
         && (distance  < checkPoint_cost + checkPoint_width )
       ) {
        get_checkPoint_cell( ab_idx_diff, ac_idx_diff, distance, state )->dist = get_ukk_cell( ab_idx_diff, ac_idx_diff, distance, state )->dist;
        get_checkPoint_cell( ab_idx_diff, ac_idx_diff, distance, state )->cost = distance + 1;          // Note adding 1 so cost==0 signifies unused cell
    }

    if ( get_ukk_cell( ab_idx_diff, ac_idx_diff, distance, state )->dist > furthestReached ) {
        furthestReached = get_ukk_cell(ab_idx_diff, ac_idx_diff, distance, state)->dist;
    }

    return get_ukk_cell( ab_idx_diff, ac_idx_diff, distance, state )->dist;
}

// IMPORTANT!!! Order of input characters is short, long, middle.
int doUkk( dyn_character_t *retCharA
         , dyn_character_t *retCharB
         , dyn_character_t *retCharC
         )
{
    checkPoint_dummyCell.dist      = 0;
    checkPoint_dummyCell.cost      = 0;
    UdummyCell.dist       = 0;
    UdummyCell.computed   = 0;
    UdummyCell.from.ab_idx_diff    = 0;
    UdummyCell.from.ac_idx_diff    = 0;
    UdummyCell.from.cost  = 0;
    UdummyCell.from.state = 0;
    finalCost             = 0;
    checkPoint_cost                = 0;
    checkPoint_width               = 0;
    completeFromInfo      = 0;

    aCharIdx = 0;
    bCharIdx = 0;
    cCharIdx = 0;
    stateIdx = 0;
    costIdx  = 0;

    costOffset      = 1;
    furthestReached = -1;

    startABG    = 0;
    startACG    = 0;
    startCostG  = 0;
    startStateG = 0;

    size_t curDist,
           finalAB,
           finalac_idx_diff,
           startDist;

    checkPoint_width = maxSingleStep;
    // Concern: what is the correct value to use for Umatrix depth.
    // Would think that maxSingleCost=maxSingleStep*2 would be enough
    // but doesn't seem to be.  No idea why. *shrug*
    myUkk_allocInfo     = allocInit(sizeof(ukk_cell_t),   maxSingleCost);
    myCheckPt_allocInfo = allocInit(sizeof(check_point_t), checkPoint_width);

    counts.cells     = 0;
    counts.innerLoop = 0;

    // Calculate starting position
    curDist = 0;
    while (   curDist < lesserLen
           && (   lesserStr[curDist] == longerStr[curDist]
               && lesserStr[curDist] == middleStr[curDist]
              )
          ) {
        curDist++;
        counts.innerLoop++;
    }
    get_ukk_cell(0, 0, 0, 0)->dist     = curDist;
    get_ukk_cell(0, 0, 0, 0)->computed = 0 + costOffset;
    startDist = curDist;

    finalAB = lesserLen - longerLen;
    finalac_idx_diff = lesserLen - middleLen;
    endA    = lesserLen;
    endB    = longerLen;
    endC    = middleLen;

    checkPoint_dist = 1;
    checkPoint_cost   = INFINITY;
    do {
        curDist++;
        Ukk(finalAB, finalac_idx_diff, curDist, 0);

        if (DEBUG_3D) {
            fprintf(stderr, "Furthest reached for cost %2zu is %2d.\n",
                    curDist, furthestReached);
        }

        int half_lesserLen = (int) (lesserLen / 2);
        if (checkPoint_dist && furthestReached >= half_lesserLen) {
            checkPoint_cost   = curDist + 1;
            checkPoint_dist = 0;

            if (DEBUG_3D) {
                fprintf(stderr, "Setting checkPoint_cost: %2d\n", checkPoint_cost);
            }
        }


    } while (findBest_DistState(finalAB, finalac_idx_diff, curDist, 0) < (int) lesserLen);

    assert(findBest_DistState(finalAB, finalac_idx_diff, curDist, 0) == (int) lesserLen);

    checkPoint_dist = 0;
    finalCost       = curDist;


    // Recurse for alignment
    int finalState = findBest_DistState(finalAB, finalac_idx_diff, finalCost, 1);
    size_t dist;

    if ( get_ukk_cell(finalAB, finalac_idx_diff, finalCost, finalState)->from.cost <= 0) {
        // We check pointed too late on this first pass.
        // So we got no useful information.  Oh well, have to do it all over again
        assert( get_ukk_cell(finalAB, finalac_idx_diff, finalCost, finalState)->computed == finalCost + costOffset);

        dist = doUkkInLimits( 0
                            , 0
                            , 0
                            , 0
                            , startDist
                            , finalAB
                            , finalac_idx_diff
                            , finalCost
                            , finalState
                            , lesserLen
                            );
    } else {
        // Use the 'from' info and do the two sub parts.
        dist = getSplitRecurse( 0
                              , 0
                              , 0
                              , 0
                              , startDist
                              , finalAB
                              , finalac_idx_diff
                              , finalCost
                              , finalState
                              , lesserLen
                              );
    }

    assert(dist == lesserLen);
    printTraceBack(retCharA, retCharB, retCharC);

    allocFinal(&myUkk_allocInfo,     (&UdummyCell.computed), (&UdummyCell));
    allocFinal(&myCheckPt_allocInfo, (&checkPoint_dummyCell.cost),    (&checkPoint_dummyCell));

    printf("doUkk: dist: = %2zu\n", curDist);
    return (int) curDist;
}

int calcUkk( int ab_idx_diff
           , int ac_idx_diff
           , int input_dist
           , int toState
           )
{
    if (DEBUG_CALL_ORDER) {
        printf("--ukk.check-point: calcUkk\n" );
        fflush(stdout);
    }
    int neighbour = neighbours[toState];
    int da,
        db,
        dc,
        ab_idx_diff1,
        ac_idx_diff1;

    from_t from;

    int bestDist;

    from.cost = -1;

    if (DEBUG_CALL_ORDER) {
        indent[indenti] = 0;

        fprintf( stderr
               , "%s CalcUKK(ab_idx_diff = %2d, ac_idx_diff = %2d, input_dist = %2d, toState = %2d)\n"
               , indent
               , ab_idx_diff
               , ac_idx_diff
               , input_dist
               , toState
               );

        indent[indenti++] = ' ';
        indent[indenti++] = ' ';
        indent[indenti]   = 0;
    }

    assert( get_ukk_cell( ab_idx_diff
                        , ac_idx_diff
                        , input_dist
                        , toState
                        )->computed < input_dist + costOffset
           );

    bestDist = -INFINITY;

    // Initialise checkPoint_ from info if necessary
    if (    input_dist >= checkPoint_cost
         && input_dist  < checkPoint_cost + checkPoint_width
       ) {
        from.ab_idx_diff    = ab_idx_diff;
        from.ac_idx_diff    = ac_idx_diff;
        from.cost  = input_dist;
        from.state = toState;
    }

    step( neighbour
        , &da
        , &db
        , &dc
        );

    ab_idx_diff1 = ab_idx_diff - da + db;
    ac_idx_diff1 = ac_idx_diff - da + dc;

    // calculate if its a valid diagonal
    if (    ab_idx_diff1 >= -endB
         && ab_idx_diff1 <=  endA
         && ac_idx_diff1 >= -endC
         && ac_idx_diff1 <=  endA
       ) {
        size_t fromState;

        // Loop over possible state we are moving from
        //   May be possible to limit this?
        for (fromState = 0; fromState < numStates; fromState++) {
            int transtartCost = stateTransitionCost( fromState, toState );
            int fromCost      = -INFINITY;
            int dist          = -INFINITY;
            int cost          = input_dist - transtartCost - contCost[toState];
            int a1            = Ukk(ab_idx_diff1, ac_idx_diff1, cost, fromState);
            int a2            = -1;
            // printf("a1: %d, da: %d, endA: %d\n", a1, da, endA);
            // printf("b1: %d, db: %d, endB: %d\n", a1, da, endA);
            // printf("c1: %d, dc: %d, endC: %d\n", a1, da, endA);
            if (    okIndex( a1      , da, endA )
                 && okIndex( a1 - ab_idx_diff1, db, endB )
                 && okIndex( a1 - ac_idx_diff1, dc, endC )
                 && ( whichCharCost( da ? lesserStr[a1]     : '-',
                                     db ? longerStr[a1-ab_idx_diff1] : '-',
                                     dc ? middleStr[a1-ac_idx_diff1] : '-' ) == 1 )
               ) {
                fromCost = cost;
                dist = a1 + da;
            } else {
                if (!secondCost[toState]) {
                    continue;
                }

                a2 = Ukk(ab_idx_diff1, ac_idx_diff1, cost - mismatchCost, fromState);

                if (   okIndex(a2,       da, endA)
                    && okIndex(a2 - ab_idx_diff1, db, endB)
                    && okIndex(a2 - ac_idx_diff1, dc, endC)
                   ) {
                    fromCost = cost - mismatchCost;
                    dist = a2 + da;
                }
            }

            // Check if this is an improvment
            if (bestDist < dist) {
                bestDist = dist;

                if (completeFromInfo) {        // Do we need to store complete from information for a base case?
                    from.ab_idx_diff    = ab_idx_diff1;
                    from.ac_idx_diff    = ac_idx_diff1;
                    from.cost  = fromCost;
                    from.state = fromState;
                } else if (input_dist >= checkPoint_cost + checkPoint_width) { // Store from info for checkPoint_
                      from = get_ukk_cell(ab_idx_diff1, ac_idx_diff1, fromCost, fromState)->from;
                }
            }
        } // End loop over from states
    } // End if valid neighbour

    // Insure that we have how we can reach for AT MOST cost input_dist

    int dist = Ukk(ab_idx_diff, ac_idx_diff, input_dist - 1, toState);
    // Check if this is an improvment
    if (okIndex(dist,      0, endA) &&
        okIndex(dist - ab_idx_diff, 0, endB) &&
        okIndex(dist - ac_idx_diff, 0, endC) &&
        bestDist < dist)
    {
        bestDist = dist;

        if (completeFromInfo) {        // Do we need to store complete from information for a base case?
            from.ab_idx_diff    = ab_idx_diff;
            from.ac_idx_diff    = ac_idx_diff;
            from.cost  = input_dist - 1;
            from.state = toState;
        } else if (input_dist >= checkPoint_cost + checkPoint_width) { // Store from info for checkPoint_
            from = get_ukk_cell(ab_idx_diff, ac_idx_diff, input_dist - 1, toState)->from;
        }
    } // end insure that we have how we can reach for AT MOST cost input_dist

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
           Ukkonen matrix and the D matrix.
        */

        // Get furthest of states for this cost
        int dist = -INFINITY;
        int from_state = -1;

        for (size_t s = 0; s < numStates; s++) {
            int thisdist;
            thisdist = (s == 0) ? bestDist : Ukk(ab_idx_diff, ac_idx_diff, input_dist, s);
            if (thisdist > dist) {
                dist       = thisdist;
                from_state = s;
            }
        }

        // Try to extend to diagonal
        while (okIndex(dist, 1, endA) &&
               okIndex(dist - ab_idx_diff, 1, endB) &&
               okIndex(dist - ac_idx_diff, 1, endC) &&
               (lesserStr[dist] == longerStr[dist - ab_idx_diff] &&
                lesserStr[dist] == middleStr[dist - ac_idx_diff])
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
                    from.ab_idx_diff    = ab_idx_diff;
                    from.ac_idx_diff    = ac_idx_diff;
                    from.cost  = input_dist;
                    from.state = from_state;
                } else if (input_dist >= checkPoint_cost + checkPoint_width) { // Store from info for checkPoint_
                    from = get_ukk_cell(ab_idx_diff, ac_idx_diff, input_dist, from_state)->from;
                }
            }
        }
    } // End attempt to extend diagonal on a run of matches

    assert( get_ukk_cell(ab_idx_diff, ac_idx_diff, input_dist, toState)->computed < input_dist + costOffset);
    get_ukk_cell(ab_idx_diff, ac_idx_diff, input_dist, toState)->dist     = bestDist;
    get_ukk_cell(ab_idx_diff, ac_idx_diff, input_dist, toState)->computed = input_dist + costOffset;
    get_ukk_cell(ab_idx_diff, ac_idx_diff, input_dist, toState)->from     = from;

    if (DEBUG_CALL_ORDER) {
        indenti        -= 2;
        indent[indenti] = 0;
        fprintf( stderr, "%sCalcUKK(ab_idx_diff = %2d, ac_idx_diff = %2d, d = %2d,    toState = %2d) = %2d\n"
               , indent
               , ab_idx_diff
               , ac_idx_diff
               , input_dist
               , toState
               , get_ukk_cell(ab_idx_diff, ac_idx_diff, input_dist, toState)->dist
               );
        fprintf( stderr, "%sFrom:   ab_idx_diff = %2d, ac_idx_diff = %2d, cost = %2d, state = %2d\n",
                 indent
               , get_ukk_cell(ab_idx_diff, ac_idx_diff, input_dist, toState)->from.ab_idx_diff
               , get_ukk_cell(ab_idx_diff, ac_idx_diff, input_dist, toState)->from.ac_idx_diff
               , get_ukk_cell(ab_idx_diff, ac_idx_diff, input_dist, toState)->from.cost
               , get_ukk_cell(ab_idx_diff, ac_idx_diff, input_dist, toState)->from.state
               );
    }

    return get_ukk_cell(ab_idx_diff, ac_idx_diff, input_dist, toState)->dist;
}

