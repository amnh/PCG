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
#include "ukkCommon.h"

#define MAXINT INT_MAX
// #define FIXED_NUM_PLANES TODO: this is also defined in ukkCommon.h; it was commented out, but then allocInit() failed to compile

alloc_info_t myUkk_allocInfo;
alloc_info_t myCheckPt_allocInfo;

// these three are also extern in ukkCommon.h
// extern unsigned int mismatchCost;
// extern unsigned int gapOpenCost;
// extern unsigned int gapExtendCost;


// TODO: globals--it'd be nice to clean these up a little


// costOffset - added to the 'computed' field of each cell. 'costOffset' is
// recursive step of the check point algorithm. 'Tis really a hack so I don't
// have to reinitialize the memory structures.
long costOffset = 1;    // must be signed for future comparisons
unsigned int finalCost;

int furthestReached = -1;
int checkPoint_editDist;       // Flag for whether to use edit distance of cost as the check pointing criterion.
                           // Check pointing on the edit distance is only done for first iteration, when the final
                           // cost is unknown.


// Use these globals cause don't want to pass 'em around, and they're needed
// by the withinMatrix func.  Be nice to have closures :-)
// must be signed; later used as possibly negative
int start_ab_diffG    = 0,
    start_ac_diffG    = 0,
    startCostG  = 0,
    startStateG = 0;

// Used to define where to end on the three strings in the
// check-point recursion. So endA contains the edit distance the recursion
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


// TODO: these two for debug call order. Find a way to delete them
    int indenti = 0;
    char indent[1000];

static inline ukk_cell_t *get_ukk_cell( int    ab_idx_diff
                                      , int    ac_idx_diff
                                      , int    editDistance
                                      , int    s
                                      , size_t numStates
                                      )
{
    return getPtr( &myUkk_allocInfo
                 , ab_idx_diff
                 , ac_idx_diff
                 , editDistance
                 , s
                 , numStates
                 );
}


/************* next three functions are static inline, so not in .h file. ******************/
static inline checkPoint_cell_t *get_checkPoint_cell( int ab_idx_diff
                                                , int ac_idx_diff
                                                , int editDistance
                                                , int s
                                                , size_t numStates
                                                )
{
    return getPtr( &myCheckPt_allocInfo
                 , ab_idx_diff
                 , ac_idx_diff
                 , editDistance
                 , s
                 , numStates
                 );
}


// TODO: unsigned ints for costs? Probably shouldn't be, actually.
/** This is the interface function to the alignment code. It takes in three characters, as well as a mismatch cost, a gap open cost and
 *  a gap extention cost (all of which should be replaced by a 3d cost matrix).
 *
 *  IMPORTANT!!! Order of input characters is short, long, middle, or at least short must be first.
 */
int powell_3D_align ( dyn_character_t *lesserChar
                    , dyn_character_t *middleChar
                    , dyn_character_t *longerChar
                    , dyn_character_t *retLesserChar
                    , dyn_character_t *retMiddleChar
                    , dyn_character_t *retLongerChar
                    , unsigned int     mismatch_cost
                    , unsigned int     gapOpen
                    , unsigned int     gapExtend
                    )
{
    if (DEBUG_CALL_ORDER) {
        printf("powell_3D_align\n");
    }

    // Allocate global costs. These will be initialized in setup().
    global_costs_t *globalCosts    = malloc( sizeof(global_costs_t) );

    // Allocate global characters. These will be initialized in setup().
    global_characters_t *globalCharacters = malloc( sizeof(global_characters_t) );

    global_arrays_t *globalCostArrays = malloc( sizeof(global_arrays_t) );

    /* Char_custom_val(lesserChar,sa);
    Char_custom_val(longerChar,sb);
    Char_custom_val(middleChar,sc);
    Char_custom_val(retLesserChar,retLesserChar);
    Char_custom_val(retMiddleChar,retMiddleChar);
    Char_custom_val(retLongerChar,retLongerChar);
    */
    /* assert (mismatchCost != 0 && gapOpenCost >= 0 && gapExtendCost > 0); */ // SKIP, because now using unsigned ints

    setup( globalCosts
         , globalCharacters
         , globalCostArrays
         , lesserChar
         , middleChar
         , longerChar
         , mismatch_cost
         , gapOpen
         , gapExtend
         );

    return doUkk( retLesserChar
                , retMiddleChar
                , retLongerChar
                , globalCosts
                , globalCharacters
                , globalCostArrays
                );
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

static inline int withinMatrix( int             ab_idx_diff
                              , int             ac_idx_diff
                              , int             d
                              , global_costs_t *globalCosts
                              )
{
    // The new method for checking the boundary condition.  Much tighter ~20%(?)  -- 28/02/1999
    int bc = ac_idx_diff - ab_idx_diff;
    int aval[3];
    int g,
        h,
        cheapest;

    if (d < 0) return 0;

    aval[0] = abs(start_ab_diffG - ab_idx_diff);
    aval[1] = abs(start_ac_diffG - ac_idx_diff);
    aval[2] = abs(start_ac_diffG - start_ab_diffG - bc);

    // Set g and h to the smallest and second smallest of aval[] respectively
    sort( aval, 3 );
    g = aval[0];
    h = aval[1];

    if (startStateG == 0) {
        // We know a good boudary check if the start state is MMM
        cheapest = (g==0 ? 0 : globalCosts->gapOpenCost + g * globalCosts->gapExtendCost)
                 + (h==0 ? 0 : globalCosts->gapOpenCost + h * globalCosts->gapExtendCost);
    } else {
        // If start state is something else. Can't charge for start of gaps unless we
        // do something more clever.
        cheapest = (g == 0 ? 0 : g * globalCosts->gapExtendCost)
                 + (h == 0 ? 0 : h * globalCosts->gapExtendCost);
    }

    if (cheapest + startCostG > d) return 0;
    else                           return 1;

}

/** For Ukkonen check point between to specified points in the U matrix... TODO: ...?
 *  All edit distances and costs are signed, as often initialized to -INFINITY
 */
int doUkkInLimits( int                  start_ab_diff
                 , int                  start_ac_diff
                 , int                  startCost
                 , int                  startState
                 , int                  startDist
                 , int                  final_ab_diff
                 , int                  final_ac_diff
                 , int                  finalCost
                 , int                  finalState
                 , int                  finalDist
                 , global_costs_t      *globalCosts
                 , global_characters_t *globalCharacters
                 , global_arrays_t     *globalCostArrays
                 )
{
    assert( startCost >= 0 && finalCost >= 0 );

    start_ab_diffG = start_ab_diff;
    start_ac_diffG = start_ac_diff;
    startCostG     = startCost;
    startStateG    = startState;
    endA           = finalDist;
    endB           = finalDist - final_ab_diff;
    endC           = finalDist - final_ac_diff;

    int curCost;

    if (DEBUG_3D) {
        fprintf(stderr
               , "Doing (start_ab_diff = %2d, start_ac_diff = %2d, startCost = %d, startState = %2d, startDist = %2d\n"
               , start_ab_diff
               , start_ac_diff
               , startCost
               , startState
               , startDist
               );
        fprintf(stderr
               , "       final_ab_diff = %2d, final_ac_diff = %2d, finalCost = %2d, finalState = %2d, finalDist = %2d\n"
               , final_ab_diff
               , final_ac_diff
               , finalCost
               , finalState
               , finalDist
               );

        fprintf(stderr, "Character to align at this step:\n");
        for (curCost = startDist; curCost < finalDist; curCost++) {
            fprintf(stderr, "%3c", globalCharacters->lesserStr[curCost]);
        }
        fprintf(stderr, "\n");
        for (curCost = startDist - start_ab_diff; curCost < finalDist - final_ab_diff; curCost++) {
            fprintf(stderr, "%3c", globalCharacters->longerStr[curCost]);
        }
        fprintf(stderr, "\n");
        for (curCost = startDist - start_ac_diff; curCost < finalDist - final_ac_diff; curCost++) {
            fprintf(stderr, "%3c", globalCharacters->middleStr[curCost]);
        }
        fprintf(stderr, "\n");
    }

    completeFromInfo = 0;

    costOffset += finalCost + 1;
    assert(costOffset > 0 && "Oops, overflow in costOffset");

    get_ukk_cell( start_ab_diff
                , start_ac_diff
                , startCost
                , startState
                , globalCharacters->numStates
                )->editDist     = startDist;

    get_ukk_cell( start_ab_diff
                , start_ac_diff
                , startCost
                , startState
                , globalCharacters->numStates
                )->computed = startCost + costOffset;

    if (finalCost - startCost <= checkPoint_width) { // Is it the base case?
        int curCost;
        completeFromInfo = 1;

        if (DEBUG_3D) {
            fprintf(stderr, "Base case.\n");
        }

        #if 0
            for (curCost = startCost; curCost <= finalCost; curCost++) {
                Ukk(final_ab_diff, final_ac_diff, curCost, 0);
            }

            assert( get_ukk_cell( final_ab_diff, final_ac_diff, finalCost, finalState)->editDist == finalDist );
        #else
        {
            int editDist;

            curCost = startCost - 1;

            do {
                curCost++;
                editDist = Ukk( final_ab_diff
                              , final_ac_diff
                              , curCost
                              , finalState
                              , globalCosts
                              , globalCharacters
                              , globalCostArrays
                              );
            } while (editDist < finalDist);

            assert(editDist == finalDist);
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
        traceBack( start_ab_diff
                 , start_ac_diff
                 , startCost
                 , startState
                 , final_ab_diff
                 , final_ac_diff
                 , finalCost
                 , finalState
                 , globalCharacters
                 );

        completeFromInfo = 0;
        return findBest_DistState( final_ab_diff
                                 , final_ac_diff
                                 , finalCost
                                 , 0
                                 , globalCharacters->numStates
                                 );
    }


    checkPoint_cost = (finalCost + startCost - checkPoint_width + 1) / 2;

    #if 0
        // Do the loop up to the desired cost.  Can't do Ukk(final_ab_diff,final_ac_diff,finalCost,finalState) directly (without
        // the loop) because the Umatrix is written to before it is actually needed.
        // Could be fixed, but this is also fine
        {
        int i;
        for (i=startCost; i<=finalCost; i++) {
          Ukk(final_ab_diff,final_ac_diff,i,0);
          //      Ukk(final_ab_diff,final_ac_diff,i,finalState);
        }
        assert(get_ukk_cell(final_ab_diff,final_ac_diff,finalCost,finalState)->editDist==finalDist);
        }
    #else
    {
        int editDist;

        curCost = startCost - 1;

        do {
            curCost++;
            // TODO: why am I updating editDist twice? Side effects?
            editDist = Ukk( final_ab_diff
                          , final_ac_diff
                          , curCost
                          , 0
                          , globalCosts
                          , globalCharacters
                          , globalCostArrays
                          );  // Need this (?) otherwise if finalState != 0 we may need larger than expected slice size.
            editDist = Ukk( final_ab_diff
                          , final_ac_diff
                          , curCost
                          , finalState
                          , globalCosts
                          , globalCharacters
                          , globalCostArrays
                          );

        } while (editDist < finalDist);

        assert(editDist == finalDist);
        if (curCost != finalCost) {
            fprintf(stderr, "Dist reached for cost %2d (old cost %2d)\n", curCost, finalCost);
            finalCost = curCost;
            assert(0);
        }
    }
    #endif

    return getSplitRecurse( start_ab_diff
                          , start_ac_diff
                          , startCost
                          , startState
                          , startDist
                          , final_ab_diff
                          , final_ac_diff
                          , finalCost
                          , finalState
                          , finalDist
                          , globalCosts
                          , globalCharacters
                          , globalCostArrays
                          );
}

/** Extracts info from the 'from' and CP info then recurses with doUkkInLimits for the two subparts.
 *  All edit distances and costs are signed, as often initialized to -INFINITY
 */
int getSplitRecurse( size_t               start_ab_diff
                   , size_t               start_ac_diff
                   , int                  startCost
                   , int                  startState
                   , int                  startDist
                   , size_t               final_ab_diff
                   , size_t               final_ac_diff
                   , int                  finalCost
                   , int                  finalState
                   , int                  finalDist
                   , global_costs_t      *globalCosts
                   , global_characters_t *globalCharacters
                   , global_arrays_t     *globalCostArrays
                   )
{
    // Get 'from' and checkPoint_ data.  Then recurse
    size_t finalLen;
    int    checkPoint_editDist;
    from_t finalCell;

    assert(    startCost >= 0
            && finalCost >= 0 );

    assert( get_ukk_cell( final_ab_diff
                        , final_ac_diff
                        , finalCost
                        , finalState
                        , globalCharacters->numStates
                        )->computed == finalCost + costOffset);

    finalCell = get_ukk_cell( final_ab_diff
                            , final_ac_diff
                            , finalCost
                            , finalState
                            , globalCharacters->numStates
                            )->from;

    assert( finalCell.cost >= 0 );

    if (get_checkPoint_cell( finalCell.ab_idx_diff
                           , finalCell.ac_idx_diff
                           , finalCell.cost
                           , finalCell.state
                           , globalCharacters->numStates
                           )->cost == 0
       ) {
        get_checkPoint_cell( finalCell.ab_idx_diff
                           , finalCell.ac_idx_diff
                           , finalCell.cost
                           , finalCell.state
                           , globalCharacters->numStates
                           )->cost = 1;
    }

    assert( get_checkPoint_cell( finalCell.ab_idx_diff
                               , finalCell.ac_idx_diff
                               , finalCell.cost
                               , finalCell.state
                               , globalCharacters->numStates
                               )->cost == finalCell.cost + 1
          );   // Use cost + 1 so can tell if not used (cost == 0)

    checkPoint_editDist = get_checkPoint_cell( finalCell.ab_idx_diff
                                             , finalCell.ac_idx_diff
                                             , finalCell.cost
                                             , finalCell.state
                                             , globalCharacters->numStates
                                             )->editDist;

    assert(checkPoint_editDist >= 0);

    if (DEBUG_3D) {
        fprintf( stderr
               , "checkPoint cost   = %2d checkPoint width = %2d\n"
               , checkPoint_cost
               , checkPoint_width
               );
        fprintf( stderr
               , "From: a b index difference = %2d a c index difference = %2d d = %2d s = %2d\n"
               , finalCell.ab_idx_diff
               , finalCell.ac_idx_diff
               , finalCell.cost
               , finalCell.state
               );
        fprintf( stderr
               , "checkPoint edit distance  = %2d\n"
               , checkPoint_editDist
               );
    }


    // Note: Doing second half of alignment first.  Only reason
    // for this is so the alignment is retrieved in exactly reverse order
    // making it easy to print out.
    finalLen = doUkkInLimits( finalCell.ab_idx_diff
                            , finalCell.ac_idx_diff
                            , finalCell.cost
                            , finalCell.state
                            , checkPoint_editDist
                            , final_ab_diff
                            , final_ac_diff
                            , finalCost
                            , finalState
                            , finalDist
                            , globalCosts
                            , globalCharacters
                            );

    // TODO: again, twice. Figure out why.
    doUkkInLimits( start_ab_diff
                 , start_ac_diff
                 , startCost
                 , startState
                 , startDist
                 , finalCell.ab_idx_diff
                 , finalCell.ac_idx_diff
                 , finalCell.cost
                 , finalCell.state
                 , checkPoint_editDist
                 , globalCosts
                 , globalCharacters
                 );

    if (DEBUG_3D) {
        fprintf( stderr, "Done.\n" );
    }

    //  return findBest_DistState(final_ab_diff,final_ac_diff,finalCost,0);
    return finalLen;
}

// -- Traceback routines --------------------------------------------------------------
void traceBack( int                  start_ab_diff
              , int                  start_ac_diff
              , int                  startCost
              , int                  startState
              , int                  final_ab_diff
              , int                  final_ac_diff
              , int                  finalCost
              , unsigned int         finalState
              , global_characters_t *globalCharacters
              )
{
    int ab_idx_diff  = final_ab_diff,
        ac_idx_diff  = final_ac_diff,
        editDistance = finalCost,
        state        = finalState;

    while (   ab_idx_diff  != start_ab_diff
           || ac_idx_diff  != start_ac_diff
           || editDistance != startCost
           || state        != startState
          ) {

        int a            = get_ukk_cell( ab_idx_diff
                                       , ac_idx_diff
                                       , editDistance
                                       , state
                                       , globalCharacters->numStates
                                       )->editDist;

        int nAB_idx_diff = get_ukk_cell( ab_idx_diff
                                       , ac_idx_diff
                                       , editDistance
                                       , state
                                       , globalCharacters->numStates
                                       )->from.ab_idx_diff;

        int nAC_idx_diff = get_ukk_cell( ab_idx_diff
                                       , ac_idx_diff
                                       , editDistance
                                       , state
                                       , globalCharacters->numStates
                                       )->from.ac_idx_diff;

        int nDistance    = get_ukk_cell( ab_idx_diff
                                       , ac_idx_diff
                                       , editDistance
                                       , state
                                       , globalCharacters->numStates
                                       )->from.cost;

        int nState       = get_ukk_cell( ab_idx_diff
                                       , ac_idx_diff
                                       , editDistance
                                       , state
                                       , globalCharacters->numStates
                                       )->from.state;

        int a1           = get_ukk_cell( nAB_idx_diff
                                       , nAC_idx_diff
                                       , nDistance
                                       , nState
                                       , globalCharacters->numStates
                                       )->editDist;


        int b  = a - ab_idx_diff,
            c  = a - ac_idx_diff,
            b1 = a1 - nAB_idx_diff,
            c1 = a1 - nAC_idx_diff;

        assert( get_ukk_cell( ab_idx_diff
                            , ac_idx_diff
                            , editDistance
                            , state
                            , globalCharacters->numStates
                            )->computed == editDistance  + costOffset
              );

        assert( get_ukk_cell( nAB_idx_diff
                             , nAC_idx_diff
                             , nDistance
                             , nState
                             , globalCharacters->numStates
                             )->computed == nDistance + costOffset
              );


        if (DEBUG_3D) {
            fprintf( stderr
                   , " ab_idx_diff = %3d,  ac_idx_diff = %3d,  editDistance = %3d,  state = %2d,  editDist = %3d, \n\
nAB_idx_diff = %3d, nAC_idx_diff = %3d, nDistance= %3d, nState = %2d, n_editDist = %3d\n\n"
                   , ab_idx_diff
                   , ac_idx_diff
                   , editDistance
                   , state
                   , a
                   , nAB_idx_diff
                   , nAC_idx_diff
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
          resultA[aCharIdx++] = globalCharacters->lesserStr[a];
          resultB[bCharIdx++] = globalCharacters->longerStr[b];
          resultC[cCharIdx++] = globalCharacters->middleStr[c];
          states[stateIdx++]  = 0;        /* The match state */
          cost[costIdx++]     = editDistance;
        }

        // The step for (nAB_idx_diff, nAC_idx_diff, nDistance, nState) -> (ab_idx_diff, ac_idx_diff, editDistance, state)
        if (   a != a1
            || b != b1
            || c != c1
           ) {
            if (a > a1) resultA[aCharIdx++] = globalCharacters->lesserStr[--a];
            else        resultA[aCharIdx++] = '-';

            if (b > b1) resultB[bCharIdx++] = globalCharacters->longerStr[--b];
            else        resultB[bCharIdx++] = '-';

            if (c > c1) resultC[cCharIdx++] = globalCharacters->middleStr[--c];
            else        resultC[cCharIdx++] = '-';

            states[stateIdx++] = state;
            cost[costIdx++]    = editDistance;
        }

        assert(    a == a1
                && b == b1
                && c == c1);

        ab_idx_diff = nAB_idx_diff;
        ac_idx_diff = nAC_idx_diff;
        editDistance = nDistance;
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

    assert(ab_idx_diff       == start_ab_diff);
    assert(ac_idx_diff       == start_ac_diff);
    assert(editDistance == startCost);
    assert(state    == startState);
}

int char_to_base (char v) {   // TODO: Can I just skip this?
    if      ('A' == v) return 1;
    else if ('C' == v) return 2;
    else if ('G' == v) return 4;
    else if ('T' == v) return 8;
    else if ('-' == v) return 16;
    else return -1;
}

void printTraceBack( dyn_character_t     *retLesserChar
                   , dyn_character_t     *retMiddleChar
                   , dyn_character_t     *retLongerChar
                   , global_costs_t      *globalCosts
                   , global_characters_t *globalCharacters
                   )
{
    // Print out the alignment

    // Add the first run of matches to the alignment
    // NB. The first run of matches must be added in reverse order.

    size_t endRun,
           i = 0;

    int j; // countdown to 0 later, so much be signed

    while (   i < globalCharacters->lesserLen
           && (   globalCharacters->lesserStr[i] == globalCharacters->longerStr[i]
               && globalCharacters->lesserStr[i] == globalCharacters->middleStr[i])
           ) {
      i++;
    }
    endRun = i;

    for (j = endRun - 1; j >= 0; j--)  {
      resultA[aCharIdx++] = globalCharacters->lesserStr[j];
      resultB[bCharIdx++] = globalCharacters->longerStr[j];
      resultC[cCharIdx++] = globalCharacters->middleStr[j];
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
      dyn_char_prepend (retLesserChar, char_to_base (resultA[j]));
      dyn_char_prepend (retMiddleChar, char_to_base (resultB[j]));
      dyn_char_prepend (retLongerChar, char_to_base (resultC[j]));
    }
    dyn_char_prepend (retLesserChar, 16);
    dyn_char_prepend (retMiddleChar, 16);
    dyn_char_prepend (retLongerChar, 16);

    assert(   aCharIdx == bCharIdx
           && aCharIdx == cCharIdx
           && aCharIdx == stateIdx
           && aCharIdx == costIdx);

    checkAlign(resultA, aCharIdx, globalCharacters->lesserStr, globalCharacters->lesserLen);
    checkAlign(resultB, bCharIdx, globalCharacters->longerStr, globalCharacters->longerLen);
    checkAlign(resultC, cCharIdx, globalCharacters->middleStr, globalCharacters->middleLen);

    assert( alignmentCost(states, resultA, resultB, resultC, aCharIdx, globalCosts) == finalCost );
}

// Find the furthest edit distance at ab_idx_diff, ac_idx_diff, d. return_the_state selects whether the
// best edit distance is returned, or the best final state (needed for ukk.alloc traceback)
int findBest_DistState( int    ab_idx_diff
                      , int    ac_idx_diff
                      , int    input_editDist
                      , int    return_the_state
                      , size_t numStates
                      )
{
    int best_editDist = -INFINITY;
    int bestState     = -1;

    for (size_t curState = 0; curState < numStates; curState++) {
        if (    ( get_ukk_cell( ab_idx_diff
                              , ac_idx_diff
                              , input_editDist
                              , curState
                              , numStates
                              )->computed == input_editDist + costOffset )
             && ( get_ukk_cell( ab_idx_diff
                              , ac_idx_diff
                              , input_editDist
                              , curState
                              , numStates
                              )->editDist > best_editDist )
           ) {
          best_editDist  = get_ukk_cell( ab_idx_diff
                              , ac_idx_diff
                              , input_editDist
                              , curState
                              , numStates
                              )->editDist;
          bestState = curState;
        }
    }

/*
    fprintf( stderr
           , "findBest_DistState(%2d, %2d, %2d, (%2d)) = %2d\n"
           , ab_idx_diff
           , ac_idx_diff
           , input_editDist
           , bestState
           , best_editDist
           );
*/
    if (return_the_state) {
        return bestState;
    } else {
        return best_editDist;
    }
}


int Ukk( int                  ab_idx_diff
       , int                  ac_idx_diff
       , int                  editDistance
       , unsigned int         state
       , global_costs_t      *globalCosts
       , global_characters_t *globalCharacters
       , global_arrays_t     *globalCostArrays
       )
{
    if ( !withinMatrix(ab_idx_diff, ac_idx_diff, editDistance, globalCosts) ) {
        return -INFINITY;
    }
    if (  get_ukk_cell( ab_idx_diff
                      , ac_idx_diff
                      , editDistance
                      , state
                      , globalCharacters->numStates
                      )->computed == editDistance + costOffset
        ) {
        return  get_ukk_cell( ab_idx_diff
                            , ac_idx_diff
                            , editDistance
                            , state
                            , globalCharacters->numStates
                            )->editDist;
    }

/*
    fprintf( stderr
           ,"Calculating get_ukk_cell(%2d, %2d, %2d, %2d)\n"
           , ab_idx_diff
           , ac_idx_diff
           , editDistance
           , state)
           ;
*/
    counts.cells++;

    calcUkk( ab_idx_diff
           , ac_idx_diff
           , editDistance
           , state
           , globalCosts
           , globalCharacters
           , globalCostArrays
           );

    // Store away check point from info in necessary
    if (     editDistance >= checkPoint_cost
         && (editDistance  < checkPoint_cost + checkPoint_width )
       ) {
        get_checkPoint_cell( ab_idx_diff
                           , ac_idx_diff
                           , editDistance
                           , state
                           , globalCharacters->numStates
                           )->editDist = get_ukk_cell( ab_idx_diff
                                                     , ac_idx_diff
                                                     , editDistance
                                                     , state
                                                     , globalCharacters->numStates
                                                     )->editDist;

        get_checkPoint_cell( ab_idx_diff
                           , ac_idx_diff
                           , editDistance
                           , state
                           , globalCharacters->numStates
                           )->cost = editDistance + 1;          // Note adding 1 so cost ==0 signifies unused cell
    }

    if ( get_ukk_cell( ab_idx_diff
                     , ac_idx_diff
                     , editDistance
                     , state
                     , globalCharacters->numStates
                     )->editDist > furthestReached
       ) {
        furthestReached = get_ukk_cell( ab_idx_diff
                                      , ac_idx_diff
                                      , editDistance
                                      , state
                                      , globalCharacters->numStates
                                      )->editDist;
    }

    return get_ukk_cell( ab_idx_diff
                       , ac_idx_diff
                       , editDistance
                       , state
                       , globalCharacters->numStates
                       )->editDist;
}

// IMPORTANT!!! Order of input characters is short, long, middle.
int doUkk( dyn_character_t     *retLesserChar
         , dyn_character_t     *retMiddleChar
         , dyn_character_t     *retLongerChar
         , global_costs_t      *globalCosts
         , global_characters_t *globalCharacters
         , global_arrays_t     *globalCostArrays
         )
{
    checkPoint_cell_t *checkPoint_dummyCell = malloc( sizeof(checkPoint_cell_t) );

    checkPoint_dummyCell->editDist = 0;
    checkPoint_dummyCell->cost     = 0;

    ukk_cell_t *UdummyCell = malloc( sizeof(ukk_cell_t) );

    UdummyCell->editDist         = 0;
    UdummyCell->computed         = 0;
    UdummyCell->from.ab_idx_diff = 0;
    UdummyCell->from.ac_idx_diff = 0;
    UdummyCell->from.cost        = 0;
    UdummyCell->from.state       = 0;

    finalCost        = 0;
    checkPoint_cost  = 0;
    checkPoint_width = 0;
    completeFromInfo = 0;

    aCharIdx = 0;
    bCharIdx = 0;
    cCharIdx = 0;
    stateIdx = 0;
    costIdx  = 0;

    costOffset      =  1;
    furthestReached = -1;

    start_ab_diffG = 0;
    start_ac_diffG = 0;
    startCostG     = 0;
    startStateG    = 0;

    size_t curDist,
           final_ab_diff,
           final_ac_idx_diff,
           startDist;

    checkPoint_width = globalCharacters->maxSingleStep;

    // TODO: what is the correct value to use for Umatrix depth.
    // Would think that MAX_SINGLE_COST = maxSingleStep * 2 would be enough
    // but doesn't seem to be. No idea why. *shrug*
    myUkk_allocInfo     = allocInit( sizeof(ukk_cell_t)
                                   , checkPoint_width * 2
                                   , globalCharacters
                                   );
    myCheckPt_allocInfo = allocInit( sizeof(checkPoint_cell_t)
                                   , checkPoint_width
                                   , globalCharacters
                                   );

    counts.cells     = 0;
    counts.innerLoop = 0;

    // Calculate starting position
    curDist = 0;
    while (   curDist < globalCharacters->lesserLen
           && (   globalCharacters->lesserStr[curDist] == globalCharacters->longerStr[curDist]
               && globalCharacters->lesserStr[curDist] == globalCharacters->middleStr[curDist]
              )
          ) {
        curDist++;
        counts.innerLoop++;
    }
    get_ukk_cell( 0, 0, 0, 0, globalCharacters->numStates )->editDist = curDist;
    get_ukk_cell( 0, 0, 0, 0, globalCharacters->numStates )->computed = 0 + costOffset;
    startDist = curDist;

    final_ab_diff    = globalCharacters->lesserLen - globalCharacters->longerLen;
    final_ac_idx_diff = globalCharacters->lesserLen - globalCharacters->middleLen;
    endA             = globalCharacters->lesserLen;
    endB             = globalCharacters->longerLen;
    endC             = globalCharacters->middleLen;

    checkPoint_editDist = 1;
    checkPoint_cost     = INFINITY;
    do {
        curDist++;
        Ukk( final_ab_diff
           , final_ac_idx_diff
           , curDist
           , 0
           , globalCosts
           , globalCharacters
           , globalCostArrays
           );

        if (DEBUG_3D) {
            fprintf(stderr, "Furthest reached for cost %2zu is %2d.\n",
                    curDist, furthestReached);
        }

        int half_lesserLen = (int) (globalCharacters->lesserLen / 2);
        if (checkPoint_editDist && furthestReached >= half_lesserLen) {
            checkPoint_cost     = curDist + 1;
            checkPoint_editDist = 0;

            if (DEBUG_3D) {
                fprintf(stderr, "Setting checkPoint_cost: %2d\n", checkPoint_cost);
            }
        }


    } while (findBest_DistState( final_ab_diff
                               , final_ac_idx_diff
                               , curDist
                               , 0
                               , globalCharacters->numStates
                               ) < (int) globalCharacters->lesserLen);

    assert( findBest_DistState( final_ab_diff
                              , final_ac_idx_diff
                              , curDist
                              , 0
                              , globalCharacters->numStates
                              ) == (int) globalCharacters->lesserLen);

    checkPoint_editDist = 0;
    finalCost           = curDist;

    // Recurse for alignment
    int finalState = findBest_DistState( final_ab_diff
                                       , final_ac_idx_diff
                                       , finalCost
                                       , 1
                                       , globalCharacters->numStates
                                       );
    size_t editDist;

    if ( get_ukk_cell( final_ab_diff
                     , final_ac_idx_diff
                     , finalCost
                     , finalState
                     , globalCharacters->numStates
                     )->from.cost <= 0) {

        // We check pointed too late on this first pass.
        // So we got no useful information.  Oh well, have to do it all over again
        assert( get_ukk_cell( final_ab_diff
                            , final_ac_idx_diff
                            , finalCost
                            , finalState
                            , globalCharacters->numStates
                            )->computed == finalCost + costOffset );

        editDist = doUkkInLimits( 0
                                , 0
                                , 0
                                , 0
                                , startDist
                                , final_ab_diff
                                , final_ac_idx_diff
                                , finalCost
                                , finalState
                                , globalCharacters->lesserLen
                                , globalCosts
                                , globalCharacters
                                );
    } else {
        // Use the 'from' info and do the two sub parts.
        editDist = getSplitRecurse( 0
                                  , 0
                                  , 0
                                  , 0
                                  , startDist
                                  , final_ab_diff
                                  , final_ac_idx_diff
                                  , finalCost
                                  , finalState
                                  , globalCharacters->lesserLen
                                  , globalCosts
                                  , globalCharacters
                                  , globalCostArrays
                                  );
    }

    assert(editDist == globalCharacters->lesserLen);
    printTraceBack( retLesserChar
                  , retMiddleChar
                  , retLongerChar
                  , globalCosts
                  , globalCharacters
                  );

    allocFinal( &myUkk_allocInfo
              , &(UdummyCell->computed)
              , UdummyCell
              , globalCharacters->numStates
              );

    allocFinal( &myCheckPt_allocInfo
              , &(checkPoint_dummyCell->cost)
              , checkPoint_dummyCell
              , globalCharacters->numStates
              );

    printf("doUkk: editDist: = %2zu\n", curDist);
    return (int) curDist;
}

int calcUkk( int                  ab_idx_diff
           , int                  ac_idx_diff
           , int                  input_editDist
           , int                  toState
           , global_costs_t      *globalCosts
           , global_characters_t *globalCharacters
           , global_arrays_t     *globalCostArrays
           )
{
    if (DEBUG_CALL_ORDER) {
        printf("--ukk.check-point: calcUkk\n" );
        fflush(stdout);
    }

    // TODO: document all of these
    int neighbour = (globalCostArrays->neighbours)[toState];
    int da,
        db,
        dc,
        ab_idx_diff1,
        ac_idx_diff1,
        a1,
        a2,
        start_transitionCost,
        best_editDist,
        fromCost,
        editDist,
        curCost,
        this_editDist;

    from_t from;

    from.cost = -1;

    if (DEBUG_CALL_ORDER) {
        indent[indenti] = 0;

        fprintf( stderr
               , "%s CalcUKK(ab_idx_diff = %2d, ac_idx_diff = %2d, input_editDist = %2d, toState = %2d)\n"
               , indent
               , ab_idx_diff
               , ac_idx_diff
               , input_editDist
               , toState
               );

        indent[indenti++] = ' ';
        indent[indenti++] = ' ';
        indent[indenti]   = 0;
    }

    assert( get_ukk_cell( ab_idx_diff
                        , ac_idx_diff
                        , input_editDist
                        , toState
                        , globalCharacters->numStates
                        )->computed < input_editDist + costOffset
           );

    best_editDist = -INFINITY;

    // Initialise checkPoint_ from info if necessary
    if (    input_editDist >= checkPoint_cost
         && input_editDist  < checkPoint_cost + checkPoint_width
       ) {
        from.ab_idx_diff = ab_idx_diff;
        from.ac_idx_diff = ac_idx_diff;
        from.cost        = input_editDist;
        from.state       = toState;
    }

    step( neighbour
        , &da
        , &db
        , &dc
        );

    ab_idx_diff1 = ab_idx_diff - da + db;
    ac_idx_diff1 = ac_idx_diff - da + dc;

    // calculate if it's a valid diagonal
    if (    ab_idx_diff1 >= -endB
         && ab_idx_diff1 <=  endA
         && ac_idx_diff1 >= -endC
         && ac_idx_diff1 <=  endA
       ) {

        // Loop over possible state we are moving from
        //   May be possible to limit this?
        for (size_t fromState = 0; fromState < globalCharacters->numStates; fromState++) {
            start_transitionCost = stateTransitionCost( fromState, toState );

            fromCost      = -INFINITY;
            editDist      = -INFINITY;

            curCost       = input_editDist - start_transitionCost - globalCostArrays->contCost[toState];
            a2            = -1;
            a1            = Ukk( ab_idx_diff1
                                , ac_idx_diff1
                                , curCost
                                , fromState
                                , globalCosts
                                , globalCharacters
                                , globalCostArrays
                                );

            // printf("a1: %d, da: %d, endA: %d\n", a1, da, endA);
            // printf("b1: %d, db: %d, endB: %d\n", a1, da, endA);
            // printf("c1: %d, dc: %d, endC: %d\n", a1, da, endA);
            if (    okIndex( a1      , da, endA )
                 && okIndex( a1 - ab_idx_diff1, db, endB )
                 && okIndex( a1 - ac_idx_diff1, dc, endC )
                 && ( whichCharCost( da ? globalCharacters->lesserStr[a1]              : '-',
                                     db ? globalCharacters->longerStr[a1-ab_idx_diff1] : '-',
                                     dc ? globalCharacters->middleStr[a1-ac_idx_diff1] : '-' ) == 1 )
               ) {
                fromCost = curCost;
                editDist = a1 + da;
            } else {
                if (!(globalCostArrays->secondCost)[toState]) {
                    continue;
                }

                a2 = Ukk( ab_idx_diff1
                        , ac_idx_diff1
                        , curCost - globalCosts->mismatchCost
                        , fromState
                        , globalCosts
                        , globalCharacters
                        , globalCostArrays
                        );

                if (   okIndex(a2,                da, endA)
                    && okIndex(a2 - ab_idx_diff1, db, endB)
                    && okIndex(a2 - ac_idx_diff1, dc, endC)
                   ) {
                    fromCost = curCost - globalCosts->mismatchCost;
                    editDist = a2 + da;
                }
            }

            // Check if this is an improvment
            if (best_editDist < editDist) {
                best_editDist = editDist;

                if (completeFromInfo) {        // Do we need to store complete from information for a base case?
                    from.ab_idx_diff    = ab_idx_diff1;
                    from.ac_idx_diff    = ac_idx_diff1;
                    from.cost  = fromCost;
                    from.state = fromState;
                } else if (input_editDist >= checkPoint_cost + checkPoint_width) { // Store from info for checkPoint_
                      from = get_ukk_cell( ab_idx_diff1
                                         , ac_idx_diff1
                                         , fromCost
                                         , fromState
                                         , globalCharacters->numStates
                                         )->from;
                }
            }
        } // End loop over from states
    } // End if valid neighbour

    // Insure that we have how we can reach for AT MOST cost input_editDist

    editDist = Ukk( ab_idx_diff
                  , ac_idx_diff
                  , input_editDist - 1
                  , toState
                  , globalCosts
                  , globalCharacters
                  , globalCostArrays
                  );

    // Check if this is an improvment
    if (okIndex(editDist,               0, endA) &&
        okIndex(editDist - ab_idx_diff, 0, endB) &&
        okIndex(editDist - ac_idx_diff, 0, endC) &&
        best_editDist < editDist)
    {
        best_editDist = editDist;

        if (completeFromInfo) {        // Do we need to store complete from information for a base case?
            from.ab_idx_diff = ab_idx_diff;
            from.ac_idx_diff = ac_idx_diff;
            from.cost        = input_editDist - 1;
            from.state       = toState;

        } else if (input_editDist >= checkPoint_cost + checkPoint_width) { // Store from info for checkPoint_
            from = get_ukk_cell( ab_idx_diff
                               , ac_idx_diff
                               , input_editDist - 1
                               , toState
                               , globalCharacters->numStates
                               )->from;
        }
    } // end insure that we have how we can reach for AT MOST cost input_editDist

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
        int editDist   = -INFINITY;
        int from_state = -1;

        for (size_t curState = 0; curState < globalCharacters->numStates; curState++) {
            this_editDist = (curState == 0) ? best_editDist
                                            : Ukk( ab_idx_diff
                                                 , ac_idx_diff
                                                 , input_editDist
                                                 , curState
                                                 , globalCosts
                                                 , globalCharacters
                                                 , globalCostArrays
                                                 );

            if (this_editDist > editDist) {
                editDist   = this_editDist;
                from_state = curState;
            }
        }

        // Try to extend to diagonal
        while (   okIndex(editDist, 1, endA)
               && okIndex(editDist - ab_idx_diff, 1, endB)
               && okIndex(editDist - ac_idx_diff, 1, endC)
               && (   globalCharacters->lesserStr[editDist] == globalCharacters->longerStr[editDist - ab_idx_diff]
                   && globalCharacters->lesserStr[editDist] == globalCharacters->middleStr[editDist - ac_idx_diff] )
              ) {
          editDist++;
          counts.innerLoop++;
        }

        // Was there an improvement?
        if (editDist > best_editDist) {
            best_editDist = editDist;  // Note: toState = MMM

            // Update 'from' information if the state we extended from was
            // not the same state we are in (the MMM state).
            if (from_state != 0) {
                if (completeFromInfo) {        // TODO: Do we need to store complete 'from' information for a base case?
                    from.ab_idx_diff = ab_idx_diff;
                    from.ac_idx_diff = ac_idx_diff;
                    from.cost        = input_editDist;
                    from.state       = from_state;
                } else if (input_editDist >= checkPoint_cost + checkPoint_width) { // Store from info for checkPoint_
                    from = get_ukk_cell( ab_idx_diff
                                       , ac_idx_diff
                                       , input_editDist
                                       , from_state
                                       , globalCharacters->numStates
                                       )->from;
                }
            }
        }
    } // End attempt to extend diagonal on a run of matches

    assert( get_ukk_cell( ab_idx_diff
                        , ac_idx_diff
                        , input_editDist
                        , toState
                        , globalCharacters->numStates
                        )->computed < input_editDist + costOffset
          );

    get_ukk_cell( ab_idx_diff
                , ac_idx_diff
                , input_editDist
                , toState
                , globalCharacters->numStates
                )->editDist = best_editDist;

    get_ukk_cell( ab_idx_diff
                , ac_idx_diff
                , input_editDist
                , toState
                , globalCharacters->numStates
                )->computed = input_editDist + costOffset;

    get_ukk_cell( ab_idx_diff
                , ac_idx_diff
                , input_editDist
                , toState
                , globalCharacters->numStates
                )->from = from;


    if (DEBUG_CALL_ORDER) {
        indenti        -= 2;
        indent[indenti] = 0;
        fprintf( stderr, "%sCalcUKK(ab_idx_diff = %2d, ac_idx_diff = %2d, d = %2d,    toState = %2d) = %2d\n"
               , indent
               , ab_idx_diff
               , ac_idx_diff
               , input_editDist
               , toState
               , get_ukk_cell( ab_idx_diff
                             , ac_idx_diff
                             , input_editDist
                             , toState
                             , globalCharacters->numStates
                             )->editDist
               );

        fprintf( stderr, "%sFrom:   ab_idx_diff = %2d, ac_idx_diff = %2d, cost = %2d, state = %2d\n",
                 indent
               , get_ukk_cell( ab_idx_diff
                             , ac_idx_diff
                             , input_editDist
                             , toState
                             , globalCharacters->numStates
                             )->from.ab_idx_diff

               , get_ukk_cell( ab_idx_diff
                             , ac_idx_diff
                             , input_editDist
                             , toState
                             , globalCharacters->numStates
                             )->from.ac_idx_diff

               , get_ukk_cell( ab_idx_diff
                             , ac_idx_diff
                             , input_editDist
                             , toState
                             , globalCharacters->numStates
                             )->from.cost

               , get_ukk_cell( ab_idx_diff
                             , ac_idx_diff
                             , input_editDist
                             , toState
                             , globalCharacters->numStates
                             )->from.state

               );
    }

    return get_ukk_cell( ab_idx_diff
                       , ac_idx_diff
                       , input_editDist
                       , toState
                       , globalCharacters->numStates
                       )->editDist;
}

