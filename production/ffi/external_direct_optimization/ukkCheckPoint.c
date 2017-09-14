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
// used to store which cost (actually d + costOffset_global) the cell contains
// instead of simply whether the cell has been computed or not.

// NOTE: all ab and ac refs below are actually lesser_longer and lesser_middle

#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "costMatrix.h"
#include "debug_constants.h"
#include "dyn_character.h"
#include "ukkCheckPoint.h"
#include "ukkCommon.h"

#define MAXINT INT_MAX
// #define FIXED_NUM_PLANES TODO: this is also defined in ukkCommon.h; it was commented out, but then allocInit() failed to compile


// TODO: globals--it'd be nice to clean these up a little

alignment_mtx_t myUkk_allocInfo_global;
alignment_mtx_t myCheckPt_allocInfo_global;


// `costOffset_global`---added to the `computed` field of each cell. `costOffset_global` is
// recursive step of the check point algorithm. 'Tis really a hack so I don't
// have to reinitialize the memory structures.
long costOffset_global = 1;    // must be signed for future comparisons
unsigned int finalCost_global;

int furthestReached_global = -1;
int checkPoint_editDist_global;   // Flag for whether to use edit distance of cost as the check pointing criterion.
                                  // Check pointing on the edit distance is only done for first iteration, when the final
                                  // cost is unknown.


// Use these globals cause don't want to pass 'em around, and they're needed
// by the withinMatrix func. Be nice to have closures :-)
// Must be signed; later used as possibly negative.
int start_lessLong_idx_diff_global = 0,
    start_lessMidd_idx_diff_global = 0,
    startCost_global               = 0,
    startState_global              = 0;


// Set to 1 for base cases, so that the 'from' info that the alignment
// can be retrieved from is set.
int completeFromInfo_global = 0;

int checkPoint_width_global;
int checkPoint_cost_global;

counts_t counts_global;

int fsm_stateIdx_global = 0,
    costIdx_global      = 0;

int  fsm_states_global[MAX_STR * 2],
     cost_global[MAX_STR * 2];


/************* next two functions are static inline, so not in .h file. ******************/

/** Returns the cell in the Ukkonnen matrix determined by `lessLong_idx_diff`, `lesMidd_idx_diff` and `editDistance`.
 *
 *  Calls getPtr, so might reallocate.
 */
static inline ukk_cell_t *get_ukk_cell( int    lessLong_idx_diff
                                      , int    lessMidd_idx_diff
                                      , int    editDistance
                                      , int    fsm_state
                                      , size_t numStates
                                      )
{
    return getPtr( &myUkk_allocInfo_global
                 ,  lessLong_idx_diff
                 ,  lessMidd_idx_diff
                 ,  editDistance
                 ,  fsm_state
                 ,  numStates
                 );
}



/** Returns the cell in the Ukkonnen matrix determined by `lessLong_idx_diff`, `lesMidd_idx_diff` and `editDistance`.
 *
 *  Calls getPtr, so might reallocate.
 */
static inline checkPoint_cell_t *get_checkPoint_cell( int    lessLong_idx_diff
                                                    , int    lessMidd_idx_diff
                                                    , int    editDistance
                                                    , int    fsm_state
                                                    , size_t numStates
                                                    )
{
    return getPtr( &myCheckPt_allocInfo_global
                 ,  lessLong_idx_diff
                 ,  lessMidd_idx_diff
                 ,  editDistance
                 ,  fsm_state
                 ,  numStates
                 );
}


int powell_3D_align( dyn_character_t *lesserChar
                   , dyn_character_t *middleChar
                   , dyn_character_t *longerChar
                   , dyn_character_t *retLesserChar
                   , dyn_character_t *retMiddleChar
                   , dyn_character_t *retLongerChar
                   , unsigned int     mismatchCost
                   , unsigned int     gapOpenCost
                   , unsigned int     gapExtendCost
                   )
{
    if (DEBUG_CALL_ORDER) {
        printf("powell_3D_align\n");
    }
    printf("short: %zu, medium: %zu, long: %zu\n", lesserChar->len, middleChar->len, longerChar->len);

    // Allocate global costs, characters and cost arrays. These will be initialized in setup().
    affine_costs_t *affineCosts    = malloc( sizeof(affine_costs_t) );

    characters_t *inputChars       = malloc( sizeof(characters_t) );

    characters_t *resultChars      = malloc( sizeof(characters_t) );

    fsm_arrays_t *fsmStateArrays = malloc( sizeof(fsm_arrays_t) );

    // TODO: should be able to forego this. Double check that that's the case.
    assert (   mismatchCost != 0
         /* && gapOpenCost >= 0 Now is an unsigned int */
            && gapExtendCost != 0
           );

    setup( affineCosts
         , inputChars
         , resultChars
         , fsmStateArrays
         , lesserChar
         , middleChar
         , longerChar
         , mismatchCost
         , gapOpenCost
         , gapExtendCost
         );

    return align3d_ukk( retLesserChar
                      , retMiddleChar
                      , retLongerChar
                      , lesserChar
                      , middleChar
                      , longerChar
                      , affineCosts
                      , inputChars
                      , resultChars
                      , fsmStateArrays
                      );
}


/** Selection sort an array of `len` values. Small set of fsm states, so selection sort is fine. */
static inline void sort( int    values[]
                       , size_t len
                       )
{
    int tempVal,
        minI;

    for (size_t i = 0; i < len; i++) {
        minI = i;

        for (size_t j = i + 1; j < len; j++) {
            if (values[j] < values[minI]) {
                minI = j;
            }
        }
        tempVal      = values[i];
        values[i]    = values[minI];
        values[minI] = tempVal;
    }
}


/**  */
static inline int withinMatrix( int             lessLong_idx_diff
                              , int             lessMidd_idx_diff
                              , int             distance
                              , affine_costs_t *affineCosts
                              )
{
    // The new method for checking the boundary condition.  Much tighter ~20%(?)  -- 28/02/1999
    int longMidd_idx_diff = lessMidd_idx_diff - lessLong_idx_diff;
    int aval[3];
    int g,
        h,
        cheapest;

    if (distance < 0) return 0;

    aval[0] = abs(start_lessLong_idx_diff_global - lessLong_idx_diff);
    aval[1] = abs(start_lessMidd_idx_diff_global - lessMidd_idx_diff);
    aval[2] = abs(start_lessMidd_idx_diff_global - start_lessLong_idx_diff_global - longMidd_idx_diff);

    // Set g and h to the smallest and second smallest of aval[] respectively.
    sort( aval, 3 );
    g = aval[0];
    h = aval[1];

    if (startState_global == 0) {
        // We know a good boundary check if the start fsm state is MMM.
        cheapest = (g == 0 ? 0
                           : affineCosts->gapOpenCost + g * affineCosts->gapExtendCost)
                 + (h == 0 ? 0
                           : affineCosts->gapOpenCost + h * affineCosts->gapExtendCost);
    } else {
        // If start fsm state is something else, can't charge for start of gaps unless we
        // do something more clever.
        cheapest = (g == 0 ? 0 : g * affineCosts->gapExtendCost)
                 + (h == 0 ? 0 : h * affineCosts->gapExtendCost);
    }

    if   (cheapest + startCost_global > distance) return 0;
    else                                          return 1;

}

/** For Ukkonen check point between to specified points in the Ukkonnen matrix... TODO: ...?
 *  All edit distances and costs are signed, as often initialized to -INFINITY
 */
int doUkkInLimits( int             start_lessLong_idx_diff
                 , int             start_lessMidd_idx_diff
                 , int             startCost
                 , int             startState
                 , int             start_editDist
                 , int             final_lessLong_idx_diff
                 , int             final_lessMidd_idx_diff
                 , int             finalCost_global
                 , int             finalState
                 , int             finalDist
                 , affine_costs_t *affineCosts
                 , characters_t   *inputChars
                 , characters_t   *resultChars
                 , fsm_arrays_t   *fsmStateArrays
                 )
{
    assert( startCost >= 0 && finalCost_global >= 0 );

    start_lessLong_idx_diff_global = start_lessLong_idx_diff;
    start_lessMidd_idx_diff_global = start_lessMidd_idx_diff;
    startCost_global               = startCost;
    startState_global              = startState;
    inputChars->lesserLen          = finalDist;
    inputChars->longerLen          = finalDist - final_lessLong_idx_diff;
    inputChars->middleLen          = finalDist - final_lessMidd_idx_diff;

    int editDist, curCost;

    if (DEBUG_3D) {
        fprintf(stderr
               , "doUkkInLimits (start_lessLong_idx_diff = %2d, start_lessMidd_idx_diff = %2d, startCost = %d,         startState = %2d, start_editDist = %2d\n"
               , start_lessLong_idx_diff
               , start_lessMidd_idx_diff
               , startCost
               , startState
               , start_editDist
               );
        fprintf(stderr
               , "               final_lessLong_idx_diff = %2d, final_lessMidd_idx_diff = %2d, finalCost_global = %2d, finalState = %2d, finalDist = %2d\n"
               , final_lessLong_idx_diff
               , final_lessMidd_idx_diff
               , finalCost_global
               , finalState
               , finalDist
               );

        fprintf(stderr, "Character to align at this step:\n");
        fprintf(stderr, "Short:\n");
        for (curCost = start_editDist; curCost < finalDist; curCost++) {
            fprintf(stderr, "%3c", inputChars->lesserStr[curCost]);
        }
        fprintf(stderr, "\n");
        fprintf(stderr, "Long:\n");
        for (curCost = start_editDist - start_lessLong_idx_diff; curCost < finalDist - final_lessLong_idx_diff; curCost++) {
            fprintf(stderr, "%3c", inputChars->longerStr[curCost]);
        }
        fprintf(stderr, "\n");
        fprintf(stderr, "Middle:\n");
        for (curCost = start_editDist - start_lessMidd_idx_diff; curCost < finalDist - final_lessMidd_idx_diff; curCost++) {
            fprintf(stderr, "%3c", inputChars->middleStr[curCost]);
        }
        fprintf(stderr, "\n");
    }

    completeFromInfo_global = 0;

    costOffset_global += finalCost_global + 1;
    assert(costOffset_global > 0 && "Oops, overflow in costOffset_global");

    get_ukk_cell( start_lessLong_idx_diff
                , start_lessMidd_idx_diff
                , startCost
                , startState
                , inputChars->numStates
                )->editDist = start_editDist;

    get_ukk_cell( start_lessLong_idx_diff
                , start_lessMidd_idx_diff
                , startCost
                , startState
                , inputChars->numStates
                )->computed = startCost + costOffset_global;

    if (finalCost_global - startCost <= checkPoint_width_global) { // Is it the base case?
        completeFromInfo_global = 1;

        if (DEBUG_3D) {
            fprintf(stderr, "Base case.\n");
        }

        // #if 0
        //     for (curCost = startCost; curCost <= finalCost_global; curCost++) {
        //         Ukk(final_lessLong_idx_diff, final_lessMidd_idx_diff, curCost, 0);
        //     }

        //     assert( get_ukk_cell( final_lessLong_idx_diff, final_lessMidd_idx_diff, finalCost_global, finalState)->editDist == finalDist );
        // #else
        { // scoped because of commented-out #if #else directives
           // int editDist;

            curCost = startCost - 1;

            do {
                curCost++;
                editDist = Ukk( final_lessLong_idx_diff
                              , final_lessMidd_idx_diff
                              , curCost
                              , finalState
                              , affineCosts
                              , inputChars
                              , fsmStateArrays
                              );
            } while (editDist < finalDist);

            assert(editDist == finalDist);
            if (curCost != finalCost_global) {
                fprintf(stderr, "Dist reached for cost %d (old cost %d)\n", curCost, finalCost_global);
                finalCost_global = curCost;
                assert(0);
            }
        } // end scope
        // #endif

        if (DEBUG_3D) {
            fprintf(stderr,"Tracing back in base case.\n");
        }

        traceBack( start_lessLong_idx_diff
                 , start_lessMidd_idx_diff
                 , startCost
                 , startState
                 , final_lessLong_idx_diff
                 , final_lessMidd_idx_diff
                 , finalCost_global
                 , finalState
                 , inputChars
                 , resultChars
                 );

        completeFromInfo_global = 0;
        return find_bestDist( final_lessLong_idx_diff
                            , final_lessMidd_idx_diff
                            , finalCost_global
                            , inputChars->numStates
                            );
    }


    checkPoint_cost_global = (finalCost_global + startCost - checkPoint_width_global + 1) / 2;

    // #if 0
        // Do the loop up to the desired cost.  Can't do Ukk(final_lessLong_idx_diff,final_lessMidd_idx_diff,finalCost_global,finalState) directly (without
        // the loop) because the Umatrix is written to before it is actually needed.
        // Could be fixed, but this is also fine
        // {
        // int i;
        // for (i=startCost; i<=finalCost_global; i++) {
        //   Ukk(final_lessLong_idx_diff,final_lessMidd_idx_diff,i,0);
        //   //      Ukk(final_lessLong_idx_diff,final_lessMidd_idx_diff,i,finalState);
        // }
        // assert(get_ukk_cell(final_lessLong_idx_diff,final_lessMidd_idx_diff,finalCost_global,finalState)->editDist==finalDist);
        // }
    // #else
    {  // scoped because of commented-out #if #else directives
        curCost = startCost - 1;

        do {
            curCost++;
            // TODO: why am I updating editDist twice? Side effects?
            editDist = Ukk( final_lessLong_idx_diff
                          , final_lessMidd_idx_diff
                          , curCost
                          , 0
                          , affineCosts
                          , inputChars
                          , fsmStateArrays
                          );  // Need this (?) otherwise if finalState != 0 we may need larger than expected slice size.
            editDist = Ukk( final_lessLong_idx_diff
                          , final_lessMidd_idx_diff
                          , curCost
                          , finalState
                          , affineCosts
                          , inputChars
                          , fsmStateArrays
                          );

        } while (editDist < finalDist);

        assert(editDist == finalDist);
        if (curCost != finalCost_global) {
            fprintf(stderr, "Dist reached for cost %2d (old cost %2d)\n", curCost, finalCost_global);
            finalCost_global = curCost;
            assert(0);
        }
    } // end scope
    // #endif

    return getSplitRecurse( start_lessLong_idx_diff
                          , start_lessMidd_idx_diff
                          , startCost
                          , startState
                          , start_editDist
                          , final_lessLong_idx_diff
                          , final_lessMidd_idx_diff
                          , finalCost_global
                          , finalState
                          , finalDist
                          , affineCosts
                          , inputChars
                          , resultChars
                          , fsmStateArrays
                          );
}

/** Extracts info from the 'from' and CP info then recurses with doUkkInLimits for the two subparts.
 *  All edit distances and costs are signed, as often initialized to -INFINITY
 */
int getSplitRecurse( int             start_lessLong_idx_diff
                   , int             start_lessMidd_idx_diff
                   , int             startCost
                   , int             startState
                   , int             start_editDist
                   , size_t          final_lessLong_idx_diff
                   , size_t          final_lessMidd_idx_diff
                   , int             finalCost_global
                   , int             finalState
                   , int             finalDist
                   , affine_costs_t *affineCosts
                   , characters_t   *inputChars
                   , characters_t   *resultChars
                   , fsm_arrays_t   *fsmStateArrays
                   )
{
    // Get 'from' and checkPoint data. Then recurse
    size_t finalLen;
    int    checkPoint_editDist_global;
    from_t finalCell;

    assert(    startCost >= 0
            && finalCost_global >= 0 );

    assert( get_ukk_cell( final_lessLong_idx_diff
                        , final_lessMidd_idx_diff
                        , finalCost_global
                        , finalState
                        , inputChars->numStates
                        )->computed == finalCost_global + costOffset_global);

    finalCell = get_ukk_cell( final_lessLong_idx_diff
                            , final_lessMidd_idx_diff
                            , finalCost_global
                            , finalState
                            , inputChars->numStates
                            )->from;

    assert( finalCell.cost >= 0 );

    if (get_checkPoint_cell( finalCell.lessLong_idx_diff
                           , finalCell.lessMidd_idx_diff
                           , finalCell.cost
                           , finalCell.fsm_state
                           , inputChars->numStates
                           )->cost == 0
       ) {
        get_checkPoint_cell( finalCell.lessLong_idx_diff
                           , finalCell.lessMidd_idx_diff
                           , finalCell.cost
                           , finalCell.fsm_state
                           , inputChars->numStates
                           )->cost = 1;
    }

    assert( get_checkPoint_cell( finalCell.lessLong_idx_diff
                               , finalCell.lessMidd_idx_diff
                               , finalCell.cost
                               , finalCell.fsm_state
                               , inputChars->numStates
                               )->cost == finalCell.cost + 1
          );   // Use cost + 1 so can tell if not used (cost == 0)

    checkPoint_editDist_global = get_checkPoint_cell( finalCell.lessLong_idx_diff
                                                    , finalCell.lessMidd_idx_diff
                                                    , finalCell.cost
                                                    , finalCell.fsm_state
                                                    , inputChars->numStates
                                                    )->editDist;

    assert(checkPoint_editDist_global >= 0);

    if (DEBUG_3D) {
        fprintf( stderr
               , "checkPoint cost   = %2d checkPoint width = %2d\n"
               , checkPoint_cost_global
               , checkPoint_width_global
               );
        fprintf( stderr
               , "From: a b index difference = %2d a c index difference = %2d d = %2d s = %2d\n"
               , finalCell.lessLong_idx_diff
               , finalCell.lessMidd_idx_diff
               , finalCell.cost
               , finalCell.fsm_state
               );
        fprintf( stderr
               , "checkPoint edit distance  = %2d\n"
               , checkPoint_editDist_global
               );
    }


    // Note: Doing second half of alignment first. Only reason
    // for this is so the alignment is retrieved in exactly reverse order
    // making it easy to print out.
    finalLen = doUkkInLimits( finalCell.lessLong_idx_diff
                            , finalCell.lessMidd_idx_diff
                            , finalCell.cost
                            , finalCell.fsm_state
                            , checkPoint_editDist_global
                            , final_lessLong_idx_diff
                            , final_lessMidd_idx_diff
                            , finalCost_global
                            , finalState
                            , finalDist
                            , affineCosts
                            , inputChars
                            , resultChars
                            , fsmStateArrays
                            );

    // Now first half of alignment
    doUkkInLimits( start_lessLong_idx_diff
                 , start_lessMidd_idx_diff
                 , startCost
                 , startState
                 , start_editDist
                 , finalCell.lessLong_idx_diff
                 , finalCell.lessMidd_idx_diff
                 , finalCell.cost
                 , finalCell.fsm_state
                 , checkPoint_editDist_global
                 , affineCosts
                 , inputChars
                 , resultChars
                 , fsmStateArrays
                 );

    if (DEBUG_3D) {
        fprintf( stderr, "Done.\n" );
    }

    //  return findBest_DistState(final_lessLong_idx_diff,final_lessMidd_idx_diff,finalCost_global,0);
    return finalLen;
}

// -- Traceback routines --------------------------------------------------------------
void traceBack( int           start_lessLong_idx_diff
              , int           start_lessMidd_idx_diff
              , int           startCost
              , int           startState
              , int           final_lessLong_idx_diff
              , int           final_lessMidd_idx_diff
              , int           finalCost_global
              , unsigned int  finalState
              , characters_t *inputChars
              , characters_t *resultChars
              )
{
    int lessLong_idx_diff = final_lessLong_idx_diff,
        lessMidd_idx_diff = final_lessMidd_idx_diff,
        editDistance      = finalCost_global,
        fsm_state         = finalState;

    while (   lessLong_idx_diff != start_lessLong_idx_diff
           || lessMidd_idx_diff != start_lessMidd_idx_diff
           || editDistance      != startCost
           || fsm_state         != startState
          ) {

        int a1;
        int a = a1             = get_ukk_cell( lessLong_idx_diff
                                             , lessMidd_idx_diff
                                             , editDistance
                                             , fsm_state
                                             , inputChars->numStates
                                             )->editDist;

        int nLessLong_idx_diff = get_ukk_cell( lessLong_idx_diff
                                             , lessMidd_idx_diff
                                             , editDistance
                                             , fsm_state
                                             , inputChars->numStates
                                             )->from.lessLong_idx_diff;

        int nLessMidd_idx_diff = get_ukk_cell( lessLong_idx_diff
                                             , lessMidd_idx_diff
                                             , editDistance
                                             , fsm_state
                                             , inputChars->numStates
                                             )->from.lessMidd_idx_diff;

        int nEditDistance      = get_ukk_cell( lessLong_idx_diff
                                             , lessMidd_idx_diff
                                             , editDistance
                                             , fsm_state
                                             , inputChars->numStates
                                             )->from.cost;

        int nFsmState          = get_ukk_cell( lessLong_idx_diff
                                             , lessMidd_idx_diff
                                             , editDistance
                                             , fsm_state
                                             , inputChars->numStates
                                             )->from.fsm_state;
        // int a1                 = get_ukk_cell( nLessLong_idx_diff
        //                                      , nLessMidd_idx_diff
        //                                      , nEditDistance
        //                                      , nFsmState
        //                                      , inputChars->numStates
        //                                      )->editDist;

        int b  = a  - lessLong_idx_diff,
            c  = a  - lessMidd_idx_diff,
            b1 = a1 - nLessLong_idx_diff,
            c1 = a1 - nLessMidd_idx_diff;

        assert( get_ukk_cell( lessLong_idx_diff
                            , lessMidd_idx_diff
                            , editDistance
                            , fsm_state
                            , inputChars->numStates
                            )->computed == editDistance  + costOffset_global
              );

        assert( get_ukk_cell( nLessLong_idx_diff
                            , nLessMidd_idx_diff
                            , nEditDistance
                            , nFsmState
                            , inputChars->numStates
                            )->computed == nEditDistance + costOffset_global
              );


        if (DEBUG_3D) {
            fprintf( stderr,
" lessLong_idx_diff = %3d,  lessMidd_idx_diff = %3d,  edit Distance = %3d,  fsm state = %2d,   edit Dist 1 = %3d, \n\
nLessLong_idx_diff = %3d, nLessMidd_idx_diff = %3d, nEdit Distance = %3d, nFsm state = %2d, n edit Dist 2 = %3d\n\n"
                   , lessLong_idx_diff
                   , lessMidd_idx_diff
                   , editDistance
                   , fsm_state
                   , a
                   , nLessLong_idx_diff
                   , nLessMidd_idx_diff
                   , nEditDistance
                   , nFsmState
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
          resultChars->lesserStr[resultChars->lesserIdx++] = inputChars->lesserStr[a];
          resultChars->longerStr[resultChars->longerIdx++] = inputChars->longerStr[b];
          resultChars->middleStr[resultChars->middleIdx++] = inputChars->middleStr[c];
          fsm_states_global[fsm_stateIdx_global++]         = 0;        /* The match fsm_state */
          cost_global[costIdx_global++]                    = editDistance;
        }

        // The step for (nLessLong_idx_diff, nLessMidd_idx_diff, nEditDistance, nFsmState) -> (lessLong_idx_diff, lessMidd_idx_diff, editDistance, fsm_state)
        if (   a != a1
            || b != b1
            || c != c1
           ) {
            if (a > a1) resultChars->lesserStr[resultChars->lesserIdx++] = inputChars->lesserStr[--a];
            else        resultChars->lesserStr[resultChars->lesserIdx++] = '-';

            if (b > b1) resultChars->longerStr[resultChars->longerIdx++] = inputChars->longerStr[--b];
            else        resultChars->longerStr[resultChars->longerIdx++] = '-';

            if (c > c1) resultChars->middleStr[resultChars->middleIdx++] = inputChars->middleStr[--c];
            else        resultChars->middleStr[resultChars->middleIdx++] = '-';

            fsm_states_global[fsm_stateIdx_global++] = fsm_state;
            cost_global[costIdx_global++]            = editDistance;
        }

        assert(    a == a1
                && b == b1
                && c == c1);

        lessLong_idx_diff = nLessLong_idx_diff;
        lessMidd_idx_diff = nLessMidd_idx_diff;
        editDistance      = nEditDistance;
        fsm_state         = nFsmState;
    }

    if (DEBUG_3D) {
        {
            int i;        // counts down to 0, must be signed

            fprintf(stderr,"Alignment so far\n");

            for (i = resultChars->lesserIdx - 1; i >= 0; i--) fprintf(stderr, "%c", resultChars->lesserStr[i]);
            fprintf(stderr, "\n");

            for (i = resultChars->longerIdx - 1; i >= 0; i--) fprintf(stderr, "%c", resultChars->longerStr[i]);
            fprintf(stderr, "\n");

            for (i = resultChars->middleIdx - 1; i >= 0; i--) fprintf(stderr, "%c", resultChars->middleStr[i]);

            // Print fsm_state information
            // for (i = fsm_stateIdx_global - 1; i >= 0; i--) {
            //   fprintf(stderr, "%s ", fsm_state2str( fsm_states_global[i]
            //                                   , fsmStateArrays->fsm_stateNum
            //                                   )
            //          );
            // }
            fprintf(stderr,"\n");
            // Print cost stuff
            for (i = costIdx_global - 1; i >= 0; i--) fprintf(stderr, "%-2d  ", cost_global[i]);
            fprintf(stderr, "\n");
        }
    }

    assert(lessLong_idx_diff == start_lessLong_idx_diff);
    assert(lessMidd_idx_diff == start_lessMidd_idx_diff);
    assert(editDistance      == startCost);
    assert(fsm_state         == startState);
}


/** Converts a character input, {A, C, G, T} to an int. Problem: on ambiguous inputs biases toward A.
 *  TODO: Also, disallows larger alphabets.
 *  Since we're just using the aligned outputs for gap placement, and since sub costs are always 1, the A bias is okay.
 *  If either of those things changes, then we have trouble.
 *  The hard-coded alphabet length constraint might be a bigger problem.
 */
int char_to_base( char v ) {   // TODO: Can I just skip this?
    if      ('A' == v) return 1;
    else if ('C' == v) return 2;
    else if ('G' == v) return 4;
    else if ('T' == v) return 8;
    else if ('-' == v) return 16;
    else return -1;
}


/** Do actual traceback.
 *  Since Powell's aligned characters have been disambiguated before being aligned---and with a strong A bias at that---the actual
 *  characters aren't useful. Instead we'll just use those characters to figure out where gaps are, then copy ambiguous elements
 *  from input characters.
 */
void doTraceback( dyn_character_t *retLesserChar
                , dyn_character_t *retMiddleChar
                , dyn_character_t *retLongerChar
                , dyn_character_t *original_lesserChar    // <---
                , dyn_character_t *original_middleChar    // <--- I need these to reambiguate
                , dyn_character_t *original_longerChar    // <---
                , affine_costs_t  *affineCosts
                , characters_t    *inputChars
                , characters_t    *resultChars
                , fsm_arrays_t    *fsmStateArrays
                )
{

    // Add the first run of matches to the alignment
    // NB. The first run of matches must be added in reverse order.

    int endRun = 0;  // signed so comparison to lesserLen doesn't complain.

    int j; // countdown to 0 later, so must be signed. Used in two loops below.

    while (   endRun < inputChars->lesserLen
           && (   inputChars->lesserStr[endRun] == inputChars->longerStr[endRun]
               && inputChars->lesserStr[endRun] == inputChars->middleStr[endRun])
           ) {
        endRun++;
    }

    // TODO: replace following with memcpy()
    for (j = endRun - 1; j >= 0; j--)  {
        resultChars->lesserStr[resultChars->lesserIdx++] = inputChars->lesserStr[j]; // <---
        resultChars->longerStr[resultChars->longerIdx++] = inputChars->longerStr[j]; // <--- note that indices are incrementing here
        resultChars->middleStr[resultChars->middleIdx++] = inputChars->middleStr[j]; // <---
        fsm_states_global[fsm_stateIdx_global++] = 0;        /* The match fsm_state */
        cost_global[costIdx_global++]            = 0;
    }
    // finished adding first run of matches

    // Reverse the alignments
    revCharArray(resultChars->lesserStr, 0, resultChars->lesserIdx);
    revCharArray(resultChars->longerStr, 0, resultChars->longerIdx);
    revCharArray(resultChars->middleStr, 0, resultChars->middleIdx);

    revIntArray(fsm_states_global, 0, fsm_stateIdx_global);
    revIntArray(cost_global,       0, costIdx_global);
    // end reverse alignments

    // Copy the alignment from original to return chars, using results as templates.
    // I.e., if result[i] is a gap, copy in a gap, otherwise copy in correct place from original sequence.
    size_t orig_lessIdx = original_lesserChar->len - 1,
           orig_longIdx = original_longerChar->len - 1,
           orig_middIdx = original_middleChar->len - 1;

    for (int j = resultChars->lesserIdx - 1; j >= 0; j--) {
        if (resultChars->lesserStr[j] == '-') {
            dyn_char_prepend( retLesserChar, 16 );
        } else {
            dyn_char_prepend( retLesserChar, original_lesserChar->char_begin[orig_lessIdx] );
            orig_lessIdx--;
        }

        if (resultChars->longerStr[j] == '-') {
            dyn_char_prepend( retLongerChar, 16 );
        } else {
            dyn_char_prepend( retLongerChar, original_longerChar->char_begin[orig_longIdx] );
            orig_longIdx--;
        }

        if (resultChars->middleStr[j] == '-') {
            dyn_char_prepend( retMiddleChar, 16 );
        } else {
            dyn_char_prepend( retMiddleChar, original_middleChar->char_begin[orig_middIdx] );
            orig_middIdx--;
        }
        // dyn_char_prepend( retLesserChar, char_to_base( resultChars->lesserStr[j] ) );
        // dyn_char_prepend( retMiddleChar, char_to_base( resultChars->longerStr[j] ) );
        // dyn_char_prepend( retLongerChar, char_to_base( resultChars->middleStr[j] ) );
    }
    // Now add that blasted opening gap.
    dyn_char_prepend( retLesserChar, 16 );
    dyn_char_prepend( retMiddleChar, 16 );
    dyn_char_prepend( retLongerChar, 16 );

    // alignment is done.

    assert(   resultChars->lesserIdx == resultChars->longerIdx
           && resultChars->lesserIdx == resultChars->middleIdx
           && resultChars->lesserIdx == fsm_stateIdx_global
           && resultChars->lesserIdx == costIdx_global);

    checkAlign( resultChars->lesserStr
              , resultChars->lesserIdx
              , inputChars->lesserStr
              , inputChars->lesserLen
              );

    checkAlign( resultChars->longerStr
              , resultChars->longerIdx
              , inputChars->longerStr
              , inputChars->longerLen
              );

    checkAlign( resultChars->middleStr
              , resultChars->middleIdx
              , inputChars->middleStr
              , inputChars->middleLen
              );


    assert( alignmentCost( fsm_states_global
                         , resultChars->lesserStr
                         , resultChars->longerStr
                         , resultChars->middleStr
                         , resultChars->lesserIdx
                         , affineCosts
                         , fsmStateArrays
                         ) == finalCost_global );
}


/** For clarity, calls findBest with return_the_fsm_state = 0 */
int find_bestDist( int    lessLong_idx_diff
                 , int    lessMidd_idx_diff
                 , int    input_editDist
                 , size_t numStates
                 )
{
    return findBest( lessLong_idx_diff
                   , lessMidd_idx_diff
                   , input_editDist
                   , 0
                   , numStates
                   );
}


/** For clarity, calls findBest with return_the_fsm_state = 1 */
int find_bestState( int    lessLong_idx_diff
                  , int    lessMidd_idx_diff
                  , int    input_editDist
                  , size_t numStates
                  )
{
    return findBest( lessLong_idx_diff
                   , lessMidd_idx_diff
                   , input_editDist
                   , 1
                   , numStates
                   );
}


/**
 *  Find the furthest distance at lessLong_idx_diff, lessMidd_idx_diff, input_editDistance. `return_the_fsm_state` defines
 *  whether the best distance is returned, or the best final fsm state (needed for ukk.alloc traceback).
 *
 *  Mutates nothing.
 */
int findBest( int    lessLong_idx_diff
            , int    lessMidd_idx_diff
            , int    input_editDist
            , int    return_the_fsm_state
            , size_t numStates
            )
{
    int best_editDist = -INFINITY;
    int bestState     = -1;

    for (size_t curState = 0; curState < numStates; curState++) {
        if (    ( get_ukk_cell( lessLong_idx_diff
                              , lessMidd_idx_diff
                              , input_editDist
                              , curState
                              , numStates
                              )->computed == input_editDist + costOffset_global )
             && ( get_ukk_cell( lessLong_idx_diff
                              , lessMidd_idx_diff
                              , input_editDist
                              , curState
                              , numStates
                              )->editDist > best_editDist )
           ) {
          best_editDist = get_ukk_cell( lessLong_idx_diff
                                      , lessMidd_idx_diff
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
           , lessLong_idx_diff
           , lessMidd_idx_diff
           , input_editDist
           , bestState
           , best_editDist
           );
*/
    if (return_the_fsm_state) {
        return bestState;
    } else {
        return best_editDist;
    }
}


int Ukk( int             lessLong_idx_diff
       , int             lessMidd_idx_diff
       , int             editDistance
       , unsigned int    fsm_state
       , affine_costs_t *affineCosts
       , characters_t   *inputChars
       , fsm_arrays_t   *fsmStateArrays
       )
{
    if ( !withinMatrix( lessLong_idx_diff
                      , lessMidd_idx_diff
                      , editDistance
                      , affineCosts
                      )
        ) {
        return -INFINITY;
    }
    if (  get_ukk_cell( lessLong_idx_diff
                      , lessMidd_idx_diff
                      , editDistance
                      , fsm_state
                      , inputChars->numStates
                      )->computed == editDistance + costOffset_global
        ) {
        // printf("Ukk: Don't update anything. Return edit dist.\n");
        return  get_ukk_cell( lessLong_idx_diff
                            , lessMidd_idx_diff
                            , editDistance
                            , fsm_state
                            , inputChars->numStates
                            )->editDist;
    }

/*
    fprintf( stderr
           ,"Calculating get_ukk_cell(%2d, %2d, %2d, %2d)\n"
           , lessLong_idx_diff
           , lessMidd_idx_diff
           , editDistance
           , fsm_state)
           ;
*/
    counts_global.cells++;

    calcUkk( lessLong_idx_diff
           , lessMidd_idx_diff
           , editDistance
           , fsm_state
           , affineCosts
           , inputChars
           , fsmStateArrays
           );

    // Store away check point from info in necessary
    if (     editDistance >= checkPoint_cost_global
         && (editDistance  < checkPoint_cost_global + checkPoint_width_global )
       ) {
        // printf("Ukk: Update checkpoint edit dist.\n");
        get_checkPoint_cell( lessLong_idx_diff
                           , lessMidd_idx_diff
                           , editDistance
                           , fsm_state
                           , inputChars->numStates
                           )->editDist = get_ukk_cell( lessLong_idx_diff
                                                     , lessMidd_idx_diff
                                                     , editDistance
                                                     , fsm_state
                                                     , inputChars->numStates
                                                     )->editDist;

        // printf("Ukk: Update checkpoint cost.\n");
        get_checkPoint_cell( lessLong_idx_diff
                           , lessMidd_idx_diff
                           , editDistance
                           , fsm_state
                           , inputChars->numStates
                           )->cost = editDistance + 1;          // Note adding 1 so cost ==0 signifies unused cell
    }

    if ( get_ukk_cell( lessLong_idx_diff
                     , lessMidd_idx_diff
                     , editDistance
                     , fsm_state
                     , inputChars->numStates
                     )->editDist > furthestReached_global
       ) {
        // printf("Ukk: Update furthestReached_global.\n");
        furthestReached_global = get_ukk_cell( lessLong_idx_diff
                                             , lessMidd_idx_diff
                                             , editDistance
                                             , fsm_state
                                             , inputChars->numStates
                                             )->editDist;
    }
    // printf("Ukk: Return edit dist.\n");
    return get_ukk_cell( lessLong_idx_diff
                       , lessMidd_idx_diff
                       , editDistance
                       , fsm_state
                       , inputChars->numStates
                       )->editDist;
}

/** Do initial initialization of global variables */
void initializeGlobals( characters_t *inputChars )
{
    finalCost_global        = 0;
    checkPoint_cost_global  = INFINITY;
    checkPoint_width_global = 0;
    completeFromInfo_global = 0;

    fsm_stateIdx_global    = 0;
    costIdx_global         = 0;

    costOffset_global      =  1;
    furthestReached_global = -1;

    start_lessLong_idx_diff_global = 0;
    start_lessMidd_idx_diff_global = 0;
    startCost_global  = 0;
    startState_global = 0;

    checkPoint_editDist_global = 1;
    checkPoint_cost_global     = INFINITY;

    checkPoint_width_global = inputChars->maxSingleStep;

    // TODO: what is the correct value to use for Umatrix depth.
    // Would think that MAX_SINGLE_COST = maxSingleStep * 2 would be enough
    // but doesn't seem to be. No idea why. *shrug* <-- Sigh. Sloppy coders....
    myUkk_allocInfo_global     = allocInit( sizeof(ukk_cell_t)
                                          , checkPoint_width_global * 2
                                          , inputChars
                                          );
    myCheckPt_allocInfo_global = allocInit( sizeof(checkPoint_cell_t)
                                          , checkPoint_width_global
                                          , inputChars
                                          );

    counts_global.cells     = 0;
    counts_global.innerLoop = 0;
}

int align3d_ukk( dyn_character_t *retLesserChar
               , dyn_character_t *retMiddleChar
               , dyn_character_t *retLongerChar
               , dyn_character_t *original_lesserChar
               , dyn_character_t *original_middleChar
               , dyn_character_t *original_longerChar
               , affine_costs_t  *affineCosts
               , characters_t    *inputChars
               , characters_t    *resultChars
               , fsm_arrays_t    *fsmStateArrays
               )
{
    initializeGlobals( inputChars );

    // Set up check point matrix.
    checkPoint_cell_t *checkPoint_dummyCell = malloc( sizeof(checkPoint_cell_t) );

    checkPoint_dummyCell->editDist = 0;
    checkPoint_dummyCell->cost     = 0;

    // Set up Ukkonnen matrix.
    ukk_cell_t *ukk_dummyCell = malloc( sizeof(ukk_cell_t) );

    ukk_dummyCell->editDist       = 0;
    ukk_dummyCell->computed       = 0;
    ukk_dummyCell->from.cost      = 0;
    ukk_dummyCell->from.fsm_state = 0;

    ukk_dummyCell->from.lessLong_idx_diff = 0;
    ukk_dummyCell->from.lessMidd_idx_diff = 0;

    resultChars->lesserIdx = 0;
    resultChars->longerIdx = 0;
    resultChars->middleIdx = 0;

    int cur_editDist = 0,        // signed for comparison with lesserLen
        start_editDist,          // signed because it gets assigned from cur_editDist
        final_lessLong_idx_diff, // signed because start as negative
        final_lessMidd_idx_diff, // signed because start as negative
        are_equal,               // are the elements at `curEditDist` in the three characters the same?
        editDist;                // signed to get rid of compiler warnings about different types


    // Calculate starting position, where elements are no longer the same for all three characters
    cur_editDist = 0;
    are_equal    =    inputChars->lesserStr[cur_editDist] == inputChars->longerStr[cur_editDist]
                   && inputChars->lesserStr[cur_editDist] == inputChars->middleStr[cur_editDist];

    while (   cur_editDist < inputChars->lesserLen
           && are_equal
          ) {
        cur_editDist++;
        counts_global.innerLoop++;

        are_equal =    inputChars->lesserStr[cur_editDist] == inputChars->longerStr[cur_editDist]
                    && inputChars->lesserStr[cur_editDist] == inputChars->middleStr[cur_editDist];
    }
    get_ukk_cell( 0, 0, 0, 0, inputChars->numStates )->editDist = cur_editDist;
    get_ukk_cell( 0, 0, 0, 0, inputChars->numStates )->computed = (int) costOffset_global; // originally costOffset_global + 0??

    start_editDist = cur_editDist;

    final_lessMidd_idx_diff = inputChars->lesserLen - inputChars->middleLen;
    final_lessLong_idx_diff = inputChars->lesserLen - inputChars->longerLen; // So these two are negative.

    // TODO: figure out where these are initialized.
    // inputChars->lesserLen =
    // inputChars->middleLen =
    // inputChars->longerLen =

    do {
        cur_editDist++;
        Ukk( final_lessLong_idx_diff
           , final_lessMidd_idx_diff
           , cur_editDist
           , 0
           , affineCosts
           , inputChars
           , fsmStateArrays
           );

        if (DEBUG_3D) {
            fprintf(stderr, "Furthest reached for cost %2d is %2d.\n",
                    cur_editDist, furthestReached_global);
        }

        int half_lesserLen = inputChars->lesserLen / 2;
        if (   checkPoint_editDist_global
            && furthestReached_global >= half_lesserLen
           ) {
            checkPoint_cost_global     = cur_editDist + 1;
            checkPoint_editDist_global = 0;

            if (DEBUG_3D) {
                fprintf(stderr, "Setting checkPoint_cost_global: %2d\n", checkPoint_cost_global);
            }
        }
    } while (find_bestDist( final_lessLong_idx_diff
                          , final_lessMidd_idx_diff
                          , cur_editDist
                          , inputChars->numStates
                          ) < inputChars->lesserLen);

    assert( find_bestDist( final_lessLong_idx_diff
                         , final_lessMidd_idx_diff
                         , cur_editDist
                         , inputChars->numStates
                         ) == inputChars->lesserLen
          );

    checkPoint_editDist_global = 0;
    finalCost_global           = cur_editDist;

    // Recurse for alignment
    int finalState = find_bestState( final_lessLong_idx_diff
                                   , final_lessMidd_idx_diff
                                   , finalCost_global
                                   , inputChars->numStates
                                   );

    // If there's no cost, can't run algorithm. Probably first run. Start again.
    if ( get_ukk_cell( final_lessLong_idx_diff
                     , final_lessMidd_idx_diff
                     , finalCost_global
                     , finalState
                     , inputChars->numStates
                     )->from.cost <= 0 ) {

        // We check pointed too late on this first pass.
        // So we got no useful information. Oh well, have to do it all over again.
        assert( get_ukk_cell( final_lessLong_idx_diff
                            , final_lessMidd_idx_diff
                            , finalCost_global
                            , finalState
                            , inputChars->numStates
                            )->computed == finalCost_global + costOffset_global );

        editDist = doUkkInLimits( 0
                                , 0
                                , 0
                                , 0
                                , start_editDist
                                , final_lessLong_idx_diff
                                , final_lessMidd_idx_diff
                                , finalCost_global
                                , finalState
                                , inputChars->lesserLen
                                , affineCosts
                                , inputChars
                                , resultChars
                                , fsmStateArrays
                                );
    } else {
        // Use the 'from' info and do the two sub parts.
        editDist = getSplitRecurse( 0
                                  , 0
                                  , 0
                                  , 0
                                  , start_editDist
                                  , final_lessLong_idx_diff
                                  , final_lessMidd_idx_diff
                                  , finalCost_global
                                  , finalState
                                  , inputChars->lesserLen
                                  , affineCosts
                                  , inputChars
                                  , resultChars
                                  , fsmStateArrays
                                  );
    }

    assert(editDist == inputChars->lesserLen);
    doTraceback( retLesserChar
               , retMiddleChar
               , retLongerChar
               , original_lesserChar
               , original_middleChar
               , original_longerChar
               , affineCosts
               , inputChars
               , resultChars
               , fsmStateArrays
               );

    deallocate_MtxCell( &myUkk_allocInfo_global
                      // , &(ukk_dummyCell->computed)
                      // ,  ukk_dummyCell
                      // ,  inputChars->numStates
                      );

    deallocate_MtxCell( &myCheckPt_allocInfo_global
                      // , &(checkPoint_dummyCell->cost)
                      // ,  checkPoint_dummyCell
                      // ,  inputChars->numStates
                      );

    printf("align3d_ukk: current editDist: = %2d\n", cur_editDist);
    return (int) cur_editDist;
}


int calcUkk( int             lessLong_idx_diff
           , int             lessMidd_idx_diff
           , int             input_editDist
           , int             toState
           , affine_costs_t *affineCosts
           , characters_t   *inputChars
           , fsm_arrays_t   *fsmStateArrays
           )
{
    // if (DEBUG_CALL_ORDER) {
        printf("---calcUkk %d %d %d\n", lessLong_idx_diff, lessMidd_idx_diff, input_editDist);
        fflush(stdout);
    // }

    // TODO: document all of these
    int neighbour = fsmStateArrays->neighbours[toState];

    int isDeleteState_A,
        isDeleteState_B,
        isDeleteState_C,
        lessLong_idx_diff1,
        lessMidd_idx_diff1,
        a1,
        a2,
        start_transitionCost,
        best_editDist,
        fromCost,
        editDist,
        curCost,
        this_editDist;

    if (DEBUG_CALL_ORDER) {
        int indent_globali_global = 0;
        char indent_global[1000];

        indent_global[indent_globali_global] = 0;

        fprintf( stderr
               , "%s CalcUKK(lessLong_idx_diff = %2d, lessMidd_idx_diff = %2d, input_editDist = %2d, toState = %2d)\n"
               , indent_global
               , lessLong_idx_diff
               , lessMidd_idx_diff
               , input_editDist
               , toState
               );

        indent_global[indent_globali_global++] = ' ';
        indent_global[indent_globali_global++] = ' ';
        indent_global[indent_globali_global]   = 0;
    }

    assert( get_ukk_cell( lessLong_idx_diff
                        , lessMidd_idx_diff
                        , input_editDist
                        , toState
                        , inputChars->numStates
                        )->computed < input_editDist + costOffset_global
           );

    best_editDist = -INFINITY;

    // Initialise from info if necessary
    from_t from;
    from.cost = -1;

    if (    input_editDist >= checkPoint_cost_global
         && input_editDist  < checkPoint_cost_global + checkPoint_width_global
       ) {
        from.lessLong_idx_diff = lessLong_idx_diff;
        from.lessMidd_idx_diff = lessMidd_idx_diff;
        from.cost        = input_editDist;
        from.fsm_state   = toState;
    }

    // Figure out which of the three transitions are deletes.
    step(  neighbour
        , &isDeleteState_A
        , &isDeleteState_B
        , &isDeleteState_C
        );

    lessLong_idx_diff1 = lessLong_idx_diff - isDeleteState_A + isDeleteState_B;
    lessMidd_idx_diff1 = lessMidd_idx_diff - isDeleteState_A + isDeleteState_C;

    // calculate if it's a valid diagonal
    if (    lessLong_idx_diff1 >= -inputChars->longerLen
         && lessLong_idx_diff1 <=  inputChars->lesserLen
         && lessMidd_idx_diff1 >= -inputChars->middleLen
         && lessMidd_idx_diff1 <=  inputChars->lesserLen
       ) {

        // Loop over possible fsm state we are moving from
        //   May be possible to limit this?
        for (size_t fromState = 0; fromState < inputChars->numStates; fromState++) {

            start_transitionCost = (fsmStateArrays->transitionCost)[fromState * MAX_STATES + toState];

            fromCost = -INFINITY;
            editDist = -INFINITY;

            curCost  = input_editDist - start_transitionCost - fsmStateArrays->fsmState_continuationCost[toState];
            a2       = -1;
            a1       = Ukk( lessLong_idx_diff1
                          , lessMidd_idx_diff1
                          , curCost
                          , fromState
                          , affineCosts
                          , inputChars
                          , fsmStateArrays
                          );
            if ( a1 >= 0 ) {
                printf("a1: %d, isDeleteState_A: %d, inputChars->lesserLen: %d\n", a1, isDeleteState_A, inputChars->lesserLen);
                printf("b1: %d, isDeleteState_B: %d, inputChars->longerLen: %d\n", a1, isDeleteState_B, inputChars->lesserLen);
                printf("c1: %d, isDeleteState_C: %d, inputChars->middleLen: %d\n", a1, isDeleteState_C, inputChars->lesserLen);
                printf("a1: %d, a1 - lessLong_idx_diff1: %d, a1 - lessMidd_idx_diff1: %d\n", a1, a1 - lessLong_idx_diff1, a1 - lessMidd_idx_diff1);
            }

            if (    okIndex( a1                     , isDeleteState_A, inputChars->lesserLen )
                 && okIndex( a1 - lessLong_idx_diff1, isDeleteState_B, inputChars->longerLen )
                 && okIndex( a1 - lessMidd_idx_diff1, isDeleteState_C, inputChars->middleLen )
                 && ( whichCharCost( isDeleteState_A ? inputChars->lesserStr[a1]
                                                     : '-'
                                   , isDeleteState_B ? inputChars->longerStr[a1 - lessLong_idx_diff1]
                                                     : '-'
                                   , isDeleteState_C ? inputChars->middleStr[a1 - lessMidd_idx_diff1]
                                                     : '-'
                                   ) == 1 )
               ) {
                fromCost = curCost;
                editDist = a1 + isDeleteState_A;
            } else {
                if (!(fsmStateArrays->secondCost)[toState]) {
                    continue;
                }

                a2 = Ukk( lessLong_idx_diff1
                        , lessMidd_idx_diff1
                        , curCost - affineCosts->mismatchCost
                        , fromState
                        , affineCosts
                        , inputChars
                        , fsmStateArrays
                        );

                if (   okIndex(a2,                      isDeleteState_A, inputChars->lesserLen)
                    && okIndex(a2 - lessLong_idx_diff1, isDeleteState_B, inputChars->longerLen)
                    && okIndex(a2 - lessMidd_idx_diff1, isDeleteState_C, inputChars->middleLen)
                   ) {
                    fromCost = curCost - affineCosts->mismatchCost;
                    editDist = a2 + isDeleteState_A;
                }
            }

            // Check if this is an improvment
            if (best_editDist < editDist) {
                best_editDist = editDist;

                if (completeFromInfo_global) {        // Do we need to store complete from information for a base case?
                    from.lessLong_idx_diff = lessLong_idx_diff1;
                    from.lessMidd_idx_diff = lessMidd_idx_diff1;
                    from.cost        = fromCost;
                    from.fsm_state   = fromState;
                } else if (input_editDist >= checkPoint_cost_global + checkPoint_width_global) { // Store from info for checkPoint_
                      from = get_ukk_cell( lessLong_idx_diff1
                                         , lessMidd_idx_diff1
                                         , fromCost
                                         , fromState
                                         , inputChars->numStates
                                         )->from;
                }
            }
        } // End loop over from fsm_states_global
    } // End if valid neighbour

    // Ensure that we have how we can reach for AT MOST cost input_editDist

    editDist = Ukk( lessLong_idx_diff
                  , lessMidd_idx_diff
                  , input_editDist - 1
                  , toState
                  , affineCosts
                  , inputChars
                  , fsmStateArrays
                  );

    // Check if this is an improvment
    if (okIndex(editDist,                     0, inputChars->lesserLen) &&
        okIndex(editDist - lessLong_idx_diff, 0, inputChars->longerLen) &&
        okIndex(editDist - lessMidd_idx_diff, 0, inputChars->middleLen) &&
        best_editDist < editDist)
    {
        best_editDist = editDist;

        if (completeFromInfo_global) {        // Do we need to store complete from information for a base case?
            from.lessLong_idx_diff = lessLong_idx_diff;
            from.lessMidd_idx_diff = lessMidd_idx_diff;
            from.cost        = input_editDist - 1;
            from.fsm_state   = toState;

        } else if (input_editDist >= checkPoint_cost_global + checkPoint_width_global) { // Store from info for checkPoint_
            from = get_ukk_cell( lessLong_idx_diff
                               , lessMidd_idx_diff
                               , input_editDist - 1
                               , toState
                               , inputChars->numStates
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

        // Get furthest of fsm states for this cost
        int editDist       = -INFINITY;
        int from_fsm_state = -1;

        for (size_t curState = 0; curState < inputChars->numStates; curState++) {
            this_editDist = (curState == 0) ? best_editDist
                                            : Ukk( lessLong_idx_diff
                                                 , lessMidd_idx_diff
                                                 , input_editDist
                                                 , curState
                                                 , affineCosts
                                                 , inputChars
                                                 , fsmStateArrays
                                                 );

            if (this_editDist > editDist) {
                editDist       = this_editDist;
                from_fsm_state = curState;
            }
        }

        // Try to extend to diagonal
        while (   okIndex(editDist,                     1, inputChars->lesserLen)
               && okIndex(editDist - lessLong_idx_diff, 1, inputChars->longerLen)
               && okIndex(editDist - lessMidd_idx_diff, 1, inputChars->middleLen)
               && (   inputChars->lesserStr[editDist] == inputChars->longerStr[editDist - lessLong_idx_diff]
                   && inputChars->lesserStr[editDist] == inputChars->middleStr[editDist - lessMidd_idx_diff] )
              ) {
          editDist++;
          counts_global.innerLoop++;
        }

        // Was there an improvement?
        if (editDist > best_editDist) {
            best_editDist = editDist;  // Note: toState = MMM

            // Update 'from' information if the fsm state we extended from was
            // not the same fsm state we are in (the MMM fsm state).
            if (from_fsm_state != 0) {
                if (completeFromInfo_global) {        // TODO: Do we need to store complete 'from' information for a base case?
                    from.lessLong_idx_diff = lessLong_idx_diff;
                    from.lessMidd_idx_diff = lessMidd_idx_diff;
                    from.cost        = input_editDist;
                    from.fsm_state   = from_fsm_state;
                } else if (input_editDist >= checkPoint_cost_global + checkPoint_width_global) { // Store from info for checkPoint_
                    from = get_ukk_cell( lessLong_idx_diff
                                       , lessMidd_idx_diff
                                       , input_editDist
                                       , from_fsm_state
                                       , inputChars->numStates
                                       )->from;
                }
            }
        }
    } // End attempt to extend diagonal on a run of matches

    assert( get_ukk_cell( lessLong_idx_diff
                        , lessMidd_idx_diff
                        , input_editDist
                        , toState
                        , inputChars->numStates
                        )->computed < input_editDist + costOffset_global
          );

    get_ukk_cell( lessLong_idx_diff
                , lessMidd_idx_diff
                , input_editDist
                , toState
                , inputChars->numStates
                )->editDist = best_editDist;

    get_ukk_cell( lessLong_idx_diff
                , lessMidd_idx_diff
                , input_editDist
                , toState
                , inputChars->numStates
                )->computed = input_editDist + costOffset_global;

    get_ukk_cell( lessLong_idx_diff
                , lessMidd_idx_diff
                , input_editDist
                , toState
                , inputChars->numStates
                )->from = from;


    if (DEBUG_CALL_ORDER) {
        int indent_globali_global = 0;
        char indent_global[1000];

        indent_globali_global        -= 2;
        indent_global[indent_globali_global] = 0;
        fprintf( stderr, "%sCalcUKK(lessLong_idx_diff = %2d, lessMidd_idx_diff = %2d, d = %2d,    toState = %2d) = %2d\n"
               , indent_global
               , lessLong_idx_diff
               , lessMidd_idx_diff
               , input_editDist
               , toState
               , get_ukk_cell( lessLong_idx_diff
                             , lessMidd_idx_diff
                             , input_editDist
                             , toState
                             , inputChars->numStates
                             )->editDist
               );

        fprintf( stderr, "%sFrom:   lessLong_idx_diff = %2d, lessMidd_idx_diff = %2d, cost = %2d, fsm_state = %2d\n",
                 indent_global
               , get_ukk_cell( lessLong_idx_diff
                             , lessMidd_idx_diff
                             , input_editDist
                             , toState
                             , inputChars->numStates
                             )->from.lessLong_idx_diff

               , get_ukk_cell( lessLong_idx_diff
                             , lessMidd_idx_diff
                             , input_editDist
                             , toState
                             , inputChars->numStates
                             )->from.lessMidd_idx_diff

               , get_ukk_cell( lessLong_idx_diff
                             , lessMidd_idx_diff
                             , input_editDist
                             , toState
                             , inputChars->numStates
                             )->from.cost

               , get_ukk_cell( lessLong_idx_diff
                             , lessMidd_idx_diff
                             , input_editDist
                             , toState
                             , inputChars->numStates
                             )->from.fsm_state

               );
    }

    return get_ukk_cell( lessLong_idx_diff
                       , lessMidd_idx_diff
                       , input_editDist
                       , toState
                       , inputChars->numStates
                       )->editDist;
}

