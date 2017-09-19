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

// ContaINS the common routines for ukk.alloc, ukk.noalign, ukk.checkp and ukk.dpa
// Also see ukkCommon.h
// Compile with -DSYSTEM_INFO to print system information of
// every run. Useful to timing runs where cpu info is important.

/**
 *  Usage: [m a b]
 *  where m is the cost of a mismatch_cost,
 *      a is the cost to start a gap,
 *      b is the cost to extend a gap.
 */

// #define UKKCOMMON_C
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

//#define NO_ALLOC_ROUTINES 1
#include "debug_constants.h"
#include "dyn_character.h"
#include "ukkCheckPoint.h"
#include "ukkCommon.h"

// extern variable (all from ukkCheckp.c)
// TODO: finish getting rid of these globals
extern      alignment_mtx_t myUkkAllocInfo;
extern      alignment_mtx_t myCheckPtAllocInfo;
extern long costOffset;
extern long finalCost;

extern int sabG,
           sacG,
           sCostG,
           sStateG;

extern size_t checkPoint_width;
extern int    checkPoint_cost;

// extern size_t furthestReached_global;
// extern size_t checkPoint_onDist;
// extern size_t endA,
//               endB,
//               endC;

extern int    completeFromInfo;
// extern Counts counts;

//int  aCharIdx, bCharIdx, cCharIdx, fsmStateIdx, costIdx;



//extern int doUkk(dyn_character_t *retCharA, dyn_character_t *retCharB, dyn_character_t *retCharC);    // Main driver function


#ifndef NO_ALLOC_ROUTINES
// Allocation stuff routines.  All inline -----------------------------------------

//U_cell_type **Umatrix;        /* 2 dimensional */

// aInfo = allocMatrix(sizeof(U_cell_type));
//void allocUmatrix() {
//  UcostSize = maxSingleCost*2;
//  Umatrix = (U_cell_type **)allocMatrix(sizeof(U_cell_type *));
//}


/** recalloc - does a realloc() but sets any new memory to 0. */
static inline void *recalloc( void   *p
                            , size_t  oldSize
                            , size_t  newSize
                            )
{
    p = realloc( p, newSize );
    if (!p || oldSize > newSize) return p;

    // Cast the void pointer to char pointer to suppress compiler warnings.
    // We assume that arithmetic takes place in terms of bytes.
    memset( ((char*) p) + oldSize, 0, newSize - oldSize );
    return p;
}


/** Attempt to allocate an additional plane of the alignment matrix. Return pointer to that plane. */
static inline void *allocPlane( alignment_mtx_t *a )
{
    void *p;

    a->memAllocated += a->lessLong_blocks * a->lessMidd_blocks * sizeof(void*);
    p = calloc( a->lessLong_blocks * a->lessMidd_blocks, sizeof(void*) );
    if (p == NULL) {
        fprintf(stderr, "Unable to alloc memory\n");
        exit(-1);
    }

    return p;
}


/** Always the previous. */
#ifdef FIXED_NUM_PLANES
    alignment_mtx_t allocInit( size_t        elemSize
                          , size_t        costSize
                          , characters_t *inputChars
                          )
#else
    alignment_mtx_t allocInit( size_t        elemSize
                          , characters_t *inputChars
                          )
#endif

{
    alignment_mtx_t retStruct;

    retStruct.memAllocated = 0;
    retStruct.elemSize     = elemSize;

    retStruct.lessLong_size   = inputChars->lesserLen + inputChars->longerLen + 1;
    retStruct.lessMidd_size   = inputChars->lesserLen + inputChars->middleLen + 1;
    retStruct.lessLong_offset = inputChars->longerLen;
    retStruct.lessMidd_offset = inputChars->middleLen;

    retStruct.lessLong_blocks = retStruct.lessLong_size / CELLS_PER_BLOCK + 1;
    retStruct.lessMidd_blocks = retStruct.lessMidd_size / CELLS_PER_BLOCK + 1;

    #ifdef FIXED_NUM_PLANES
        retStruct.costSize  = costSize;
        retStruct.baseAlloc = costSize;
    #else
        retStruct.baseAlloc = 20;       /* Whatever not really important, will increase as needed */
    #endif

    retStruct.memAllocated += retStruct.baseAlloc * sizeof(void *);

    retStruct.matrix = calloc( retStruct.baseAlloc, sizeof(void *) );

    if (retStruct.matrix == NULL) {
        fprintf(stderr,"Unable to alloc memory\n");
        exit(-1);
    }

    return retStruct;
}

static inline void *allocEntry( alignment_mtx_t *a
                              , size_t        numStates
                              )
{
    void *p;

    size_t entries = CELLS_PER_BLOCK * CELLS_PER_BLOCK * numStates;
    a->memAllocated += entries * a->elemSize;

    p = calloc(entries, a->elemSize);

    if (p == NULL) {
        fprintf(stderr,"Unable to alloc memory\n");
        exit(-1);
    }

    return p;
}

static inline size_t allocGetSubIndex( alignment_mtx_t *inputMtx
                                     , int              lessMidd_idx_diff
                                     , int              lessLong_idx_diff
                                     , int              fsm_state
                                     , size_t           numStates
                                     )
{
    size_t index = 0;

    /* Because why not just % (and yes, it took me a while to figure that out).
    size_t i = (lessLong_idx_diff + inputMtx->lessLong_offset) / CELLS_PER_BLOCK;
    size_t j = (lessMidd_idx_diff + inputMtx->lessMidd_offset) / CELLS_PER_BLOCK;

    int lessLong_adjusted = lessLong_idx_diff + inputMtx->lessLong_offset - i * CELLS_PER_BLOCK;
    int lessMidd_adjusted = lessMidd_idx_diff + inputMtx->lessMidd_offset - j * CELLS_PER_BLOCK;
    */

    int lessMidd_adjusted = (lessMidd_idx_diff + inputMtx->lessMidd_offset) % CELLS_PER_BLOCK;
    int lessLong_adjusted = (lessLong_idx_diff + inputMtx->lessLong_offset) % CELLS_PER_BLOCK;

    /*
    if (   lessLong_adjusted != lessLong_adjusted_2
        || lessMidd_adjusted != lessMidd_adjusted_2
       ) {
        fprintf( stderr, "lessLong_idx_diff = %2d, lessLong_idx_diff offset = %2zu, lessLong_idx_diff adjusted = %2d\nlessMidd_idx_diff = %2d, lessMidd_idx_diff offset = %2zu, lessMidd_idx_diff adjusted = %2d\n"
               , lessLong_idx_diff
               , inputMtx->lessLong_offset
               , lessLong_adjusted
               , lessMidd_idx_diff
               , inputMtx->lessMidd_offset
               , lessMidd_adjusted
               );
        exit(1);
    }


    assert(lessLong_adjusted >= 0 && lessLong_adjusted < CELLS_PER_BLOCK);
    assert(lessMidd_adjusted >= 0 && lessMidd_adjusted < CELLS_PER_BLOCK);
    */

    printf("fsm_state %2d, numStates %2zu\n", fsm_state, numStates);
    assert( fsm_state >= 0 && fsm_state < (int) numStates );

    index = (index + lessMidd_adjusted) * CELLS_PER_BLOCK;
    index = (index + lessLong_adjusted) * numStates;
    index = (index + fsm_state);

    return index;
}


void deallocate_MtxCell( alignment_mtx_t *inputMtx
                       // , void            *flag
                       // , void            *top
                       // , size_t           numStates
                       )
{
    // Cast the void pointers to long longs because we intend to treat the
    // pointers as integral values.
    // int usedFlag = ((long long) flag) - ((long long) top);

    // size_t planesUsed = 0;

    // size_t blocksTotal = 0,
    //        blocksUsed  = 0;

    // size_t cellsTotal = 0,
    //        cellsUsed  = 0;

    // long tblocksUsed;
    void **matrix;

    for (size_t i = 0; i < inputMtx->baseAlloc; i++) {
        // tblocksUsed = 0;
        matrix = inputMtx->matrix[i];

        if (!matrix) {
            continue;
        }

        // planesUsed++;

        // long tcellsUsed;
        // void *block;

        for (size_t j = 0; j < inputMtx->lessLong_blocks * inputMtx->lessMidd_blocks; j++) {
            // tcellsUsed = 0;
            // block      = matrix[j];
            // blocksTotal++;

            if (!matrix[j]) {
                continue;
            }

            // blocksUsed++;
            // tblocksUsed++;

            // for (size_t cIndex = 0; cIndex < numStates * CELLS_PER_BLOCK * CELLS_PER_BLOCK; cIndex++) {
            //     cellsTotal++;

            //     // Cast the void pointer to char pointer to suppress compiler warnings.
            //     // We assume that arithmetic takes place in terms of bytes.
            //     if ( *(int*) ( ( (char*) block ) + (cIndex * inputMtx->elemSize) + usedFlag) ) {
            //         cellsUsed++;
            //         tcellsUsed++;
            //     }
            // }
            free (matrix[j]);
        }
        free (matrix);
    }
    free (inputMtx->matrix);
    inputMtx->matrix = NULL;

}


void *getPtr( alignment_mtx_t *inputMtx
            , size_t           lessMidd_idx_diff
            , size_t           lessLong_idx_diff
            , size_t           editDist
            , int              fsm_state
            , size_t           numStates
            )
{
    void **matrix,
          *this_baseArr;

    int oldSize;

    size_t index,
           i,     // index into
           j;     // index into

    #ifdef FIXED_NUM_PLANES
        // If doing a noalign or checkPoint,  remap 'editDist' into 0 .. costSize - 1.
        editDist = editDist % inputMtx->costSize;
    #endif

    // Increase the base array as needed.
    while (editDist >= inputMtx->baseAlloc) {

        // Keep doubling size of allocation until edit distance is within (what I assume are) Ukkonnen barriers
        oldSize              = inputMtx->baseAlloc;
        inputMtx->baseAlloc *= 2;
        inputMtx->matrix     = recalloc( inputMtx->matrix
                                       , oldSize             * sizeof(void *)
                                       , inputMtx->baseAlloc * sizeof(void *)
                                       );

        if (inputMtx->matrix == NULL) {
            fprintf(stderr, "Unable to alloc memory\n");
            exit(-1);
        }

        inputMtx->memAllocated += oldSize * sizeof(void *); // it's doubling in size
    }

    assert(editDist < inputMtx->baseAlloc);

    if (inputMtx->matrix[editDist] == NULL)  inputMtx->matrix[editDist] = allocPlane( inputMtx );

    matrix = inputMtx->matrix[editDist];

    i = (lessMidd_idx_diff + inputMtx->lessMidd_offset) / CELLS_PER_BLOCK;
    j = (lessLong_idx_diff + inputMtx->lessLong_offset) / CELLS_PER_BLOCK;

    assert(i >= 0 && i < inputMtx->lessMidd_blocks);
    assert(j >= 0 && j < inputMtx->lessLong_blocks);

    if (matrix[(i * inputMtx->lessMidd_blocks) + j] == NULL) {
        matrix[(i * inputMtx->lessMidd_blocks) + j] = allocEntry( inputMtx, numStates );
    }

    this_baseArr = matrix[(i * inputMtx->lessMidd_blocks) + j];
    assert(this_baseArr != NULL);

    index = allocGetSubIndex( inputMtx
                            , lessMidd_idx_diff
                            , lessLong_idx_diff
                            , fsm_state
                            , numStates
                            );

    /*  fprintf( stderr
               , "getPtr(lessLong_idx_diff = %d,lessMidd_idx_diff = %d, d = %d, s = %d): this_baseArr = %p index = %d\n"
               , lessLong_idx_diff, lessMidd_idx_diff, d, s
               , this_baseArr, index)
               ;
    */

    // Cast the void pointer to char pointer to suppress compiler warnings.
    // We assume that arithmetic takes place in terms of bytes.
    return ( (char*) this_baseArr ) + index * inputMtx->elemSize;
}


#endif // NO_ALLOC_ROUTINES


void copyCharacter ( char            *str
                   , dyn_character_t *inChar
                   )
{
    if (DEBUG_CALL_ORDER) {
        printf("copyCharacter\n");
    }
    size_t len, i;
    elem_t *char_begin;
    len        = inChar->len;
    char_begin = inChar->char_begin;

    for (i = 1; i < len; i++) {
        // printf ("seq_begin[%zu] = %d\n", i, char_begin[i]);
        if (char_begin[i] & 1) {
            str[i - 1] = 'A';
        } else if (char_begin[i] & 2) {
            str[i - 1] = 'C';
        } else if (char_begin[i] & 4) {
            str[i - 1] = 'G';
        } else if (char_begin[i] & 8) {
            str[i - 1] = 'T';
        } else {
            printf ("This is impossible! The character is not an A, C, G or T. It's empty: %d !!", char_begin[i]);
            fflush(stdout);
            exit(1);
        }
    }
    str[len - 1] = 0;
    return;
}


/** Is this a Match Insertion Insertion (MII), etc.?
 *  Matches return 0.
 *  Subs (which are also coded as M), various transitions, add gap open, gap continuation all return 1.
 *  Mismatches, or all different, return 2.
 *  TODO: Is ^^^ that right?
 */
int whichCharCost(char a, char b, char c)
{
    if (DEBUG_CALL_ORDER) {
        printf("whichCharCost\n");
    }
    printf("a: %d, b: %d, c: %d\n", a, b, c);
    // This would be all deletions, which doesn't really make any sense.
    assert(   a != 0
           && b != 0
           && c != 0
          );
    /*
      When running as a ukk algorithm (ie. not the dynamic programming algorithm, DPA), then
      a == b == c only when there is a run of MATCH_SUBs after a fsm_state other that MMM,
      and since we are moving to a MMM fsm_state, this cost will NEVER be used,
      so it doesn't matter what we return.
      When running as the DPA, it can occur at a == b == c, return 0 in this case.
    */
    if (a == b && a == c) {
      return 0;
    }
    /* return 1 for any two the same, ie. the following
         x-- -x- --x  <- two gaps
         xx- x-x -xx  <- two xs and a gap
         xxy xyx yxx  <- two xs and a y
    */
    if (a == b || a == c || b == c) {
        return 1;
    }
    /* return 2 for the following
         xy- x-y -xy xyz (all three different, so sub)
    */
    return 2;
}


/** Make sure that index is valid for a given set of array indices */
int okIndex( int idx
           , int start
           , int end
           )
{
    // if (DEBUG_CALL_ORDER) printf("okIndex\n");

    if (idx   <  0) return 0;
    if (start == 0) return 1;

    return (idx < end);
    //  return (idx<0 ? 0 : (start==0 ? 1 : idx<end));
}




/******** Setup routines ********/

/*
int fsmState_transitionCost( int from
                           , int to
                           , int *transitionCost
                           )
{
    return transitionCost[from][to];
}
*/


/** Mutates a, b, and c such that each is true or false if the least significant first, second or third digit, respectively,
 *  of neighbour is 1.
 *  I.e., if one of the transitions of the `neighbour` fsm is `delete`, set that state to true; otherwise, set to 0.
 */
void step( int  neighbour
         , int *state1
         , int *state2
         , int *state3
         )
{
    assert(neighbour > 0 && neighbour <= 7);
    *state1 =  neighbour       & 1;
    *state2 = (neighbour >> 1) & 1;
    *state3 = (neighbour >> 2) & 1;
}


/** Returns the value of the neighbor fsm state where the current fsm state is ijk, and each of i, j, k can be 1 or 0.
 *  Creates a binary number where i is the least significant bit and k is the most.
 */
int neighbourNum( int i
                , int j
                , int k
                )
{
    return (i * 1) + (j * 2) + (k * 4);
}


// --------------------------------------------------

/** Resets a transitionn array that holds the transitions fsm states for three fsm state-transition FSMs. There are 27 possible fsm states.
 *  For instance, if the fsm_state is 1, then the FSMs are in the cumulative fsm_state [DEL, MATCH_SUB, MATCH_SUB], whereas if the
 *  fsm_state were 22, the cumulative fsm_state would be [DEL, DEL, INS] (which is actually not possible, as it signifies a gap in all
 *  three dynamic characters, which is meaningless).
 */
void transitions( Trans  fsmState_transitions[3]
                , size_t fsm_state
                )
{
    fsmState_transitions[0] =  fsm_state      % 3;    // repeats 0, 1, 2, 0, 1, 2, ..., i.e. MATCH_SUB, DEL, INS
    fsmState_transitions[1] = (fsm_state / 3) % 3;    // repeats 0, 0, 0, 1, 1, 1, 2, 2, 2, 0, ...
    fsmState_transitions[2] = (fsm_state / 9) % 3;    // repeats: nine 0s, nine 1s, nine 2s, start over
}


/** Takes an array of fsm states by value and returns a string of those fsm states as match, delete, insert.
 *  Used only in printing of fsm_state array if DEBUG_3D is set.
 */
char *fsm_state2str( size_t  fsm_state
               , int    *fsmState_num
               )
{
    static char returnStr[4];
    Trans fsmState_transitions[3];

    transitions( fsmState_transitions, fsmState_num[fsm_state] );

    for (size_t i = 0; i < 3; i++) {
        returnStr[i] = ( fsmState_transitions[i] == MATCH_SUB ? 'M' : ( fsmState_transitions[i] == DEL ? 'D' : 'I') );
    }
    return returnStr;
}


/** Count number of times whichTransition appears in fsmState_transitions, where MATCH_SUB = 0, DEL = 1, INS = 2. */
size_t countThisTransition( Trans fsmState_transitions[3]
                          , Trans whichTransition
                          )
{
    size_t num = 0;

    for (size_t i = 0; i < 3; i++) {
        if (fsmState_transitions[i] == whichTransition) num++;
    }
    return num;
}


void setup( affine_costs_t  *affineCosts
          , characters_t    *inputChars
          , characters_t    *resultChars
          , fsm_arrays_t    *fsmStateArrays
          , dyn_character_t *in_lesserChar
          , dyn_character_t *in_middleChar
          , dyn_character_t *in_longerChar
          , unsigned int     mismatch_cost
          , unsigned int     gapOpen
          , unsigned int     gapExtend
          )
{
    // Initialize global costs. These will be passed around to remove globals and make functional side effects more clear.
    affineCosts->mismatchCost  = mismatch_cost;
    affineCosts->gapOpenCost   = gapOpen;
    affineCosts->gapExtendCost = gapExtend;

    inputChars->maxSingleStep  = inputChars->numStates
                               = 0;
    size_t i;  // for use in multiple for loops below

    // TODO: change this from char to something else. Can we alloc this more intelligently, like not using MAX_STR?
    inputChars->lesserStr = calloc( MAX_STR, sizeof(char) );
    inputChars->middleStr = calloc( MAX_STR, sizeof(char) );
    inputChars->longerStr = calloc( MAX_STR, sizeof(char) );

    resultChars->lesserStr = calloc( MAX_STR * 2, sizeof(char) );
    resultChars->middleStr = calloc( MAX_STR * 2, sizeof(char) );
    resultChars->longerStr = calloc( MAX_STR * 2, sizeof(char) );

    // Initialize all characters. As with affineCosts, these will be passed around to remove globals and functional side effects.
    copyCharacter( inputChars->lesserStr, in_lesserChar) ;
    copyCharacter( inputChars->middleStr, in_middleChar) ;
    copyCharacter( inputChars->longerStr, in_longerChar) ;

    inputChars->lesserLen = in_lesserChar->len;
    inputChars->middleLen = in_middleChar->len;
    inputChars->longerLen = in_longerChar->len;

    resultChars->lesserLen = 0;
    resultChars->middleLen = 0;
    resultChars->longerLen = 0;

    resultChars->lesserIdx = 0;
    resultChars->middleIdx = 0;
    resultChars->longerIdx = 0;

    fsmStateArrays->neighbours                = calloc( MAX_STATES,              sizeof(int) );
    fsmStateArrays->fsmState_continuationCost = calloc( MAX_STATES,              sizeof(int) );
    fsmStateArrays->secondCost                = calloc( MAX_STATES,              sizeof(int) );
    fsmStateArrays->transitionCost            = calloc( MAX_STATES * MAX_STATES, sizeof(int) );
    fsmStateArrays->fsmState_num              = calloc( MAX_STATES,              sizeof(int) );

    int thisCost,
        cost,
        maxCost = 0;

    size_t fsmState_reindex_num  = 0;    // we reindex fsm_state sets to eliminate extras.

/* Don't need to do this; calloc'ed above
    for (i = 0; i < MAX_STATES - 1; i++) {
        affineCosts->neighbours[i] = 0;
        affineCosts->fsmState_continuationCost[i]   = 0;
        affineCosts->secondCost[i] = 0;
        affineCosts->fsmState_num[i]   = 0;
        for (size_t j = 0; j < MAX_STATES - 1; j++) {
            affineCosts->transitionCost[i][j] = 0;
        }
    }
*/


    /**** Now set cost arrays which correspond to FSM fsm_state-transition sets. This won't work for non-constant TCMs. ****/
    // TODO: Hard code this. It's a ridiculous amount of wasted time and code.

    Trans fsmState_transitions[3];    // safe because this gets reset in first line of for loop:
    size_t numInserts;
    size_t two_fsmStates_continuing; // if there are, for instance, two deletions that carry forward in the same

    // For each possible 3-FSM fsm_state, set fsm_state transitions.
    // If none of the three fsm states is a match, skip it.
    // Not clear why there can't be two inserts but can be two deletes. Maybe it's in Powell's thesis.
    for (size_t fsm_state = 0; fsm_state < MAX_STATES; fsm_state++) {
        transitions( fsmState_transitions, fsm_state );

        // TODO: document why this makes sense
        if (countThisTransition( fsmState_transitions, MATCH_SUB ) == 0) continue; // Must be at least one match
        if (countThisTransition( fsmState_transitions, INS       )  > 1) continue; // Can't be more than 1 insert fsm_state!  (7/7/1998)

/* Not doing this
        #ifdef LIMIT_TO_GOTOH
            // Gotoh86 only allowed fsm states that had a least 2 match fsm states. (Total of 7 possible)
            if (countThisTransition(fsmState_transitions, INS) + countThisTransition(fsmState_transitions, DEL) > 1) {
                continue;
            }
        #endif
*/
        fsmStateArrays->fsmState_num[fsmState_reindex_num] = fsm_state; // compacting possible fsm states into smaller set.
                                                                        // From now on can just loop over fsmState_reindex_num,
                                                                        // which means continuing to skip meaningless FSM
                                                                        // states.
        printf("%zu %zu\n", fsmState_reindex_num, fsm_state);

        // Set up possible neighbours for fsm states (neighbours[])
        numInserts = countThisTransition(fsmState_transitions, INS);

        if (numInserts == 0) { // if no inserts, then match/sub is 1 and del is 0 in resulting binary number
            fsmStateArrays->neighbours[fsmState_reindex_num] = neighbourNum( fsmState_transitions[0] == MATCH_SUB ? 1 : 0
                                                                           , fsmState_transitions[1] == MATCH_SUB ? 1 : 0
                                                                           , fsmState_transitions[2] == MATCH_SUB ? 1 : 0
                                                                           );
        } else { // numInserts == 1, as we've already eliminated any fsm states which have two INSs // TODO: looks this up
                 // if one insert, then match/sub or del is 0, ins is 1 in resulting binary number
            fsmStateArrays->neighbours[fsmState_reindex_num] = neighbourNum( fsmState_transitions[0] == INS ? 1 : 0
                                                                           , fsmState_transitions[1] == INS ? 1 : 0
                                                                           , fsmState_transitions[2] == INS ? 1 : 0
                                                                           );
        }
        // End setting up neighbours


        // Set up cost for continuing an fsm state (fsmState_continuationCost[])
        // For a given fsm_state, either 1 or more fsm states continue. If 2 or 3 continue,
        //
        // TODO: Why not hard code these arrays? They could be brought back if we ever move to larger alphabet sizes.
        if (countThisTransition( fsmState_transitions, INS ) > 0) { // TODO: hasn't this already been eliminated by the continue above?
            cost = affineCosts->gapExtendCost;           /* Can only continue 1 insert at a time. */ // TODO: look this up
            two_fsmStates_continuing = 0;
        } else if (countThisTransition( fsmState_transitions, MATCH_SUB ) == 3) {
            cost = affineCosts->mismatchCost;            /* No indel */
            two_fsmStates_continuing = 1;
        } else if (countThisTransition( fsmState_transitions, DEL ) == 1) {
            cost = affineCosts->gapExtendCost;        /* Continuing delete */ // Two fsm states must match
            two_fsmStates_continuing = 1;
        } else {
            cost = 2 * affineCosts->gapExtendCost;    /* Continuing 2 deletes */
            two_fsmStates_continuing = 0;
        }

        fsmStateArrays->fsmState_continuationCost[fsmState_reindex_num] = cost;
        fsmStateArrays->secondCost[fsmState_reindex_num]            = two_fsmStates_continuing;
        // End setup of fsmState_continuationCost[]

        fsmState_reindex_num++; // Because of continues, above, does not track `fsm_state`.
    }

    inputChars->numStates = fsmState_reindex_num;

    // Setup fsm_state transition costs (transitionCost[][])
    Trans from[3],
          to[3];

    for (size_t stateIdx1 = 0; stateIdx1 < inputChars->numStates; stateIdx1++) {
        for (size_t stateIdx2 = 0; stateIdx2 < inputChars->numStates; stateIdx2++) {

            cost = 0;
            transitions( from, fsmStateArrays->fsmState_num[stateIdx1] );
            transitions( to  , fsmStateArrays->fsmState_num[stateIdx2] );

            for (i = 0; i < 3; i++) {
                if (    (to[i] == INS || to[i] == DEL)
                     && (to[i] != from[i])
                   ){
                    cost += affineCosts->gapOpenCost;
                }
            }
            fsmStateArrays->transitionCost[stateIdx1 * MAX_STATES + stateIdx2] = cost;

            // Determine biggest single step cost
            thisCost = cost + fsmStateArrays->fsmState_continuationCost[stateIdx2];
            Trans fsmState_transitions[3];

            transitions( fsmState_transitions, fsmStateArrays->fsmState_num[stateIdx2] );

            thisCost += affineCosts->mismatchCost * ( countThisTransition(fsmState_transitions, MATCH_SUB) - 1 );
            maxCost   = (maxCost < thisCost ? thisCost : maxCost);

        }
    }

    inputChars->maxSingleStep = maxCost;
    // End setup of transition costs
}


/* Ensure that output characters are aligned. */
void checkAlign( char   *alignment
               , size_t  alLen
               , char   *str
               , size_t  strLen
               )
{
    size_t j = 0;

    for (size_t i = 0; i < alLen; i++) {
        if (alignment[i] == '-') continue;

        assert( alignment[i] == str[j] && "Output alignment not equal to input string" );
        j++;
    }

    assert( j == strLen && "Output alignment not equal length to input string" );
}


void revIntArray( int    *arr
                , size_t  start
                , size_t  end
                )
{
    if (end <= start) return;

    int swap;

    for (size_t i = start; i < (end + start) / 2; i++) {
        swap                     = arr[i];
        arr[i]                   = arr[end - i + start - 1];
        arr[end - i + start - 1] = swap;
    }
}


void revCharArray( char   *arr
                 , size_t  start
                 , size_t  end
                 )
{
    if (end <= start) return;

    char swap;

    for (size_t i = start; i < (end + start) / 2; i++) {
        swap                     = arr[i];
        arr[i]                   = arr[end - i + start - 1];
        arr[end - i + start - 1] = swap;
    }
}


/** Once three dynamic characters are aligned, step down the arrays and see when to
 *  add affine (deleted and instert) and mismatch costs.
 */
unsigned int alignmentCost( int             fsm_states[]
                          , char           *aligned_1
                          , char           *aligned_2
                          , char           *aligned_3
                          , size_t          len
                          , affine_costs_t *affineCosts
                          , fsm_arrays_t   *fsmStateArrays
                          )
{
    unsigned int totalCost = 0;
    size_t fsm_transitionIdx,        // FSM fsm_transitionIdx
           localCIdx;

    Trans last_fsmState_transitions[3] = { MATCH_SUB, MATCH_SUB, MATCH_SUB };
    Trans cur_fsmState_transitions[3];

    for (size_t i = 0; i < len; i++) {
        transitions( cur_fsmState_transitions, fsmStateArrays->fsmState_num[ fsm_states[i] ] );

    //    if (i > 0) fprintf( stderr, "%-2d  ", totalCost );

        // Pay for begining a gap.
        for (fsm_transitionIdx = 0; fsm_transitionIdx < 3; fsm_transitionIdx++) {
            if (   cur_fsmState_transitions[fsm_transitionIdx] != MATCH_SUB
                && cur_fsmState_transitions[fsm_transitionIdx] != last_fsmState_transitions[fsm_transitionIdx]
               ) {
                totalCost += affineCosts->gapOpenCost;
            }
        }

        for (fsm_transitionIdx = 0; fsm_transitionIdx < 3; fsm_transitionIdx++) {
            last_fsmState_transitions[fsm_transitionIdx] = cur_fsmState_transitions[fsm_transitionIdx];
        }

        // Pay for continuing an insert
        if (countThisTransition(cur_fsmState_transitions, INS) > 0) {
            assert(countThisTransition(cur_fsmState_transitions,INS) == 1);
            totalCost += affineCosts->gapExtendCost;
            continue;
        }

        // Pay for continuing deletes
        totalCost += affineCosts->gapExtendCost * countThisTransition(cur_fsmState_transitions, DEL);

        // Pay for mismatches
        char ch[3];
        localCIdx = 0;

        // For each character, if the state is match/sub
        // push onto `ch` array.
        if (cur_fsmState_transitions[0] == MATCH_SUB) {
            assert(aligned_1[i] != '-');
            ch[localCIdx++] = aligned_1[i];
        }

        if (cur_fsmState_transitions[1] == MATCH_SUB) {
            assert(aligned_2[i] != '-');
            ch[localCIdx++] = aligned_2[i];
        }

        if (cur_fsmState_transitions[2] == MATCH_SUB) {
            assert(aligned_3[i] != '-');
            ch[localCIdx++] = aligned_3[i];
        }

        localCIdx--;

        // Now loop over parts of `ch` that have been changed
        // and add a sub cost if there are mismatches.
        for (; localCIdx > 0; localCIdx--) {
            if (ch[localCIdx - 1] != ch[localCIdx]) {
                totalCost += affineCosts->mismatchCost;
            }
        }

        // Finally, this kludge because if ch[0] and ch[2] match, but ch[1] doesn't, then sub cost will have been added twice and
        // it shouldn't have been.
        if (   countThisTransition(cur_fsmState_transitions, MATCH_SUB) == 3
            && ch[0] == ch[2]
            && ch[0] != ch[1]
           ) {
            totalCost -= affineCosts->mismatchCost; // end pay for mismatch_costs
        }
    }

    printf ("The recomputed total cost is %d\n", totalCost);

    return totalCost;
}



/* ---------------------------------------------------------------------- */

// End of ukkCommon.c

