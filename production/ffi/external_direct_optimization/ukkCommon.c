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
extern      alloc_info_t myUkkAllocInfo;
extern      alloc_info_t myCheckPtAllocInfo;
extern long costOffset;
extern long finalCost;

extern int    sabG,
              sacG,
              sCostG,
              sStateG;

extern size_t checkPoint_width;
extern int    checkPoint_cost;

extern size_t furthestReached;
extern size_t checkPoint_onDist;
extern size_t endA,
              endB,
              endC;

extern int    completeFromInfo;
// extern Counts counts;

//int  aCharIdx, bCharIdx, cCharIdx, stateIdx, costIdx;



//extern int doUkk(dyn_character_t *retCharA, dyn_character_t *retCharB, dyn_character_t *retCharC);    // Main driver function


#ifndef NO_ALLOC_ROUTINES
// Allocation stuff routines.  All inline -----------------------------------------

//U_cell_type **Umatrix;        /* 2 dimensional */

// aInfo = allocMatrix(sizeof(U_cell_type));
//void allocUmatrix() {
//  UcostSize = maxSingleCost*2;
//  Umatrix = (U_cell_type **)allocMatrix(sizeof(U_cell_type *));
//}

// recalloc - does a realloc() but sets any new memory to 0.
static inline void *recalloc( void *p
                            , size_t oldSize
                            , size_t newSize
                            )
{
    p = realloc(p, newSize);
    if (!p || oldSize > newSize) {
        return p;
    }

    // Cast the void pointer to char pointer to suppress compiler warnings.
    // We assume that arithmetic takes place in terms of bytes.
    memset(((char*)p) + oldSize, 0, newSize - oldSize);
    return p;
}

static inline void *allocPlane( alloc_info_t *a )
{
    void *p;

    a->memAllocated += a->abBlocks * a->acBlocks * sizeof(void*);
    p = calloc( a->abBlocks * a->acBlocks, sizeof(void*) );
    if (p==NULL) {
        fprintf(stderr, "Unable to alloc memory\n");
        exit(-1);
    }

    return p;
}

#ifdef FIXED_NUM_PLANES
    alloc_info_t allocInit( size_t elemSize
                          , size_t costSize
                          , global_characters_t *globalCharacters
                          )
#else
    alloc_info_t allocInit( size_t elemSize
                          , global_characters_t *globalCharacters
                          )
#endif

{
    alloc_info_t retStruct;

    retStruct.memAllocated = 0;
    retStruct.elemSize     = elemSize;

    retStruct.abSize   = globalCharacters->lesserLen + globalCharacters->longerLen + 1;
    retStruct.acSize   = globalCharacters->lesserLen + globalCharacters->middleLen + 1;
    retStruct.abOffset = globalCharacters->longerLen;
    retStruct.acOffset = globalCharacters->middleLen;

    retStruct.abBlocks = retStruct.abSize / CELLS_PER_BLOCK + 1;
    retStruct.acBlocks = retStruct.acSize / CELLS_PER_BLOCK + 1;

    #ifdef FIXED_NUM_PLANES
        retStruct.costSize  = costSize;
        retStruct.baseAlloc = costSize;
    #else
        retStruct.baseAlloc = 20;       /* Whatever not really important, will increase as needed */
    #endif

    retStruct.memAllocated += retStruct.baseAlloc * sizeof(void *);

    retStruct.basePtr = calloc( retStruct.baseAlloc, sizeof(void *) );

    if (retStruct.basePtr == NULL) {
        fprintf(stderr,"Unable to alloc memory\n");
        exit(-1);
    }

    return retStruct;
}

static inline void *allocEntry( alloc_info_t *a
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

static inline size_t allocGetSubIndex( alloc_info_t *a
                                     , int           ab
                                     , int           ac
                                     , int           state
                                     , size_t        numStates
                                     )
{
    size_t index = 0;

    size_t i = (ab + a->abOffset) / CELLS_PER_BLOCK;
    size_t j = (ac + a->acOffset) / CELLS_PER_BLOCK;
    int abAdjusted = ab + a->abOffset - i * CELLS_PER_BLOCK;
    int acAdjusted = ac + a->acOffset - j * CELLS_PER_BLOCK;

    //  fprintf(stderr,"ab=%d ac=%d abA=%d acA=%d abO=%d acO=%d i=%d j=%d\n",
    //    ab,ac,abAdjusted,acAdjusted,a->abOffset,a->acOffset,i,j);

    assert(abAdjusted >= 0 && abAdjusted < CELLS_PER_BLOCK);
    assert(acAdjusted >= 0 && acAdjusted < CELLS_PER_BLOCK);
    assert(state      >= 0 && state      < (int) numStates);

    index = (index + abAdjusted) * CELLS_PER_BLOCK;
    index = (index + acAdjusted) * numStates;
    index = (index + state);

    return index;
}


void allocFinal( alloc_info_t *a
               , void         *flag
               , void         *top
               , size_t        numStates
               )
{
    // Cast the void pointers to long longs because we intend to treat the
    // pointers as integral values.
    int usedFlag = ((long long) flag) - ((long long) top);

    size_t i,
           j,
           cIndex;

    size_t planesUsed = 0;

    size_t blocksTotal = 0,
           blocksUsed  = 0;

    size_t cellsTotal = 0,
           cellsUsed  = 0;

    for (i = 0; i < a->baseAlloc; i++) {
        long tblocksUsed = 0;
        void **p = a->basePtr[i];

        if (!p) {
            continue;
        }

        planesUsed++;

        for (j = 0; j < a->abBlocks * a->acBlocks; j++) {
            long tcellsUsed = 0;
            void *block = p[j];
            blocksTotal++;

            if (!block) {
                continue;
            }

            blocksUsed++;
            tblocksUsed++;

            for (cIndex = 0; cIndex < numStates * CELLS_PER_BLOCK * CELLS_PER_BLOCK; cIndex++) {
                cellsTotal++;

                // Cast the void pointer to char pointer to suppress compiler warnings.
                // We assume that arithmetic takes place in terms of bytes.
                if ( *(int*)(((char*)block) + (cIndex * a->elemSize) + usedFlag)) {
                    cellsUsed++;
                    tcellsUsed++;
                }
            }
            free (block);
      }
      free (p);
    }
    free (a->basePtr);
    a->basePtr = NULL;

}

void *getPtr( alloc_info_t *a
            , int           ab
            , int           ac
            , size_t        editDist
            , int           state
            , size_t        numStates
            )
{
    size_t i, j;
    void **bPtr;
    void  *base;
    size_t index;

    #ifdef FIXED_NUM_PLANES
        // If doing a noalign or checkp,  remap 'd' into 0..costSize-1
        editDist = editDist % a->costSize;
    #endif

    // Increase the base array as needed
    while (editDist >= a->baseAlloc) {

        int oldSize   = a->baseAlloc;
        a->baseAlloc *= 2;
        a->basePtr    = recalloc( a->basePtr
                                , oldSize * sizeof(void *)
                                , a->baseAlloc * sizeof(void *)
                                );

        if (a->basePtr == NULL) {
            fprintf(stderr, "Unable to alloc memory\n");
            exit(-1);
        }

        a->memAllocated += oldSize * sizeof(void *);
    }

    assert(editDist < a->baseAlloc);

    if (a->basePtr[editDist] == NULL)  a->basePtr[editDist] = allocPlane( a );

    bPtr = a->basePtr[editDist];

    i = (ab + a->abOffset) / CELLS_PER_BLOCK;
    j = (ac + a->acOffset) / CELLS_PER_BLOCK;
    assert(i < a->abBlocks);
    assert(j < a->acBlocks);

    if (bPtr[(i * a->acBlocks) + j] == NULL) {
        bPtr[(i * a->acBlocks) + j] = allocEntry( a, numStates );
    }

    base = bPtr[(i * a->acBlocks) + j];
    assert(base != NULL);

    index = allocGetSubIndex( a
                            , ab
                            , ac
                            , state
                            , numStates
                            );

    //  fprintf(stderr,"getPtr(ab=%d,ac=%d,d=%d,s=%d): base=%p index=%d\n",
    //    ab,ac,d,s,
    //    base,index);

    // Cast the void pointer to char pointer to suppress compiler warnings.
    // We assume that arithmetic takes place in terms of bytes.
    return ( (char*) base) + (index * a->elemSize);
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
            printf ("This is impossible! %d !!", char_begin[i]);
            fflush(stdout);
            exit(1);
        }
    }
    str[len - 1] = 0;
    return;
}


int whichCharCost(char a, char b, char c) {
    if (DEBUG_CALL_ORDER) {
        printf("whichCharCost\n");
    }
    // printf("a: %c, b: %c, c: %c\n", a, b, c);
    assert(a!=0 && b!=0 && c!=0);
    /*
      When running as a ukk algorithm (ie. not the DPA), then
      a == b == c only when there is a run of MATCHes after a state other that MMM,
      and since we are moving to a MMM state, this cost will NEVER be used,
      so it doesn't matter what we return
      When running as the DPA, it can occur at a == b == c, return 0 in this case
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


/** Make sure that index a is valid for a given set of array indices */
int okIndex( int a
           , int da
           , int end
           )
{
    // if (DEBUG_CALL_ORDER) {
    //     printf("okIndex\n");
    // }
    if (a < 0)           return 0;
    if ( da && a <  end) return 1;
    if (!da && a <= end) return 1;

    return 0;
    //  return (a<0 ? 0 : (da==0 ? 1 : a<end));
}




/******** Setup routines ********/

/*
int stateTransitionCost( int from
                       , int to
                       , int *transCost
                       )
{
    return transCost[from][to];
}
*/

// --------------------------------------------------
void step( int n
         , int *a
         , int *b
         , int *c
         )
{
    assert(n > 0 && n <= 7);
    *a = (n >> 0) & 1;
    *b = (n >> 1) & 1;
    *c = (n >> 2) & 1;
}

int neighbourNum( int i
                , int j
                , int k
                )
{
    return (i * 1) + (j * 2) + (k * 4);
}

// --------------------------------------------------

void transitions( Trans  stateTransitions[3]
                , size_t state
                )
{
    stateTransitions[0] = (state / 1) % 3;
    stateTransitions[1] = (state / 3) % 3;
    stateTransitions[2] = (state / 9) % 3;
}


/** Takes an array of states by value and returns a string of those states as match, delete, insert.
 *  Used only in printing of state array if DEBUG_3D is set.
 */
char *state2str( size_t  state
               , int    *stateNum
               )
{
    static char returnStr[4];
    Trans stateTransitions[3];

    transitions( stateTransitions, stateNum[state] );

    for (size_t i = 0; i < 3; i++) {
        returnStr[i] = ( stateTransitions[i] == MATCH ? 'M' : ( stateTransitions[i] == DEL ? 'D' : 'I') );
    }
    return returnStr;
}


/** Count number of times whichTransition appears in stateTransitions */
size_t countThisTransition( Trans stateTransitions[3]
                          , Trans whichTransition
                          )
{
    size_t num = 0;

    for (size_t i = 0; i < 3; i++) {
        if (stateTransitions[i] == whichTransition) num++;
    }
    return num;
}


/** Set up the Ukkonnen and check point matrices before running alignment. */
void setup( global_costs_t      *globalCosts
          , global_characters_t *globalCharacters
          , global_arrays_t     *globalCostArrays
          , dyn_character_t     *lesserChar
          , dyn_character_t     *middleChar
          , dyn_character_t     *longerChar
          , unsigned int         mismatch_cost
          , unsigned int         gapOpen
          , unsigned int         gapExtend
          )
{

    // Initialize global costs. These will be passed around to remove globals and functional side effects.
    globalCosts->mismatchCost       = mismatch_cost;
    globalCosts->gapOpenCost        = gapOpen;
    globalCosts->gapExtendCost      = gapExtend;
    globalCosts->deleteOpenCost     = gapOpen;
    globalCosts->deleteExtendCost   = gapExtend;
    globalCharacters->maxSingleStep = globalCharacters->numStates
                                    = 0;
    size_t i;

    // TODO: change this from char to something else. Can we alloc this more intelligently, like not using MAX_STR?
    globalCharacters->lesserStr = calloc( MAX_STR, sizeof(char) );
    globalCharacters->longerStr = calloc( MAX_STR, sizeof(char) );
    globalCharacters->middleStr = calloc( MAX_STR, sizeof(char) );

    // Initialize all characters. As with globalCosts, these will be passed around to remove globals and functional side effects.
    copyCharacter (globalCharacters->lesserStr, lesserChar);
    copyCharacter (globalCharacters->longerStr, longerChar);
    copyCharacter (globalCharacters->middleStr, middleChar);

    globalCharacters->lesserLen = lesserChar->len;
    globalCharacters->longerLen = longerChar->len;
    globalCharacters->middleLen = middleChar->len;

    globalCostArrays->neighbours = calloc( MAX_STATES,              sizeof(int) );
    globalCostArrays->contCost   = calloc( MAX_STATES,              sizeof(int) );
    globalCostArrays->secondCost = calloc( MAX_STATES,              sizeof(int) );
    globalCostArrays->transCost  = calloc( MAX_STATES * MAX_STATES, sizeof(int) );
    globalCostArrays->stateNum   = calloc( MAX_STATES,              sizeof(int) );

    int thisCost,
        cost,
        maxCost = 0,
        nState  = 0;

    size_t s1,
           s2;

/* Don't need to do this; calloc'ed above
    for (i = 0; i < MAX_STATES - 1; i++) {
        globalCosts->neighbours[i] = 0;
        globalCosts->contCost[i]   = 0;
        globalCosts->secondCost[i] = 0;
        globalCosts->stateNum[i]   = 0;
        for (size_t j = 0; j < MAX_STATES - 1; j++) {
            globalCosts->transCost[i][j] = 0;
        }
    }
*/

    assert(globalCosts->gapOpenCost   == globalCosts->deleteOpenCost   && "Need to rewrite setup routine");
    assert(globalCosts->gapExtendCost == globalCosts->deleteExtendCost && "Need to rewrite setup routine");

    for (size_t state = 0; state < MAX_STATES; state++) {
        Trans stateTransitions[3];
        transitions( stateTransitions, state );

        if (countThisTransition(stateTransitions, MATCH) == 0) {
          continue;     // Must be at least one match
        }

        if (countThisTransition(stateTransitions, INS) > 1) {
          continue;     // Can't be more than 1 insert state!  (7/7/1998)
        }

        #ifdef LIMIT_TO_GOTOH
            // Gotoh86 only allowed states that had a least 2 match states. (Total of 7 possible)
            if (countThisTransition(stateTransitions, INS) + countThisTransition(stateTransitions, DEL) > 1) {
              continue;
            }
        #endif

        globalCostArrays->stateNum[nState] = state;

        // Setup possible neighbours for states (neighbours[])
        int numInserts = countThisTransition(stateTransitions, INS);
        if (numInserts == 0) {
            globalCostArrays->neighbours[nState] = neighbourNum( stateTransitions[0] == MATCH ? 1 : 0
                                                               , stateTransitions[1] == MATCH ? 1 : 0
                                                               , stateTransitions[2] == MATCH ? 1 : 0
                                                               );
        } else { // (numInserts == 1)
            globalCostArrays->neighbours[nState] = neighbourNum( stateTransitions[0] == INS ? 1 : 0
                                                               , stateTransitions[1] == INS ? 1 : 0
                                                               , stateTransitions[2] == INS ? 1 : 0
                                                               );
        }
        // End setting up neighbours


        // Setup cost for continuing a state (contCost[])
        int cont2;
        if (countThisTransition(stateTransitions, INS) > 0) {
            cost  = globalCosts->gapExtendCost;    /* Can only continue 1 insert at a time */
            cont2 = 0;
        } else if (countThisTransition(stateTransitions, MATCH) == 3) {
            cost  = globalCosts->mismatchCost;        /* All match states */
            cont2 = 1;
        } else if (countThisTransition(stateTransitions, DEL) == 1) {
            cost  = globalCosts->deleteExtendCost;    /* Continuing  delete */
            cont2 = 1;
        } else {
            cost  = 2 * globalCosts->deleteExtendCost;    /* Continuing 2 deletes */
            cont2 = 0;
        }
        globalCostArrays->contCost[nState]   = cost;
        globalCostArrays->secondCost[nState] = cont2;
        // End setup of contCost[]

        nState++; // Because of continues, above, does not track `state`.
    }

    globalCharacters->numStates = nState;

    // Setup state transition costs (transCost[][])

    assert(globalCosts->gapOpenCost == globalCosts->deleteOpenCost && "Need to rewrite setup routine");
    for (s1 = 0; s1 < globalCharacters->numStates; s1++) {
        for (s2 = 0; s2 < globalCharacters->numStates; s2++) {
            Trans from[3], to[3];
            int cost = 0;
            transitions( from, globalCostArrays->stateNum[s1] );
            transitions( to  , globalCostArrays->stateNum[s2] );

            for (i = 0; i < 3; i++) {
                if (    (to[i] == INS || to[i] == DEL)
                     && (to[i] != from[i])
                   ){
                    cost += globalCosts->gapOpenCost;
                }
            }
            globalCostArrays->transCost[s1 * MAX_STATES + s2] = cost;

            // Determine biggest single step cost
            thisCost = cost + globalCostArrays->contCost[s2];
            Trans stateTransitions[3];

            transitions( stateTransitions, globalCostArrays->stateNum[s2] );

            thisCost += globalCosts->mismatchCost * (countThisTransition(stateTransitions, MATCH) - 1);
            maxCost   = (maxCost < thisCost ? thisCost : maxCost);

        }
    }

    globalCharacters->maxSingleStep = maxCost;
    // End setup of transition costs
}




/* ---------------------------------------------------------------------- */
/* Some alignment checking routines */
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


/**  */
unsigned int alignmentCost( int              states[]
                          , char            *al1
                          , char            *al2
                          , char            *al3
                          , size_t           len
                          , global_costs_t  *globalCosts
                          , global_arrays_t *globalCostArrays
                          )
{
    unsigned int totalCost = 0;
    size_t state;

    Trans last_stateTransitions[3] = {MATCH, MATCH, MATCH};
    Trans stateTransitions[3];

    assert( globalCosts->gapOpenCost == globalCosts->deleteOpenCost );

    for (size_t i = 0; i < len; i++) {
        transitions( stateTransitions, globalCostArrays->stateNum[ states[i] ] );

    //    if (i>0) fprintf(stderr,"%-2d  ",totalCost);

        // Pay for begining a gap.
        for (state = 0; state < 3; state++) {
            if (   stateTransitions[state] != MATCH
                && stateTransitions[state] != last_stateTransitions[state]
               ) {
                totalCost += globalCosts->gapOpenCost;
            }
        }

        for (state = 0; state < 3; state++) {
            last_stateTransitions[state] = stateTransitions[state];
        }

        // Pay for continuing an insert
        if (countThisTransition(stateTransitions, INS) > 0) {
            assert(countThisTransition(stateTransitions,INS) == 1);
            totalCost += globalCosts->gapExtendCost;
            continue;
        }

        // Pay for continuing deletes
        totalCost += globalCosts->deleteExtendCost * countThisTransition(stateTransitions, DEL);

        // Pay for mismatches
        char ch[3];
        int localCIdx = 0;

        if (stateTransitions[0] == MATCH) {
            assert(al1[i] != '-');
            ch[localCIdx++] = al1[i];
        }

        if (stateTransitions[1] == MATCH) {
            assert(al2[i] != '-');
            ch[localCIdx++] = al2[i];
        }

        if (stateTransitions[2] == MATCH) {
            assert(al3[i] != '-');
            ch[localCIdx++] = al3[i];
        }

        localCIdx--;

        for (; localCIdx > 0; localCIdx--) {
            if (ch[localCIdx-1] != ch[localCIdx])  totalCost += globalCosts->mismatchCost;
        }

        if (   countThisTransition(stateTransitions, MATCH) == 3
            && ch[0] == ch[2]
            && ch[0] != ch[1]
           ) {
            totalCost -= globalCosts->mismatchCost; // end pay for mismatch_costes
        }
    }

    printf ("The recomputed total cost is %d\n", totalCost);

    return totalCost;
}



/* ---------------------------------------------------------------------- */

// End of ukkCommon.c

