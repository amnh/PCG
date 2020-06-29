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
 * Additional edits by Eric Ford at AMNH <eford@amnh.org>
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
 *  where m is the cost of a mismatch_cost,
 *      a is the cost to start a gap,
 *      b is the cost to extend a gap.
 */

// #define UKKCOMMON_C
#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#define NO_ALLOC_ROUTINES
#include "dyn_character.h"
#include "ukkCheckPoint.h"
#include "ukkCommon.h"

int neighbours[MAX_STATES];
int contCost[MAX_STATES];
int secondCost[MAX_STATES];
int transCost[MAX_STATES][MAX_STATES];
int stateNum[MAX_STATES];

int misCost_g        = 1;
int startInsert_g    = 3;             // a: w(k) = a + b * k
int continueInsert_g = 1;             // b:
int startDelete_g    = 3;
int continueDelete_g = 1;

elem_t gap_char_g;

int numStates_g;
int maxSingleStep_g;


int powell_3D_align ( characters_t *inputSeqs     // lengths set correctly; idices set to 0
                    , characters_t *outputSeqs    // lengths set correctly; idices set to 0
                    , size_t        alphabetSize  // not including gap
                    , int           mm            // mismatch cost, must be > 0
                    , int           go            // gap open cost, must be >= 0
                    , int           ge            // gap extension cost, must be > 0
                    )
{
    gap_char_g = 1 << alphabetSize;

    outputSeqs->idxSeq1 = 0;
    outputSeqs->idxSeq2 = 0;
    outputSeqs->idxSeq3 = 0;

    misCost_g        = mm;
    startInsert_g    = go;
    continueInsert_g = ge;
    startDelete_g    = go;    // note that these are same as insert
    continueDelete_g = ge;    // note that these are same as insert

    if (DEBUG_3D) {
        int i;
        for (i = 0; i < inputSeqs->lenSeq1; i++) {
            printf( "%d  ", inputSeqs->seq1[i] );
        }
        printf("\n");
        for (i = 0; i < inputSeqs->lenSeq2; i++) {
            printf( "%d  ", inputSeqs->seq2[i] );
        }
        printf("\n");
        for (i = 0; i < inputSeqs->lenSeq3; i++) {
            printf( "%d  ", inputSeqs->seq3[i] );
        }
        printf("\n");
    }

    setup();
    return doUkk( inputSeqs, outputSeqs );
}



/* ---------------------------------------------------------------------- */

int whichCharCost( int a, int b, int c )
{
    // This because it was calloc'ed, so a 0 means we've run past the end of the array.
    // TODO: This prevents us from using A == 0. What to do?
    // Ah, actually this might be because of null-terminated strings. Testing must happen.
    assert(   a != 0
           && b != 0
           && c != 0 );

    /*
      When running as a ukk algorithm (ie. not the DPA), then
      a = b = c only when there is a run of matches after a state other that MMM,
      and since we are moving to a MMM state, this cost will NEVER be used,
      so it doesn't matter what we return

      When running as the DPA, it can occur at a == b == c, return 0 in this case.

      Return 1 for the following
         x-- -x--- x
         xx- x - x -xx
         xxy xyx yxx

       return 2 for the following
         xy- x - y -xy
         xyz
    */
    if (a == b && a == c)    return 0;

    // Take care of any 2 the same
    if (a == b || a == c || b == c)    return 1;

    return 2;
}


int okIndex( int a, int da, int end)
{
    if (a < 0)           return 0;
    if (da && a < end)   return 1;
    if (!da && a <= end) return 1;

    return 0;
}




/*-- -------------------------------------------------------------------- */
/* Common setup routines */

int stateTransitionCost( int from, int to )
{
  return transCost[from][to];
}

void exists_neighbor_in_delete_state( int n, int *a, int *b, int *c )
{
  assert( n > 0 && n <= 7 );

  *a = (n >> 0) & 1;
  *b = (n >> 1) & 1;
  *c = (n >> 2) & 1;
}

int neighbourNum(int i, int j, int k) {
  return (i * 1) + (j * 2) + (k * 4);
}

//-- ----------------------------------------------- -

void transitions(int s, Trans st[3])
{
    st[0] = (s / 1) % 3;
    st[1] = (s / 3) % 3;
    st[2] = (s / 9) % 3;
}

char *state2str(int s)
{
    static char str[4];
    Trans st[3];
    int i;
    transitions(stateNum[s], st);
    for (i = 0; i < 3; i++) {
        str[i] = ( st[i] == match ? 'M'
                                  : ( st[i] == del ? 'D'
                                                   : 'I' ) );
    }
    return str;
}

int countTrans(Trans st[3], Trans t)
{
    int i, n = 0;
    for (i = 0; i < 3; i++) {
       if (st[i] == t) n++;
    }
    return n;
}


// TODO: Should be able to replace the first part of this with static array declaration.
// Also, see my update of this using mod.
void setup()
{
    int s,
        ns = 0;

    assert( startInsert_g    == startDelete_g    && "Need to rewrite setup routine" );
    assert( continueInsert_g == continueDelete_g && "Need to rewrite setup routine" );

    for (s = 0; s < MAX_STATES; s++) {
        Trans st[3];
        transitions(s, st);

        if (countTrans(st, match) == 0)    continue;     // Must be at least one match

        if (countTrans(st, ins) > 1)       continue;     // Can't be more than 1 insert state!  (7/7/1998)

#ifdef LIMIT_TO_GOTOH
        // Gotoh86 only allowed states that had a least 2 match states. (Total of 7 possible)
        if (countTrans(st, ins) + countTrans(st, del) > 1)    continue;
#endif

        stateNum[ns] = s;

        { // Setup possible neighbours for states (neighbours[])
            int numInserts = countTrans(st, ins);
            if (numInserts == 0) {
                neighbours[ns] = neighbourNum( st[0] == match ? 1 : 0,
                                               st[1] == match ? 1 : 0,
                                               st[2] == match ? 1 : 0 );
            } else { // (numInserts == 1)
                neighbours[ns] = neighbourNum( st[0] == ins ? 1 : 0,
                                               st[1] == ins ? 1 : 0,
                                               st[2] == ins ? 1 : 0 );
            }
        } // End setting up neighbo ur s


        { // Setup cost for continuing a state (contCost[])
            int cost, cont2;
            if (countTrans(st, ins) > 0) {
                cost  = continueInsert_g;        /* Can only continue 1 insert at a time */
                cont2 = 0;
            } else if (countTrans(st, match) == 3) {
                cost  = misCost_g;               /* All match states */
                cont2 = 1;
            } else if (countTrans(st, del) == 1) {
                cost  = continueDelete_g;        /* Continuing a delete */
                cont2 = 1;
            } else {
                cost = 2 * continueDelete_g;     /* Continuing 2 deletes */
                cont2 = 0;
            }
            contCost[ns]   = cost;
            secondCost[ns] = cont2;
        } // End setup of contCost[]

      ns++;
    } // end MAX STATES assignments

    numStates_g = ns;

    { // Setup state transition costs (transCost[][])
        int s1, s2;
        int maxCost = 0;

        assert( startInsert_g == startDelete_g && "Need to rewrite setup routine" );
        for (s1 = 0; s1 < numStates_g; s1++) {
            for (s2 = 0; s2 < numStates_g; s2++) {
                Trans from[3], to[3];
                int cost = 0;

                transitions(stateNum[s1], from);
                transitions(stateNum[s2], to);

                for (int i = 0; i < 3; i++) {
                    if (     (to[i] == ins || to[i] == del)
                          && (to[i] != from[i]) ) {
                        cost += startInsert_g;
                    }
                }
                transCost[s1][s2] = cost;

                { // Determine biggest single step cost
                    int thisCost = cost + contCost[s2];
                    Trans st[3];
                    transitions( stateNum[s2], st );
                    thisCost += misCost_g * (countTrans(st, match) - 1);
                    maxCost   = (maxCost < thisCost ? thisCost
                                                    : maxCost);
                } // biggest single step cost
            }
        }
        maxSingleStep_g = maxCost;
        fprintf(stderr, "Maximum single step cost = %d\n", maxSingleStep_g);
    } // End setup of transition costs
}


/* ---------------------------------------------------------------------- */
/* Some alignment checking routines */
void checkAlign( elem_t *al, int alLen, elem_t *str, int strLen )
{
    int i,
        j = 0;
    // char errorMsg[1024]; // for assertion outputs

    for (i = 0; i < alLen; i++) {
        if (al[i] == gap_char_g)    continue;
        printf( "Element in output alignment equals element in input string. a[i]: %2u, str[j]: %2u\n", al[i], str[j] );
        // assert( al[i] == str[j] );
        j++;
    }
    // sprintf( errorMsg, "Output alignment not equal length to input string. Index: %d, character length: %d", j, strLen);
    // assert( j == strLen );
}


void revIntArray( int *arr, int start, int end )
{
    if (end <= start)    return;

    for (int i = start; i < (end + start) / 2; i++) {
        int t  = arr[i];
        arr[i] = arr[end - i + start - 1];
        arr[end - i + start - 1] = t;
    }
}


void revElem_tArray( elem_t *arr, int start, int end )
{
    if (end <= start)    return;

    for (int i = start; i < (end + start) / 2; i++) {
        elem_t t                 = arr[i];
        arr[i]                   = arr[end - i + start - 1];
        arr[end - i + start - 1] = t;
    }
}


int alignmentCost(          int  states[]
                 , unsigned int *al1
                 , unsigned int *al2
                 , unsigned int *al3
                 ,          int  len
                 )
{
    int cost = 0;
    Trans last_st[3] = { match, match, match };

    assert( startInsert_g == startDelete_g );

    for (int i = 0; i < len; i++) {
        int s;
        Trans st[3];
        transitions(stateNum[states[i]], st);

//    if (i > 0) fprintf(stderr, "% - 2d  ", cost);

        // Pay for begining of gaps.
        for (s = 0; s < 3; s++) {
            if (st[s] != match && st[s] != last_st[s])   cost += startInsert_g;
        }

        for (s = 0; s < 3; s++)   last_st[s] = st[s];

        // Pay for continuing an insert
        if (countTrans(st, ins) > 0) {
            assert(countTrans(st, ins) == 1);
            cost += continueInsert_g;
            continue;
        }

        // Pay for continuing deletes
        cost += continueDelete_g * countTrans(st, del);

        // Pay for mismatches
        {
            int ch[3];
            int ci = 0;
            if (st[0] == match) {
                assert( al1[i] != gap_char_g );
                ch[ci++] = al1[i];
            }
            if (st[1] == match) {
                assert( al2[i] != gap_char_g );
                ch[ci++] = al2[i];
            }
            if (st[2] == match) {
                assert( al3[i] != gap_char_g );
                ch[ci++] = al3[i];
            }
            ci--;
            for (; ci > 0; ci--) {
                if (ch[ci - 1] != ch[ci])    cost += misCost_g;
            }
            if (     countTrans(st, match) == 3
                  && ch[0] == ch[2]
                  && ch[0] != ch[1]) {
                cost -= misCost_g;
            }
        }

    }

    return cost;
}


void *allocEntry( AllocInfo_t *a )
{
    void *p;

    long entries     = CellsPerBlock * CellsPerBlock * numStates_g;
    a->memAllocated += entries * a->elemSize;

    p = calloc(entries, a->elemSize);
    assert( p != NULL && "allocEntry: Unable to alloc memory." );

    return p;
}
