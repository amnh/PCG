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

//#define NO_ALLOC_ROUTINES 1
#include "debug_constants.h"
#include "dyn_character.h"
//#include "ukkCheckp.h"
#include "ukkCommon.h"

// extern variable (all from ukkCheckp.c)

extern      alloc_info_t myUkkAllocInfo;
extern      alloc_info_t myCheckPtAllocInfo;
extern long costOffset;
extern long finalCost;

extern int    sabG,
              sacG,
              sCostG,
              sStateG;

extern size_t CPwidth;
extern int    CPcost;

extern size_t furthestReached;
extern size_t CPonDist;
extern size_t endA,
              endB,
              endC;

extern int    completeFromInfo;
// extern Counts counts;

//int  aCharIdx, bCharIdx, cCharIdx, stateIdx, costIdx;


// GLOBAL VARIABLES
// TODO: Can we make these variables non-global?
int neighbours[MAX_STATES];
int contCost  [MAX_STATES];
int secondCost[MAX_STATES];
int transCost [MAX_STATES] [MAX_STATES];
int stateNum  [MAX_STATES];

// TODO: can/should we make these unsigned ints?
unsigned int mismatchCost     = 1;
unsigned int gapOpenCost      = 3;             // a: w(k) = a + b * k
unsigned int gapExtendCost    = 1;             // b:
unsigned int deleteOpenCost   = 3;
unsigned int deleteExtendCost = 1;

size_t numStates;
size_t maxSingleStep;

char lesserStr[MAX_STR];
char longerStr[MAX_STR];
char middleStr[MAX_STR];

size_t lesserLen,
       longerLen,
       middleLen;

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
static inline void *recalloc(void *p, size_t oldSize, size_t newSize) {
    p = realloc(p, newSize);
    if (!p || oldSize > newSize) {
        return p;
    }

    // Cast the void pointer to char pointer to suppress compiler warnings.
    // We assume that arithmetic takes place in terms of bytes.
    memset(((char*)p) + oldSize, 0, newSize - oldSize);
    return p;
}

static inline void *allocPlane(alloc_info_t *a) {
    void *p;

    a->memAllocated += a->abBlocks * a->acBlocks * sizeof(void*);
    p = calloc(a->abBlocks * a->acBlocks, sizeof(void*));
    if (p==NULL) {
        fprintf(stderr,"Unable to alloc memory\n");
        exit(-1);
    }

    return p;
}

#ifdef FIXED_NUM_PLANES
    alloc_info_t allocInit(int elemSize, int costSize)
#else
    alloc_info_t allocInit(int elemSize)
#endif

{
    alloc_info_t retStruct;

    retStruct.memAllocated = 0;
    retStruct.elemSize     = elemSize;

    retStruct.abSize   = lesserLen + longerLen + 1;
    retStruct.acSize   = lesserLen + middleLen + 1;
    retStruct.abOffset = longerLen;
    retStruct.acOffset = middleLen;

    retStruct.abBlocks = retStruct.abSize / CELLS_PER_BLOCK + 1;
    retStruct.acBlocks = retStruct.acSize / CELLS_PER_BLOCK + 1;

    #ifdef FIXED_NUM_PLANES
        retStruct.costSize  = costSize;
        retStruct.baseAlloc = costSize;
    #else
        retStruct.baseAlloc = 20;       /* Whatever not really important, will increase as needed */
    #endif

    retStruct.memAllocated += retStruct.baseAlloc * sizeof(void *);
    retStruct.basePtr = calloc(retStruct.baseAlloc, sizeof(void *));

    if (retStruct.basePtr == NULL) {
        fprintf(stderr,"Unable to alloc memory\n");
        exit(-1);
    }

    return retStruct;
}

static inline void *allocEntry(alloc_info_t *a) {
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

static inline size_t allocGetSubIndex(alloc_info_t *a, int ab, int ac, int s)
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
    assert(s >= 0  && s < (int) numStates);

    index = (index + abAdjusted) * CELLS_PER_BLOCK;
    index = (index + acAdjusted) * numStates;
    index = (index + s);

    return index;
}


void allocFinal(alloc_info_t *a, void *flag, void *top) {
    // Cast the void pointers to long longs because we intend to treat the
    // pointers as integral values.
    int usedFlag = ((long long)flag) - ((long long )top);

    size_t i, j, cIndex;

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

void *getPtr(alloc_info_t *a, int ab, int ac, size_t d, int s) {
    size_t i, j;
    void **bPtr;
    void  *base;
    size_t index;

    #ifdef FIXED_NUM_PLANES
        // If doing a noalign or checkp,  remap 'd' into 0..costSize-1
        d = d % a->costSize;
    #endif

    // Increase the base array as needed
    while (d >= a->baseAlloc) {
        int oldSize   = a->baseAlloc;
        a->baseAlloc *= 2;
        a->basePtr    = recalloc(a->basePtr, oldSize * sizeof(void *), a->baseAlloc * sizeof(void *));
        if (a->basePtr == NULL) {
            fprintf(stderr, "Unable to alloc memory\n");
            exit(-1);
        }
        a->memAllocated += oldSize * sizeof(void *);
    }
    assert(d < a->baseAlloc);

    if (a->basePtr[d] == NULL) {
        a->basePtr[d] = allocPlane(a);
    }

    bPtr = a->basePtr[d];

    i = (ab + a->abOffset) / CELLS_PER_BLOCK;
    j = (ac + a->acOffset) / CELLS_PER_BLOCK;
    assert(i < a->abBlocks);
    assert(j < a->acBlocks);

    if (bPtr[(i * a->acBlocks) + j] == NULL) {
        bPtr[(i * a->acBlocks) + j] = allocEntry(a);
    }

    base = bPtr[(i * a->acBlocks) + j];
    assert(base != NULL);

    index = allocGetSubIndex(a, ab, ac, s);

    //  fprintf(stderr,"getPtr(ab=%d,ac=%d,d=%d,s=%d): base=%p index=%d\n",
    //    ab,ac,d,s,
    //    base,index);

    // Cast the void pointer to char pointer to suppress compiler warnings.
    // We assume that arithmetic takes place in terms of bytes.
    return ((char*)base) + (index * a->elemSize);
}


#endif // NO_ALLOC_ROUTINES


void copyCharacter (char *str, dyn_character_t *inChar) {
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

// IMPORTANT!!! Order of input characters is short, long, middle.
int powell_3D_align ( dyn_character_t *lesserChar
                    , dyn_character_t *longerChar
                    , dyn_character_t *middleChar
                    , dyn_character_t *retCharA
                    , dyn_character_t *retCharB
                    , dyn_character_t *retCharC
                    , unsigned int     mismatch
                    , unsigned int     gapOpen
                    , unsigned int     gapExtend
                    )
{
    if (DEBUG_CALL_ORDER) {
        printf("powell_3D_align\n");
    }

    // Nota bene: following are assigning to global vars.
    gapOpenCost      = gapOpen;
    gapExtendCost    = gapExtend;
    deleteOpenCost   = gapOpenCost;
    deleteExtendCost = gapExtendCost;
    mismatchCost     = mismatch;
    /* Char_custom_val(lesserChar,sa);
    Char_custom_val(longerChar,sb);
    Char_custom_val(middleChar,sc);
    Char_custom_val(retCharA,retCharA);
    Char_custom_val(retCharB,retCharB);
    Char_custom_val(retCharC,retCharC);
    */
    /* assert (mismatchCost != 0 && gapOpenCost >= 0 && gapExtendCost > 0); */ // SKIP, because now using unsigned ints

    copyCharacter (lesserStr, lesserChar);
    copyCharacter (longerStr, longerChar);
    copyCharacter (middleStr, middleChar);

    lesserLen = lesserChar->len;
    longerLen = longerChar->len;
    middleLen = middleChar->len;

    setup();

    return doUkk (retCharA, retCharB, retCharC);
}


int whichCharCost(char a, char b, char c) {
    if (DEBUG_CALL_ORDER) {
        printf("whichCharCost\n");
    }
    // printf("a: %c, b: %c, c: %c\n", a, b, c);
    assert(a!=0 && b!=0 && c!=0);
    /*
      When running as a ukk algorithm (ie. not the DPA), then
      a == b == c only when there is a run of matches after a state other that MMM,
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


int okIndex( size_t a
           , size_t da
           , size_t end
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




/* ---------------------------------------------------------------------- */
/* Common setup routines */

int stateTransitionCost(int from, int to) {
    return transCost[from][to];
}

// --------------------------------------------------
void step(int n, int *a, int *b, int *c) {
    assert(n > 0 && n <= 7);
    *a = (n >> 0) & 1;
    *b = (n >> 1) & 1;
    *c = (n >> 2) & 1;
}

int neighbourNum(int i, int j, int k) {
    return (i * 1) + (j * 2) + (k * 4);
}

// --------------------------------------------------

void transitions(int s, Trans st[3]) {
    st[0] = (s / 1) % 3;
    st[1] = (s / 3) % 3;
    st[2] = (s / 9) % 3;
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

void setup() {
    maxSingleStep = numStates
                  = 0;
    size_t i;

    for (i = 0; i < MAX_STATES - 1; i++) {
        neighbours[i] = 0;
        contCost[i]   = 0;
        secondCost[i] = 0;
        stateNum[i]   = 0;
        for (size_t j = 0; j < MAX_STATES - 1; j++) {
            transCost[i][j] = 0;
        }
    }
    int s, ns = 0;

    assert(gapOpenCost   == deleteOpenCost   && "Need to rewrite setup routine");
    assert(gapExtendCost == deleteExtendCost && "Need to rewrite setup routine");

    for (s = 0; s < MAX_STATES; s++) {
        Trans st[3];
        transitions(s, st);

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
        if (numInserts == 0) {
            neighbours[ns] = neighbourNum(st[0]==match ? 1 : 0,
                                          st[1]==match ? 1 : 0,
                                          st[2]==match ? 1 : 0);
        } else { // (numInserts == 1)
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
    size_t s1, s2;
    int maxCost = 0;

    assert(gapOpenCost == deleteOpenCost && "Need to rewrite setup routine");
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
            transitions(stateNum[s2], st);
            thisCost += mismatchCost * (countTrans(st, match) - 1);
            maxCost = (maxCost < thisCost ? thisCost : maxCost);

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

unsigned int alignmentCost( int     states[]
                          , char   *al1
                          , char   *al2
                          , char   *al3
                          , size_t  len
                          )
{
    unsigned int cost = 0;
    Trans last_st[3] = {match, match, match};

    assert(gapOpenCost == deleteOpenCost);

    for (size_t i = 0; i < len; i++) {
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
        int localCIdx = 0;

        if (st[0] == match) {
            assert(al1[i] != '-');
            ch[localCIdx++] = al1[i];
        }
        if (st[1] == match) {
            assert(al2[i] != '-');
            ch[localCIdx++] = al2[i];
        }
        if (st[2] == match) {
            assert(al3[i] != '-');
            ch[localCIdx++] = al3[i];
        }
        localCIdx--;
        for (; localCIdx > 0; localCIdx--) {
            if (ch[localCIdx-1] != ch[localCIdx])  cost += mismatchCost;
        }
        if (   countTrans(st, match) == 3
            && ch[0] == ch[2]
            && ch[0] != ch[1]
            ) {
            cost -= mismatchCost;
        }
        // end pay for mismatches
    }

    printf ("The recomputed cost is %d\n", cost);

    return cost;
}



/* ---------------------------------------------------------------------- */

// End of ukkCommon.c

