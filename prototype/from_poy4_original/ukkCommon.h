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


// Name:         ukkCommon.h
// FileType:     C Header
// Created:      Tue May 11 13:23:45 1999
// Author:       David Powell
//
// Contains the common routines for ukk.alloc, ukk.noalign, ukk.checkp and ukk.dpa
// Has the mem allocation routines, in header files because they are all  funcs
// Use the NO_ALLOC_ROUTINES flag to compile without alloc routines.
// Use the FIXED_NUM_PLANES flag to make routines allocate on d^2 memory.

#ifndef __UKKCOMMON_H__
#define __UKKCOMMON_H__

#include <string.h>

#define MAX_STR 100000

#define MAX_STATES 27                 // Maximum number of possible states, 3^3

#define MAX_COST (2*MAX_STR)
#define INFINITY MAXINT/2

typedef enum {match, del, ins} Trans;  // The 3 possible state-machine states

#ifndef __UKKCOMMON_C__
extern int misCost;
extern int startInsert;
extern int continueInsert;
extern int startDelete;
extern int continueDelete;

extern int neighbours[MAX_STATES];
extern int contCost[MAX_STATES];
extern int secondCost[MAX_STATES];
extern int transCost[MAX_STATES][MAX_STATES];
extern int stateNum[MAX_STATES];

extern int numStates;
extern int maxSingleStep;

extern char Astr[MAX_STR];
extern char Bstr[MAX_STR];
extern char Cstr[MAX_STR];
extern int Alen,Blen,Clen;
#endif

#define maxSingleCost  (maxSingleStep*2)

int whichCharCost(char a, char b, char c);
int okIndex(int a, int da, int end);

// Setup routines
int stateTransitionCost(int from, int to);
void step(int n, int *a, int *b, int *c);
int neighbourNum(int i, int j, int k);
void transitions(int s, Trans st[3]);
char *state2str(int s) ;
int countTrans(Trans st[3], Trans t);
void setup();

// Alignment checking routines
void checkAlign(char *al, int alLen, char *str, int strLen);
void revIntArray(int *arr, int start, int end);
void revCharArray(char *arr, int start, int end);
int alignmentCost(int states[], char *al1, char *al2, char *al3, int len);



#ifndef NO_ALLOC_ROUTINES
// Allocation stuff routines.  All  -----------------------------------------

/*
  Memory organisation:

  basePtr -> pointer to array of pointers.  Array is indexed by edit cost,
             and contains a pointer to the plane for that cost.

  A Block is square containing CellsPerBlock x CellsPerBlock cells
    (note: each cell contains numStates states)

  A plane is 2d array of pointers to Blocks.  Enough pointers to cover
  from ab=-|B|..|A| and ac=-|C|..|A|.  Each block is only allocated as needed.

*/

//#define FIXED_NUM_PLANES

#define FULL_ALLOC_INFO 0



typedef struct {
  int elemSize;
  int abSize,acSize;
  int abOffset,acOffset;
  long abBlocks, acBlocks;

// #ifdef FIXED_NUM_PLANES
  int costSize;
// #endif

  long baseAlloc;
  void **basePtr;

  long memAllocated;
} AllocInfo;

//U_cell_type **Umatrix;		/* 2 dimensional */

// aInfo = allocMatrix(sizeof(U_cell_type));
//void allocUmatrix() {
//  UcostSize = maxSingleCost*2;
//  Umatrix = (U_cell_type **)allocMatrix(sizeof(U_cell_type *));
//}

// #ifdef FIXED_NUM_PLANES
 AllocInfo allocInit(int elemSize, int costSize);
// #else
//  AllocInfo allocInit(int elemSize);
// #endif

 void *allocEntry(AllocInfo *a);

 long allocGetSubIndex(AllocInfo *a, int ab,int ac,int s);

 void *allocPlane(AllocInfo *a);

// recalloc - does a realloc() but sets any new memory to 0.
 void *recalloc(void *p, size_t oldSize, size_t newSize);

 void *getPtr(AllocInfo *a, int ab, int ac, int d, int s);

 void allocFinal(AllocInfo *a, void *flag, void *top);

int powell_3D_align (seqt sa, seqt sb, seqt sc, seqt ra, seqt rb, \
        seqt rc, int mm, int go, int ge);

#endif // NO_ALLOC_ROUTINES


#endif // UKKCOMMON_H
