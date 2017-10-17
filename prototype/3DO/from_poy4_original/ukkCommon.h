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
// Has the mem allocation routines, in header files because they are all inline funcs
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
// Allocation stuff routines.  All inline -----------------------------------------

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

#define CellsPerBlock    10

typedef struct {
  int elemSize;
  int abSize,acSize;
  int abOffset,acOffset;
  long abBlocks, acBlocks;

#ifdef FIXED_NUM_PLANES
  int costSize;
#endif

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

#ifdef FIXED_NUM_PLANES
inline AllocInfo allocInit(int elemSize, int costSize)
#else
inline AllocInfo allocInit(int elemSize)
#endif
{
  AllocInfo a;
  
  a.memAllocated = 0;
  a.elemSize     = elemSize;

  a.abSize = Alen + Blen+1;
  a.acSize = Alen + Clen+1;
  a.abOffset = Blen;
  a.acOffset = Clen;

  a.abBlocks = a.abSize/CellsPerBlock+1;
  a.acBlocks = a.acSize/CellsPerBlock+1;

#ifdef FIXED_NUM_PLANES
  a.costSize  = costSize;
  a.baseAlloc = costSize;
#else
  a.baseAlloc = 20;		/* Whatever not really important, will increase as needed */
#endif

  a.memAllocated += a.baseAlloc * sizeof(void *);
  a.basePtr = calloc(a.baseAlloc, sizeof(void *));

  if (a.basePtr==NULL) {
    fprintf(stderr,"Unable to alloc memory\n");
    exit(-1);
  }

  return a;
}

inline void *allocEntry(AllocInfo *a)
{
  void *p;
  
  long entries = CellsPerBlock * CellsPerBlock * numStates;
  a->memAllocated += entries * a->elemSize;

  p=calloc(entries, a->elemSize);

  if (p==NULL) { fprintf(stderr,"Unable to alloc memory\n"); exit(-1); }

  return p;
}

inline long allocGetSubIndex(AllocInfo *a, int ab,int ac,int s)
{
  long index=0;

  int i=(ab + a->abOffset)/CellsPerBlock;
  int j=(ac + a->acOffset)/CellsPerBlock;
  int abAdjusted = ab + a->abOffset - i*CellsPerBlock;
  int acAdjusted = ac + a->acOffset - j*CellsPerBlock;

  //  fprintf(stderr,"ab=%d ac=%d abA=%d acA=%d abO=%d acO=%d i=%d j=%d\n",
  //	  ab,ac,abAdjusted,acAdjusted,a->abOffset,a->acOffset,i,j);
  
  assert(abAdjusted>=0 && abAdjusted<CellsPerBlock);
  assert(acAdjusted>=0 && acAdjusted<CellsPerBlock);
  assert(s>=0  && s<numStates);

  index = (index+abAdjusted)*CellsPerBlock;
  index = (index+acAdjusted)*numStates;
  index = (index + s);

  return index;
}

inline void *allocPlane(AllocInfo *a)
{
  void *p;

  a->memAllocated += a->abBlocks * a->acBlocks * sizeof(void*);
  p = calloc(a->abBlocks * a->acBlocks, sizeof(void*));
  if (p==NULL) { fprintf(stderr,"Unable to alloc memory\n"); exit(-1); }
  
  return p;
}

// recalloc - does a realloc() but sets any new memory to 0.
inline void *recalloc(void *p, size_t oldSize, size_t newSize)
{
  p = realloc(p, newSize);
  if (!p || oldSize>newSize) return p;

  memset(p+oldSize, 0, newSize-oldSize);
  return p;
}

inline void *getPtr(AllocInfo *a, int ab, int ac, int d, int s)
{
  int i,j;
  void **bPtr;
  void *base;
  int index;

#ifdef FIXED_NUM_PLANES
  // If doing a noalign or checkp,  remap 'd' into 0..costSize-1
  d = d % a->costSize;
#endif

  // Increase the base array as needed
  while (d >= a->baseAlloc) {
    int oldSize = a->baseAlloc;
    a->baseAlloc *= 2;
    a->basePtr = recalloc(a->basePtr, oldSize*sizeof(void *), a->baseAlloc*sizeof(void*));
    if (a->basePtr==NULL) { fprintf(stderr,"Unable to alloc memory\n"); exit(-1); }
    a->memAllocated += oldSize * sizeof(void*);
  }
  assert(d>=0 && d<a->baseAlloc);

  if (a->basePtr[d] == NULL)
    a->basePtr[d] = allocPlane(a);

  bPtr = a->basePtr[d];

  i=(ab+a->abOffset)/CellsPerBlock;
  j=(ac+a->acOffset)/CellsPerBlock;
  assert(i>=0 && i<a->abBlocks);
  assert(j>=0 && j<a->acBlocks);

  if (bPtr[(i*a->acBlocks)+j]==NULL)
    bPtr[(i*a->acBlocks)+j] = allocEntry(a);
  
  base = bPtr[(i*a->acBlocks)+j];
  assert(base != NULL);

  index = allocGetSubIndex(a, ab,ac,s);
  assert(index>=0);

  //  fprintf(stderr,"getPtr(ab=%d,ac=%d,d=%d,s=%d): base=%p index=%d\n", 
  //	  ab,ac,d,s,
  //	  base,index);

  return base + (index * a->elemSize);
}

inline void allocFinal(AllocInfo *a, void *flag, void *top)
{
  int usedFlag = flag-top;
  {
    int i,j,cIndex;
    long planesUsed=0;
    long blocksTotal=0, blocksUsed=0;
    long cellsTotal=0, cellsUsed=0;
    for (i=0; i<a->baseAlloc; i++) {
      long tblocksUsed=0;
      void **p = a->basePtr[i];
      if (!p) continue;
      planesUsed++;
      for (j=0; j<a->abBlocks * a->acBlocks; j++) {
	long tcellsUsed=0;
	void *block = p[j];
	blocksTotal++;
	if (!block) continue;
	blocksUsed++;
	tblocksUsed++;
	for (cIndex=0; cIndex<CellsPerBlock*CellsPerBlock*numStates; cIndex++) {
	  cellsTotal++;
	  if (  *(int*)(block+(cIndex*a->elemSize)+usedFlag)) {
	    cellsUsed++;
	    tcellsUsed++;
	  }
	}
    free (block);
      }
      free (p);
    }
    free (a->basePtr);
    a->basePtr=NULL;
  }
}
#endif


#endif
