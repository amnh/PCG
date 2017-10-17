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


// Name:         ukkCommon.c
// Type:         C Source
// Created:      Tue May 11 13:22:07 1999
// Author:       David Powell

// Contains the common routines for ukk.alloc, ukk.noalign, ukk.checkp and ukk.dpa
// Also see ukkCommon.h
// Compile with -DSYSTEM_INFO to print system information of
// every run.  Useful to timing runs where cpu info is
// important.

#define __UKKCOMMON_C__
#include <stdio.h>
#include <stdlib.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/fail.h>
#define NDEBUG 1
#include <assert.h>
#include "seq.h"

#define NO_ALLOC_ROUTINES
#include "ukkCommon.h"

int neighbours[MAX_STATES];
int contCost[MAX_STATES];
int secondCost[MAX_STATES];
int transCost[MAX_STATES][MAX_STATES];
int stateNum[MAX_STATES];

int misCost        = 1;
int startInsert    = 3;             // a: w(k)=a+b*k
int continueInsert = 1;             // b:
int startDelete = 3;
int continueDelete = 1;

int numStates;
int maxSingleStep;

char Astr[MAX_STR];
char Bstr[MAX_STR];
char Cstr[MAX_STR];
int Alen,Blen,Clen;

int doUkk();            // Main driver function

int progDesc(char *);         // Description of program unique to each prog

void copyright() {
  printf("Copyright (C) David Powell <david@drp.id.au>\n");
  printf("  This program comes with ABSOLUTELY NO WARRANTY; and is provided\n");
  printf("  under the GNU Public License v2, for details see file COPYRIGHT\n\n");
}

void usage(char *prog) {
  printf(
    "Usage: %s [m a b]\n"
    "  where m is the cost of a mismatch,\n"
    "        a is the cost to start a gap,\n"
    "        b is the cost to extend a gap.\n"
    "\n\n"
    ,prog
  );
}

void 
copySequence (struct seq *s, char *str) {
    int len, i;
    int v;
    SEQT *begin;
    len = seq_get_len (s);
    begin = seq_get_begin(s);
    for (i = 1; i < len; i++) {
        if (begin[i] & 1) 
            str[i - 1] = 'A';
        else if (begin[i] & 2) 
            str[i - 1] = 'C';
        else if (begin[i] & 4)
            str[i - 1] = 'G';
        else if (begin[i] & 8)
            str[i - 1] = 'T';
        else
            failwith ("This is impossible!");
    }
    str[len - 1] = 0;
    return;
}

value powell_3D_align (value sa, value sb, value sc, value ra, value rb, \
        value rc, value mm, value go, value ge)
{
    CAMLparam5(sa, sb, sc, mm, go);
    CAMLxparam4(ge, ra, rb, rc);

    struct seq *csa, *csb, *csc;
    struct seq *cra, *crb, *crc;

    misCost        = Int_val(mm);
    startInsert    = Int_val(go);
    continueInsert = Int_val(ge);
    startDelete = startInsert;
    continueDelete = continueInsert;
    Seq_custom_val(csa,sa);
    Seq_custom_val(csb,sb);
    Seq_custom_val(csc,sc);
    Seq_custom_val(cra,ra);
    Seq_custom_val(crb,rb);
    Seq_custom_val(crc,rc);

  assert(misCost != 0 && startInsert>=0 && continueInsert>0);
  

  copySequence (csa, Astr);
  copySequence (csb, Bstr);
  copySequence (csc, Cstr);

  Alen=strlen(Astr);
  Blen=strlen(Bstr);
  Clen=strlen(Cstr);

  setup();
  printf("powell: gap open = %2d, gap extend = %2d, mismatch = %2d\n", startInsert, continueInsert, misCost);

  CAMLreturn(Val_int(doUkk(cra, crb, crc)));
}

value
powell_3D_align_bc (value *argv, int argn) {
    return (powell_3D_align (argv[0], argv[1], argv[2], argv[3], argv[4], \
                argv[5], argv[6], argv[7], argv[8]));
}

/* ---------------------------------------------------------------------- */

int whichCharCost(char a, char b, char c)
{
  assert(a!=0 && b!=0 && c!=0);

  /* 
    When running as a ukk algorithm (ie. not the DPA), then
    a=b=c only when there is a run of matches after a state other that MMM,
    and since we are moving to a MMM state, this cost will NEVER be used,
    so it doesn't matter what we return

    When running as the DPA, it can occur at a=b=c, return 0 in this case
  */
  if (a==b && a==c)
    return 0;


  /* return 1 for the following
       x-- -x- --x
       xx- x-x -xx
       xxy xyx yxx
  
     return 2 for the following
       xy- x-y -xy
       xyz
  */

  // Take care of any 2 the same
  if (a==b || a==c || b==c) return 1;  
  return 2;
}


int okIndex(int a, int da, int end) {
  if (a<0) return 0;
  if (da && a<end) return 1;
  if (!da && a<=end) return 1;
  return 0;
  //  return (a<0 ? 0 : (da==0 ? 1 : a<end));
}




/* ---------------------------------------------------------------------- */
/* Common setup routines */

int stateTransitionCost(int from, int to)
{
  return transCost[from][to];
}

// --------------------------------------------------
void step(int n, int *a, int *b, int *c)
{
  assert(n>0 && n<=7);
  *a=(n>>0)&1;
  *b=(n>>1)&1;
  *c=(n>>2)&1;
}

int neighbourNum(int i, int j, int k) {
  return (i*1)+(j*2)+(k*4);
}

// --------------------------------------------------

void transitions(int s, Trans st[3])
{
  st[0] = (s/1)%3;
  st[1] = (s/3)%3;
  st[2] = (s/9)%3;
}

char *state2str(int s) 
{
  static char str[4];
  Trans st[3];
  int i;
  transitions(stateNum[s], st);
  for (i=0;i<3;i++)
    str[i]=(st[i]==match ? 'M' : (st[i]==del ? 'D' : 'I'));
  return str;
}

int countTrans(Trans st[3], Trans t)
{
  int i,n=0;
  for (i=0;i<3;i++)
    if (st[i]==t) n++;
  return n;
}

void setup()
{
    maxSingleStep = numStates = 0;
    int i, j;
    for (i = 0; i < MAX_STATES - 1; i++) {
        neighbours[i] = 0;
        contCost[i] = 0;
        secondCost[i] = 0;
        stateNum[i] = 0;
        for (j = 0; j < MAX_STATES - 1; j++) 
            transCost[i][j] = 0;
    }
  int s,ns=0;

  assert(startInsert==startDelete && "Need to rewrite setup routine");
  assert(continueInsert==continueDelete && "Need to rewrite setup routine");

  for (s=0; s<MAX_STATES; s++) {
    Trans st[3];
    transitions(s,st);

    if (countTrans(st, match) == 0)
      continue;     // Must be at least one match

    if (countTrans(st, ins) > 1)
      continue;     // Can't be more than 1 insert state!  (7/7/1998)

#ifdef LIMIT_TO_GOTOH
    // Gotoh86 only allowed states that had a least 2 match states. (Total of 7 possible)
    if (countTrans(st, ins) + countTrans(st, del) > 1)
      continue;
#endif

    stateNum[ns] = s;

    { // Setup possible neighbours for states (neighbours[])
      int numInserts = countTrans(st, ins);
      if (numInserts==0) {
        neighbours[ns] = neighbourNum(st[0]==match ? 1 : 0,
                                      st[1]==match ? 1 : 0,
                                      st[2]==match ? 1 : 0);
      } else { // (numInserts==1)
        neighbours[ns] = neighbourNum(st[0]==ins ? 1 : 0,
                                      st[1]==ins ? 1 : 0,
                                      st[2]==ins ? 1 : 0);
      }
    } // End setting up neighbours


    { // Setup cost for continuing a state (contCost[])
      int cost, cont2;
      if (countTrans(st, ins)>0) {
        cost=continueInsert;	/* Can only continue 1 insert at a time */
	cont2=0;
      } else if (countTrans(st, match)==3) {
        cost=misCost;		/* All match states */
	cont2=1;
      } else if (countTrans(st, del)==1) {
        cost=continueDelete;	/* Continuing a delete */
	cont2=1;
      } else {
	cost=2*continueDelete;	/* Continuing 2 deletes */
	cont2=0;
      }
      contCost[ns] = cost;
      secondCost[ns] = cont2;
    } // End setup of contCost[]

    ns++;
  }

  numStates = ns;

  { // Setup state transition costs (transCost[][])
    int s1,s2;
    int maxCost=0;
    
    assert(startInsert==startDelete && "Need to rewrite setup routine");
    for (s1=0;s1<numStates;s1++) {
      for (s2=0;s2<numStates;s2++) {
        Trans from[3],to[3];
        int cost=0,i;
        transitions(stateNum[s1],from);
        transitions(stateNum[s2],to);

        for (i=0;i<3;i++) {
          if ((to[i]==ins || to[i]==del) && (to[i]!=from[i]))
            cost += startInsert;
        }
        transCost[s1][s2] = cost;

	{ // Determine biggest single step cost
	  int thisCost = cost + contCost[s2];
	  Trans st[3];
	  transitions(stateNum[s2],st);
	  thisCost += misCost*(countTrans(st,match)-1);
	  maxCost = (maxCost<thisCost ? thisCost : maxCost);
	}
      }
    }
    
    maxSingleStep = maxCost;
  } // End setup of transition costs
}




/* ---------------------------------------------------------------------- */
/* Some alignment checking routines */
void checkAlign(char *al, int alLen, char *str, int strLen) 
{
  int i,j=0;
  for (i=0; i<alLen; i++) {
    if (al[i] == '-') continue;
    assert(al[i]==str[j] && "Output alignment not equal to input string");
    j++;
  }
  assert(j==strLen && "Output alignment not equal length to input string");
}

void revIntArray(int *arr, int start, int end)
{
  int i;
  if (end<=start) return;
  for (i=start; i<(end+start)/2; i++) {
    int t = arr[i];
    arr[i] = arr[end-i+start-1];
    arr[end-i+start-1] = t;
  }
}

void revCharArray(char *arr, int start, int end)
{
  int i;
  if (end<=start) return;
  for (i=start; i<(end+start)/2; i++) {
    char t = arr[i];
    arr[i] = arr[end-i+start-1];
    arr[end-i+start-1] = t;
  }
}

int alignmentCost(int states[], char *al1, char *al2, char *al3, int len)
{
  int i;
  int cost=0;
  Trans last_st[3] = {match, match, match};

  assert(startInsert == startDelete);

  for (i=0; i<len; i++) {
    int s;
    Trans st[3];
    transitions(stateNum[states[i]], st);

//    if (i>0) fprintf(stderr,"%-2d  ",cost);
    
    // Pay for begining of gaps.
    for (s=0; s<3; s++)
      if (st[s]!=match && st[s] != last_st[s])
	cost += startInsert;

    for (s=0; s<3; s++)
      last_st[s] = st[s];
    
    // Pay for continuing an insert
    if (countTrans(st, ins)>0) {
      assert(countTrans(st,ins) == 1);
      cost += continueInsert;
      continue;
    }

    // Pay for continuing deletes
    cost += continueDelete * countTrans(st, del);

    // Pay for mismatches
    {
      char ch[3];
      int ci=0;
      if (st[0] == match) { assert(al1[i]!='-'); ch[ci++] = al1[i]; };
      if (st[1] == match) { assert(al2[i]!='-'); ch[ci++] = al2[i]; };
      if (st[2] == match) { assert(al3[i]!='-'); ch[ci++] = al3[i]; };
      ci--;
      for (; ci>0; ci--) {
	if (ch[ci-1] != ch[ci]) cost+=misCost;
      }
      if (countTrans(st, match)==3 && ch[0]==ch[2] && ch[0]!=ch[1]) cost-=misCost;
    }
    
  }
  printf ("The recomputed cost is %d\n", cost);

  return cost;
}
/* ---------------------------------------------------------------------- */

// End of ukkCommon.c

