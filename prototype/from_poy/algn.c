/* POY 5.1.1. A phylogenetic analysis program using Dynamic Homologies.       */
/* Copyright (C) 2014 Andrés Varón, Lin Hong, Nicholas Lucaroni, Ward Wheeler,*/
/* and the American Museum of Natural History.                                */
/*                                                                            */
/* This program is free software; you can redistribute it and/or modify       */
/* it under the terms of the GNU General Public License as published by       */
/* the Free Software Foundation; either version 2 of the License, or          */
/* (at your option) any later version.                                        */
/*                                                                            */
/* This program is distributed in the hope that it will be useful,            */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of             */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              */
/* GNU General Public License for more details.                               */
/*                                                                            */
/* You should have received a copy of the GNU General Public License          */
/* along with this program; if not, write to the Free Software                */
/* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301   */
/* USA                                                                        */

/* Alignment libray in C */

#include <stdio.h>
#include <malloc.h>
#include <stdio.h>
#include <limits.h>
#define NDEBUG 1    //NDEBUG=1 also turns off assertions
#include <assert.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include "zarr.h"
#include "matrices.h"
#include "array_pool.h"
#include "seq.h"
#include "cm.h"

#define HIGH_NUM 1000000 //this is a big number, but if the cost of substitution/or gap opening is bigger than this, it won't work
#define NPRINT_DM 1
#define NPRINT_CM 1
#define NDEBUG_BT 1

#include "matrices.c"
#include "cm.c"
#include "seq.c"
#include "array_pool.c"

#ifdef DEBUG_ALL_ASSERTIONS
int *_algn_max_matrix = NULL;
DIRECTION_MATRIX *_algn_max_direction = NULL;
#endif

#define MAX(a,b) ( ((a)>(b))? (a):(b) )
#define MAX3(a,b,c) ( (MAX(a,b)>MAX(b,c))? (MAX(a,b)):(MAX(b,c)) )

#define my_prepend(a,b) assert (a->cap > a->len); \
    (a)->begin = ((a)->begin) - 1; (a)->len = 1 + (a)->len; *((a)->begin) = b

#define my_get(a,b) ((a)->begin)[b]

#define has_flag(dir,flag) (dir&flag)

#define has_align(dir)  has_flag(dir,ALIGN)
#define has_insert(dir) has_flag(dir,INSERT)
#define has_delete(dir) has_flag(dir,DELETE)

#define choose_other(compare,a,b) (a==compare)?b:a

#define INT_SWAP(x1,x2) x1 = x1 + x2; x2 = x1 - x2; x1 = x1 - x2

/** helper functions for traceback; used to modify variables and prepend seq
 * data for the aligned sequence data */
#ifdef _WIN32
__inline void 
#else
inline void 
#endif
follow_insertion (DIRECTION_MATRIX ** endp, int l, const cmt c, seqt r1, seqt r2, seqt s2, int* algn_s2)
{
    assert( has_insert(**endp) );
    my_prepend(r1,cm_get_gap (c) );
    *algn_s2=*algn_s2-1;
    my_prepend(r2, my_get(s2,*algn_s2));
    *endp -= 1;
}

/** helper functions for traceback; used to modify variables and prepend seq
 * data for the aligned sequence data */
#ifdef _WIN32
__inline void 
#else
inline void 
#endif
follow_deletetion (DIRECTION_MATRIX ** endp, int l, const cmt c, seqt r1, seqt r2, seqt s1, int* algn_s1)
{
    assert( has_delete(**endp) );
    *algn_s1=*algn_s1-1;
    my_prepend(r1, my_get(s1,*algn_s1));
    my_prepend(r2, cm_get_gap(c));
    *endp -= l;
}

/** helper functions for traceback; used to modify variables and prepend seq
 * data for the aligned sequence data. */
void
follow_insertion_or_deletion (DIRECTION_MATRIX ** endp, int swaped, int l,
        const cmt c, seqt r1, seqt r2, seqt s1, seqt s2, int* algn_s1, int* algn_s2)
{
    if (swaped) {
        if (has_insert(**endp))
            follow_insertion(endp,l,c,r1,r2,s2,algn_s2);
        else
            follow_deletetion(endp,l,c,r1,r2,s1,algn_s1);
    } else {
        if (has_delete(**endp))
            follow_deletetion(endp,l,c,r1,r2,s1,algn_s1);
        else
            follow_insertion(endp,l,c,r1,r2,s2,algn_s2);
    }
    return;
}

void
algn_fill_gapnum (int pos, int hasalgn, int hasinsert, int hasdelete,  DIRECTION_MATRIX * pgm1, DIRECTION_MATRIX * gm1, DIRECTION_MATRIX * pgm2, DIRECTION_MATRIX * gm2)
{
    int i = pos ;
    int debug = 0;
    if (debug) { printf("algn_fill_gapnum,pos=%d:",pos); fflush(stdout); }
    if (hasalgn) {
        if(hasdelete&&hasinsert)//insert|delete|align
        {//MAX3(s1_gapnum_fromM,s1_gapnum_fromL+1,s1_gapnum_fromR),MAX3(s2_gapnum_fromM,s2_gapnum_fromL,s2_gapnum_fromR+1)
        if (debug) { printf("insert|delete|align\n"); fflush(stdout); }
            gm1[i] = MAX3(pgm1[i-1],gm1[i-1]+1,pgm1[i]);
            gm2[i] = MAX3(pgm2[i-1],gm2[i-1],pgm2[i]+1);
        }
        else if (hasdelete)//delete|align
        {
        if (debug) { printf("delete|align\n"); fflush(stdout); }
            gm1[i] = MAX(pgm1[i],pgm1[i-1]);
            gm2[i] = MAX(pgm2[i-1],pgm2[i]+1);
        }
        else if (hasinsert) //insert|align 
        {
        if (debug) { printf("insert|align\n"); fflush(stdout); }
            gm1[i] = MAX(gm1[i-1]+1,pgm1[i-1]);
            gm2[i] = MAX(pgm2[i-1],gm2[i-1]);
        }
        else//just align
        {
        if (debug) { printf("align\n"); fflush(stdout); }
            gm1[i] = pgm1[i-1]; 
            gm2[i] = pgm2[i-1];
        }
    }
    else if (hasinsert) {
        if (hasdelete) {//delete|insert
            //MAX(s1_gapnum_fromL+1,s1_gapnum_fromR), MAX(s2_gapnum_fromR+1,s2_gapnum_fromL)
        if (debug) { printf("insert|delete\n"); fflush(stdout); }
            gm1[i] = MAX(gm1[i-1]+1,pgm1[i]);
            gm2[i] = MAX(pgm2[i]+1,gm2[i-1]);
        }
        else {//just insert
        if (debug) { printf("insert\n"); fflush(stdout); }
            gm1[i] = gm1[i-1]+1;
            gm2[i] = gm2[i-1];
        }
    }
    else {//just delete
        if (debug) { printf("delete\n"); fflush(stdout); }
        gm1[i] = pgm1[i]; 
        gm2[i] = pgm2[i]+1; 
    }
}


/******************************************************************************/
/*                        Pairwise Standard Alignment                         */
/******************************************************************************/
/*
 * As standard, all the caml binding functions are called algn_CAML_<function
 * name>
 */

/* Fill a row in a two dimentional alignment
 *
 * When pairwise alignment is performed, two sequences are compared over a
 * transformation cost matrix. Let's call them sequences x and y written over
 * some alphabet a of length |a|. Each base of x
 * is represented by a column in the transformation cost matrix and each base of
 * y by a row. However, note that the actual values that are added during the
 * alignment are produced by comparing every single base of x with only |a|
 * elements. Now, in order to to make these operations vectorizable, we perform
 * the comparisons with |a| precalculated vectors. This puts in context the
 * explanation of each parameter in the function.
 *
 * @param mm is the cost matrix row to be filled with values.
 * @param pm is the row above mm in the cost matrix being filled.
 * @param gap_row is the cost of aligning each base in x with a gap.
 * @param alg_row is the cost of aligning each base in x wit hthe base
 * represented by the base of the row of mm in y.
 * @param dm is the directional matrix for the backtrack
 * @param c is the cost of an insertion. As an insertion can only occur for one
 * particular base in the alphabet, corresponding to the base in y represented
 * by the row that is being filled.
 * @param st is the starting cell for the filling.
 * @param end is the final cell for the filling.
 * If you modify this code check algn_fill_cube as there is simmilar code there
 * used in the first plane of the alignment. It didn't use this function because
 * the direction codes are different for three dimensional alignments.
 */
#if ( __GNUC__ && __MMX__ )
void 
algn_fill_row (int *mm, const int *pm, const int *gap_row, const int *alg_row,
                DIRECTION_MATRIX *dm, int c, int i, int end)
{

    register int aa, bb, cc;
    register const int TWO = 0x200; 
    register const int ZERO = 0;
    
    /*
        The algorithm has not changed. Only have the conditional branches been eliminated for better performance.
        Since gcc (4.0.3 atleast) didn't generate cmov's, we changed the code manually.
        Things that have been done for optimizing this function:
            - assembly code for determining min(aa, bb, cc) and getting the bit pattern
            - loop unrolling with a factor of 8, to still keep this function inlined
            - rearrangement of expressions => better register usage and (probably) less cache misses

        Restrictions:
        ALIGN is bound to the value 4, INSERT to 2 and DELETE to 1
        If these constants changes, there are problems with the assembler code,
        since they are really optimized for this purpose

        Furthermore, this code does only work with the gnu compiler (but shouldn't be hard to switch to icc), so add
        #ifdef __GNUC__ as a macro for this section. I also suppose that cmov's can be handled by most
        of the computers used today (of those using this program at least).

        I have also removed the debug sections. These can of course be added if a debug is needed.
        I recommend that debugging is only used with the original function, since this one generates exactly same results.
    */

    bb = mm[i - 1];
    for (; i <= end - 7; i+=8) {

        aa = pm[i - 1] + alg_row[i]; // aka tmp3
        bb += gap_row[i]; // aka tmp2
        cc = pm[i] + c; // aka tmp1
        __asm__(
            "cmp %0, %1\n\t"    // compare aa with bb (the needed cmp flag remains even if registers are switched)
            "cmovg %0, %1\n\t"  // if bb > aa was flagged, put aa into bb's register. Now we know that bb is always the smallest value
            "mov $0x0, %0\n\t"  // aa is now trash and will not be used anymore. Clean the register (set it to 0x0)
            "cmovle %4, %0\n\t" // if bb <= aa was flagged in the first instruction, then put TWO (0x200) into register 0 (aa)
            "setge %b0\n\t"     // if bb >= aa was flagged in the first instruction, then set the lowest byte in register 0 to 1 (0x01)
            "cmp %1, %2\n\t"    // compare reg.1 (the lowest of aa and bb) with cc.
            "cmovl %2, %1\n\t"  // if cc < bb was flagged in the 2nd comparison, then move cc into bb. min(aa, bb, cc) is now stored in bb.
            "mov $0x4, %2\n\t"  // put a 4 (ALIGN) in register 2 (cc) since cc is trash.
            "cmovl %3, %0\n\t"  // if cc < bb was flagged in the 2nd comparison, then clear register 0.
            "cmovg %3, %2\n\t"  // if cc > bb was flagged, then clean the register. (Note that you can only move register->register with a cmov.) 
            "add %b0, %h0\n\t"  // finally, add low byte and high byte of register 0 into register 0's low byte.
            "add %h0, %b2\n\t"  // add high byte from reg.0 to low byte in reg.2 (cc) and we are done. cc contains now the bitpattern.
            : "+Q" (aa), "+r" (bb), "+Q" (cc)   // registers: aa = %0, bb = %1, cc = %2
            : "r" (ZERO), "r" (TWO)         // ZERO = %3, TWO = %4
        );
        mm[i] = bb; // bb is min(aa, bb, cc)
        dm[i] = cc; // cc is the bitpattern

        aa = pm[i] + alg_row[i + 1];
        bb += gap_row[i + 1]; // bb is already assigned the minimum value of the three to be compared, so loading bb from memory would be waste.
        cc = pm[i + 1] + c;
        __asm__(
            "cmp %0, %1\n\t"
            "cmovg %0, %1\n\t"
            "mov $0x0, %0\n\t"
            "cmovle %4, %0\n\t"
            "setge %b0\n\t"
            "cmp %1, %2\n\t"
            "cmovl %2, %1\n\t" 
            "mov $0x4, %2\n\t" 
            "cmovl %3, %0\n\t" 
            "cmovg %3, %2\n\t" 
            "add %b0, %h0\n\t" 
            "add %h0, %b2\n\t" 
            : "+Q" (aa), "+r" (bb), "+Q" (cc)
            : "r" (ZERO), "r" (TWO)
        );
        mm[i + 1] = bb;
        dm[i + 1] = cc;

        aa = pm[i + 1] + alg_row[i + 2];
        bb += gap_row[i + 2];
        cc = pm[i + 2] + c;
        __asm__(
            "cmp %0, %1\n\t"
            "cmovg %0, %1\n\t"
            "mov $0x0, %0\n\t"
            "cmovle %4, %0\n\t"
            "setge %b0\n\t"
            "cmp %1, %2\n\t"
            "cmovl %2, %1\n\t"
            "mov $0x4, %2\n\t"
            "cmovl %3, %0\n\t" 
            "cmovg %3, %2\n\t"
            "add %b0, %h0\n\t" 
            "add %h0, %b2\n\t" 
            : "+Q" (aa), "+r" (bb), "+Q" (cc)
            : "r" (ZERO), "r" (TWO)
        );
        mm[i + 2] = bb;
        dm[i + 2] = cc;

        aa = pm[i + 2] + alg_row[i + 3];
        bb += gap_row[i + 3];
        cc = pm[i + 3] + c;
        __asm__(
            "cmp %0, %1\n\t"
            "cmovg %0, %1\n\t"
            "mov $0x0, %0\n\t"
            "cmovle %4, %0\n\t"
            "setge %b0\n\t"
            "cmp %1, %2\n\t"
            "cmovl %2, %1\n\t"
            "mov $0x4, %2\n\t"
            "cmovl %3, %0\n\t" 
            "cmovg %3, %2\n\t"
            "add %b0, %h0\n\t" 
            "add %h0, %b2\n\t"
            : "+Q" (aa), "+r" (bb), "+Q" (cc)
            : "r" (ZERO), "r" (TWO)
        );
        mm[i + 3] = bb;
        dm[i + 3] = cc;

        aa = pm[i + 3] + alg_row[i + 4];
        bb += gap_row[i + 4];
        cc = pm[i + 4] + c;
        __asm__(
            "cmp %0, %1\n\t"
            "cmovg %0, %1\n\t"
            "mov $0x0, %0\n\t"
            "cmovle %4, %0\n\t"
            "setge %b0\n\t"
            "cmp %1, %2\n\t"
            "cmovl %2, %1\n\t" 
            "mov $0x4, %2\n\t"
            "cmovl %3, %0\n\t" 
            "cmovg %3, %2\n\t"
            "add %b0, %h0\n\t" 
            "add %h0, %b2\n\t"
            : "+Q" (aa), "+r" (bb), "+Q" (cc)
            : "r" (ZERO), "r" (TWO)
        );
        mm[i + 4] = bb;
        dm[i + 4] = cc;

        aa = pm[i + 4] + alg_row[i + 5];
        bb += gap_row[i + 5];
        cc = pm[i + 5] + c;
        __asm__(
            "cmp %0, %1\n\t"
            "cmovg %0, %1\n\t"
            "mov $0x0, %0\n\t"
            "cmovle %4, %0\n\t"
            "setge %b0\n\t"
            "cmp %1, %2\n\t"
            "cmovl %2, %1\n\t"
            "mov $0x4, %2\n\t"
            "cmovl %3, %0\n\t" 
            "cmovg %3, %2\n\t"
            "add %b0, %h0\n\t" 
            "add %h0, %b2\n\t"
            : "+Q" (aa), "+r" (bb), "+Q" (cc)
            : "r" (ZERO), "r" (TWO)
        );
        mm[i + 5] = bb;
        dm[i + 5] = cc;

        aa = pm[i + 5] + alg_row[i + 6];
        bb += gap_row[i + 6];
        cc = pm[i + 6] + c;
        __asm__(
            "cmp %0, %1\n\t"
            "cmovg %0, %1\n\t"
            "mov $0x0, %0\n\t"
            "cmovle %4, %0\n\t"
            "setge %b0\n\t"
            "cmp %1, %2\n\t"
            "cmovl %2, %1\n\t"
            "mov $0x4, %2\n\t"
            "cmovl %3, %0\n\t" 
            "cmovg %3, %2\n\t"
            "add %b0, %h0\n\t" 
            "add %h0, %b2\n\t"
            : "+Q" (aa), "+r" (bb), "+Q" (cc)
            : "r" (ZERO), "r" (TWO)
        );
        mm[i + 6] = bb;
        dm[i + 6] = cc;

        aa = pm[i + 6] + alg_row[i + 7];
        bb += gap_row[i + 7];
        cc = pm[i + 7] + c;
        __asm__(
            "cmp %0, %1\n\t"
            "cmovg %0, %1\n\t"
            "mov $0x0, %0\n\t"
            "cmovle %4, %0\n\t"
            "setge %b0\n\t"
            "cmp %1, %2\n\t"
            "cmovl %2, %1\n\t"
            "mov $0x4, %2\n\t"
            "cmovl %3, %0\n\t" 
            "cmovg %3, %2\n\t"
            "add %b0, %h0\n\t" 
            "add %h0, %b2\n\t"
            : "+Q" (aa), "+r" (bb), "+Q" (cc)
            : "r" (ZERO), "r" (TWO)
        );
        mm[i + 7] = bb;
        dm[i + 7] = cc;
    }

    for (; i <= end; i++) {
        aa = pm[i - 1] + alg_row[i];
        bb += gap_row[i];
        cc = pm[i] + c;
        __asm__(
            "cmp %0, %1\n\t"
            "cmovg %0, %1\n\t"
            "mov $0x0, %0\n\t"
            "cmovle %4, %0\n\t"
            "setge %b0\n\t"
            "cmp %1, %2\n\t"
            "cmovl %2, %1\n\t"
            "mov $0x4, %2\n\t"
            "cmovl %3, %0\n\t" 
            "cmovg %3, %2\n\t"
            "add %b0, %h0\n\t" 
            "add %h0, %b2\n\t"
            : "+Q" (aa), "+r" (bb), "+Q" (cc)
            : "r" (ZERO), "r" (TWO)
        );
        mm[i] = bb;
        dm[i] = cc;
    }
    return;

}
#else /* __GNUC__ */

#ifdef _WIN32
__inline void 
#else
inline void 
#endif
algn_fill_row (int *mm, const int *pm, const int *gap_row, const int *alg_row,
            DIRECTION_MATRIX *dm, int c, int st, int end)
{
    int i, tmp1, tmp2, tmp3;
    int cc;
    int hasalgn, hasdelete, hasinsert;
    for (i = st; i <= end; i++) {
        /* try align with substitution */
        tmp1 = pm[i] + c;
        tmp2 = mm[i - 1] + gap_row[i];
        tmp3 = pm[i - 1] + alg_row[i];
        /* check whether insertion is better */
        /* This option will allow all the possible optimal paths to be stored
         * concurrently on the same backtrack matrix. This is important for the
         * sequences being able to choose the appropriate direction while
         * keeping the algorithm that assumes that s2 is at most as long as s1.
         * */
        if (tmp1 < tmp3) { 
            if (tmp1 < tmp2) {
                mm[i] = tmp1;
                dm[i] = DELETE;
            } else if (tmp2 < tmp1) {
                mm[i] = tmp2;
                dm[i] = INSERT;
            } else {
                mm[i] = tmp2;
                dm[i] = (INSERT | DELETE);
            }
        }
        else if (tmp3 < tmp1) {
            if (tmp3 < tmp2) {
                mm[i] = tmp3;
                dm[i] = ALIGN;
            } else if (tmp2 < tmp3) {
                mm[i] = tmp2;
                dm[i] = INSERT;
            } else {
                mm[i] = tmp2;
                dm[i] = (ALIGN | INSERT);
            }
        }
        else { /* tmp3 == tmp1 */
            if (tmp3 < tmp2) {
                mm[i] = tmp3;
                dm[i] = (ALIGN | DELETE);
            } else if (tmp2 < tmp3) {
                mm[i] = tmp2; 
                dm[i] = INSERT;
            } else {
                mm[i] = tmp2;
                dm[i] = (DELETE | INSERT | ALIGN);
            }
        }
        cc = dm[i];
        if (!NDEBUG && !NPRINT_DM) {
            /* Print the alignment matrix */
            if (INSERT & dm[i]) 
                printf ("I");
            if (DELETE & dm[i])
                printf ("D");
            if (ALIGN & dm[i])
                printf ("A");
            printf ("\t");
        }
        if (!NDEBUG &&!NPRINT_CM) {
            /* Print the cost matrix */
            printf ("%d\t", mm[i]);
            fflush (stdout);
        }
    }
    if (!NDEBUG && (!NPRINT_CM || !NPRINT_DM))  {
        printf ("\n");
        fflush (stdout);
    }
    return;
}
#endif /* __GNUC__ */

#ifdef _WIN32
__inline void 
#else
inline void 
#endif
algn_fill_ukk_right_cell (int *mm, const int *pm, const int *gap_row,
                    const int *alg_row, DIRECTION_MATRIX *dm, int c, int pos)
{
    int tmp2, tmp3;
    /* try align with substitution */
    tmp2 = mm[pos - 1] + gap_row[pos];
    tmp3 = pm[pos - 1] + alg_row[pos];
    /* check whether insertion is better */
    if (tmp2 < tmp3) {
        mm[pos] = tmp2;
        dm[pos] = INSERT;
    } else if (tmp3 < tmp2) {
        mm[pos] = tmp3;
        dm[pos] = ALIGN;
    } else {
        mm[pos] = tmp3;
        dm[pos] = INSERT | ALIGN;
    }
    if (!NDEBUG && !NPRINT_DM) {
        /* Print the alignment matrix */
        if (INSERT & dm[pos]) 
            printf ("I");
        if (DELETE & dm[pos])
            printf ("D");
        if (ALIGN & dm[pos])
            printf ("A");
        printf ("\t");
    }
    if (!NDEBUG &&!NPRINT_CM) {
        /* Print the cost matrix */
        printf ("%d\t", mm[pos]);
        fflush (stdout);
    }
    if (!NDEBUG && (!NPRINT_CM || !NPRINT_DM)) {
        printf ("\n");
        fflush (stdout);
    }
    return;
}

#ifdef _WIN32
__inline void 
#else
inline void 
#endif
algn_fill_ukk_left_right_cell (int *mm, const int *pm, const int *alg_row,
                                        DIRECTION_MATRIX *dm, int c, int pos)
{
    int tmp3;
    /* try align with substitution */
    tmp3 = pm[pos - 1] + alg_row[pos];
    mm[pos] = tmp3;
    dm[pos] = ALIGN;
    if (!NDEBUG && !NPRINT_DM) {
        printf ("A");
        printf ("\t");
    }
    if (!NDEBUG &&!NPRINT_CM) {
        /* Print the cost matrix */
        printf ("%d\t", mm[pos]);
        fflush (stdout);
    }
    return;
}


#ifdef _WIN32
__inline void 
#else
inline void 
#endif
algn_fill_ukk_left_cell (int *mm, const int *pm, const int *gap_row,
                    const int *alg_row, DIRECTION_MATRIX *dm,int c, int pos)
{
    int tmp1, tmp3;
    /* try align with substitution */
    tmp1 = pm[pos] + c;
    tmp3 = pm[pos - 1] + alg_row[pos];
    if (tmp1 < tmp3) {
            mm[pos] = tmp1;
            dm[pos] = DELETE;
    } else if (tmp3 < tmp1) {
            mm[pos] = tmp3;
            dm[pos] = ALIGN;
    } else {
            mm[pos] = tmp1;
            dm[pos] = ALIGN | DELETE;
    }
    if (!NDEBUG && !NPRINT_DM) {
        /* Print the alignment matrix */
        if (INSERT & dm[pos]) 
            printf ("I");
        if (DELETE & dm[pos])
            printf ("D");
        if (ALIGN & dm[pos])
            printf ("A");
        printf ("\t");
    }
    if (!NDEBUG &&!NPRINT_CM) {
        /* Print the cost matrix */
        printf ("%d\t", mm[pos]);
        fflush (stdout);
    }
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
algn_fill_last_column (int *mm, const int *pm, int tlc, int l, DIRECTION_MATRIX *dm)
{
    int cst;
    if (l > 0) {
        cst = tlc + pm[l];
        if (cst < mm[l]) {
            mm[l] = cst;
            dm[l] = DELETE;
        } else if (cst == mm[l]) {
            dm[l] = dm[l] | DELETE;
        }
    }
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
//no barriers are set for this function
algn_fill_full_row (int *mm, const int *pm, const int *gap_row, const int *alg_row,
                        DIRECTION_MATRIX *dm, int c, int tlc, int l)
{
    /* first entry is delete */
    mm[0] = c + pm[0];
    dm[0] = DELETE;
    if ((!NDEBUG) && (!NPRINT_CM)) {
        printf ("%d\t", mm[0]);
        fflush (stdout);
    }
    if ((!NDEBUG) && (!NPRINT_DM))
        printf ("D\t");
    algn_fill_row (mm, pm, gap_row, alg_row, dm, c, 1, l-1);
    algn_fill_last_column (mm, pm, tlc, l - 1, dm);
    return;
}

void
algn_fill_first_row (int *mm, DIRECTION_MATRIX *dm, int len, int const *gap_row)
{
    int i;
    /* We fill the first cell to start with */
    mm[0] = 0;
    dm[0] = ALIGN;
    /* Now the rest of the row */
    if ((!NDEBUG) && (!NPRINT_DM))
        printf ("A\t");
    if ((!NDEBUG) && (!NPRINT_CM)) {
        printf ("%d\t", mm[0]);
        fflush (stdout);
    }
    for (i = 1; i < len; i++) {
        mm[i] = mm[i - 1] + gap_row[i];
        dm[i] = INSERT;
        if ((!NDEBUG) && (!NPRINT_DM))
            printf ("I\t");
        if ((!NDEBUG) && (!NPRINT_CM)) {
            printf ("%d\t", mm[i]);
            fflush (stdout);
        }
    }
    return;
}

void
algn_fill_first_cell (int *mm, int pm, DIRECTION_MATRIX *dm, int gap) {
    *mm = pm + gap;
    *dm = DELETE;
    if ((!NDEBUG) && (!NPRINT_DM))
        printf ("D\t");
    if ((!NDEBUG) && (!NPRINT_CM)) {
        printf ("%d\t", *mm);
        fflush (stdout);
    }
    return;
}

/* In the following three functions, we maintain the following invariants in
 * each loop:
 * 1. mm is a row that has not been filled and is the next to be.
 * 4. dm is the current row of the direction matrix
 * 2. pm is the previous row, located right above mm, which has been filled
 * already.
 * 3. i is the number of the row of mm in its containing matrix
 * 5. gap_row is the cost of aligning each base of s2 with a gap. This is
 * constant for all the loop.
 * 6. cur_s1 is the i'th base of s1
 * 7. const_val is the cost of cur_s1 aligned with a gap
 * 8. alg_row is the vector of costs of aligning s2 with cur_s1
 */

int *
algn_fill_extending_right (const seqt s1, int *prec, int s1_len, int s2_len,
            int *mm, int *pm, DIRECTION_MATRIX *dm, const cmt c, int start_row,
            int end_row, int len)
{
    int i;
    int *tmp, cur_s1, const_val;
    const int *gap_row, *alg_row;
    /** Invariants block 
     * len is the number of items in the row to be filled **/
    i = start_row;
    /* This is what we will perform conceptually, I will stop using the
     * cm_get_precal_row function to speed this up a little bit 
        gap_row = cm_get_precal_row (prec, cm_get_gap (c), s2_len); */
    gap_row = prec + (c->gap * s2_len);
    while (i < end_row) {
        /** Invariants block */
        cur_s1 = s1->begin[i];
        if(cm_check_level(c) == 1)
            const_val = cm_get_cost (c->cost, cur_s1, c->gap, c->map_sz+1);
        else
            const_val = cm_calc_cost (c->cost, cur_s1, c->gap, c->lcm);
        /* This is conceptually what we do in the next line 
            alg_row = cm_get_precal_row (prec, cur_s1, s2_len); */
        alg_row = prec + (cur_s1 * s2_len);
        /* Align! */
        algn_fill_first_cell (mm, pm[0], dm, alg_row[0]);
        algn_fill_row (mm, pm, gap_row, alg_row, dm, const_val, 1, len - 2);
        algn_fill_ukk_right_cell (mm, pm, gap_row, alg_row, dm, const_val, len-1);
        /** Invariants block */
        tmp = mm;
        mm = pm;
        pm = tmp;
        i++;
        dm += s2_len;
        len++;
    }
    return (mm);
}

int *
algn_fill_extending_left_right (const seqt s1, int *prec, int s1_len, int s2_len,
        int *mm, int *pm, DIRECTION_MATRIX *dm, const cmt c, int start_row,
        int end_row, int start_column, int len)
{
    int i;
    int *tmp, cur_s1, const_val;
    const int *gap_row, *alg_row;
    /** Invariants block 
     * len is the number of cells to fill in the current row minus 1
     * start_column is the first cell to fill in the row */
    i = start_row;
    /* Conceptually 
        gap_row = cm_get_precal_row (prec, cm_get_gap (c), s2_len); */
    gap_row = prec + (c->gap * s2_len);
    //len--;
    while (i < end_row) {
        /** Invariants block */
        cur_s1 = s1->begin[i];
        if(cm_check_level(c) == 1)
            const_val = cm_get_cost (c->cost, cur_s1, c->gap, c->map_sz+1);
        else
            const_val = cm_calc_cost (c->cost, cur_s1, c->gap, c->lcm);
        /* Conceptually 
            alg_row = cm_get_precal_row (prec, cur_s1, s2_len); */
        alg_row = prec + (cur_s1 * s2_len);
        /* Align! */
        if (len==1) {
            algn_fill_ukk_left_right_cell(mm,pm,alg_row,dm,const_val,start_column);
        } else {
            algn_fill_ukk_left_cell (mm, pm, gap_row, alg_row, dm, const_val, start_column);
            algn_fill_row (mm, pm, gap_row, alg_row, dm, const_val, start_column + 1, start_column + (len - 2));
            algn_fill_ukk_right_cell (mm, pm, gap_row, alg_row, dm, const_val, start_column + len - 1);
        }
        /** Invariants block */
        tmp = mm;
        mm = pm;
        pm = tmp;
        i++;
        dm += s2_len;
        start_column++;
    }
    //here we return mm=previous line,so later in [choose_other c a b] will give
    //us current line, why don't we just return the current line?
    return (mm);
}

int *
algn_fill_extending_left (const seqt s1, int *prec, int s1_len, int s2_len,
        int *mm, int *pm, DIRECTION_MATRIX *dm, const cmt c, int start_row,
        int end_row, int start_column, int len)
{
    int i;
    int *tmp, cur_s1, const_val, const_val_tail;
    const int *gap_row, *alg_row;
    /** Invariants block 
     * start_column is the first cell to fill in the row 
     * len is the number of cells to fill in the current row minus 1 */
    i = start_row;
    /* Conceptually 
    gap_row = cm_get_precal_row (prec, cm_get_gap (c), s2_len);
    */
    gap_row = prec + (c->gap * s2_len);
    while (i < end_row) {
        /** Invariants block */
        cur_s1 = s1->begin[i];
        if(cm_check_level(c) == 1)
            const_val = cm_get_cost (c->cost, cur_s1, c->gap, c->map_sz+1);
        else
            const_val = cm_calc_cost (c->cost, cur_s1, c->gap, c->lcm);
        const_val_tail = (cm_get_tail_cost (c))[cur_s1];
        /* Conceptually 
            alg_row = cm_get_precal_row (prec, cur_s1, s2_len); */
        alg_row = prec + (cur_s1 * s2_len);
        /* Align! */
        algn_fill_ukk_left_cell (mm, pm, gap_row, alg_row, dm, const_val, start_column);
        algn_fill_row (mm, pm, gap_row, alg_row, dm, const_val, start_column + 1, start_column + len - 1);
        algn_fill_last_column (mm, pm, const_val_tail, start_column + len - 1, dm);
        /** Invariants block */
        tmp = mm;
        mm = pm;
        pm = tmp;
        i++;
        dm += s2_len;
        start_column++;
        len--;
    }
    if (!NDEBUG && !NPRINT_CM) {
        printf ("S2 gap cost\n");
        fflush (stdout);
        for (i = 0; i < s2_len; i++) {
            printf ("%d\t", gap_row[i]);
            fflush (stdout);
        }
        printf ("\n");
        printf ("The S1 - gap cost is %d\n", const_val);
        fflush (stdout);
    }

    return (mm);
}

int *
algn_fill_no_extending (const seqt s1, int *prec, int s1_len, int s2_len, int *mm,
        int *pm, DIRECTION_MATRIX *dm, const cmt c, int start_row, int end_row)
{
    int i;
    int *tmp, cur_s1, const_val, const_val_tail;
    const int *gap_row, *alg_row;
    /** Invariants block */
    i = start_row;
    /* Conceptually 
        gap_row = cm_get_precal_row (prec, cm_get_gap (c), s2_len); */
    gap_row = prec + (c->gap * s2_len);
    while (i < end_row) {
        /** Invariants block */
        cur_s1 = s1->begin[i];
        if(cm_check_level(c) == 1)
              const_val = cm_get_cost (c->cost, cur_s1, c->gap, c->map_sz+1);
        else
            const_val = cm_calc_cost (c->cost, cur_s1, c->gap, c->lcm);
        const_val_tail = (cm_get_tail_cost (c))[cur_s1];
        /* Conceptually 
            alg_row = cm_get_precal_row (prec, cur_s1, s2_len); */
        alg_row = prec + (cur_s1 * s2_len);
        /* Align! */
        algn_fill_first_cell (mm, pm[0], dm, alg_row[0]);
        algn_fill_row (mm, pm, gap_row, alg_row, dm, const_val, 1, s2_len - 1);
        algn_fill_last_column (mm, pm, const_val_tail, s2_len - 1, dm);
        /** Invariants block */
        tmp = mm;
        mm = pm;
        pm = tmp;
        i++;
        dm += s2_len;
    }
    return (mm);
}

/* Simmilar to the previous but when no barriers are set */
int
algn_fill_plane (const seqt s1, int *prec, int s1_len, int s2_len, int *mm,
                    DIRECTION_MATRIX *dm, const cmt c)
{
    int i;
    const int *alg_row;
    int const_val, const_val_tail, *nm, *tmp;
    const int *gap_row, *first_gap_row;
    int gapcode;
    /* A precalculated cost of a gap aligned with each base in the array */
    gapcode = cm_get_gap (c);
    gap_row = cm_get_precal_row (prec, gapcode, s2_len);
    first_gap_row = cm_get_precal_row (prec, 0, s2_len);
    nm = mm;
    mm[0] = 0;
    dm[0] = ALIGN;
    if ((!NDEBUG) && (!NPRINT_CM))
        printf ("%d\t", mm[0]);
    if ((!NDEBUG) && (!NPRINT_DM))
        printf ("A\t");
    /* We fill the first row to start with */
    for (i = 1; i < s2_len; i++) {
        mm[i] = mm[i - 1] + first_gap_row[i];
        dm[i] = INSERT;
        if ((!NDEBUG) && (!NPRINT_CM))
            printf ("%d\t", mm[i]);
        if ((!NDEBUG) && (!NPRINT_DM))
            printf ("I\t");
    }
    if (!NDEBUG && (!NPRINT_DM || !NPRINT_CM))
        printf ("\n");
    mm += s2_len;
    /* Now we fill the rest of the matrix */
    for (i = 1, dm += s2_len; i < s1_len; i++, dm += s2_len) {
        const_val_tail = (cm_get_tail_cost (c))[seq_get(s1, i)];
        if(cm_check_level(c) == 1)
            const_val = cm_get_cost(c->cost, seq_get(s1, i), c->gap , c->map_sz+1);
        else
            const_val = cm_calc_cost (c->cost, seq_get(s1, i), c->gap, c->lcm);
        alg_row = cm_get_precal_row (prec, seq_get (s1, i), s2_len);
        algn_fill_full_row (mm, nm, gap_row, alg_row, dm, const_val, const_val_tail, s2_len);
        /* We swap mm and nm for the next round */
        tmp = mm;
        mm = nm;
        nm = tmp;
    }
    return (nm[s2_len - 1]);
}

/** Returns the greater number of gaps in the aligned s1 and s2; used to test
 * the ending conditions for the ukkonen barrier. Variation of the function,
 * backtrace_2d; look their for further details. */
int backtrace_2d_gaps (const matricest m, const cmt c, int algn_s1, int algn_s2)
{
    int num_deletes = 0, num_inserts = 0, l;
    DIRECTION_MATRIX *beg, *end;

    l   = algn_s2;
    beg = mat_get_2d_direct (m);
    end = beg + (algn_s2 * (algn_s1 - 1)) + algn_s2 - 1;

    assert(!(cm_get_affine_flag (c)));
    while (end >= beg) {
        if (has_align(*end)) {
            algn_s1--;
            algn_s2--;
            end -= l + 1;
        } else if ( has_insert(*end) ){
            ++num_inserts;
            end -= 1;
            algn_s2--;
        } else {
            assert( has_delete(*end) );
            ++num_deletes;
            end -= l;
            algn_s1--;
        }
    }
    return (MAX(num_deletes,num_inserts));
}

int*
algn_newkk_fill_a_row (const seqt s1, int *prec, int *a, int * b, DIRECTION_MATRIX *dm,
        const cmt c, int s1_len, int s2_len, int i, int startj, int endj,
        int has_right_border, int has_left_border)
{
    /* keep what we were doing, a row in newkkonen matrix , if not a full
     * row, might have a right border, or a left border, or both */
    int start_column = startj;
    int length = endj-startj+1;
    int *next_row;
    int *next_pm;
    DIRECTION_MATRIX *to_go_dm;
    to_go_dm = dm + (i * s2_len);
    if (has_right_border&&has_left_border) {
        next_row = algn_fill_extending_left_right (s1, prec, s1_len, s2_len, b, a, to_go_dm,c, i, i+1, start_column, length);
    } else if (has_right_border) {
        next_row = algn_fill_extending_right (s1, prec, s1_len, s2_len, b, a, to_go_dm, c, i, i+1, length);
    } else if (has_left_border) {
        next_row = algn_fill_extending_left (s1, prec, s1_len, s2_len, b, a, to_go_dm, c, i, i+1, start_column, length);
    } else {
        next_row = algn_fill_no_extending (s1, prec, s1_len, s2_len, b, a, to_go_dm, c, i, i+1);
    }
    next_pm = choose_other (next_row, a, b);
    return next_pm;
}

void
print_array (const char *title, const int *arr, const int start, const int max)
{
    int i;
    printf ("%s,[%d,%d] : ", title, start, max);
    for (i = start; i <= max; i++) {
        if (arr[i] >= HIGH_NUM) printf ("INF ");
        else printf ("%d ", arr[i]); fflush (stdout);
    }
    printf ("\n");
    fflush (stdout);
    return;
}

void
print_dm (char *title, DIRECTION_MATRIX *arr, int start,int max) {
    int i;
    printf ("%s,start,end=%d,%d: ", title, start, max);
    for (i = start; i <= max; i++) {
        if (arr[i] >= HIGH_NUM) printf ("INF ");
        else   printf ("%d ", arr[i]); 
        fflush(stdout);
    }
    printf ("\n");
    fflush (stdout);
    return;
}


int
algn_newkk_test (const seqt s1, int *prec, int lenX, int lenY, matricest m,
                    const cmt c, int const *gap_row, int p, int * cost)
{ 
    int *a, *b;
    int startj,endj;
    int has_left_border, has_right_border, added_gaps;
    int i, *mm, j;
    int newk=p;
    if (p>=lenX) newk=lenX-1;
    int len_first_row = lenY-lenX+1+p;
    if (len_first_row>lenY) len_first_row = lenY;
    
    DIRECTION_MATRIX *dm;
    mm = mat_get_2d_matrix(m);
    dm = mat_get_2d_direct(m);

    //update first row of matrix
    algn_fill_first_row (mm, dm, len_first_row, gap_row);
    //point b to the row we want to update, a to the previous one that we just updated
    a = mm;
    b = mm + lenY;
    for (i=1;i<lenX;i++) {
        if ( (i-newk)>0 ){
            startj = i-newk; has_left_border=1;
        } else {
           startj = 0; has_left_border=0;
        }
        if ( (i+lenY-lenX+newk)<=(lenY-1) ){
           endj = i+lenY-lenX+newk; has_right_border = 1;
        } else {
           endj = lenY-1; has_right_border = 0;
        }
       /* fill a row, a points to the previous updated row, b points to the row 
            we are going to update, gm1&gm2 point to the gap num array for the
            previous updated row, next_gm1 and next_gm2 point to the row we are
            going to update */
       a = algn_newkk_fill_a_row(s1,prec,a,b,dm,c,lenX,lenY,i,startj,endj,has_right_border,has_left_border);
       //a points to the line we just updated, set b to the next line
       if (i<lenX-1) { b = a + lenY; }
    }
    if( !NDEBUG ){
        for(i = 0; i < lenX; ++i ){
            for( j = 0; j < lenY; ++j)
                printf("[%5d]  ", dm[(i * lenY) + j] );
            printf("\n");
        }
    }
    *cost = a[lenY-1];
    //Do a traceback to determine the number of insertions/deletions
    added_gaps = backtrace_2d_gaps( m , c, lenX, lenY);
    return added_gaps;
}


int
algn_newkk_increaseT (const seqt s1, int *prec, int lenX, int lenY, matricest m,
                        const cmt c, int const *gap_row, int T)
{
    int gap_num, cost, p,newp;
    p = (T - (lenY-lenX))/2;
    gap_num = algn_newkk_test(s1,prec,lenX,lenY,m,c,gap_row,p,&cost);
    newp = (2*T - (lenY-lenX))/2;
    if (((gap_num+1)<p)||(newp-lenY+1>=0)) {
        return cost;
    } else {
        return algn_newkk_increaseT(s1,prec,lenX,lenY,m,c,gap_row,T*2);
    }
}


int
algn_fill_plane_2 (const seqt s1, int *prec, int s1_len, int s2_len,
        matricest m, const cmt c, int width, int height, int dwidth_height)
{
    int delta, iniT;
    int const *gap_row;
    assert(s1_len<=s2_len); // assertion that should be met previously.

    width = width + dwidth_height;
    if (width > s2_len) width = s2_len;
    height = height + dwidth_height;
    if (height > s1_len) height = s1_len;

    gap_row = cm_get_precal_row (prec, 0, s2_len);

    delta = cm_get_min_non0_cost(c);
    iniT = (s2_len-s1_len+1) * delta;

    /* We have to consider three cases in this new alignment procedure (much
     * cleaner than the previous): 
     *
     * Case 1:
     *  If s1 is much longer than s2, then there is no point on using the
     * barriers, we rather fill the full matrix in one shot */
    if (((float) s1_len) >= (1.5 * (float)s2_len)){
        return (algn_fill_plane (s1, prec, s1_len, s2_len,
                    mat_get_2d_matrix(m), mat_get_2d_direct(m), c));
    /* Case 2:
     *  There are no full rows to be filled, therefore we have to break the
     * procedure in three different subsets */
    } else if ((2 * height) < s1_len) {
        return algn_newkk_increaseT (s1,prec,s1_len,s2_len,m,c,gap_row,iniT);
    /* Case 3: (final case)
     * There is a block in the middle of with full rows that have to be filled */
        /* We will simplify this case even further, if the size of the leftover
         * is too small, don't use the barriers at all, just fill it up all */
    } else {
        if (8 >= (s1_len - height)) {
            return (algn_fill_plane (s1, prec, s1_len, s2_len,
                        mat_get_2d_matrix(m), mat_get_2d_direct(m), c));
        } else { 
            return algn_newkk_increaseT(s1,prec,s1_len,s2_len,m,c,gap_row,iniT);
        }
    }
}
/******************************************************************************/


#define algn_assign_dm(dm,pos,v) dm[pos] = dm[pos] | v

#define DEBUG_AFFINE 0

#define ALIGN_TO_ALIGN 1
#define ALIGN_TO_VERTICAL 2
#define ALIGN_TO_HORIZONTAL 4
#define ALIGN_TO_DIAGONAL 8
#define BEGIN_BLOCK 16
#define END_BLOCK 32
#define BEGIN_VERTICAL 64
#define END_VERTICAL 128
#define BEGIN_HORIZONTAL 256
#define END_HORIZONTAL 512
#define DO_ALIGN 1024
#define DO_VERTICAL 2048
#define DO_HORIZONTAL 4096
#define HORIZONTAL_EQ_VERTICAL 8192
#define DO_DIAGONAL 16384 
// DO_DIAGONAL MUST BE THE LAST ONE //why?

#define TMPGAP 16
#define NTMPGAP 15

#define LOR_WITH_DIRECTION_MATRIX(mask,direction_matrix) direction_matrix |= mask

enum MODE { m_todo, m_vertical, m_horizontal, m_diagonal, m_align } backtrace_mode;

#define has_doflag(dir,flag) (*dir&flag)

#define has_doalign(dir) (has_doflag(dir,DO_ALIGN) || has_doflag(dir,DO_DIAGONAL))
#define has_doinsert(dir) has_doflag(dir,DO_HORIZONTAL)
#define has_dodelete(dir) has_doflag(dir,DO_VERTICAL)

#ifdef _WIN32
__inline int
#else
inline int
#endif
HAS_GAP_EXTENSION (SEQT base, const cmt c) {
    if(cm_check_level(c) == 1)
        return (cm_get_cost(c->cost,base,c->gap,c->map_sz+1));
    else
        return (cm_calc_cost(c->cost,base,c->gap,c->lcm));
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
/*
 * get_go(int base,int prebase, int idx, int gap_opening)
{
    if (idx ==1 && HAS_GAP(base) ) return 0;
    else if ( idx > 1 && !HAS_GAP(prebase) && HAS_GAP(base) ) return 0;
    else return gap_opening;
}
 * */
HAS_GAP_OPENING (int idx, SEQT prev, SEQT curr, int gap, int gap_open, int gapstart) {
    if(gapstart>1) //when level is set, gapstart number will be passed
    {
       if ((prev<gapstart)&&(curr>=gapstart)) return gap_open;
       else return 0;
    }
    else
    {
        //printf("HAS_GAP_OPENING,prev=%d,curr=%d,gap=%d,gap & prev=%d,gap & curr=%d\n",prev,curr,gap,gap & prev,gap & curr);
        if (idx==1 && (gap & curr) ) return 0;
        else if ( idx>1 && (!(gap & prev)) && (gap & curr)) return 0;
        else return gap_open;
    }
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
FILL_EXTEND_HORIZONTAL_NOBT (int sj_horizontal_extension, int sj_gap_extension,
        int sj_gap_opening, int j, int *extend_horizontal, const cmt c,
        const int *close_block_diagonal)
{
    int ext_cost, open_cost;
    ext_cost = extend_horizontal[j - 1] + sj_horizontal_extension;
    open_cost = close_block_diagonal[j - 1] +  sj_gap_opening + sj_gap_extension;
    if (ext_cost < open_cost) 
        extend_horizontal[j] = ext_cost;
    else 
        extend_horizontal[j] = open_cost;
    //if (0 && DEBUG_AFFINE) printf ("The final cost is %d\n", extend_horizontal[j]);
    return;
}


#ifdef _WIN32
__inline DIRECTION_MATRIX
#else
inline DIRECTION_MATRIX
#endif
FILL_EXTEND_HORIZONTAL (int sj_horizontal_extension, int sj_gap_extension, int sj_gap_opening,
        int j, int *extend_horizontal, const cmt c, const int *close_block_diagonal,
        DIRECTION_MATRIX direction_matrix)
{
    int ext_cost, open_cost;
    ext_cost = extend_horizontal[j - 1] + sj_horizontal_extension;
    open_cost = close_block_diagonal[j - 1] + sj_gap_opening + sj_gap_extension;
    if (ext_cost < open_cost) {
        LOR_WITH_DIRECTION_MATRIX(BEGIN_HORIZONTAL,direction_matrix);
        extend_horizontal[j] = ext_cost;
    } else {
        LOR_WITH_DIRECTION_MATRIX(END_HORIZONTAL,direction_matrix);
        extend_horizontal[j] = open_cost;
    }
    //if (DEBUG_AFFINE)  printf ("The final cost is %d\n", extend_horizontal[j]);
    return (direction_matrix);
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
FILL_EXTEND_VERTICAL_NOBT (int si_vertical_extension, int si_gap_extension,
        int si_gap_opening, int j, int *extend_vertical, const int *prev_extend_vertical,
        const cmt c, const int *prev_close_block_diagonal)
{
    int ext_cost, open_cost;
    ext_cost = prev_extend_vertical[j] + si_vertical_extension;
    open_cost = prev_close_block_diagonal[j] + si_gap_opening + si_gap_extension;
    if (ext_cost < open_cost) 
        extend_vertical[j] = ext_cost;
    else 
        extend_vertical[j] = open_cost;
    return;
}

#ifdef _WIN32
__inline DIRECTION_MATRIX
#else
inline DIRECTION_MATRIX
#endif
FILL_EXTEND_VERTICAL (int si_vertical_extension, int si_gap_extension, int si_gap_opening,
        int j, int *extend_vertical, const int *prev_extend_vertical, const cmt c,
        const int *prev_close_block_diagonal, DIRECTION_MATRIX direction_matrix)
{
    int ext_cost, open_cost;
    ext_cost = prev_extend_vertical[j] + si_vertical_extension;
    open_cost = prev_close_block_diagonal[j] + si_gap_opening + si_gap_extension;
    if (ext_cost < open_cost) {
        LOR_WITH_DIRECTION_MATRIX(BEGIN_VERTICAL,direction_matrix);
        extend_vertical[j] = ext_cost;
    } else {
        LOR_WITH_DIRECTION_MATRIX(END_VERTICAL,direction_matrix);
        extend_vertical[j] = open_cost;
    }
    return (direction_matrix);
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
FILL_EXTEND_BLOCK_DIAGONAL_NOBT (SEQT si_base, SEQT sj_base, SEQT si_prev_base, 
        SEQT sj_prev_base, int gap_open, int j, int *extend_block_diagonal,
        const int *prev_extend_block_diagonal, const int *prev_close_block_diagonal)
{
    if (j<=0) failwith ("FILL_EXTEND_BLOCK_DIAGONAL,j must bigger than 0");
    int ext_cost, open_cost;
    int diag, open_diag, flag, flag2;
    flag = ((TMPGAP & si_base) && (TMPGAP & sj_base));
    flag2= (!(TMPGAP & si_prev_base) && (!(TMPGAP & sj_prev_base)));
    diag = flag?0:HIGH_NUM;
    open_diag = flag?(flag2?0:(2 * gap_open)):HIGH_NUM;
    ext_cost = prev_extend_block_diagonal[j - 1] + diag;
    open_cost = prev_close_block_diagonal[j - 1] + open_diag;
    if (ext_cost < open_cost) 
        extend_block_diagonal[j] = ext_cost;
    else 
        extend_block_diagonal[j] = open_cost;
    return;
}

#ifdef _WIN32
__inline DIRECTION_MATRIX
#else
inline DIRECTION_MATRIX
#endif
FILL_EXTEND_BLOCK_DIAGONAL (SEQT si_base, SEQT sj_base, SEQT si_prev_base, SEQT sj_prev_base,
        int gap_open, int j, int *extend_block_diagonal, const int *prev_extend_block_diagonal, 
        const int *prev_close_block_diagonal, DIRECTION_MATRIX direction_matrix)
{
    if (j<=0) failwith ("FILL_EXTEND_BLOCK_DIAGONAL,j must bigger than 0");
    int ext_cost, open_cost;
    int diag, open_diag, flag, flag2;
    flag = ((TMPGAP & si_base) && (TMPGAP & sj_base));
    flag2= (!(TMPGAP & si_prev_base) && (!(TMPGAP & sj_prev_base)));
    diag = flag?0:HIGH_NUM;
    open_diag = flag?(flag2?0:(2 * gap_open)):HIGH_NUM;
    ext_cost = prev_extend_block_diagonal[j - 1] + diag;
    open_cost = prev_close_block_diagonal[j - 1] + open_diag;
    if (ext_cost < open_cost) {
        LOR_WITH_DIRECTION_MATRIX(BEGIN_BLOCK,direction_matrix);
        extend_block_diagonal[j] = ext_cost;
    }
    else {
        LOR_WITH_DIRECTION_MATRIX(END_BLOCK,direction_matrix);
        extend_block_diagonal[j] = open_cost;
    }
    return (direction_matrix);
}

void
FILL_CLOSE_BLOCK_DIAGONAL_NOBT(SEQT si_base, SEQT sj_base, SEQT si_no_gap,
        SEQT sj_no_gap, int si_gap_opening, int sj_gap_opening, int j,
        const int *c, int *close_block_diagonal, const int *prev_close_block_diagonal,
        const int *prev_extend_vertical, const int *prev_extend_horizontal,
        const int *prev_extend_block_diagonal)
{
    if (j<=0) failwith ("FILL_CLOSE_BLOCK_DIAGONAL,j must bigger than 0");
    int diag, extra_gap_opening;
    int algn, from_vertical, from_horizontal, from_diagonal;
    diag = c[sj_no_gap];
    extra_gap_opening = (sj_gap_opening < si_gap_opening)?si_gap_opening:sj_gap_opening;
    algn = prev_close_block_diagonal[j - 1] + diag;
    if (si_base == si_no_gap)
        from_vertical = prev_extend_vertical[j - 1] + diag;
    else
        from_vertical = prev_extend_vertical[j - 1] + diag + sj_gap_opening;

    if (sj_base == sj_no_gap)
        from_horizontal = prev_extend_horizontal[j - 1] + diag;
    else
        from_horizontal = prev_extend_horizontal[j - 1] + diag + si_gap_opening;
    
    from_diagonal = prev_extend_block_diagonal[j - 1] + diag + extra_gap_opening;
    close_block_diagonal[j] = algn;

    if (close_block_diagonal[j] > from_vertical) 
        close_block_diagonal[j] = from_vertical;
    if (close_block_diagonal[j] > from_horizontal) 
            close_block_diagonal[j] = from_horizontal;
    if (close_block_diagonal[j] > from_diagonal) 
            close_block_diagonal[j] = from_diagonal;
    return;
}

DIRECTION_MATRIX
FILL_CLOSE_BLOCK_DIAGONAL(SEQT si_base, SEQT sj_base, SEQT si_no_gap,
        SEQT sj_no_gap, int si_gap_opening, int sj_gap_opening, int j, const int *c,
        int *close_block_diagonal, const int *prev_close_block_diagonal,
        const int *prev_extend_vertical, const int *prev_extend_horizontal,
        const int *prev_extend_block_diagonal, DIRECTION_MATRIX direction_matrix)
{
    if (j<=0) failwith ("FILL_CLOSE_BLOCK_DIAGONAL,j must bigger than 0");
    int diag, extra_gap_opening;
    int algn, from_vertical, from_horizontal, from_diagonal;
    DIRECTION_MATRIX mask;
    diag = c[sj_no_gap];
    extra_gap_opening = 
        (sj_gap_opening < si_gap_opening)?si_gap_opening:sj_gap_opening;
    if (0 && DEBUG_AFFINE) {
        printf ("Between %d and %d: Diag : %d, Extra gap opening: %d\n", si_no_gap, sj_no_gap, diag, extra_gap_opening);
        fflush (stdout);
    }
    algn = prev_close_block_diagonal[j - 1] + diag;
    if (si_base == si_no_gap)
        from_vertical = prev_extend_vertical[j - 1] + diag;
    else
        from_vertical = prev_extend_vertical[j - 1] + diag + sj_gap_opening;
    if (sj_base == sj_no_gap)
        from_horizontal = prev_extend_horizontal[j - 1] + diag;
    else
        from_horizontal = prev_extend_horizontal[j - 1] + diag + si_gap_opening;
    from_diagonal = prev_extend_block_diagonal[j - 1] + diag + extra_gap_opening;
    mask = ALIGN_TO_ALIGN;
    close_block_diagonal[j] = algn;
    if (close_block_diagonal[j] >= from_vertical) {
        if (close_block_diagonal[j] > from_vertical) {
            close_block_diagonal[j] = from_vertical;
            mask = ALIGN_TO_VERTICAL;
        } else {
            mask = mask | ALIGN_TO_VERTICAL;
        }
    }
    if (close_block_diagonal[j] >= from_horizontal) {
        if (close_block_diagonal[j] > from_horizontal) {
            close_block_diagonal[j] = from_horizontal;
            mask = ALIGN_TO_HORIZONTAL;
        } else {
            mask = mask | ALIGN_TO_HORIZONTAL;
        }
    }
    if (close_block_diagonal[j] >= from_diagonal) {
        if (close_block_diagonal[j] > from_diagonal) {
            close_block_diagonal[j] = from_diagonal;
            mask = ALIGN_TO_DIAGONAL;
        }
        else {
            mask = mask | ALIGN_TO_DIAGONAL;
        }
    }
    LOR_WITH_DIRECTION_MATRIX(mask,direction_matrix);
    return (direction_matrix);
}



void print_dir (DIRECTION_MATRIX * dir)
{
    printf("[");
    if(has_doflag(dir,DO_VERTICAL)) printf("DO_VERTICAL,");
    if(has_doflag(dir,DO_DIAGONAL)) printf("DO_DIAGONAL,");
    if(has_doflag(dir,DO_ALIGN)) printf("DO_ALIGN,");
    if(has_doflag(dir,ALIGN_TO_ALIGN)) printf("ALIGN_TO_ALIGN,");
    if(has_doflag(dir,ALIGN_TO_VERTICAL)) printf("ALIGN_TO_VERTICAL,");
    if(has_doflag(dir,ALIGN_TO_HORIZONTAL)) printf("ALIGN_TO_HORIZONTAL,");
    if(has_doflag(dir,ALIGN_TO_DIAGONAL)) printf("ALIGN_TO_DIAGONAL,");
    if(has_doflag(dir,BEGIN_BLOCK)) printf("BEGIN_BLOCK,");
    if(has_doflag(dir,END_BLOCK)) printf("END_BLOCK,");
    if(has_doflag(dir,BEGIN_VERTICAL)) printf("BEGIN_VERTICAL,");
    if(has_doflag(dir,END_VERTICAL)) printf("END_VERTICAL,");
    if(has_doflag(dir,BEGIN_HORIZONTAL)) printf("BEGIN_HORIZONTAL,");
    if(has_doflag(dir,DO_HORIZONTAL)) printf("DO_HORIZONTAL,");
    if(has_doflag(dir,END_HORIZONTAL)) printf("END_HORIZONTAL,");
    printf("]\n");
    fflush(stdout);
}

void
print_dm2 (char *title, DIRECTION_MATRIX *arr, int start,int max) {
    int i;
    printf ("%s,start,end=%d,%d: \n", title, start, max);
    for (i = start; i <= max; i++) {
        print_dir (&arr[i]); fflush(stdout);
    }
    printf ("\n");
    fflush (stdout);
    return;
}



void follow_horizontal_affine (int* j,DIRECTION_MATRIX **direction_matrix_p,
        enum MODE * mode,SEQT* jc_p,seqt median,seqt medianwg,seqt resi,seqt resj,
        const seqt sj)
{
    //continue with insertion, until we reach the gap opening position. or some
    //position where gap extension from vertical and horizontal cost the same
    if (has_doflag(*direction_matrix_p,END_HORIZONTAL)||has_doflag(*direction_matrix_p,HORIZONTAL_EQ_VERTICAL))
        *mode = m_todo;
    if (!(*jc_p & TMPGAP)) {
        seq_prepend (median, (*jc_p | TMPGAP));
        seq_prepend (medianwg, (*jc_p | TMPGAP));
    } else {
        seq_prepend (medianwg, TMPGAP);
    }
    seq_prepend (resi, TMPGAP);
    seq_prepend (resj, *jc_p);
    *j = *j-1;
    *direction_matrix_p -= 1;
    *jc_p = seq_get(sj, *j);
}

void
follow_vertical_affine (int* i,DIRECTION_MATRIX **direction_matrix_p,
        enum MODE * mode,SEQT* ic_p,seqt median,seqt medianwg,seqt resi,seqt resj,
        const seqt si,int offset)
{
    //continue with deletion, until we reach the gap opening position. or some
    //position where gap extension from vertical and horizontal cost the same
    if (has_doflag(*direction_matrix_p,END_VERTICAL)||has_doflag(*direction_matrix_p,HORIZONTAL_EQ_VERTICAL)) *mode = m_todo;
    if (!(*ic_p & TMPGAP)) {
        seq_prepend (median, (*ic_p | TMPGAP));
        seq_prepend (medianwg, (*ic_p | TMPGAP));
    } 
    else 
    { seq_prepend (medianwg, TMPGAP); }
    seq_prepend(resi, *ic_p);
    seq_prepend(resj, TMPGAP);
    *i = *i -1;
    //move pointer one line up
    *direction_matrix_p -= (offset);
    *ic_p = seq_get(si,*i);
}

void 
follow_vertical_or_horizontal_affine (int* i,int* j,DIRECTION_MATRIX **direction_matrix,
        enum MODE * mode,SEQT* ic_p,SEQT* jc_p,seqt median,seqt medianwg,seqt resi,
        seqt resj,const seqt si,const seqt sj, int offset,int swaped)
{
    if (!swaped) {
        if (*mode == m_vertical) {
            follow_vertical_affine(i,direction_matrix,mode,ic_p,median,medianwg,resi,resj,si,offset);
        } else {
            follow_horizontal_affine(j,direction_matrix,mode,jc_p,median,medianwg,resi,resj,sj);
        }
    } else {
        if (*mode == m_horizontal) {
            follow_horizontal_affine(j,direction_matrix,mode,jc_p,median,medianwg,resi,resj,sj);
        } else {
            follow_vertical_affine(i,direction_matrix,mode,ic_p,median,medianwg,resi,resj,si,offset);
        }
    }
}

void choose_dir (int has_diag,int has_algn,int has_vert, int has_hrzn, int swaped, enum MODE * mode)
{
    if (!swaped) {
        if (has_vert) {
            *mode = m_vertical;
        } else if (has_hrzn){
            *mode = m_horizontal;
        } else if (has_diag){
            *mode = m_diagonal;
        } else {
            assert(has_algn);
            *mode = m_align;
        }
    } else {
        if (has_hrzn){
            *mode = m_horizontal;
        } else if (has_vert){
            *mode = m_vertical;
        } else if (has_diag) {
            *mode = m_diagonal;
        } else {
            assert(has_algn);
            *mode = m_align;
        }
    }
}

#ifdef _WIN32
__inline void 
#else
inline void 
#endif
update_mode(enum MODE* mode, DIRECTION_MATRIX *direction_matrix)
{
    if ( has_doflag(direction_matrix,DO_VERTICAL) ){
        *mode = m_vertical;
    } else if ( has_doflag(direction_matrix,DO_HORIZONTAL) ){
        *mode = m_horizontal;
    } else if ( has_doflag(direction_matrix,DO_DIAGONAL) ){
        *mode = m_diagonal;
    } else {
        assert( has_doflag(direction_matrix,DO_ALIGN) );
        *mode = m_align;
    }
}

/** Returns the greater number of gaps in the aligned s1 and s2; used to test
 * the ending conditions for the ukkonen barrier. Variation of the function,
 * backtrace_2d; look their for further details. */
int
backtrace_aff_gaps (DIRECTION_MATRIX *direction_matrix, const seqt si, const seqt sj)
{
    int gaps_i, gaps_j;
    enum MODE mode;
    int i, j, offset;
    //offset is the length of each row
    offset = seq_get_len(sj) + 1;
    gaps_i = gaps_j = 0;
    //these '-1' is really unconvenient
    i = seq_get_len(si) - 1;
    j = seq_get_len(sj) - 1;
    //move pointer to the right bottom of dir matrix
    //we allocate len1+1 * len2+1 for direction matrix in matrices.c, last cell
    //of each line is not in use in this case. 
    direction_matrix = direction_matrix + (((seq_get_len(si) ) * (seq_get_len(sj) + 1)) - 2);
    mode = m_todo;
    while ((i != 0) && (j != 0)) {
        //start of new code
        if (mode == m_vertical ){
            if (has_doflag(direction_matrix,END_VERTICAL)||has_doflag(direction_matrix,HORIZONTAL_EQ_VERTICAL)){
                direction_matrix -= offset;
                update_mode(&mode,direction_matrix);
            } else {
                direction_matrix -= offset;
            }
            i--;
            gaps_j++;
        } else if (mode == m_horizontal) {
            if (has_doflag(direction_matrix,END_HORIZONTAL)||has_doflag(direction_matrix,HORIZONTAL_EQ_VERTICAL)){
                direction_matrix -= 1;
                update_mode(&mode,direction_matrix);
            } else {
                direction_matrix -= 1;
            }
            j--;
            gaps_i++;
        } else if (mode == m_diagonal){
            if (has_doflag(direction_matrix,END_BLOCK)){
                direction_matrix -= offset+1;
                update_mode(&mode,direction_matrix);
            } else {
                direction_matrix -= offset+1;
            }
            gaps_i++;
            gaps_j++;
            i--;
            j--;
        } else {
            assert (mode == m_align);
            if ( has_doflag(direction_matrix,ALIGN_TO_VERTICAL) ){
                mode = m_vertical;
            } else if ( has_doflag(direction_matrix,ALIGN_TO_HORIZONTAL) ){
                mode = m_horizontal;
            } else if ( has_doflag(direction_matrix,ALIGN_TO_DIAGONAL) ){
                mode = m_diagonal;
            } else {
                assert( has_doflag(direction_matrix,ALIGN_TO_ALIGN) );
                mode = m_align;
            }
            i--; 
            j--;
            direction_matrix -= offset+1;
        }
    }
    gaps_i += j;
    gaps_j += i;
    return (MAX(gaps_j,gaps_i));
}


void
backtrace_aff (DIRECTION_MATRIX *direction_matrix, const seqt si, const seqt sj,
        seqt median, seqt medianwg, seqt resi, seqt resj, const cmt c,int swaped)
{
    int mode_vert, mode_hrzn;
    int flag_vert, flag_hrzn, flag_diag, flag_algn;
    int flag_algn2vert, flag_algn2hrzn, flag_algn2diag, flag_algn2algn;
    enum MODE mode = m_todo;
    int i, j;
    SEQT ic, jc, prep;
    //offset is the length of each row
    int offset = seq_get_len(sj) + 1;
    //these '-1' is really unconvenient
    i = seq_get_len(si) - 1;
    j = seq_get_len(sj) - 1;
    assert (i <= j);
    ic = seq_get(si,i);
    jc = seq_get(sj,j);
    //move pointer to the right bottom of dir matrix
    //we allocate len1+1 * len2+1 for direction matrix in matrices.c, last cell
    //of each line is not in use in this case. 
    direction_matrix = direction_matrix + (((seq_get_len(si) ) * (seq_get_len(sj) + 1)) - 2);
    while ((i != 0) && (j != 0)) {
        if (0 && DEBUG_AFFINE) {
            printf ("In position %d %d of backtrace\n", i, j);
            fflush (stdout);
        }
        mode_vert = (mode == m_vertical);
        mode_hrzn = (mode == m_horizontal);
        //start of new code
        if (mode == m_todo) {
            flag_vert = has_doflag(direction_matrix,DO_VERTICAL);        
            flag_hrzn = has_doflag(direction_matrix,DO_HORIZONTAL);
            flag_diag = has_doflag(direction_matrix,DO_DIAGONAL);
            flag_algn = has_doflag(direction_matrix,DO_ALIGN);
            choose_dir(flag_diag,flag_algn,flag_vert,flag_hrzn,swaped,&mode);
        } else if (mode_vert||mode_hrzn) {
            follow_vertical_or_horizontal_affine (&i,&j, &direction_matrix,&mode,&ic,&jc,median,medianwg,resi,resj,si,sj,offset,swaped);
        } else if (mode == m_diagonal){
            if (has_doflag(direction_matrix,END_BLOCK)) mode = m_todo;
            seq_prepend(resi, ic);
            seq_prepend(resj, jc);
            seq_prepend(medianwg, TMPGAP);
            i--; 
            j--;
            //move pointer to left up cell
            direction_matrix -= (offset + 1);
            jc = seq_get(sj, j);
            ic = seq_get(si, i);
        } else {
            assert (mode == m_align);
            flag_algn2diag = has_doflag(direction_matrix,ALIGN_TO_DIAGONAL);
            flag_algn2vert = has_doflag(direction_matrix,ALIGN_TO_VERTICAL);
            flag_algn2hrzn = has_doflag(direction_matrix,ALIGN_TO_HORIZONTAL);
            flag_algn2algn = has_doflag(direction_matrix,ALIGN_TO_ALIGN); 
            choose_dir(flag_algn2diag,flag_algn2algn,flag_algn2vert,flag_algn2hrzn,swaped,&mode);
            prep = cm_get_median(c,(ic & (NTMPGAP)),(jc & (NTMPGAP)));
            seq_prepend(median, prep);
            seq_prepend(medianwg, prep);
            seq_prepend(resi, ic);
            seq_prepend(resj, jc);
            i--; 
            j--;
            //move pointer to left up cell
            direction_matrix -= (offset + 1);
            jc = seq_get(sj, j);
            ic = seq_get(si, i);
        } 
        //end of new code
    }
    while (i != 0) {
        assert (initial_direction_matrix < direction_matrix);
        if (!(ic & TMPGAP)) {
            seq_prepend (median, (ic | TMPGAP));
            seq_prepend (medianwg, (ic | TMPGAP));
        }
        else seq_prepend (medianwg, TMPGAP);
        seq_prepend(resi, ic);
        seq_prepend(resj, TMPGAP);
        //move pointer one line up
        direction_matrix -= (offset);
        i--;
        ic = seq_get(si, i);
    }
    while (j != 0) {
        assert (initial_direction_matrix < direction_matrix);
        if (!(jc & TMPGAP)) {
            seq_prepend (median, (jc | TMPGAP));
            seq_prepend (medianwg, (jc | TMPGAP));
        }
        else seq_prepend (medianwg, TMPGAP);
        seq_prepend (resi, TMPGAP);
        seq_prepend (resj, jc);
        j--;
        //move pointer one cell left
        direction_matrix -= 1;
        jc = seq_get(sj, j);
    }
    seq_prepend(resi, TMPGAP);
    seq_prepend(resj, TMPGAP);
    seq_prepend(medianwg,TMPGAP);
    if (TMPGAP != seq_get(median,0))
        seq_prepend(median,TMPGAP);
    return;

}

void
initialize_matrices_affine_nobt (int go, const seqt si, const seqt sj, const cmt c,
        int *close_block_diagonal, int *extend_block_diagonal, int *extend_vertical,
        int *extend_horizontal, const int *prec)
{
    int lenj, j = 1, r;
    const int *gap_row;
    lenj = seq_get_len(sj) - 1;
    int offset = seq_get_len(sj) + 1;
    //g[0,0]=0, d[0,0]=INF, v[0,0]=GO, h[0,0]=GO
    close_block_diagonal[0] = 0;//g[0,0]=0
    extend_block_diagonal[0] = HIGH_NUM; //0; d[0,0]=INF  -- fix
    extend_horizontal[0] = go;//h[0,0]=GO
    extend_vertical[0] = go;//v[0,0]=GO
    //final_cost_matrix[0] = 0;//assign best cost
    //direction_matrix[0] = 0xFFFF;
    gap_row = cm_get_precal_row(prec,0,lenj);
    if (DEBUG_AFFINE) {
        printf ("The gap opening parameter is %d\n", go);
        print_array ("EH:", extend_horizontal, 0, lenj);
        print_array ("EV:", extend_vertical, 0, lenj);
        print_array ("EB:", extend_block_diagonal, 0, lenj);
        print_array ("CB:", close_block_diagonal, 0, lenj);
    }
    for (; j <= lenj; j++) {
        //g[0,i] = d[0,i] = v[0,i] = INF, h[0,i] = h[0,i-1] + GO
        r = extend_horizontal[j - 1] + gap_row[j];
        extend_horizontal[j] = r;//  h[0,i] = h[0,i-1] + GO
        close_block_diagonal[j] = HIGH_NUM; //r; g[0,i] = INF -- fix
        extend_block_diagonal[j] = HIGH_NUM;// d[0,i] = INF
        extend_vertical[j] = HIGH_NUM; // v[0,i] = INF
        //final_cost_matrix[j] = r;//assign best cost
    }
     if (DEBUG_AFFINE) {
        printf ("offset=%d,Just initialized\n",offset);
        print_array ("EH:", extend_horizontal, 0, lenj);
        print_array ("EV:", extend_vertical, 0, lenj);
        print_array ("EB:", extend_block_diagonal, 0, lenj);
        print_array ("CB:", close_block_diagonal, 0, lenj);
        printf ("Finished initialized\n");
    }
    return;
}

void
initialize_matrices_affine (int go, const seqt si, const seqt sj, const cmt c, 
        int *close_block_diagonal, int *extend_block_diagonal, int *extend_vertical,
        int *extend_horizontal, int *final_cost_matrix,
        DIRECTION_MATRIX *direction_matrix, const int *prec)
{
    int lenj, j = 1, r;
    const int *gap_row;
    lenj = seq_get_len(sj) - 1;
    //g[0,0]=0, d[0,0]=INF, v[0,0]=GO, h[0,0]=GO
    close_block_diagonal[0] = 0;//g[0,0]=0
    extend_block_diagonal[0] = HIGH_NUM; //0; d[0,0]=INF  -- fix
    extend_horizontal[0] = go;//h[0,0]=GO
    extend_vertical[0] = go;//v[0,0]=GO
    final_cost_matrix[0] = 0;//assign best cost
    direction_matrix[0] = 0xFFFF;
    gap_row = cm_get_precal_row(prec,0,lenj);
    if (DEBUG_AFFINE) {
        printf ("The gap opening parameter is %d\n", go);
        print_array ("EH:", extend_horizontal, 0, lenj);
        print_array ("EV:", extend_vertical, 0, lenj);
        print_array ("EB:", extend_block_diagonal, 0, lenj);
        print_array ("CB:", close_block_diagonal, 0, lenj);
        print_array ("FC:", final_cost_matrix, 0, lenj);
    }
    for (j=1; j <= lenj; j++) {
        //g[0,i] = d[0,i] = v[0,i] = INF, h[0,i] = h[0,i-1] + GO
        r = extend_horizontal[j - 1] + gap_row[j];
        extend_horizontal[j] = r;//  h[0,i] = h[0,i-1] + GO
        close_block_diagonal[j] = HIGH_NUM; //r; g[0,i] = INF -- fix
        extend_block_diagonal[j] = HIGH_NUM;// d[0,i] = INF
        extend_vertical[j] = HIGH_NUM; // v[0,i] = INF
        final_cost_matrix[j] = r;//assign best cost
        direction_matrix[j] = DO_HORIZONTAL | END_HORIZONTAL;
    }
    if (DEBUG_AFFINE) {
        printf ("Just initialized\n");
        print_array ("EH:", extend_horizontal, 0, lenj);
        print_array ("EV:", extend_vertical, 0, lenj);
        print_array ("EB:", extend_block_diagonal, 0, lenj);
        print_array ("CB:", close_block_diagonal, 0, lenj);
        print_array ("FC:", final_cost_matrix, 0, lenj);
        printf ("Finished initialized\n");
    }
    /* we are doing newkkonen with barrier, j might not start from 0. there is no need to init first cell of each column here;
    int *prev_extend_vertical; 
    int offset = seq_get_len(sj) + 1;
    SEQT  ic, ip;
    int i;
     for (i=1; i <= leni; i++) { 
        prev_extend_vertical = extend_vertical;
        extend_vertical += offset;
        close_block_diagonal += offset;
        final_cost_matrix += offset;
        extend_block_diagonal += offset;
        extend_horizontal += offset;
        //direction_matrix += offset;
        ic = seq_get(si,i);
        ip = seq_get(si,i - 1);
        r = prev_extend_vertical[0] + (HAS_GAP_EXTENSION(ic,c));
        extend_horizontal[0] = HIGH_NUM;
        close_block_diagonal[0] = r;
        final_cost_matrix[0] = r;
        extend_block_diagonal[0] = HIGH_NUM;
        extend_vertical[0] = r;
        direction_matrix[0] = DO_VERTICAL | END_VERTICAL;
     }*/
    return;
}

DIRECTION_MATRIX
ASSIGN_MINIMUM (int *final_cost_matrix, int extend_horizontal, int extend_vertical,
        int extend_block_diagonal, int close_block_diagonal, DIRECTION_MATRIX direction_matrix,
        DIRECTION_MATRIX * pgm1, DIRECTION_MATRIX * gm1, DIRECTION_MATRIX * pgm2,
        DIRECTION_MATRIX * gm2, int pos)
{
    int mask;
    mask = DO_HORIZONTAL;
    *final_cost_matrix = extend_horizontal;//insert
    if (*final_cost_matrix >= extend_vertical) {
        if (*final_cost_matrix > extend_vertical) {//just insert
            *final_cost_matrix = extend_vertical;
            mask = DO_VERTICAL;
        } else {
            mask = mask | DO_VERTICAL;//insert|delete
        }
    }
    //final_cost_matrix <- min (insert,delete)
    if (*final_cost_matrix >= extend_block_diagonal) {
        if (*final_cost_matrix > extend_block_diagonal) {// just diag
            *final_cost_matrix = extend_block_diagonal;
            mask = DO_DIAGONAL;
        } else {
            mask = mask | DO_DIAGONAL;//diag | xxx
        }
    }
    //final_cost_matrix <- min (insert,delete,diag)
    if (*final_cost_matrix >= close_block_diagonal) {
        if (*final_cost_matrix > close_block_diagonal) { //just algn
            *final_cost_matrix = close_block_diagonal;
            mask = DO_ALIGN;
        } else {
            mask = mask | DO_ALIGN; //algn | xxxx
        }
    }
    //final_cost_matrix now is min (insert,delete,diag,align)
    //if gap extension from vertical and horizontal are the same cost, mark it
    //with HORIZONTAL_EQ_VERTICAL -- we need to stop at position like this to
    //choose next direction in traceback.
    if ( (*final_cost_matrix==extend_horizontal) &&(extend_horizontal==extend_vertical) ){
        mask = mask | HORIZONTAL_EQ_VERTICAL;
    }
    LOR_WITH_DIRECTION_MATRIX(mask,direction_matrix);
    DIRECTION_MATRIX *dir = &direction_matrix;
    algn_fill_gapnum(pos,has_doalign(dir),has_doinsert(dir),has_dodelete(dir),pgm1,gm1,pgm2,gm2);
    return (direction_matrix);
}


//[algn_fill_plane_3_aff_nobt] is the affine alignment function without backtrace
//it is called by [algn_CAML_cost_affine_3] as cost function
int
algn_fill_plane_3_aff_nobt (const seqt si, const seqt sj, int leni, int lenj,
        const cmt c, int *extend_horizontal, int *extend_vertical, 
        int *close_block_diagonal, int *extend_block_diagonal, const int *prec,
        int *gap_open_prec, int *sj_horizontal_extension)
{
    int start_pos, end_pos,  i=1, j, res;
    int *prev_extend_horizontal, *prev_extend_vertical, *prev_close_block_diagonal, 
        *prev_extend_block_diagonal;
    const int *si_no_gap_vector;
    int si_gap_opening, si_gap_extension, sj_gap_opening, sj_gap_extension;
    int gap, gap_open; int gap_startNO;
    const int *gap_row;
    int si_vertical_extension;
    gap = c->gap;
    gap_open = c->gap_open;
    gap_startNO = c->gap_startNO;
    assert (lenj >= leni);
    //set prev_xxx_xxx
    int offset = lenj+2;
    prev_extend_horizontal = extend_horizontal+offset;
    prev_extend_vertical = extend_vertical+offset;
    prev_extend_block_diagonal = extend_block_diagonal+offset;
    prev_close_block_diagonal = close_block_diagonal+offset;
    //fill in gap_row
    gap_row = cm_get_precal_row(prec,0,lenj);
    if (!NDEBUG) {
        print_array ("EH:", extend_horizontal, 0, lenj);
        print_array ("EV:", extend_vertical, 0, lenj);
        print_array ("EB:", extend_block_diagonal, 0, lenj);
        print_array ("CB:", close_block_diagonal, 0, lenj);
    }
    //we don't need barrier here, for alignment without traceback only need one line, that is only O(n) of space.
    start_pos = 1;
    end_pos = lenj;
    SEQT jc, jp, ic, ip, si_no_gap, sj_no_gap;
    SEQT *begini, *beginj;
    begini = si->begin;
    beginj = sj->begin;
    ic = begini[0]; 
    sj_horizontal_extension[0] = 0; 
    //sj_horizontal_extension[1] = gap_open; 
    for (j = 1; j <= lenj; j++) {
        gap_open_prec[j] = HAS_GAP_OPENING(j,beginj[j - 1],beginj[j],gap,gap_open,gap_startNO);
        if ((beginj[j-1] & gap) && (!(beginj[j] & gap)))
            sj_horizontal_extension[j] = gap_open_prec[j] + gap_row[j];
        else
            sj_horizontal_extension[j] = gap_row[j];
    }
    sj_horizontal_extension[1] = gap_row[1];
    int r;
    //we don't need to remember directions with xxx_NOBT functions, but we need
    //the direction to update gap number array. here I pass best_cost and
    //best_dir between [FILL_EXTEND_xxx_NOBT], 
    //so when we reach function [FILL_CLOSE_BLOCK_DIAGONAL_NOBT], we can fill in correct gap numbers
    int * tmp;
    for (i=1;i <= leni; i++) {
	//if(i==1 || i==50) debug = 1; else debug = 0;
        //switch pointers
        tmp = prev_extend_horizontal;
        prev_extend_horizontal = extend_horizontal;
        extend_horizontal = tmp;
        tmp = prev_extend_vertical;
        prev_extend_vertical = extend_vertical;
        extend_vertical = tmp;
        tmp = prev_extend_block_diagonal;
        prev_extend_block_diagonal = extend_block_diagonal;
        extend_block_diagonal = tmp;
        tmp = prev_close_block_diagonal;
        prev_close_block_diagonal = close_block_diagonal;
        close_block_diagonal = tmp;
        extend_horizontal[start_pos - 1] = HIGH_NUM;
        ip = ic;
        ic = begini[i];
        si_gap_extension = HAS_GAP_EXTENSION(ic,c);
        si_gap_opening = HAS_GAP_OPENING (i,ip,ic,gap,gap_open,gap_startNO);
        si_no_gap = (NTMPGAP) & ic;
        if ((i > 1) && ((ip & gap) && (!(ic & gap))))
            si_vertical_extension = si_gap_opening + si_gap_extension;
        else si_vertical_extension = si_gap_extension;
        r = prev_extend_vertical[start_pos - 1] + si_vertical_extension;
        extend_horizontal[start_pos - 1] = HIGH_NUM;
        close_block_diagonal[start_pos - 1] = r;
        extend_block_diagonal[start_pos - 1] = HIGH_NUM;
        extend_vertical[start_pos - 1] = r;
        jc = beginj[start_pos - 1];
        close_block_diagonal[start_pos - 1] = HIGH_NUM;
        si_no_gap_vector = c->cost + (si_no_gap << c->lcm); // we should not use '<<' for level,fix this for 3D later
        for (j=start_pos; j <= end_pos; j++) {
            //if(j==180 || j==179) debug_fcbd=1; else debug_fcbd=0;
	        jp = jc;
            jc = beginj[j];
            sj_no_gap = (NTMPGAP) & jc;
            sj_gap_extension = gap_row[j];
            sj_gap_opening = gap_open_prec[j];
            FILL_EXTEND_HORIZONTAL_NOBT(sj_horizontal_extension[j], sj_gap_extension,
                    sj_gap_opening, j, extend_horizontal,c, close_block_diagonal);
            FILL_EXTEND_VERTICAL_NOBT(si_vertical_extension, si_gap_extension,si_gap_opening,j,
                    extend_vertical,prev_extend_vertical,c, prev_close_block_diagonal);
            FILL_EXTEND_BLOCK_DIAGONAL_NOBT(ic,jc,ip,jp,gap_open,j,extend_block_diagonal,
                    prev_extend_block_diagonal, prev_close_block_diagonal);
            FILL_CLOSE_BLOCK_DIAGONAL_NOBT(ic,jc,si_no_gap,sj_no_gap, si_gap_opening,
                    sj_gap_opening,j,si_no_gap_vector,close_block_diagonal, prev_close_block_diagonal,
                    prev_extend_vertical, prev_extend_horizontal, prev_extend_block_diagonal);
        }
        if (end_pos < lenj) {//if we set end_pos to lenj, this if will never be called
            end_pos++;
            extend_vertical[end_pos] = HIGH_NUM;
            close_block_diagonal[end_pos] = HIGH_NUM;
            extend_horizontal[end_pos] = HIGH_NUM;
            extend_block_diagonal[end_pos] = HIGH_NUM;
        }
        if (!NDEBUG) {
            print_array ("EH:", extend_horizontal, 0, lenj);
            print_array ("EV:", extend_vertical, 0, lenj);
            print_array ("EB:", extend_block_diagonal, 0, lenj);
            print_array ("CB:", close_block_diagonal, 0, lenj);
        }
    }
    res = extend_horizontal[lenj];
    if (res > extend_vertical[lenj]) res = extend_vertical[lenj];
    if (res > extend_block_diagonal[lenj]) res = extend_block_diagonal[lenj];
    if (res > close_block_diagonal[lenj]) res = close_block_diagonal[lenj];
    return res;
}

void
algn_newkk_fill_a_row_aff (int k, int baseband, int i, int start_pos, int end_pos,
        SEQT* beginj, const int * gap_row, int gap_open, int ic,int ip, int si_vertical_extension,
        int si_gap_extension, int si_gap_opening, int si_no_gap, const int* si_no_gap_vector,
        int *prev_extend_horizontal,int *prev_extend_vertical,int *prev_close_block_diagonal,
        int *prev_extend_block_diagonal, int lenj, int *final_cost_matrix, DIRECTION_MATRIX *direction_matrix,
        DIRECTION_MATRIX * pgm1, DIRECTION_MATRIX * gm1, DIRECTION_MATRIX * pgm2,
        DIRECTION_MATRIX * gm2, const cmt c, int *extend_horizontal, int *extend_vertical, 
        int *close_block_diagonal, int *extend_block_diagonal, int *gap_open_prec, int *sj_horizontal_extension)
{
    SEQT jc, jp, sj_no_gap;
    int sj_gap_extension,sj_gap_opening;
    int j;
    DIRECTION_MATRIX tmp_direction_matrix;
    jc = beginj[start_pos];
    int at_left_border, at_right_border;
    for (j=start_pos; j <= end_pos; j++) {
        int k_ji = j - i;
        if (k_ji - baseband + 1 == k) at_right_border=1; else at_right_border=0;
        if (j==start_pos) at_left_border=1; else at_left_border=0;
        jp = jc;
        jc = beginj[j];
        tmp_direction_matrix = 0;
        sj_no_gap = (NTMPGAP) & jc;
        sj_gap_extension = gap_row[j];
        sj_gap_opening = gap_open_prec[j];
        if (at_left_border==0) {
            tmp_direction_matrix =
                FILL_EXTEND_HORIZONTAL(sj_horizontal_extension[j], sj_gap_extension,
                    sj_gap_opening, j, extend_horizontal,c, close_block_diagonal,
                    tmp_direction_matrix);
        } else {
            extend_horizontal[j] = HIGH_NUM;
        }
        if (at_right_border==0) {
            tmp_direction_matrix =
                FILL_EXTEND_VERTICAL(si_vertical_extension, si_gap_extension,
                        si_gap_opening,j, extend_vertical,prev_extend_vertical,c,
                        prev_close_block_diagonal,tmp_direction_matrix);
        } else {
            extend_vertical[j] = HIGH_NUM; 
        }
        if (j>0) {
            tmp_direction_matrix =
                FILL_EXTEND_BLOCK_DIAGONAL(ic,jc,ip,jp,gap_open,j,extend_block_diagonal,
                    prev_extend_block_diagonal, prev_close_block_diagonal, tmp_direction_matrix);
            tmp_direction_matrix =
                FILL_CLOSE_BLOCK_DIAGONAL(ic,jc,si_no_gap,sj_no_gap, si_gap_opening,
                    sj_gap_opening,j,si_no_gap_vector,close_block_diagonal, 
                    prev_close_block_diagonal, prev_extend_vertical, prev_extend_horizontal, 
                    prev_extend_block_diagonal, tmp_direction_matrix);
        } else { 
            close_block_diagonal[j] = HIGH_NUM; 
            extend_block_diagonal[j] = HIGH_NUM;
        }
        tmp_direction_matrix =
            ASSIGN_MINIMUM (final_cost_matrix + j, extend_horizontal[j], extend_vertical[j],
                    extend_block_diagonal[j], close_block_diagonal[j], tmp_direction_matrix,pgm1,gm1,pgm2,gm2,j);
        direction_matrix[j] = tmp_direction_matrix;
    }
    if (!NDEBUG) {
        print_array ("done with this line,FC:", final_cost_matrix, start_pos, end_pos);
        print_array ("EH:", extend_horizontal, start_pos, end_pos);
        print_array ("EV:", extend_vertical, start_pos, end_pos);
        print_array ("EB:", extend_block_diagonal, start_pos, end_pos);
        print_array ("CB:", close_block_diagonal, start_pos, end_pos);
    }
    return;
}




int
algn_newkk_test_aff (int * res_cost, int p, const seqt si, const seqt sj, int leni,
        int lenj, int *final_cost_matrix, DIRECTION_MATRIX *direction_matrix, 
        DIRECTION_MATRIX * gm1, DIRECTION_MATRIX * next_gm1, DIRECTION_MATRIX * gm2,
        DIRECTION_MATRIX * next_gm2, const cmt c, int *extend_horizontal,
        int *extend_vertical, int *close_block_diagonal, int *extend_block_diagonal,
        const int *prec, int *gap_open_prec, int *sj_horizontal_extension)
{
    int startj,endj;
    int lenY = lenj, lenX = leni;
    int newk=p;
    if (p>=lenX) newk=lenX-1;
    SEQT ic, ip, si_no_gap; //jc, jp, , sj_no_gap;
    SEQT *begini, *beginj;
    int start_pos = 1, end_pos,  i=1, j;
    int *prev_extend_horizontal, *prev_extend_vertical, *prev_close_block_diagonal, 
        *prev_extend_block_diagonal;
    const int *si_no_gap_vector;
    int si_gap_opening, si_gap_extension;//, sj_gap_opening, sj_gap_extension;
    int gap, gap_open; int gap_startNO;
    const int *gap_row;
    int si_vertical_extension;
    gap = c->gap;
    gap_open = c->gap_open;
    gap_startNO = c->gap_startNO;
    //make sure seq1 is not longer than seq2
    assert (lenj >= leni);
    //set prev_xxx_xxx
    int offset = lenj+2;
    prev_extend_horizontal = extend_horizontal+offset;
    prev_extend_vertical = extend_vertical+offset;
    prev_extend_block_diagonal = extend_block_diagonal+offset;
    prev_close_block_diagonal = close_block_diagonal+offset;
    gap_row = cm_get_precal_row(prec,0,lenj);
    begini = si->begin;
    beginj = sj->begin;
    ic = begini[0];
    //init first line
    extend_vertical[0] = HIGH_NUM;
    close_block_diagonal[0] = 0; 
    sj_horizontal_extension[0] = 0; 
    //sj_horizontal_extension[1] = gap_open;
    gm1[0] = 0; gm2[0] = 0;
    //init first line, fill in sj_horizontal_extension[1~lenj]
    //if(debug) printf("init line#.0:\n");
    for (j = 1; j <= lenj; j++) {
        gap_open_prec[j] = HAS_GAP_OPENING(j,beginj[j - 1],beginj[j],gap,gap_open,gap_startNO);
        if ((beginj[j-1] & gap) && (!(beginj[j] & gap)))//there is a gap_opening 
            sj_horizontal_extension[j] = gap_open_prec[j] + gap_row[j];
        else
            sj_horizontal_extension[j] = gap_row[j];
        gm1[j] = j; gm2[j]=0; 
        //init next_gm1 and next_gm2
        next_gm1[j] = -1; next_gm2[j] = -1;
        //init direction matrix
        direction_matrix[j] = DO_HORIZONTAL;
        //init extend_vertical -- set to infinity
        extend_vertical[j] = HIGH_NUM;
        //init close_block_diagonal 
        close_block_diagonal[j] = close_block_diagonal[j-1] + sj_horizontal_extension[j];
        extend_horizontal[j] = close_block_diagonal[j];
        //if(debug) printf("extend_horizontal&close_block_diagonal[%d] <- %d + %d = %d \n",j,close_block_diagonal[j-1],sj_horizontal_extension[j],close_block_diagonal[j]);
    }
    sj_horizontal_extension[1] = gap_row[1];
    DIRECTION_MATRIX * tmp1,  * tmp2;
    DIRECTION_MATRIX * current_direction_matrix;
    int * tmp;
    for (i=1;i <= leni; i++) {
        //point current_direction_matrix to the line we are updating
        current_direction_matrix = direction_matrix + i*offset;
        //set barrier by k 
        startj= ((i-newk)>0)?i-newk:0;
        endj=( (i+lenY-lenX+newk)<=(lenY-1) )?i+lenY-lenX+newk:lenY;
        start_pos = startj;
        end_pos = endj;
        //switch pointers prev_xxx and xxx
        tmp = prev_extend_horizontal;
        prev_extend_horizontal = extend_horizontal;
        extend_horizontal = tmp;
        tmp = prev_extend_vertical;
        prev_extend_vertical = extend_vertical;
        extend_vertical = tmp;
        tmp = prev_extend_block_diagonal;
        prev_extend_block_diagonal = extend_block_diagonal;
        extend_block_diagonal = tmp;
        tmp = prev_close_block_diagonal;
        prev_close_block_diagonal = close_block_diagonal;
        close_block_diagonal = tmp;
        ip = ic;
        ic = begini[i];
        si_gap_extension = HAS_GAP_EXTENSION(ic,c);
        si_gap_opening = HAS_GAP_OPENING (i,ip,ic,gap,gap_open,gap_startNO);
        si_no_gap = (NTMPGAP) & ic;
        si_no_gap_vector = c->cost + (si_no_gap << c->lcm); // we should not use '<<' for level, fix this later
        if ((i > 1) && ((ip & gap) && (!(ic & gap)))) { 
            si_vertical_extension = si_gap_opening + si_gap_extension;
        } else {
            si_vertical_extension = si_gap_extension;
        }
        algn_newkk_fill_a_row_aff (newk, lenY-lenX+1, i, start_pos, end_pos, beginj, 
                gap_row, gap_open, ic, ip, si_vertical_extension, si_gap_extension,
                si_gap_opening, si_no_gap, si_no_gap_vector, prev_extend_horizontal,
                prev_extend_vertical, prev_close_block_diagonal, prev_extend_block_diagonal,
                lenj, final_cost_matrix, current_direction_matrix, gm1,next_gm1,gm2,next_gm2,
                c, extend_horizontal, extend_vertical, close_block_diagonal, 
                extend_block_diagonal,  gap_open_prec, sj_horizontal_extension);
        //switch pointers, current row become previous row in the next
        //iteration. previous row is useless, use it for new row.
        if (i<=leni-1) {
           tmp1 = gm1;
           gm1 = next_gm1;
           next_gm1 = tmp1;
           tmp2 = gm2;
           gm2 = next_gm2;
           next_gm2 = tmp2;
        }
    }//end of for (i=1;i<=leni;i++)
    *res_cost = final_cost_matrix[lenj];
    // this was found to be slightly slower...
    // return (backtrace_aff_gaps( direction_matrix, si, sj ));
    return (MAX(next_gm1[lenj],next_gm2[lenj]));
}


//rec function
int
algn_newkk_increaseT_aff (int T, const seqt si, const seqt sj, int leni, int lenj,
        int *final_cost_matrix, DIRECTION_MATRIX *direction_matrix,
        DIRECTION_MATRIX * gm1, DIRECTION_MATRIX * next_gm1, DIRECTION_MATRIX * gm2,
        DIRECTION_MATRIX * next_gm2, const cmt c, int *extend_horizontal, int *extend_vertical,
        int *close_block_diagonal, int *extend_block_diagonal, const int *prec,
        int *gap_open_prec, int *sj_horizontal_extension)
{
    int lenY = lenj; int lenX = leni;
    int p = (T - (lenY-lenX))/2;
    int gap_num, res_cost;
    gap_num =
        algn_newkk_test_aff ( &res_cost, p, si, sj, leni, lenj, final_cost_matrix,
                direction_matrix, gm1,next_gm1,gm2,next_gm2, c, extend_horizontal,
                extend_vertical, close_block_diagonal, extend_block_diagonal, prec,
                gap_open_prec, sj_horizontal_extension);
     int newp = (2*T - (lenY-lenX))/2;
    // if (costT <= T)
    if ((gap_num<p)||(newp-lenY+1>=0)) {
        return res_cost;
    } else {
        return algn_newkk_increaseT_aff (2*T, si, sj, leni, lenj, final_cost_matrix,
                        direction_matrix, gm1,next_gm1,gm2,next_gm2, c, extend_horizontal,
                        extend_vertical, close_block_diagonal, extend_block_diagonal, 
                        prec,  gap_open_prec, sj_horizontal_extension);
    }
}


//[algn_fill_plane_3_aff] is the affine alignment function with backtrace, it is called by [algn_CAML_align_affine_3] to get the median&cost
int
algn_fill_plane_3_aff (const seqt si, const seqt sj, int leni, int lenj,
        int *final_cost_matrix, DIRECTION_MATRIX *direction_matrix,
        DIRECTION_MATRIX *gm1,DIRECTION_MATRIX *next_gm1, DIRECTION_MATRIX *gm2,
        DIRECTION_MATRIX *next_gm2, const cmt c, int *extend_horizontal, int *extend_vertical,
        int *close_block_diagonal, int *extend_block_diagonal, const int *prec,
        int *gap_open_prec, int *sj_horizontal_extension)
{
    int delta = cm_get_min_non0_cost(c);
    int iniT = (lenj-leni+1)*delta;
    return algn_newkk_increaseT_aff (iniT, si, sj, leni, lenj, final_cost_matrix,
                    direction_matrix, gm1,next_gm1,gm2,next_gm2, c, extend_horizontal,
                    extend_vertical, close_block_diagonal, extend_block_diagonal, prec,
                    gap_open_prec, sj_horizontal_extension);
}


//this is the affine align function we are calling from CAML side now.
value
algn_CAML_align_affine_3 (value si, value sj, value cm, value am, value resi, 
        value resj, value median, value medianwg, value swaped) {
    CAMLparam4(si,sj,cm,am);
    CAMLxparam5(resi,resj,median,medianwg,swaped);
    seqt csi, csj;
    seqt cresj, cresi, cmedian, cmedianwg;
    cmt ccm;
    matricest cam;
    int spd;
    int leni, lenj;
    int *matrix;
    int *close_block_diagonal;
    int *extend_block_diagonal;
    int *extend_vertical;
    int *extend_horizontal;
    int *final_cost_matrix;
    int *prec;
    int *gap_open_prec;
    int *s_horizontal_gap_extension;
    int res, largest;
    int uselevel;
    spd = Int_val(swaped);
    DIRECTION_MATRIX *direction_matrix;
    DIRECTION_MATRIX *gm1;
    DIRECTION_MATRIX *gm2;
    DIRECTION_MATRIX *gm3;
    DIRECTION_MATRIX *gm4;
    Seq_custom_val(csi,si);
    Seq_custom_val(csj,sj);
    ccm = Cost_matrix_struct(cm);
    cam = Matrices_struct(am);
    Seq_custom_val(cresi,resi);
    Seq_custom_val(cresj,resj);
    Seq_custom_val(cmedian,median);
    Seq_custom_val(cmedianwg,medianwg);
    leni = seq_get_len(csi);
    lenj = seq_get_len(csj);
    if (lenj<leni) failwith("pass the shorter one as first");
    uselevel = cm_check_level(ccm);
    //if trace back is needed, we need a full matrix for directions (size of len_seq1 * len_seq2)
    //for all other cost matrixs (close_block_diagonal,extend_block_diagonal,extend_vertical,etc), we just need two rows, one for the row we just filled, one for the current one we are working on.  
    if(uselevel==1) 
        mat_setup_size (cam, leni+11, lenj, 0, 0, cm_get_map_sz(ccm),uselevel);
    else
        mat_setup_size (cam, leni+11, lenj, 0, 0, cm_get_lcm(ccm),0);
    matrix = mat_get_2d_matrix(cam);
    prec = mat_get_2d_prec(cam);
    largest = lenj+1;
    close_block_diagonal = (int *) matrix;
    extend_block_diagonal = (int *) (matrix + (2 * largest));
    extend_vertical = (int *) (matrix + (4 * largest));
    extend_horizontal = (int *) (matrix + (6 * largest));
    final_cost_matrix = (int *) (matrix + (8 * largest));
    gap_open_prec = (int *) (matrix + (10 * largest));
    s_horizontal_gap_extension = (int *) (matrix + (11 * largest));
    direction_matrix =  mat_get_2d_direct(cam);
    gm1 = mat_get_2d_gapnum1 (cam);
    gm2 = mat_get_2d_gapnum2 (cam);
    gm3 = mat_get_2d_gapnum3 (cam);
    gm4 = mat_get_2d_gapnum4 (cam);
    /*sanity check
    printf("init final_cost_matrix:\n"); fflush(stdout);
    init_array(final_cost_matrix,lenj);
    print_array("final_cost_matrix",final_cost_matrix,0,lenj-1);
    //end of sanity check*/
    cm_precalc_4algn(ccm,cam,csj);
    initialize_matrices_affine(ccm->gap_open,csi,csj,ccm,close_block_diagonal, 
            extend_block_diagonal, extend_vertical, extend_horizontal, 
            final_cost_matrix, direction_matrix, prec);
    //print_dm2 ("check dm after initialize_matrices_affine:",direction_matrix,0,lenj-1);
    //we pass leni-1, NOT leni to [algn_fill_plane_3_aff] !
    res = algn_fill_plane_3_aff (csi, csj, leni - 1, lenj - 1, final_cost_matrix,
        direction_matrix, gm1, gm2, gm3, gm4, ccm, extend_horizontal, extend_vertical, 
        close_block_diagonal, extend_block_diagonal, prec, gap_open_prec, 
        s_horizontal_gap_extension);
    /*printf("check dm before backtrace:\n");
    direction_matrix =  mat_get_2d_direct(cam);
    int k; int offset = lenj+1;
    DIRECTION_MATRIX * current_dm;
    for(k=0;k<leni;k++)
    {
        printf("line %d: ",k);
        current_dm = direction_matrix + k * offset;
        print_dm2 ("dm",current_dm,0,lenj-1);
    }*/
    backtrace_aff(direction_matrix, csi, csj, cmedian, cmedianwg, \
            cresi, cresj, ccm, spd);
    CAMLreturn(Val_int(res));
}

value 
algn_CAML_align_affine_3_bc (value *argv, int argn) {
    return (algn_CAML_align_affine_3 (argv[0], argv[1], argv[2], argv[3], argv[4], \
                argv[5], argv[6], argv[7], argv[8]));
}

// cost function for affine called from caml side 
value
algn_CAML_cost_affine_3 (value si, value sj, value cm, value am) {
    CAMLparam4(si,sj,cm,am);
    seqt csi, csj;
    cmt ccm;
    matricest cam;
    int leni, lenj;
    int *matrix;
    int *close_block_diagonal;
    int *extend_block_diagonal;
    int *extend_vertical;
    int *extend_horizontal;
    int *prec;
    int *gap_open_prec;
    int *s_horizontal_gap_extension;
    int res, largest;
    int uselevel;
    Seq_custom_val(csi,si);
    Seq_custom_val(csj,sj);
    ccm = Cost_matrix_struct(cm);
    cam = Matrices_struct(am);
    leni = seq_get_len(csi);
    lenj = seq_get_len(csj);
    if (leni > lenj)
        largest = leni;
    else largest = lenj;
    uselevel = cm_check_level(ccm);
    //printf("algn_CAML_cost_affine_3,leni/lenj=%d/%d\n",leni,lenj);
    if(uselevel==1) 
         mat_setup_size (cam, largest, largest, 0, 0, cm_get_map_sz(ccm),uselevel);
    else
        mat_setup_size (cam, largest, largest, 0, 0, cm_get_lcm(ccm),0);
    matrix = mat_get_2d_matrix(cam);
    close_block_diagonal = (int *) matrix;
    extend_block_diagonal = (int *) (matrix + (2 * largest));
    extend_vertical = (int *) (matrix + (4 * largest));
    extend_horizontal = (int *) (matrix + (6 * largest));
    gap_open_prec = (int *) (matrix + (10 * largest));
    s_horizontal_gap_extension = (int *) (matrix + (11 * largest));
    prec = mat_get_2d_prec(cam);
    if (leni <= lenj) {
        cm_precalc_4algn(ccm,cam,csj);
        initialize_matrices_affine_nobt(ccm->gap_open,csi,csj,ccm,close_block_diagonal, 
                extend_block_diagonal, extend_vertical, extend_horizontal, 
                prec);
        res = algn_fill_plane_3_aff_nobt (csi, csj, leni - 1, lenj - 1, 
            ccm, extend_horizontal, extend_vertical, 
            close_block_diagonal, extend_block_diagonal, prec, gap_open_prec, 
            s_horizontal_gap_extension);
    } else {
        cm_precalc_4algn(ccm,cam,csi);
        initialize_matrices_affine_nobt(ccm->gap_open,csj,csi,ccm,close_block_diagonal, 
                extend_block_diagonal, extend_vertical, extend_horizontal, prec);
        res = algn_fill_plane_3_aff_nobt (csj, csi, lenj - 1, leni - 1, 
            ccm, extend_horizontal, extend_vertical, 
            close_block_diagonal, extend_block_diagonal, prec, gap_open_prec, 
            s_horizontal_gap_extension);
    }
    CAMLreturn(Val_int(res));
}


/** Fill parallel must have been called before */
#ifdef _WIN32
static __inline void
#else
static inline void
#endif
fill_moved (int s3_len, const int *prev_m, const int *upper_m, \
        const int *diag_m, const int *s1gs3, const int *gs2s3, \
        const int *s1s2s3, int *mm, DIRECTION_MATRIX *dm) {
    int k, tmp0, tmp1, tmp;
    for (k = 1; k < s3_len; k++) {
        tmp0 = upper_m[k] + s1gs3[k];
        if (tmp0 < mm[k]) {
            mm[k] = tmp0;
            dm[k] = S3;
        }
        tmp = prev_m[k] + gs2s3[k];
        if (tmp < mm[k]) {
            mm[k] = tmp;
            dm[k] = S1;
        }
        tmp1 = diag_m[k] + s1s2s3[k];
        if (tmp1 < mm[k]) {
            mm[k] = tmp1;
            dm[k] = S2;
        }
    }
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
fill_parallel (int s3_len, const int *prev_m, const int *upper_m, \
        const int *diag_m, int s1gg, int gs2g, int s1s2g, int *mm, \
        DIRECTION_MATRIX *dm) {
    int k, tmp1, tmp;
    for (k = 0; k < s3_len; k++) {
        mm[k] = upper_m[k] + s1gg;
        dm[k] = P3;
        tmp = prev_m[k] + gs2g;
        if (tmp < mm[k]) {
            mm[k] = tmp;
            dm[k] = P1;
        }
        tmp1 = diag_m[k] + s1s2g;
        if (tmp1 < mm[k]) {
            mm[k] = tmp1;
            dm[k] = P2;
        }
    }
}

/*
 * s1 is a pointer to the sequence s1 (vertical), and s2 (horizontal 1) defined
 * in the same way. prec is a pointer to the three dimensional matrix that holds
 * the alignment values for all the combinations of the alphabet of sequences
 * s1, s2 and s3, with the sequence s3 (see cm_precalc_4algn_3d for more
 * information). s1_len, s2_len and s3_len is the length of the three sequences
 * to be aligned, and *mm is the first element of the alignment cube that will
 * cold the matrix of the dynamic programming algorithm, while dm does the same
 * job, holding the direction information for the backtrack. uk is the value of
 * the Ukkonen barriers (not used in this version of the program 
 * conside all combinations
 * s1, g, g -> const for plane
 * g, s2, g -> const const per row
 * s1, s2, g -> const const per row
 * g, s2, s3 -> vector (changes on each row)
 * s1, g, s3 -> vector (change per plane)
 * s1, s2, s3 -> vector (changes on each row)
 * g, g, s3 -> vector (the last one to be done, not parallelizable
 *
 */
int
algn_fill_cube (const seqt s1, const seqt s2, const int *prec, int s1_len,
    int s2_len, int s3_len, int *mm, DIRECTION_MATRIX *dm, int uk, int gap, int a_sz)
{
    SEQT *s1p, *s2p;
    /* Each of the following arrays hold some precalculated value for the
     * sequence s3 which is not passed as argument. */
    const int *gs2s3;     /** Align a gap and the current base of s2 with s3 */
    const int *s1gs3;     /** Align the current base of s1 and a gap with s3 */
    const int *s1s2s3;    /** Align the current bases of s1 and s2 with s3 */
    const int *ggs3;      /** Align two gaps with s3 */
    /* Each of the following arrays hold the arrays of the three dimensional
     * matrix that are located around the row that is filled on each iteration.
     * These rows are already filled in some previous iteration and used in 
     * the dynamic programming matrix. */
    int *upper_m;       /** The row in the upper plane of the cube */
    int *prev_m;        /** The row behind the current row in the same plane */
    int *diag_m;        /** The upper_m relative to prev_m */
    int *tmp_mm;        /** A temporary pointer to the row that is being filled 
                        currently */
    DIRECTION_MATRIX *tmp_dm;       /** Same as previous for dm */
    int i, j, k, tmp;

    s1p = seq_get_begin (s1);
    s2p = seq_get_begin (s2);
    tmp_dm = dm;
    tmp_mm = mm;
    upper_m = mm + s3_len;
    diag_m = mm;
    if (!NDEBUG) 
        printf ("Three dimensional sequence alignment matrix.\n");
    /* Fill the first plane only at the beginning, this is special */
    {
        mm[0] = 0;              /* Fill the first cell, of course to 0 */
        dm[0] = S2;

        /* Fill first row based on precalculated row.
         * The first row consists of aligning s3 with gaps, we have that
         * precalculated, so all we really need is to add up that vector.*/
        ggs3 = cm_get_row_precalc_3d (prec, s3_len, a_sz, gap, gap);
        for (i = 1; i < s3_len; i++) {
            mm[i] = mm[i - 1] + ggs3[i];
            dm[i] = SS;
        }

        prev_m = mm;
        mm += s3_len;
        dm += s3_len;

        /* Finish filling the first plane.
         * In this plane filling, all we really need to deal with is aligning s2
         * and s3, as the first plane holds the inital gap of s1. */
        for (i = 1; i < s2_len; i++, prev_m += s3_len, mm += s3_len, dm += s3_len) {
            gs2s3 = cm_get_row_precalc_3d (prec, s3_len, a_sz, gap, s2p[i]);

            /** Fill the first cell with the cost of extending the gap from the
             * previous row */
            mm[0] = prev_m[0] + gs2s3[0];
            dm[0] = P1;
            /* Everyone else requires the three comparisons we used when filling
             * rows as usual in a two dimensional alignment. Note that this code
             * is almost the same as algn_fill_row, so if something is modified
             * there, there will be need of modifying it in the same way here. */
            /* TODO: Add the ukkonen barriers */
            for (j = 1; j < s3_len; j++) {
                mm[j] = prev_m[j] + gs2s3[0];
                dm[j] = P1;
                tmp = prev_m[j - 1] + gs2s3[j];
                if (tmp < mm[j]) {
                    mm[j] = tmp;
                    dm[j] = S1;
                }
                tmp = mm[j - 1] + ggs3[j];
                if (tmp < mm[j]) {
                    mm[j] = tmp;
                    dm[j] = SS;
                }
            }
        }
        if (!NDEBUG && !NPRINT_CM) {  /* Printing the cost matrix */
            int *tmp_mm_debug;
            tmp_mm_debug = tmp_mm;
            printf ("\n");
            for (i = 0; i < s2_len; i++) {
                for (j = 0; j < s3_len; j++)
                    printf ("%d\t", tmp_mm_debug[j]);
                tmp_mm_debug += s2_len;
                printf ("\n");
            }
            printf ("\n");
        }
    } /* Finished the first plane filling */

    /* Fill plane by plane */
    mm = tmp_mm + (s3_len * s2_len);
    dm = tmp_dm + (s3_len * s2_len);
    diag_m = tmp_mm;
    upper_m = tmp_mm + s3_len;
    prev_m = mm - s3_len;
    for (i = 1; i < s1_len; i++) { /* For each plane */
        int s1_it; /* The element in s1 represented by this plane */
        s1_it = s1p[i];
        s1gs3 = cm_get_row_precalc_3d (prec, s3_len, a_sz, s1_it, gap);
        /* Filling the first row of the current plane. */
        {   
            /* This requires only three operations, equivalent to the
             2-dimensional alignment (the three for loops) */
            mm[0] = diag_m[0] + s1gs3[0]; /* diag is upper in this step */
            dm[0] = P3;
            if (!NDEBUG && !NPRINT_CM)
                printf ("%d\t", mm[0]);
            for (j = 1, k = 0; j < s3_len; j++, k++) {
                mm[j] = diag_m[j] + s1gs3[0];
                dm[j] = P3;
                tmp = diag_m[k] + s1gs3[j];
                if (tmp < mm[j]) {
                    mm[j] = tmp;
                    dm[j] = S3;
                }
                tmp = ggs3[j] + mm[k];
                if (tmp < mm[j]) {
                    mm[j] = tmp;
                    dm[j] = SS;
                }
                if (!NDEBUG && !NPRINT_CM) 
                    printf ("%d\t", mm[j]);
            }
            if (!NDEBUG)
                printf ("\n");
            /* Now we should move to the next row to continue filling the matrix
             * */
            dm += s3_len;
            mm += s3_len;
        }
        /* Now, go on with the rest of the rows.  On each row, mm is the row
         * being constructed, prev_m is the previous row in the same horizontal
         * plane, upper_m is the previous row in the vertical plane and diag_m
         * is the row in the previous planes (vertical and horizontal).
         */
        for (j = 1; j < s2_len; j++, diag_m += s3_len, 
                upper_m += s3_len, prev_m += s3_len, mm += s3_len, 
                dm += s3_len) {
            /* We first set the vectors that are needed */
            int s2_it;
            s2_it = s2p[j];
            gs2s3 = cm_get_row_precalc_3d (prec, s3_len, a_sz, gap, s2_it);
            s1s2s3 = cm_get_row_precalc_3d (prec, s3_len, a_sz, s1_it, s2_it);
            fill_parallel (s3_len, prev_m, upper_m, diag_m, s1gs3[0], gs2s3[0], 
                    s1s2s3[0], mm, dm);
            fill_moved (s3_len, prev_m - 1, upper_m - 1, diag_m - 1, s1gs3, \
                    gs2s3, s1s2s3, mm, dm);
            /* In the final step we run over the array filling the self check.
             * */
            if (!NDEBUG)
                printf ("%d\t", mm[0]);
            for (k = 1; k < s3_len; k++) {
                tmp = mm[k - 1] + ggs3[k];
                if (tmp < mm[k]) {
                    mm[k] = tmp;
                    dm[k] = SS;
                }
                if (!NDEBUG)
                    printf ("%d\t", mm[k]);
            }
            if (!NDEBUG)
                printf ("\n");
        }
        if (!NDEBUG)
            printf ("\n");
    }
    return (mm[-1]); /** We return the last item in the previous row */
}

/* Same as the previous function but with Ukkonen barriers turned on. The
 * additional parameters are:
 * @param w is the maximum width to be filled.
 * @param d is the maximum depth to be filled
 * @param h is the maximum height to be filled.
 * TODO: Finish this implementation, I will stop now to test the sequence
 * analysis procedures in the rest of POY.
 * */
#ifdef _WIN32
__inline int
#else
inline int
#endif
algn_fill_cube_ukk (const seqt s1, const seqt s2, const int *prec, \
        int s1_len, int s2_len, int s3_len, int *mm, DIRECTION_MATRIX *dm, int uk, \
        int gap, int a_sz, int w, int d, int h) {
    SEQT *s1p, *s2p;
    /* Each of the following arrays hold some precalculated value for the
     * sequence s3 which is not passed as argument. */
    const int *gs2s3;     /** Align a gap and the current base of s2 with s3 */
    const int *s1gs3;     /** Align the current base of s1 and a gap with s3 */
    const int *s1s2s3;    /** Align the current bases of s1 and s2 with s3 */
    const int *ggs3;      /** Align two gaps with s3 */
    /* Each of the following arrays hold the arrays of the three dimensional
     * matrix that are located around the row that is filled on each iteration.
     * These rows are already filled in some previous iteration and used in 
     * the dynamic programming matrix. */
    int *upper_m;       /** The row in the upper plane of the cube */
    int *prev_m;        /** The row behind the current row in the same plane */
    int *diag_m;        /** The upper_m relative to prev_m */
    int *tmp_mm;        /** A temporary pointer to the row that is being filled 
                        currently */
    DIRECTION_MATRIX *tmp_dm;       /** Same as previous for dm */
    int i, j, k, tmp;

    s1p = seq_get_begin (s1);
    s2p = seq_get_begin (s2);
    tmp_dm = dm;
    tmp_mm = mm;
    upper_m = mm + s3_len;
    diag_m = mm;
    if (!NDEBUG) 
        printf ("Three dimensional sequence alignment matrix.\n");
    /* Fill the first plane only at the beginning, this is special */
    {
        mm[0] = 0;              /* Fill the first cell, of course to 0 */
        dm[0] = S2;

        /* Fill first row based on precalculated row.
         * The first row consists of aligning s3 with gaps, we have that
         * precalculated, so all we really need is to add up that vector.*/
        ggs3 = cm_get_row_precalc_3d (prec, s3_len, a_sz, gap, gap);
        for (i = 1; i < s3_len; i++) {
            mm[i] = mm[i - 1] + ggs3[i];
            dm[i] = SS;
        }

        prev_m = mm;
        mm += s3_len;
        dm += s3_len;

        /* Finish filling the first plane.
         * In this plane filling, all we really need to deal with is aligning s2
         * and s3, as the first plane holds the inital gap of s1. */
        for (i = 1; i < s2_len; i++, prev_m += s3_len, mm += s3_len, dm += s3_len) {
            gs2s3 = cm_get_row_precalc_3d (prec, s3_len, a_sz, gap, s2p[i]);

            /** Fill the first cell with the cost of extending the gap from the
             * previous row */
            mm[0] = prev_m[0] + gs2s3[0];
            dm[0] = P1;
            /* Everyone else requires the three comparisons we used when filling
             * rows as usual in a two dimensional alignment. Note that this code
             * is almost the same as algn_fill_row, so if something is modified
             * there, there will be need of modifying it in the same way here. */
            /* TODO: Add the ukkonen barriers */
            for (j = 1; j < s3_len; j++) {
                mm[j] = prev_m[j] + gs2s3[0];
                dm[j] = P1;
                tmp = prev_m[j - 1] + gs2s3[j];
                if (tmp < mm[j]) {
                    mm[j] = tmp;
                    dm[j] = S1;
                }
                tmp = mm[j - 1] + ggs3[j];
                if (tmp < mm[j]) {
                    mm[j] = tmp;
                    dm[j] = SS;
                }
            }
        }
        if (!NDEBUG) {  /* Printing the cost matrix */
            int *tmp_mm_debug;
            tmp_mm_debug = tmp_mm;
            for (i = 0; i < s2_len; i++) {
                for (j = 0; j < s3_len; j++)
                    printf ("%d\t", tmp_mm_debug[j]);
                tmp_mm_debug += s2_len;
                printf ("\n");
            }
            printf ("\n");
        }
    } /* Finished the first plane filling */

    /* Fill plane by plane */
    mm = tmp_mm + (s3_len * s2_len);
    dm = tmp_dm + (s3_len * s2_len);
    diag_m = tmp_mm;
    upper_m = tmp_mm + s3_len;
    prev_m = mm - s3_len;
    for (i = 1; i < s1_len; i++) { /* For each plane */
        int s1_it; /* The element in s1 represented by this plane */
        s1_it = s1p[i];
        s1gs3 = cm_get_row_precalc_3d (prec, s3_len, a_sz, s1_it, gap);
        /* Filling the first row of the current plane. */
        {   
            /* This requires only three operations, equivalent to the
             2-dimensional alignment (the three for loops) */
            mm[0] = diag_m[0] + s1gs3[0]; /* diag is upper in this step */
            dm[0] = P3;
            if (!NDEBUG)
                printf ("%d\t", mm[0]);
            for (j = 1, k = 0; j < s3_len; j++, k++) {
                mm[j] = diag_m[j] + s1gs3[0];
                dm[j] = P3;
                tmp = diag_m[k] + s1gs3[j];
                if (tmp < mm[j]) {
                    mm[j] = tmp;
                    dm[j] = S3;
                }
                tmp = ggs3[j] + mm[k];
                if (tmp < mm[j]) {
                    mm[j] = tmp;
                    dm[j] = SS;
                }
                if (!NDEBUG) 
                    printf ("%d\t", mm[j]);
            }
            if (!NDEBUG)
                printf ("\n");
            /* Now we should move to the next row to continue filling the matrix
             * */
            dm += s3_len;
            mm += s3_len;
        }
        /* Now, go on with the rest of the rows.  On each row, mm is the row
         * being constructed, prev_m is the previous row in the same horizontal
         * plane, upper_m is the previous row in the vertical plane and diag_m
         * is the row in the previous planes (vertical and horizontal).
         */
        for (j = 1; j < s2_len; j++, diag_m += s3_len, 
                upper_m += s3_len, prev_m += s3_len, mm += s3_len, 
                dm += s3_len) {
            /* We first set the vectors that are needed */
            int s2_it;
            s2_it = s2p[j];
            gs2s3 = cm_get_row_precalc_3d (prec, s3_len, a_sz, gap, s2_it);
            s1s2s3 = cm_get_row_precalc_3d (prec, s3_len, a_sz, s1_it, s2_it);
            fill_parallel (s3_len, prev_m, upper_m, diag_m, s1gs3[0], gs2s3[0], 
                    s1s2s3[0], mm, dm);
            fill_moved (s3_len, prev_m - 1, upper_m - 1, diag_m - 1, s1gs3, \
                    gs2s3, s1s2s3, mm, dm);
            /* In the final step we run over the array filling the self check.
             * */
            if (!NDEBUG)
                printf ("%d\t", mm[0]);
            for (k = 1; k < s3_len; k++) {
                tmp = mm[k - 1] + ggs3[k];
                if (tmp < mm[k]) {
                    mm[k] = tmp;
                    dm[k] = SS;
                }
                if (!NDEBUG)
                    printf ("%d\t", mm[k]);
            }
            if (!NDEBUG)
                printf ("\n");
        }
        if (!NDEBUG)
            printf ("\n");
    }
    return (mm[-1]); /** We return the last item in the previous row */
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
algn_nw_limit (const seqt s1, const seqt s2, const cmt c, matricest m,
                int deltawh, int st_s1, int len_s1, int st_s2, int len_s2)
{
    int *prec, s1_len, s2_len;
    s1_len = seq_get_len (s1);
    s2_len = seq_get_len (s2);
    cm_precalc_4algn (c, m, s2);
    prec = mat_get_2d_prec (m);
    assert( !(cm_get_affine_flag (c)) );
    return (algn_fill_plane_2 (s1, prec, s1_len, s2_len, m , c, 50, (len_s1-len_s2)+50, deltawh));
}


#ifdef _WIN32
__inline int
#else
inline int
#endif
algn_nw (const seqt s1, const seqt s2, const cmt c,matricest m, int deltawh) {
    return (algn_nw_limit (s1, s2, c, m, deltawh, 0, seq_get_len(s1), 0, seq_get_len(s2)));
}

int
algn_nw_3d (const seqt s1, const seqt s2, const seqt s3, const cm_3dt c, matricest m, int w) {
    int *mm, *prec, s1_len, s2_len, s3_len, gap, res;
    int uselevel;
    DIRECTION_MATRIX *dm;
     uselevel = cm_check_level_3d(c);
    if(uselevel==1){
         mat_setup_size (m, seq_get_len (s2), seq_get_len (s3), seq_get_len (s1), w, cm_get_map_sz_3d(c),uselevel);
    } else {
        mat_setup_size (m, seq_get_len (s2), seq_get_len (s3), seq_get_len (s1), w, c->lcm,0);
    }
    mm = mat_get_3d_matrix (m);
    dm = mat_get_3d_direct (m);
    prec = mat_get_3d_prec (m);
    s1_len = seq_get_len (s1);
    s2_len = seq_get_len (s2);
    s3_len = seq_get_len (s3);
    gap = cm_get_gap_3d (c);
    cm_precalc_4algn_3d (c, prec, s3);
    /* TODO Check how is this ukkonen barrier affecting this fill cube, the w
     * was called uk */
    res = algn_fill_cube (s1, s2, prec, s1_len, s2_len, s3_len, mm, dm, w, \
            gap, c->a_sz);
    return res;
}

int
algn_calculate_from_2_aligned (seqt s1, seqt s2, cmt c, int *matrix, int uselevel) {
    int i, res = 0, gap_opening, gap_row = 0; int gap_startNO=0;
    SEQT gap, s1b, s2b;
    gap = cm_get_gap (c);
    gap_startNO = cm_get_gap_startNO(c);
    /* We initialize i to the proper location */
    s1b = seq_get (s1, 0);
    s2b = seq_get (s2, 0);
    if (
        ((uselevel==0) && c->combinations && (gap & s1b) && (gap & s2b)) ||
        ((uselevel==0) && !c->combinations && (gap == s1b) && (gap == s2b)) ||
        ((uselevel==1) && (s1b>=gap_startNO) && (s2b>=gap_startNO))
        ){
        i = 1;
    } else{
        i = 0;
    }
    gap_opening = cm_get_gap_opening_parameter (c);
    assert ((seq_get_len (s1)) == (seq_get_len (s2)));
    for (; i < seq_get_len (s1); i++) {
        s1b = seq_get (s1, i);
        s2b = seq_get (s2, i);
        if (0 == gap_row) { /* We have no gaps */
            if (
                    ((uselevel==0) && c->combinations && (s1b & gap) && !(s2b & gap)) || 
                    ((uselevel==0) && (!c->combinations) && (s1b == gap)) ||
                    ((uselevel==1) && (s1b>=gap_startNO) && (s2b<gap_startNO))
                )
            {
                res += gap_opening;
                gap_row = 1;
            } else if 
                (
                 ((uselevel==0) && c->combinations && (s2b & gap) && !(s1b & gap)) || 
                 ((uselevel==0) && (!c->combinations) && (s2b == gap)) ||
                 ( (uselevel==1) && (s2b>=gap_startNO) )
                ) {
                res += gap_opening;
                gap_row = 2;
            }
        }
        else if (1 == gap_row) { /* We are in s1's block of gaps */
            if (
                    ((uselevel==0) && c->combinations && !(s1b & gap)) || 
                    ((uselevel==0) && (!c->combinations) && (s1b != gap)) ||
                    ((uselevel==1) && (s1b<gap_startNO))
                ) 
            {
                if (
                    ((uselevel==0) && c->combinations && (s2b & gap) && !(s1b & gap)) || 
                    ((uselevel==0) && (!c->combinations) && (s2b == gap)) ||
                    ((uselevel==1) && (s1b<gap_startNO) && (s2b>=gap_startNO))
                    ) 
                {
                    res += gap_opening;
                    gap_row = 2;
                }
                else gap_row = 0;
            }
        } 
        else { /* We are in s2's block of gaps */
            assert (2 == gap_row);
            if (
                ((uselevel==0) && c->combinations && !(s2b & gap)) || 
                ((uselevel==0) && (!c->combinations) && (s2b != gap)) ||
                ( (uselevel==1) && (s2b<gap_startNO) )
                )
            {
                if (
                        ((uselevel==0) && (c->combinations && (s1b & gap))) || 
                        ((uselevel==0) && (!c->combinations) && (s1b == gap)) ||
                        ((uselevel==1) && (s1b>=gap_startNO))
                    ) 
                {
                    res += gap_opening;
                    gap_row = 1;
                }
                else gap_row = 0;
            }
        }
        if(uselevel == 1)
            res += (cm_get_cost(matrix, seq_get (s1, i), seq_get (s2, i), c->map_sz+1));
        else
            res += (cm_calc_cost (matrix, seq_get (s1, i), seq_get (s2, i), c->lcm));
    }
    return (res);
}

int
algn_worst_2 (seqt s1, seqt s2, cmt c) {
    if(cm_check_level(c)==1)     
        return (algn_calculate_from_2_aligned (s1, s2, c, c->worst, 1));
    else
        return (algn_calculate_from_2_aligned (s1, s2, c, c->worst, 0));
}

int
algn_verify_2 (seqt s1, seqt s2, cmt c) {
    if(cm_check_level(c)==1)     
        return (algn_calculate_from_2_aligned (s1, s2, c, c->cost, 1));
    else
        return (algn_calculate_from_2_aligned (s1, s2, c, c->cost, 0));
}

value
algn_CAML_worst_2 (value s1, value s2, value c) {
    CAMLparam3(s1, s2, c);
    cmt tc;
    int res;
    seqt s1p, s2p;
    Seq_custom_val(s1p,s1);
    Seq_custom_val(s2p,s2);
    tc = Cost_matrix_struct(c);
    res = algn_worst_2 (s1p, s2p, tc);
    CAMLreturn(Val_int(res));
}

value 
algn_CAML_verify_2 (value s1, value s2, value c) {
    CAMLparam3(s1, s2, c);
    cmt tc;
    int res;
    seqt s1p, s2p;
    Seq_custom_val(s1p,s1);
    Seq_custom_val(s2p,s2);
    tc = Cost_matrix_struct(c);
    res = algn_verify_2 (s1p, s2p, tc);
    CAMLreturn(Val_int(res));
}

value 
algn_CAML_simple_2 (value s1, value s2, value c, value a, value deltawh) {
    CAMLparam5(s1, s2, c, a, deltawh);
    seqt s1p, s2p;
    int res;
    cmt tc;
    int uselevel;
    matricest ta;
    tc = Cost_matrix_struct(c);
    ta = Matrices_struct(a);
    Seq_custom_val(s1p,s1);
    Seq_custom_val(s2p,s2);
    uselevel = cm_check_level(tc);
    if(uselevel==1) 
         mat_setup_size (ta, seq_get_len(s1p), seq_get_len(s2p), 0, 0, \
            cm_get_map_sz(tc),uselevel);
    else
        mat_setup_size (ta, seq_get_len(s1p), seq_get_len(s2p), 0, 0, \
            cm_get_lcm(tc),0);
#ifdef DEBUG_ALL_ASSERTIONS
    _algn_max_matrix = ta->matrix + ta->len_eff;
    _algn_max_direction = ta->matrix_d + ta->len;
#endif
    res = algn_nw (s1p, s2p, tc, ta, Int_val(deltawh));
    CAMLreturn(Val_int(res));
}

value 
algn_CAML_limit_2 (value s1, value s2, value c, value a, value w, value h,
        value s1_st, value s2_st, value s1_len, value s2_len) {
    CAMLparam5(s1, s2, c, a, w);
    CAMLxparam5(h, s1_st, s2_st, s1_len, s2_len);
    seqt s1p, s2p;
    int res, uselevel;
    cmt tc;
    matricest ta;
    //cw = Int_val(w);
    tc = Cost_matrix_struct(c);
    ta = Matrices_struct(a);
    Seq_custom_val(s1p,s1);
    Seq_custom_val(s2p,s2);
    uselevel = cm_check_level(tc);
    if(uselevel==1)
        mat_setup_size (ta, seq_get_len(s1p), seq_get_len(s2p), 0, 0, \
            cm_get_map_sz(tc),uselevel);
    else
        mat_setup_size (ta, seq_get_len(s1p), seq_get_len(s2p), 0, 0, \
            cm_get_lcm(tc),0);
    /* TODO: Fix this deltaw binding */
    res = algn_nw_limit (s1p, s2p, tc, ta, Int_val(w), 
            Int_val(s1_st), Int_val(s1_len), Int_val(s2_st), Int_val(s2_len));
    CAMLreturn(Val_int(res));
}

value 
algn_CAML_limit_2_bc (value *argv, int argn) {
    return (algn_CAML_limit_2 (argv[0], argv[1], argv[2], argv[3], argv[4], \
                argv[5], argv[6], argv[7], argv[8], argv[9]));
}

value
algn_CAML_simple_3 (value s1, value s2, value s3, value c, value a, value uk) {
    CAMLparam5(s1, s2, s3, c, a);
    CAMLxparam1(uk);
    seqt s1p, s2p, s3p;
    int res;
    cm_3dt tc;
    matricest ta;
    tc = Cost_matrix_struct_3d(c);
    ta = Matrices_struct(a);
    Seq_custom_val(s1p,s1);
    Seq_custom_val(s2p,s2);
    Seq_custom_val(s3p,s3);
    res = algn_nw_3d (s1p, s2p, s3p, tc, ta, Int_val(uk));
    CAMLreturn(Val_int(res));
}

value
algn_CAML_simple_3_bc (value *argv, int argn) {
    return (algn_CAML_simple_3 (argv[0], argv[1], argv[2], argv[3], argv[4], \
                argv[5]));
}

void
print_bcktrck (const seqt s1, const seqt s2, \
        const matricest m) {
    int i, j;
    DIRECTION_MATRIX *d;
    d = mat_get_2d_direct (m);
    printf ("\n");
    for (i = 0; i < seq_get_len (s1); i++) {
        for (j = 0; j < seq_get_len (s2); j++) {
            printf ("%d", (int) *(d + j));
            fflush (stdout);
        }
        d += j;
        printf ("\n");
        fflush (stdout);
    }
    printf ("\n\n");
    fflush (stdout);
    return;
}

value
algn_CAML_print_bcktrck (value s1, value s2, value matrix) {
    CAMLparam3(s1, s2, matrix);
    seqt s1c, s2c;
    matricest m;
    Seq_custom_val(s1c,s1);
    Seq_custom_val(s2c,s2);
    m = Matrices_struct(matrix);
    print_bcktrck (s1c, s2c, m);
    CAMLreturn (Val_unit);
}

void
print_dynmtrx (const seqt s1, const seqt s2, \
        matricest m) {
    int i, j;
    int *d;
    d = mat_get_2d_matrix (m);
    for (i = 0; i < seq_get_len (s1); i++) {
        for (j = 0; j < seq_get_len (s2); j++) 
            printf ("%d", (int) *(d + j));
        d += j;
        printf ("\n");
    }
    return;
}

void
algn_string_of_2d_direction (DIRECTION_MATRIX v) {
    if (v & ALIGN) printf ("A");
    if (v & DELETE) printf ("D");
    if (v & INSERT) printf ("I");
    if (v & ALIGN_V) printf ("VA");
    if (v & DELETE_V) printf ("VD");
    if (v & ALIGN_H) printf ("HA");
    if (v & INSERT_H) printf ("HI");
    return;
}


void
backtrace_2d (const seqt s1, const seqt s2, seqt r1, seqt r2, const matricest m,
        const cmt c, int st_s1, int st_s2, int algn_s1, int algn_s2, int swaped,
        value a, value b)
{
    int l;
    DIRECTION_MATRIX *beg, *end;

    l = seq_get_len (s2);
    beg = st_s2 + mat_get_2d_direct (m);
    end = beg + (l * (algn_s1 - 1)) + algn_s2 - 1;
    algn_s1 = algn_s1 + st_s1;
    algn_s2 = algn_s2 + st_s2;

    /* The following pair of while loops are the same lines of code, each 
     * has swaped INSERT and DELETE procedures, so that depending on the ordering 
     * of the two sequences (swap) either INSERTING or DELETING will be preferred. 
     * During the downpass and all the optimization procedures, keeping the same
     * ordering for the medians output is important to keep consistency in the
     * diagnosis at every step. In other words, if a join is performed starting
     * in any position of the tree, it is necessary to make sure that the very
     * same median would be produced if the calculation started in any of it's
     * children. 
     * Note that this two lines could be defined as macros, but we (I?) have
     * decided not to do so to keep it readable. Besides, once correct, there is
     * nothing (or very few things) to do here. */

    //why we are checking affine_flag here? we have whole other set of function
    //dealing with affine alignment and backtrace, we should never use the
    //"else" part of this "if"
    assert(!(cm_get_affine_flag (c)));
    /* Please note that in sequence.ml, function [create_edited_2] 
     * pass the longer seq as seq1.
     * I know it's different in affine alignment(algn_fill_plane_3_aff), 
     * or alignment from newkkonen ( newkkonen.c), 
     * or the floating version from likelihood (according to nic). 
     * as a result, INSERT here is acctually DELETE in those three. 
     */
    while (end >= beg) {
        if (has_align(*end)) {
            algn_s1--;
            my_prepend(r1,my_get(s1,algn_s1));
            algn_s2--;
            my_prepend(r2, my_get(s2,algn_s2) );
            end -= l + 1;
        } else {
            follow_insertion_or_deletion(&end,swaped,l,c,r1,r2,s1,s2,&algn_s1,&algn_s2);
        }
    }
    return;
}

char *
algn_string_of_3d_direction (char v) {
    if (v & S2) return "S2";
    else if (v & S3) return "S3";
    else if (v & S1) return "S1";
    else if (v & P3) return "P3";
    else if (v & SS) return "SS";
    else if (v & P1) return "P1";
    else if (v & P2) return "P2";
    else {
        assert (0);
    }
    return "Empty";
}

void
backtrack_3d (const seqt s1, const seqt s2, seqt s3, seqt r1, seqt r2, seqt r3,
                matricest m, const cm_3dt c)
{
    int l, l1, l2, l3;
    int a_plane, a_line, a_cell = 1;
    DIRECTION_MATRIX *beg, *end;
    l1 = seq_get_len (s1);
    l2 = seq_get_len (s2);
    l3 = seq_get_len (s3);
    l = l1 * l2 * l3;
    a_plane = l2 * l3;
    a_line = l3;
    beg = mat_get_3d_direct (m);
    if (!NDEBUG) {
        char *toprint;
        DIRECTION_MATRIX *beg_debug;
        int i, j, k;
        beg_debug = beg;
        printf ("Printing a three dimensional direction matrix.\n");
        for (i = 0; i < l1; i++) {
            for (j  = 0; j < l2; j++) {
                for (k = 0 ; k < l3; k++, beg_debug++) {
                    toprint = algn_string_of_3d_direction (*beg_debug);
                    printf ("%s\t", toprint);
                }
                printf ("\n");
            }
            printf ("\n");
        }
    }
    end = beg + l - 1;
    l1--;
    l2--;
    l3--;
    while (end > beg) {
        if (*end & S2) {        /* A plane, line, and cell */
            seq_prepend (r1, seq_get (s1, l1--));
            seq_prepend (r2, seq_get (s2, l2--));
            seq_prepend (r3, seq_get (s3, l3--));
            end -= a_plane + a_line + a_cell;
        } else if (*end & S3) { /* A plane and cell */
            seq_prepend (r1, seq_get (s1, l1--));
            seq_prepend (r2, cm_get_gap_3d (c));
            seq_prepend (r3, seq_get (s3, l3--));
            end -= a_plane + a_cell;
        } else if (*end & S1) { /* A line and cell */
            seq_prepend (r1, cm_get_gap_3d (c));
            seq_prepend (r2, seq_get (s2, l2--));
            seq_prepend (r3, seq_get (s3, l3--));
            end -= a_line + a_cell;
        } else if (*end & P3) { /* A plane */
            seq_prepend (r1, seq_get (s1, l1--));
            seq_prepend (r2, cm_get_gap_3d (c));
            seq_prepend (r3, cm_get_gap_3d (c));
            end -= a_plane;
        } else if (*end & SS) { /* A cell */
            seq_prepend (r1, cm_get_gap_3d (c));
            seq_prepend (r2, cm_get_gap_3d (c));
            seq_prepend (r3, seq_get (s3, l3--));
            end -= a_cell;
        } else if (*end & P1) { /* A line */
            seq_prepend (r1, cm_get_gap_3d (c));
            seq_prepend (r2, seq_get (s2, l2--));
            seq_prepend (r3, cm_get_gap_3d (c));
            end -= a_line;
        } else if (*end & P2) { /* A plane and line */
            seq_prepend (r1, seq_get (s1, l1--));
            seq_prepend (r2, seq_get (s2, l2--));
            seq_prepend (r3, cm_get_gap_3d (c));
            end -= a_plane + a_line;
        }
        else {
            assert (0);
        }
    }
    return;
}

value
algn_CAML_backtrace_2d (value s1, value s2, value s1p, value s2p, value a, value c, value swap)
{
    CAMLparam5(s1, s2, s1p, s2p, a);
    CAMLxparam2(c, swap);
    seqt ss1, ss2, ss1p, ss2p;
    matricest ta;
    cmt cc;
    ta = Matrices_struct(a);
    Seq_custom_val(ss1,s1);
    Seq_custom_val(ss2,s2);
    Seq_custom_val(ss1p,s1p);
    Seq_custom_val(ss2p,s2p);
    cc = Cost_matrix_struct(c);
    backtrace_2d (ss1, ss2, ss1p, ss2p, ta, cc, 0, 0, seq_get_len(ss1), \
            seq_get_len(ss2), Bool_val(swap), s1, s2);
    CAMLreturn(Val_unit);
}

value 
algn_CAML_backtrace_2d_bc (value *argv, int argn) {
    return (algn_CAML_backtrace_2d (argv[0], argv[1], argv[2], argv[3], \
                argv[4], argv[5], argv[6]));
}

value 
algn_CAML_backtrace_2d_limit (value s1, value s2, value s1p, \
        value s2p, value a, value c, value st_s1, value st_s2, \
        value algn_s1, value algn_s2, value swaped) {
    CAMLparam5 (s1, s2, s1p, s2p, a);
    CAMLxparam5 (c, st_s1, st_s2, algn_s1, algn_s2);
    CAMLxparam1 (swaped);
    seqt ss1, ss2, ss1p, ss2p;
    matricest ta;
    cmt cc;
    ta = Matrices_struct(a);
    Seq_custom_val(ss1,s1);
    Seq_custom_val(ss2,s2);
    Seq_custom_val(ss1p,s1p);
    Seq_custom_val(ss2p,s2p);
    cc = Cost_matrix_struct(c);
    backtrace_2d (ss1, ss2, ss1p, ss2p, ta, cc, Int_val(st_s1), \
            Int_val(st_s2), Int_val(algn_s1), Int_val(algn_s2), 
            Bool_val(swaped), s1, s2);
    CAMLreturn (Val_unit);
}

value 
algn_CAML_backtrace_2d_limit_bc (value *argv, int argn) {
    return (algn_CAML_backtrace_2d_limit (argv[0], argv[1], argv[2], argv[3], \
                argv[4], argv[5], argv[6], argv[7], argv[8], argv[9], argv[10]));
}

value
algn_CAML_backtrack_3d (value s1, value s2, value s3, value s1p, value s2p, \
        value s3p, value a, value c) {
    CAMLparam5(s1, s2, s1p, s2p, a);
    CAMLxparam2(s3, s3p);
    seqt ss1, ss2, ss3, ss1p, ss2p, ss3p;
    matricest ta;
    cm_3dt tc;
    ta = Matrices_struct(a);
    Seq_custom_val(ss1,s1);
    Seq_custom_val(ss2,s2);
    Seq_custom_val(ss3,s3);
    Seq_custom_val(ss1p,s1p);
    Seq_custom_val(ss2p,s2p);
    Seq_custom_val(ss3p,s3p);
    tc = Cost_matrix_struct_3d(c);
    backtrack_3d (ss1, ss2, ss3, ss1p, ss2p, ss3p, ta, tc);
    CAMLreturn(Val_unit);
}

value
algn_CAML_backtrack_3d_bc (value *argv, int argc) {
    return (algn_CAML_backtrack_3d (argv[0], argv[1], argv[2], argv[3], \
                argv[4], argv[5], argv[6], argv[7]));
}

value 
algn_CAML_align_2d (value s1, value s2, value c, value a, value s1p, \
        value s2p, value deltawh, value swaped)
{
    CAMLparam5(s1, s2, c, a, s1p);
    CAMLxparam3(s2p, deltawh, swaped);
    CAMLlocal1(res);
    res = algn_CAML_simple_2 (s1, s2, c, a, deltawh);
    algn_CAML_backtrace_2d (s1, s2, s1p, s2p, a, c, swaped);
    CAMLreturn(res);
}

value
algn_CAML_align_2d_bc (value *argv, int argn) {
    return (algn_CAML_align_2d (argv[0], argv[1], argv[2], argv[3], argv[4], \
                argv[5], argv[6], argv[7])); 
}

value 
algn_CAML_align_3d (value s1, value s2, value s3, value c, value a, \
        value s1p, value s2p, value s3p, value uk) {
    CAMLlocal1(res);
    res = algn_CAML_simple_3 (s1, s2, s3, c, a, uk);
    algn_CAML_backtrack_3d (s1, s2, s3, s1p, s2p, s3p, a, c);
    return (res);
}

value
algn_CAML_align_3d_bc (value *argv, int argn) {
    return (algn_CAML_align_3d (argv[0], argv[1], argv[2], argv[3], argv[4], \
                argv[5], argv[6], argv[7], argv[8])); 
}


void
algn_remove_gaps (int gap, seqt s) {
    int i, len;
    len = seq_get_len (s);
    SEQT *source, *destination;
    int newlen = 0; 
    source = destination = s->end;
    for (i = len - 1; i >= 0; i--) {
        if (gap != *source) {
            *destination = *source;
            destination--;
            newlen++;
        }
        source--;
    }
    s->len = newlen;
    s->begin = destination + 1;
    /* We restore the leading gap */
    seq_prepend (s, gap);
    return;
}

void
algn_correct_blocks_affine (int gap, seqt s, seqt a, seqt b) {
    int i, len, ab, bb, sb, extending_gap, 
        inside_block = 0, prev_block = 0;
    len = seq_get_len (s);
    extending_gap = 0;
    inside_block = 0;
    for (i = 0; i < len; i++) {
        ab = seq_get (a, i);
        bb = seq_get (b, i);
        sb = seq_get (s, i);
        if (i > 2) {

        }
        if (!inside_block && (!(ab & gap) || !(bb & gap)))
            inside_block = 0;
        else if (inside_block && ((!(ab & gap)) || (!(bb & gap))))
            inside_block = 0;
        else if (((ab & gap) || (bb & gap)) && ((ab != gap) || (bb != gap)))
            inside_block = 1;
        else 
            inside_block = 0;
        if (((gap & ab) || (gap & bb)) && (!(sb & gap)) && !extending_gap) {
            prev_block = inside_block;
            extending_gap = 1;
        }
        else if ((gap & ab) && (gap & bb) && (sb & gap) && (sb != gap) && 
                extending_gap && inside_block && !prev_block) {
            sb = (~gap) & sb;
            prev_block = 0;
        }
        else if ((gap & ab) && (gap & bb) && (1 == extending_gap)) {
            prev_block = inside_block;
            extending_gap = 0;
        }

        seq_set (s, i, sb);
    }
    algn_remove_gaps (gap, s);
    return;
}

void
algn_ancestor_2 (seqt s1, seqt s2, cmt m, seqt sm ) {
    SEQT *begin1, *begin2;
    int interm;
    int i, gap, is_combinations, cost_model;
    begin1 = seq_get_begin (s1);
    begin2 = seq_get_begin (s2);
    gap = cm_get_gap (m);
    is_combinations = m->combinations;
    cost_model = m->cost_model_type;
    for (i = seq_get_len (s1) - 1; i >= 0; i--){
        interm = cm_get_median (m, begin1[i], begin2[i]);
        if(interm==0)
            failwith("median should not be 0\n");
        if ((!is_combinations) || (1 != cost_model)) {
            if (interm != gap) seq_prepend (sm, interm);
        }
        else seq_prepend (sm, interm);
    }
    if ((!is_combinations) || ((1 != cost_model) && (gap != seq_get (sm, 0))))
        seq_prepend (sm, gap);
    else if (is_combinations)
        algn_correct_blocks_affine (gap, sm, s1, s2);
    return;
}



/* 
 * Given three aligned sequences s1, s2, and s3, the median between them is
 * returned in the sequence sm, using the cost matrix stored in m.
 */
#ifdef _WIN32
__inline void
#else
inline void
#endif
algn_get_median_3d (seqt s1, seqt s2, seqt s3, \
        cm_3dt m, seqt sm) {
    SEQT *end1, *end2, *end3;
    int interm;
    int i;
    end1 = seq_get_end (s1);
    end2 = seq_get_end (s2);
    end3 = seq_get_end (s3);
    for (i = seq_get_len (s1) - 1; i >= 0; i--) {
        interm = cm_get_median_3d (m, *end1, *end2, *end3);
        seq_prepend (sm, interm);
    }
    return;
}

void
algn_union (seqt s1, seqt s2, seqt su) {
    assert (seq_get_len (s1) == seq_get_len (s2));
    assert (seq_get_cap (s1) >= seq_get_len (s2));
    int len, i;
    len = seq_get_len (s1);
    for (i = len - 1; i >= 0; i--)
        seq_prepend (su, (seq_get (s1, i) | seq_get (s2, i)));
    return;
}

value 
algn_CAML_union (value s1, value s2, value su) {
    CAMLparam3(s1, s2, su);
    seqt ss1, ss2, ssu;
    Seq_custom_val(ss1,s1);
    Seq_custom_val(ss2,s2);
    Seq_custom_val(ssu,su);
    algn_union (ss1, ss2, ssu);
    CAMLreturn(Val_unit);
}


value 
algn_CAML_median_3 (value s1, value s2, value s3, value m, value sm) {
    CAMLparam5(s1, s2, s3, m, sm);
    seqt ss1, ss2, ss3, ssm;
    cm_3dt tm;
    Seq_custom_val(ss1,s1);
    Seq_custom_val(ss2,s2);
    Seq_custom_val(ss3,s3);
    Seq_custom_val(ssm,sm);
    tm = Cost_matrix_struct_3d(m);
    algn_get_median_3d (ss1, ss2, ss3, tm, ssm);
    CAMLreturn(Val_unit);
}

/* Alignment following the algorithm of Myers , 1986. */
static zarrt v = NULL;

int
algn_myers (zarrt v, seqt a, seqt b, int max) {
    int d, kp1, km1, la, lb, k, x, y;
    la = seq_get_len (a) - 1;
    lb = seq_get_len (b) - 1;
    if (zarr_clear (v, max)) {
        for (d = 0; d <= max; d++) {
            for (k = -d; k <= d; k+=2) {
                zarr_get (v, k + 1, &kp1);
                zarr_get (v, k - 1, &km1);
                if ((k == -d) || ((k != d) && (km1 < kp1))) x = kp1;
                else x = km1 + 1;
                y = x - k;

                while ((x < la) && (y < lb) &&
                        (seq_get (a, x + 1) == seq_get (b, y + 1))) {
                    x++;
                    y++;
                }
                zarr_set (v, k, x);
                if ((x >= la) && (y >= lb)) return d;
            }
        }
    }
    return -1;
}

value algn_CAML_myers (value sa, value sb) {
    CAMLparam0();
    seqt a, b;
    int res, max;
    Seq_custom_val(a,sa);
    Seq_custom_val(b,sb);
    max = seq_get_len (a) + seq_get_len (b); 
    if (max > zarr_length (v)) {
        if ((v != NULL) && (zarr_realloc (v, max))) failwith ("Allocation error.");
        else {
            v = zarr_alloc (max);
            if (v == NULL) failwith ("Allocation error.");
        }
    }
    res = algn_myers (v, a, b, max);
    CAMLreturn (Val_int (res));
}

value 
algn_CAML_ancestor_2 (value sa, value sb, value cm, value sab) {
    CAMLparam4(sa, sb, cm, sab);
    seqt a, b, ab;
    cmt tm;
    Seq_custom_val(a,sa);
    Seq_custom_val(b,sb);
    Seq_custom_val(ab,sab);
    tm = Cost_matrix_struct(cm);
    algn_ancestor_2 (a, b, tm, ab);
    CAMLreturn (Val_unit);
}

void
algn_copy_backtrack (int s1, int s2, const matricest m, value res) {
    value row;
    int i, j;
    DIRECTION_MATRIX *d;
    d = mat_get_2d_direct (m);
    for (i = 0; i < s1; i++) {
        row = Field(res, i);
        for (j = 0; j < s2; j++) {
            Store_field(row, j, Val_int (*(d + j)));
        }
        d += j;
    }
    return;
}

value
algn_CAML_create_backtrack (value lsa, value lsb, value scm, value sres) {
    CAMLparam4(lsa, lsb, scm, sres);
    int la, lb;
    matricest m;

    la = Int_val(lsa);
    lb = Int_val(lsb);
    m = Matrices_struct(scm);
    algn_copy_backtrack (la, lb, m, sres);
    CAMLreturn(Val_unit);
}

#include "union.c"
