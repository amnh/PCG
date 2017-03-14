/* POY 4.0 Beta. A phylogenetic analysis program using Dynamic Homologies.    */
/* Copyright (C) 2007  Andrés Varón, Le Sy Vinh, Illya Bomash, Ward Wheeler,  */
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

/******************************************************************************/
/*                        Pairwise Standard Alignment                         */
/******************************************************************************/
/*
 * As standard, all the caml binding functions are called algn_CAML_<function
 * name>
 */

                                                  /**************** IMPORTANT!!! ****************/

/**************** Note that all included 3d funtions are beta and unused. Instead UkkCommon code, due to Powell, is used. ****************/

/** Fill a row in a two dimentional alignment
 *
 *  When pairwise alignment is performed, two sequences are compared over a
 *  transformation cost matrix. Let's call them sequences x and y, written over
 *  some alphabet a of length |a|. Each base of x
 *  is represented by a column in the transformation cost matrix and each base of
 *  y by a row. However, note that the actual values that are added during the
 *  alignment are produced by comparing every single base of x with only |a|
 *  elements. Now, in order to make these operations vectorizable, we perform
 *  the comparisons with |a| precalculated vectors. This puts in context the
 *  explanation of each parameter in the function.
 *
 *  @param curRow is the cost matrix row to be filled with values.
 *  @param prevRow is the row above curRow in the cost matrix being filled.
 *  @param gap_row is the cost of aligning each base in x with a gap.
 *  @param align_row is the cost of aligning each base in x with the base
 *      represented by the base of the row of curRow in y.
 *  @param dirMtx is the directional matrix for the backtrace
 *  @param c is the cost of an insertion. An insertion can only occur for one
 *      particular base in the alphabet, corresponding to the base in y represented
 *      by the row that is being filled.
 *  @param st is the starting cell to be filled.
 *  @param end is the final cell to be filled.
 *
 *  If you modify this code check algn_fill_3dMtx as there is similar code there
 *  used in the first plane of the alignment. I didn't use this function because
 *  the direction codes are different for three dimensional alignments.
 */

#include <assert.h>
#include <limits.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>


#include "seqAlign.h"
#include "debug_constants.h"
// #include "cm.h"
// #include "matrices.h"
// #include "seq.h"


#ifdef DEBUG_ALL_ASSERTIONS
int *_algn_max_matrix = NULL;
DIR_MTX_ARROW_t *_algn_max_direction = NULL;
#endif

/*
#if ( __GNUC__ && __MMX__ )
static inline void
algn_fill_row (int *curRow, const int *prevRow, const int *gap_row,
               const int *align_row, DIR_MTX_ARROW_t *dirMtx, int c, int i, int end) {

    register int aa, bb, cc;
    register const int TWO = 0x200;
    register const int ZERO = 0;

    bb = curRow[i - 1];

    for (; i <= end - 7; i+=8) {

        aa = prevRow[i - 1] + align_row[i]; // aka tmp3
        bb += gap_row[i]; // aka tmp2
        cc = prevRow[i] + c; // aka tmp1
*/
        /**
         *  The algorithm has not changed. Only the conditional branches been eliminated for
         *  better performance.
         *
         *  Since gcc (4.0.3 at least) didn't generate cmov's, we changed the code manually.
         *  Things that have been done to optimize this function:
         *      - assembly code for determining min(aa, bb, cc) and getting the bit pattern
         *        loop unrolling with a factor of 8, to still keep this function inlined
         *      - rearrangement of expressions => better register usage and (probably)
         *        fewer cache misses

         *  Restrictions:
         *  ALIGN is bound to the value 4, INSERT to 2 and DELETE to 1
         *  If these constants change, there will be problems with the assembler code,
         *  since it is really optimized for this purpose

         *  Furthermore, this code only works with the gnu compiler (but shouldn't be
         *  hard to switch to icc), so add
         *  #ifdef __GNUC__ as a macro for this section. I also suppose that cmov's
         *  can be handled by most of the computers used today
         *  (of those using this program at least).

         *  I have also removed the debug sections. These can of course be added if
         *  a debug is needed.
         *  I recommend that debugging is only used with the original function,
         *  since this one generates exactly same results.
         */

/*
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

        curRow[i] = bb; // bb is min(aa, bb, cc)
        dirMtx[i] = cc; // cc is the bitpattern


        aa = prevRow[i] + align_row[i + 1];
        bb += gap_row[i + 1]; // bb is already assigned the minimum value of the three to be compared, so loading bb from memory would be waste.
        cc = prevRow[i + 1] + c;


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

        curRow[i + 1] = bb;
        dirMtx[i + 1] = cc;

        aa = prevRow[i + 1] + align_row[i + 2];
        bb += gap_row[i + 2];
        cc = prevRow[i + 2] + c;


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

        curRow[i + 2] = bb;
        dirMtx[i + 2] = cc;

        aa = prevRow[i + 2] + align_row[i + 3];
        bb += gap_row[i + 3];
        cc = prevRow[i + 3] + c;


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

        curRow[i + 3] = bb;
        dirMtx[i + 3] = cc;



        aa = prevRow[i + 3] + align_row[i + 4];
        bb += gap_row[i + 4];
        cc = prevRow[i + 4] + c;


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

        curRow[i + 4] = bb;
        dirMtx[i + 4] = cc;


        aa = prevRow[i + 4] + align_row[i + 5];
        bb += gap_row[i + 5];
        cc = prevRow[i + 5] + c;


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

        curRow[i + 5] = bb;
        dirMtx[i + 5] = cc;

        aa = prevRow[i + 5] + align_row[i + 6];
        bb += gap_row[i + 6];
        cc = prevRow[i + 6] + c;


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

        curRow[i + 6] = bb;
        dirMtx[i + 6] = cc;

        aa = prevRow[i + 6] + align_row[i + 7];
        bb += gap_row[i + 7];
        cc = prevRow[i + 7] + c;


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

        curRow[i + 7] = bb;
        dirMtx[i + 7] = cc;
    }



    for (; i <= end; i++) {

        aa = prevRow[i - 1] + align_row[i];
        bb += gap_row[i];
        cc = prevRow[i] + c;


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

        curRow[i] = bb;
        dirMtx[i] = cc;
    }
    return;
}
#else // __GNUC__
*/

static inline void
algn_fill_row (int *currRow,
               const int *prevRow,
               const int *gap_row,
               const int *align_row,
               DIR_MTX_ARROW_t *dirVect,
               int c,
               int startIndex,
               int finalIndex) {
    int i, upwardCost, leftwardCost, diagonalCost;

    printf("Start Index 'startIndex': %d\n", startIndex);
    printf("End   Index 'finalIndex': %d\n", finalIndex);
    printf("Input Array 'prevRow': \n[ ");
    for (i = startIndex; i <= finalIndex; i++) {
        printf("%d, ", prevRow[i]);
    }
    printf("]\n");

    printf("Input Array 'gap_row': \n[ ");
    for (i = startIndex; i <= finalIndex; i++) {
        printf("%d, ", gap_row[i]);
    }
    printf("]\n");

    printf("Input Array 'align_row': \n[ ");
    for (i = startIndex; i <= finalIndex; i++) {
        printf("%d, ", align_row[i]);
    }
    printf("]\n");
    fflush(stdout);


    for (i = startIndex; i <= finalIndex; i++) {
        // try align with substitution
        upwardCost   = prevRow[i    ] + c;
        leftwardCost = currRow[i - 1] + gap_row[i];
        diagonalCost = prevRow[i - 1] + align_row[i];
        /* check whether insertion is better */
        /* This option will allow all the possible optimal paths to be stored
         * concurrently on the same backtrace matrix. This is important for the
         * sequences being able to choose the appropriate direction while
         * keeping the algorithm that assumes that seq2 is at most as long as seq1.
         * */
        if (upwardCost < diagonalCost) {
            if (upwardCost < leftwardCost) {
                currRow[i] = upwardCost;
                dirVect[i] = DELETE;
            } else if (leftwardCost < upwardCost) {
                currRow[i] = leftwardCost;
                dirVect[i] = INSERT;
            } else {
                currRow[i] = leftwardCost;
                dirVect[i] = INSERT | DELETE;
            }
        } else if (diagonalCost < upwardCost) {
            if (diagonalCost < leftwardCost) {
                currRow[i] = diagonalCost;
                dirVect[i] = ALIGN;
            } else if (leftwardCost < diagonalCost) {
                currRow[i] = leftwardCost;
                dirVect[i] = INSERT;
            } else {
                currRow[i] = leftwardCost;
                dirVect[i] = ALIGN | INSERT;
            }
        } else { // diagonalCost == upwardCost
            if (diagonalCost < leftwardCost) {
                currRow[i] = diagonalCost;
                dirVect[i] = ALIGN | DELETE;
            } else if (leftwardCost < diagonalCost) {
                currRow[i] = leftwardCost;
                dirVect[i] = INSERT;
            } else {
                currRow[i] = leftwardCost;
                dirVect[i] = DELETE | INSERT | ALIGN;
            }
        }
        if (DEBUG_DIR_M) {
	    // Print the alignment matrix
            if (INSERT & dirVect[i])
                printf ("I");
            if (DELETE & dirVect[i])
                printf ("D");
            if (ALIGN & dirVect[i])
                printf ("A");
            printf ("\t");
        }
        if (DEBUG_COST_M) {
	    // Print the cost matrix
            printf ("%d\t", currRow[i]);
            fflush (stdout);
        }
    }
    if (DEBUG_COST_M || DEBUG_DIR_M)  {
        printf ("\n");
        fflush (stdout);
    }

    printf("Result Array 'currRow': \n[ ");
    for (i = startIndex; i <= finalIndex; i++) {
        printf("%d, ", currRow[i]);
    }
    printf("]\n");

    printf("Result Array 'dirVect': \n[ ");
    for (i = startIndex; i <= finalIndex; i++) {
        printf("%d, ", currRow[i]);
    }
    printf("]\n\n");
    fflush(stdout);

    return;
}
/*
#endif // __GNUC__
*/

static inline void
algn_fill_ukk_right_cell (int *curRow,
                          const int *prevRow,
                          const int *gap_row,
                          const int *align_row,
                          DIR_MTX_ARROW_t *dirMtx,
                          int c,
                          int pos) {
    int tmp2, tmp3;
    /* try align with substitution */
    tmp2 = curRow[pos - 1] + gap_row[pos];
    tmp3 = prevRow[pos - 1] + align_row[pos];
    /* check whether insertion is better */
    if (tmp2 < tmp3) {
        curRow[pos] = tmp2;
        dirMtx[pos] = INSERT;
    }
    else if (tmp3 < tmp2) {
        curRow[pos] = tmp3;
        dirMtx[pos] = ALIGN;
    }
    else {
        curRow[pos] = tmp3;
        dirMtx[pos] = INSERT | ALIGN;
    }
    if (DEBUG_DIR_M) {
        /* Print the alignment matrix */
        if (INSERT & dirMtx[pos])
            printf ("I");
        if (DELETE & dirMtx[pos])
            printf ("D");
        if (ALIGN & dirMtx[pos])
            printf ("A");
        printf ("\t");
    }
    if (DEBUG_COST_M) {
        /* Print the cost matrix */
        printf ("%d\t", curRow[pos]);
        fflush (stdout);
    }
    if (DEBUG_COST_M || DEBUG_DIR_M) {
        printf ("\n");
        fflush (stdout);
    }
    return;
}

static inline void
algn_fill_ukk_left_cell (int *curRow,
                         const int *prevRow,
                         const int *gap_row,
                         const int *align_row,
                         DIR_MTX_ARROW_t *dirMtx,
                         int c,
                         int pos) {
    int tmp1, tmp3;
    /* try align with substitution */
    tmp1 = prevRow[pos] + c;
    tmp3 = prevRow[pos - 1] + align_row[pos];
        if (tmp1 < tmp3) {
            curRow[pos] = tmp1;
            dirMtx[pos] = DELETE;
        }
        else if (tmp3 < tmp1) {
            curRow[pos] = tmp3;
            dirMtx[pos] = ALIGN;
        }
        else {
            curRow[pos] = tmp1;
            dirMtx[pos] = ALIGN | DELETE;
        }
    if (DEBUG_DIR_M) {
        /* Print the alignment matrix */
        if (INSERT & dirMtx[pos])
            printf ("I");
        if (DELETE & dirMtx[pos])
            printf ("D");
        if (ALIGN & dirMtx[pos])
            printf ("A");
        printf ("\t");
    }
    if (DEBUG_COST_M) {
        /* Print the cost matrix */
        printf ("%d\t", curRow[pos]);
        fflush (stdout);
    }
    return;
}

static inline void
algn_fill_last_column (int *curRow, const int *prevRow, int tlc, int lastColumnIndex, DIR_MTX_ARROW_t *dirMtx) {
    int cost;
    if (lastColumnIndex > 0) {
        cost = prevRow[lastColumnIndex] + tlc; // Gotta add some tender loving care to the cost!
        if (cost < curRow[lastColumnIndex]) {
	    curRow[lastColumnIndex] = cost;
            dirMtx[lastColumnIndex] = DELETE;
        }
        else if (cost == curRow[lastColumnIndex])
	    dirMtx[lastColumnIndex] = dirMtx[lastColumnIndex] | DELETE;
    }
    return;
}

static inline void
algn_fill_full_row (int *curRow,
                    const int *prevRow,
                    const int *gap_row,
                    const int *align_row,
                    DIR_MTX_ARROW_t *dirMtx,
                    int c,
                    int tlc,
                    int rowLength) {
    /* first entry is delete */
    curRow[0] = c + prevRow[0];
    dirMtx[0] = DELETE;
    if (DEBUG_COST_M) {
        printf ("%d\t", curRow[0]);
        fflush (stdout);
    }
    if (DEBUG_DIR_M) {
        printf ("D\t");
    }

    algn_fill_row (curRow, prevRow, gap_row, align_row, dirMtx, c, 1, rowLength - 1);
    algn_fill_last_column (curRow, prevRow, tlc, rowLength - 1, dirMtx);

    return;
}

void
algn_fill_first_row (int *curRow, DIR_MTX_ARROW_t *dirMtx, int len, int const *gap_row) {
    int i;
    /* We fill the first cell to start with */
    curRow[0] = 0;
    dirMtx[0] = ALIGN;
    /* Now the rest of the row */
    if (DEBUG_DIR_M)
        printf ("A\t");
    if (DEBUG_DIR_M) {
        printf ("%d\t", curRow[0]);
        fflush (stdout);
    }
    for (i = 1; i < len; i++) {
        curRow[i] = curRow[i - 1] + gap_row[i];
        dirMtx[i] = INSERT;
        if (DEBUG_DIR_M)
            printf ("I\t");
        if (DEBUG_DIR_M) {
            printf ("%d\t", curRow[i]);
            fflush (stdout);
        }
    }
    return;
}

void
algn_fill_first_cell (int *curRow, int prevRow, DIR_MTX_ARROW_t *dirMtx, int gap_char) {
    *curRow = prevRow + gap_char;
    *dirMtx = DELETE;
    if (DEBUG_DIR_M)
        printf ("D\t");
    if (DEBUG_DIR_M) {
        printf ("%d\t", *curRow);
        fflush (stdout);
    }
    return;
}

/* In the following three functions, we maintain the following invariants in
 * each loop:
 * 1. curRow is a row that has not been filled and is the next to be.
 * 4. dirMtx is the current row of the direction matrix
 * 2. prevRow is the previous row, located right above curRow, which has been filled
 *    already.
 * 3. i is the number of the row of curRow in its containing matrix
 * 5. gap_row is the cost of aligning each base of seq2 with a gap. This is
 *    constant for all the loop.
 * 6. cur_seq1 is the i'th base of seq1
 * 7. const_val is the cost of cur_seq1 aligned with a gap
 * 8. align_row is the vector of costs of aligning seq2 with cur_seq1
 */

int *
algn_fill_extending_right (const seq_p seq1,
                           int *precalcMtx,
                           int seq1_len,
                           int seq2_len,
                           int *curRow,
                           int *prevRow,
                           DIR_MTX_ARROW_t *dirMtx,
                           const cost_matrices_2d_p c,
                           int start_row,
                           int end_row,
                           int len) {
    int i;
    int *tmp, cur_seq1, const_val;
    const int *gap_row, *align_row;
    /** Invariants block
     * len is the number of items in the row to be filled **/
    i = start_row;
    /* This is what we will perform conceptually, I will stop using the
     * cm_get_precal_row function to speed this up a little bit
    gap_row = cm_get_precal_row (precalcMtx, cm_get_gap (c), seq2_len);
    */
    gap_row = precalcMtx + (c->gap_char * seq2_len);
    while (i < end_row) {
        /** Invariants block */
        cur_seq1 = seq1->seq_begin[i];
        const_val = cm_calc_cost (c->cost, cur_seq1, c->gap_char, c->costMtxDimension);
        /* This is conceptually what we do in the next line
        align_row = cm_get_precal_row (precalcMtx, cur_seq1, seq2_len);
        */
        align_row = precalcMtx + (cur_seq1 * seq2_len);
        /* Align! */
        algn_fill_first_cell (curRow, prevRow[0], dirMtx, align_row[0]);
        algn_fill_row (curRow, prevRow, gap_row, align_row, dirMtx, const_val, 1, len - 2);
        algn_fill_ukk_right_cell (curRow, prevRow, gap_row, align_row, dirMtx, const_val, len - 1);
        /** Invariants block */
        tmp = curRow;
        curRow = prevRow;
        prevRow = tmp;
        i++;
        dirMtx += seq2_len;
        len++;
    }
    return (curRow);
}

int *
algn_fill_extending_left_right (const seq_p seq1,
                                int *precalcMtx,
                                int seq1_len,
                                int seq2_len,
                                int *curRow,
                                int *prevRow,
                                DIR_MTX_ARROW_t *dirMtx,
                                const cost_matrices_2d_p c,
                                int start_row,
                                int end_row,
                                int start_column,
                                int len) {
    int i;
    int *tmp, cur_seq1, const_val;
    const int *gap_row, *align_row;
    /** Invariants block
     * len is the number of cells to fill in the current row minus 1
     * start_column is the first cell to fill in the row */
    i = start_row;
    /* Conceptually,
       gap_row = cm_get_precal_row (precalcMtx, cm_get_gap (c), seq2_len);
    */
    gap_row = precalcMtx + (c->gap_char * seq2_len);
    len--;
    while (i < end_row) {
        /** Invariants block */
        cur_seq1 = seq1->seq_begin[i];
        const_val = cm_calc_cost (c->cost, cur_seq1, c->gap_char, c->costMtxDimension);
        /* Conceptually,
           align_row = cm_get_precal_row (precalcMtx, cur_seq1, seq2_len);
        */
        align_row = precalcMtx + (cur_seq1 * seq2_len);
        /* Align! */
        algn_fill_ukk_left_cell (curRow, prevRow, gap_row, align_row, dirMtx, const_val, start_column);
        algn_fill_row (curRow, prevRow, gap_row, align_row, dirMtx, const_val,
                       start_column + 1, start_column + (len - 2));
        algn_fill_ukk_right_cell (curRow, prevRow, gap_row, align_row, dirMtx, const_val,
                                  start_column + len - 1);
        /** Invariants block */
        tmp     = curRow;
        curRow  = prevRow;
        prevRow = tmp;
        i++;
        dirMtx += seq2_len;
        start_column++;
    }
    return (curRow);
}

int *
algn_fill_extending_left (const seq_p seq1,
                          int *precalcMtx,
                          int seq1_len,
                          int seq2_len,
                          int *curRow,
                          int *prevRow,
                          DIR_MTX_ARROW_t *dirMtx,
                          const cost_matrices_2d_p c,
                          int start_row,
                          int end_row,
                          int start_column,
                          int len) {
    int i;
    int *tmp, cur_seq1, const_val, const_val_tail;
    const int *gap_row, *align_row;
    /** Invariants block
     *  start_column is the first cell to fill in the row
     *  len is the number of cells to fill in the current row minus 1 */
    i = start_row;
    /* Conceptually,
       gap_row = cm_get_precal_row (precalcMtx, cm_get_gap (c), seq2_len);
    */
    gap_row = precalcMtx + (c->gap_char * seq2_len);
    while (i < end_row) {
        /** Invariants block */
        cur_seq1 = seq1->seq_begin[i];
        const_val = cm_calc_cost (c->cost, cur_seq1, c->gap_char, c->costMtxDimension);
        const_val_tail = (cm_get_tail_cost (c))[cur_seq1];
        /* Conceptually,
           align_row = cm_get_precal_row (precalcMtx, cur_seq1, seq2_len);
        */
        align_row = precalcMtx + (cur_seq1 * seq2_len);
        /* Align! */
        algn_fill_ukk_left_cell (curRow, prevRow, gap_row, align_row, dirMtx, const_val, start_column);
        algn_fill_row (curRow, prevRow, gap_row, align_row, dirMtx, const_val,
                       start_column + 1, start_column + len - 1);
        algn_fill_last_column (curRow, prevRow, const_val_tail, start_column + len - 1, dirMtx);
        /** Invariants block */
        tmp = curRow;
        curRow = prevRow;
        prevRow = tmp;
        i++;
        dirMtx += seq2_len;
        start_column++;
        len--;
    }
    if (DEBUG_COST_M) {
        printf ("A_A_A gap cost\n");
        fflush (stdout);
        for (i = 0; i < seq2_len; i++) {
            printf ("%d\t", gap_row[i]);
            fflush (stdout);
        }
        printf ("\n");
        printf ("The G_A_A - gap cost is %d\n", const_val);
        fflush (stdout);
    }

    return (curRow);
}

int *
algn_fill_no_extending (const seq_p seq1,
                        int *precalcMtx,
                        int seq1_len,
                        int seq2_len,
                        int *curRow,
                        int *prevRow,
                        DIR_MTX_ARROW_t *dirMtx,
                        const cost_matrices_2d_p c,
                        int start_row,
                        int end_row) {
    int i;
    int *tmp, cur_seq1, const_val, const_val_tail;
    const int *gap_row, *align_row;
    /** Invariants block */
    i = start_row;
    /* Conceptually,
       gap_row = cm_get_precal_row (precalcMtx, cm_get_gap (c), seq2_len);
    */
    gap_row = precalcMtx + (c->gap_char * seq2_len);
    while (i < end_row) {
        /** Invariants block */
        cur_seq1 = seq1->seq_begin[i];
        const_val = cm_calc_cost (c->cost, cur_seq1, c->gap_char, c->costMtxDimension);
        const_val_tail = (cm_get_tail_cost (c))[cur_seq1];
        /* Conceptually,
           align_row = cm_get_precal_row (precalcMtx, cur_seq1, seq2_len);
        */
        align_row = precalcMtx + (cur_seq1 * seq2_len);
        /* Align! */
        algn_fill_first_cell (curRow, prevRow[0], dirMtx, align_row[0]);
        algn_fill_row (curRow, prevRow, gap_row, align_row, dirMtx, const_val, 1, seq2_len - 1);
        algn_fill_last_column (curRow, prevRow, const_val_tail, seq2_len - 1, dirMtx);
        /** Invariants block */
        tmp = curRow;
        curRow = prevRow;
        prevRow = tmp;
        i++;
        dirMtx += seq2_len;
    }
    return (curRow);
}

/* Similar to the previous but when no barriers are set */

static inline int
algn_fill_plane (const seq_p longerSequence,
                 int *precalcMtx,
                 int longerSequenceLength, //larger, horizontal dimension
                 int lesserSequenceLength, //smaller,  vertical dimension
                 int *curRow,
                 DIR_MTX_ARROW_t *dirMtx,
                 const cost_matrices_2d_p costMtx) {
    printf("lesserSequenceLength: %d\n", lesserSequenceLength);
    printf("longerSequenceLength: %d\n", longerSequenceLength);

    int i;
    const int *align_row,
              *gap_row,
              *first_gap_row;

    int        const_val,
               const_val_tail,
              *newNWMtx,
              *tmp,
               gapcode,
               curSeq1_elem;

    /* A precalculated cost of a gap aligned with each base in the array */
    gapcode       = cm_get_gap_char_2d (costMtx);
    gap_row       = cm_get_precal_row (precalcMtx, gapcode, lesserSequenceLength);
    first_gap_row = cm_get_precal_row (precalcMtx,       0, lesserSequenceLength);
    newNWMtx  = curRow;
    curRow[0] = 0;
    dirMtx[0] = ALIGN;

    if (DEBUG_COST_M) {
        printf ("%d\t", curRow[0]);
    }
    if (DEBUG_DIR_M) {
        printf ("A\t");
    }

    /* We fill the first row to start with */
    for (i = 1; i < lesserSequenceLength; i++) {
        curRow[i] = curRow[i - 1] + first_gap_row[i];
        dirMtx[i] = INSERT;
        if (DEBUG_COST_M) {
            printf ("%d\t", curRow[i]);
        }
        if (DEBUG_DIR_M) {
            printf ("I\t");
        }
    }
    if (DEBUG_DIR_M || DEBUG_COST_M) {
        printf ("\n");
    }

    curRow += lesserSequenceLength;


    /* Now we fill the rest of the matrix */
    for (i = 1, dirMtx += lesserSequenceLength; i < longerSequenceLength; i++, dirMtx += lesserSequenceLength) {
        curSeq1_elem   = seq_get_element(longerSequence, i);
        const_val_tail = (cm_get_tail_cost(costMtx))[curSeq1_elem]; // get tail cost in c at pointer
                                                                    // at position i in seq1
	printf("const_val_tail: %d\n",const_val_tail);
        const_val = cm_calc_cost ( costMtx->cost
                                 , seq_get_element(longerSequence, i)
                                 , costMtx->gap_char
                                 , costMtx->costMtxDimension);
        align_row = cm_get_precal_row ( precalcMtx
   				      , seq_get_element (longerSequence, i)
                                      , lesserSequenceLength);
        algn_fill_full_row ( curRow
                           , newNWMtx
                           , gap_row
                           , align_row
                           , dirMtx
                           , const_val
                           , const_val_tail
                           , lesserSequenceLength);

        /* We swap curRow and newNWMtx for the next round */
        tmp      = curRow;
        curRow   = newNWMtx;
        newNWMtx = tmp;
    }

    return (newNWMtx[lesserSequenceLength - 1]);
}

static inline int *
choose_other (int *compare, int *a, int *b) {
    if (a == compare) return b;
    else return a;
}

static inline int
algn_fill_plane_2 (const seq_p seq1,
                   int *precalcMtx,
                   int  len_longerSeq,
                   int  seq2_len,
                   int *curRow,
                   DIR_MTX_ARROW_t *dirMtx,
                   const cost_matrices_2d_p costMtx,
                   int width,
                   int height,
                   int dwidth_height)
{
    // printf("algn_fill_plane_2 %d", iteration);
    //fflush(stdout);
    int *next_row,
        *next_prevRow,
        *b,
         start_row,
         final_row,
         start_column,
         length;

    int const *gap_row;

    DIR_MTX_ARROW_t *to_go_dirMtx;
    width = width + dwidth_height;

    if (width > seq2_len) {
        width = seq2_len; // width is at most seq2_len
    }

    height = height + dwidth_height;

    if (height > len_longerSeq) {
        height = len_longerSeq; // likewise, height is at most len_longerSeq
    }

    b = curRow + seq2_len;
    gap_row = cm_get_precal_row (precalcMtx, 0, seq2_len); // We want the first horizontal row

    /* We have to consider three cases in this new alignment procedure (much
     * cleaner than the previous):
     *
     * Case 1:
     * If len_longerSeq >= 1.5 * seq2_len, there is no point in using the
     * barriers. Rather, we fill the full matrix in one shot */
    if (len_longerSeq >= 1.5 * seq2_len) { // deleted a bunch of casts here
        return (algn_fill_plane (seq1, precalcMtx, len_longerSeq, seq2_len, curRow, dirMtx, costMtx));
    }
    /* Case 2:
     * There are no full rows to be filled, therefore we have to break the
     * procedure into two different subsets */
    // subset 1:
    else if (2 * height < len_longerSeq) {
        algn_fill_first_row (curRow, dirMtx, width, gap_row);
        start_row = 1;
        final_row = height;
        start_column = 0;
        length = width + 1;
        to_go_dirMtx = dirMtx + (start_row * seq2_len);
        /* Now we fill that space */
        next_row = algn_fill_extending_right (seq1, precalcMtx, len_longerSeq, seq2_len, b, curRow,
                                              to_go_dirMtx, costMtx, start_row, final_row, length);
        next_prevRow = choose_other (next_row, curRow, b);
        /* Next group */
        start_row    = final_row;
        final_row    = len_longerSeq - (height - 1);
        start_column = 1;
        length       = width + height;
        to_go_dirMtx = dirMtx + (start_row * seq2_len);
        next_row     = algn_fill_extending_left_right ( seq1
                                                      , precalcMtx
                                                      , len_longerSeq
                                                      , seq2_len
                                                      , next_row
                                                      , next_prevRow
                                                      , to_go_dirMtx
                                                      , costMtx
                                                      , start_row
                                                      , final_row
                                                      , start_column
                                                      , length);
        next_prevRow  = choose_other (next_row, curRow, b);
        /* The final group */
        start_row = final_row;
        final_row = len_longerSeq;
        length = length - 2;
        start_column = seq2_len - length;
        to_go_dirMtx = dirMtx + (start_row * seq2_len);
        next_row = algn_fill_extending_left (seq1, precalcMtx, len_longerSeq, seq2_len,
                                             next_row, next_prevRow, to_go_dirMtx, costMtx, start_row, final_row,
                                             start_column, length);
        next_prevRow = choose_other (next_row, curRow, b);
    }
    /* Case 3: (final case)
     * There is a block in the middle with full rows that have to be filled
     */
    else {
        /* We will simplify this case even further: If the size of the leftover
         * is too small, don't use the barriers at all, just fill it all up
         */
        // subset 2:
        if (8 >= (len_longerSeq - height)) {
            return (algn_fill_plane (seq1, precalcMtx, len_longerSeq, seq2_len, curRow, dirMtx, costMtx));
        // subset 3:
        } else {
            algn_fill_first_row (curRow, dirMtx, width, gap_row);
            start_row    = 1;
            final_row    = (seq2_len - width) + 1;
            start_column = 0;
            length       = width + 1;
            to_go_dirMtx = dirMtx + (seq2_len * start_row);
            next_row     = algn_fill_extending_right ( seq1
                                                     , precalcMtx
                                                     , len_longerSeq
                                                     , seq2_len
                                                     , b
                                                     , curRow
                                                     , to_go_dirMtx
                                                     , costMtx
                                                     , start_row
                                                     , final_row
                                                     , length);

            next_prevRow = choose_other (next_row, curRow, b);
            start_row    = final_row;
            final_row    = len_longerSeq - (seq2_len - width) + 1;
            length       = seq2_len;
            to_go_dirMtx = dirMtx + (seq2_len * start_row);
            next_row     = algn_fill_no_extending ( seq1
                                                  , precalcMtx
                                                  , len_longerSeq
                                                  , seq2_len
                                                  , next_row
                                                  , next_prevRow
                                                  , to_go_dirMtx
                                                  , costMtx
                                                  , start_row
                                                  , final_row);

            next_prevRow = choose_other (next_row, curRow, b);
            start_row    = final_row;
            final_row    = len_longerSeq;
            start_column = 1;
            length       = seq2_len - 1;
            to_go_dirMtx = dirMtx + (seq2_len * start_row);
            next_row     = algn_fill_extending_left ( seq1
                                                    , precalcMtx
                                                    , len_longerSeq
                                                    , seq2_len
                                                    , next_row
                                                    , next_prevRow
                                                    , to_go_dirMtx
                                                    , costMtx
                                                    , start_row
                                                    , final_row
                                                    , start_column
                                                    , length);

            next_prevRow = choose_other (next_row, curRow, b);
        }
    }
    return next_prevRow[seq2_len - 1];
}
/******************************************************************************/

/******************************************************************************/
/*                         Pairwise Affine Alignment                          */
/******************************************************************************/
/*
 *
 * WARNING! This is a copy of the pairwise standard alignment, modified slightly
 * for the affine case. This is for performance issues! so beware, any change
 * here must also go there.
 */

#define algn_assign_dirMtx(dirMtx, pos, v) dirMtx[pos] = dirMtx[pos] | v

static inline void
algn_fill_row_affine (int *curRow,
                      const int *prevRow,
                      const int *gap_row,
                      const int *align_row,
                      DIR_MTX_ARROW_t *dirMtx,
                      int c,
                      int cprev,
                      int st,
                      int end,
                      int *dncurRow,
                      const int *pdncurRow,
                      int *htcurRow,
                      int open_gap) {

    int i, tmp1, tmp2, tmp3, tmp4, tmp5;

    for (i = st; i <= end; i++) {
        /* try align with substitution */
#ifdef DEBUG_ALL_ASSERTIONS
        assert ((curRow + i) < _algn_max_matrix);
        assert ((prevRow + i) < _algn_max_matrix);
        assert ((dirMtx + i) < _algn_max_direction);
        assert ((dncurRow + i) < _algn_max_matrix);
        assert ((pdncurRow + i) < _algn_max_matrix);
        assert ((htcurRow + i) < _algn_max_matrix);
#endif
        dirMtx[i] = 0;
        { /* We deal with the difficultness of using an opening gap as
             another DIR_MTX_ARROW_t */
            if ((0 == cprev) && (0 != c)) {
                tmp1 = pdncurRow[i] + open_gap + c;
                tmp4 = prevRow[i] + open_gap + c;
            }
            else if ((0 != cprev) && (0 == c)) {
                tmp1 = pdncurRow[i] + open_gap + c;
                tmp4 = prevRow[i];
            }
            else {
                tmp1 = pdncurRow[i] + c;
                tmp4 = prevRow[i] + open_gap + c;
            }

            if ((0 == gap_row[i - 1]) && (0 != gap_row[i])) {
                tmp5 = htcurRow[i - 1] + open_gap + gap_row[i];
                tmp2 = curRow[i - 1] + open_gap + gap_row[i];
            }
            else if ((0 != gap_row[i - 1]) && (0 == gap_row[i])) {
                tmp5 = htcurRow[i - 1] + open_gap + gap_row[i];
                tmp2 = curRow[i - 1];
            }
            else {
                tmp2 = curRow[i - 1] + open_gap + gap_row[i];
                tmp5 = htcurRow[i - 1] + gap_row[i];
            }

            if ((((0 == gap_row[i]) && (0 != c)) ||
                    ((0 != gap_row[i]) && (0 == c))) &&
                    ((0 == gap_row[i - 1]) || (0 == cprev)))
                tmp3 = prevRow[i - 1] + open_gap + align_row[i];
            else
                tmp3 = prevRow[i - 1] + align_row[i];
        }
        if (tmp1 < tmp4)
            algn_assign_dirMtx(dirMtx, i, DELETE_V);
        else {
            algn_assign_dirMtx(dirMtx, i, ALIGN_V);
            tmp1 = tmp4;
        }

        if (tmp2 <= tmp5) {
            algn_assign_dirMtx(dirMtx, i, ALIGN_H);
        }
        else {
            tmp2 = tmp5;
            algn_assign_dirMtx(dirMtx, i, INSERT_H);
        }
        dncurRow[i] = tmp1;
        htcurRow[i] = tmp2;
        /* check whether insertion is better */
        /* This option will allow all the possible optimal paths to be stored
         * concurrently on the same backtrace matrix. This is important for the
         * sequences being able to choose the appropriate direction while
         * keeping the algorithm that assumes that seq2 is at most as long as seq1.
         * */
        if (tmp1 < tmp3) {
            if (tmp1 < tmp2) {
                curRow[i] = tmp1;
                algn_assign_dirMtx(dirMtx, i, DELETE);
            }
            else if (tmp2 < tmp1) {
                curRow[i] = tmp2;
                algn_assign_dirMtx(dirMtx, i, INSERT);
            }
            else {
                curRow[i] = tmp2;
                algn_assign_dirMtx(dirMtx, i, (DELETE | INSERT));
            }
        }
        else if (tmp3 < tmp1) {
            if (tmp3 < tmp2) {
                curRow[i] = tmp3;
                algn_assign_dirMtx(dirMtx, i, ALIGN);
            }
            else if (tmp2 < tmp3) {
                curRow[i] = tmp2;
                algn_assign_dirMtx(dirMtx, i, INSERT);
            }
            else {
                curRow[i] = tmp2;
                algn_assign_dirMtx(dirMtx, i, ALIGN | INSERT);
            }
        }
        else { /* tmp3 == tmp1 */
            if (tmp3 < tmp2) {
                curRow[i] = tmp3;
                algn_assign_dirMtx(dirMtx, i, ALIGN | DELETE);
            }
            else if (tmp2 < tmp3) {
                curRow[i] = tmp2;
                algn_assign_dirMtx(dirMtx, i, INSERT);
            }
            else {
                curRow[i] = tmp2;
                algn_assign_dirMtx(dirMtx, i, DELETE | INSERT | ALIGN);
            }
        }
        if (DEBUG_DIR_M) {
            /* Print the alignment matrix */
            if (INSERT & dirMtx[i])
                printf ("I");
            if (DELETE & dirMtx[i])
                printf ("D");
            if (ALIGN & dirMtx[i])
                printf ("A");
            printf ("\t");
        }
        if (DEBUG_COST_M) {
            /* Print the cost matrix */
            printf ("(%d, %d, %d)\t", curRow[i], htcurRow[i], dncurRow[i]);
            fflush (stdout);
        }
    }
    if (DEBUG_DIR_M) {
        printf ("\n");
        fflush (stdout);
    }
    return;
}

static inline void
algn_fill_ukk_right_cell_affine (int *curRow,
                                 const int *prevRow,
                                 const int *gap_row,
                                 const int *align_row,
                                 DIR_MTX_ARROW_t *dirMtx,
                                 int c,
                                 int cprev,
                                 int pos,
                                 int *dncurRow,
                                 int *htcurRow,
                                 int open_gap) {
    int tmp2, tmp3, tmp4;
    /* try align with substitution */
#ifdef DEBUG_ALL_ASSERTIONS
        assert ((curRow + pos) < _algn_max_matrix);
        assert ((prevRow + pos) < _algn_max_matrix);
        assert ((dirMtx + pos) < _algn_max_direction);
        assert ((htcurRow + pos) < _algn_max_matrix);
#endif
    dirMtx[pos] = 0;
    { /* Affine gap difficultness */
        if ((0 != gap_row[pos - 1]) && (0 == gap_row[pos]))
            tmp2 = curRow[pos - 1];
        else
            tmp2 = curRow[pos - 1] + open_gap + gap_row[pos];

        if (((0 == gap_row[pos - 1]) && (0 != gap_row[pos])) ||
            ((0 != gap_row[pos - 1]) && (0 == gap_row[pos])))
            tmp4 = htcurRow[pos - 1] + open_gap + gap_row[pos];
        else
            tmp4 = htcurRow[pos - 1] + gap_row[pos];

        if ((((0 == gap_row[pos]) && (0 != c)) ||
                ((0 != gap_row[pos]) && (0 == c))) &&
                ((0 == gap_row[pos - 1]) || (0 == cprev)))
            tmp3 = prevRow[pos - 1] + open_gap + align_row[pos];
        else
            tmp3 = prevRow[pos - 1] + align_row[pos];
    }
    if (tmp2 <= tmp4)
        algn_assign_dirMtx(dirMtx, pos, ALIGN_H);
    else {
        tmp2 = tmp4;
        algn_assign_dirMtx(dirMtx, pos, INSERT_H);
    }
    htcurRow[pos] = tmp2;
    dncurRow[pos] = VERY_LARGE_NUMBER;
    /* check whether insertion is better */
    if (tmp2 < tmp3) {
        curRow[pos] = tmp2;
        algn_assign_dirMtx(dirMtx, pos, (INSERT));
    }
    else if (tmp3 < tmp2) {
        curRow[pos] = tmp3;
        algn_assign_dirMtx(dirMtx, pos, (ALIGN));
    }
    else {
        curRow[pos] = tmp3;
        algn_assign_dirMtx(dirMtx, pos, (INSERT | ALIGN));
    }
    if (DEBUG_DIR_M) {
        /* Print the alignment matrix */
        if (INSERT & dirMtx[pos])
            printf ("I");
        if (DELETE & dirMtx[pos])
            printf ("D");
        if (ALIGN & dirMtx[pos])
            printf ("A");
        printf ("\t");
    }
    if (DEBUG_COST_M) {
        /* Print the cost matrix */
        printf ("(%d, %d)\t", curRow[pos], htcurRow[pos]);
    }
    if (DEBUG_DIR_M || DEBUG_COST_M)
        printf ("\n");
    return;
}

static inline void
algn_fill_ukk_left_cell_affine (int *curRow,
                                const int *prevRow,
                                const int *gap_row,
                                const int *align_row,
                                DIR_MTX_ARROW_t *dirMtx,
                                int c,
                                int cprev,
                                int pos,
                                int *dncurRow,
                                int *pdncurRow,
                                int *htcurRow,
                                int open_gap) {
    int tmp1, tmp3, tmp5;
    /* try align with substitution */
#ifdef DEBUG_ALL_ASSERTIONS
        assert ((curRow + pos) < _algn_max_matrix);
        assert ((prevRow + pos) < _algn_max_matrix);
        assert ((dirMtx + pos) < _algn_max_direction);
        assert ((dncurRow + pos) < _algn_max_matrix);
        assert ((pdncurRow + pos) < _algn_max_matrix);
#endif
    dirMtx[pos] = 0;
    { /* Affine gap difficultness */
        if ((0 != cprev) && (0 == c))
            tmp1 = prevRow[pos];
        else
            tmp1 = prevRow[pos] + open_gap + c;

        if (((0 == cprev) && (0 != c)) ||
            ((0 != cprev) && (0 == c)))
            tmp5 = pdncurRow[pos] + open_gap + c;
        else
            tmp5 = pdncurRow[pos] + c;

        if ((((0 == gap_row[pos]) && (0 != c)) ||
                ((0 != gap_row[pos]) && (0 == c))) &&
                    ((0 == gap_row[pos - 1]) || (0 == cprev)))
            tmp3 = prevRow[pos - 1] + open_gap + align_row[pos];
        else
            tmp3 = prevRow[pos - 1] + align_row[pos];
    }
    if (tmp1 <= tmp5)
        algn_assign_dirMtx(dirMtx, pos, ALIGN_V);
    if (tmp5 < tmp1) {
        algn_assign_dirMtx(dirMtx, pos, DELETE_V);
        tmp1 = tmp5;
    }
    dncurRow[pos] = tmp1;
    htcurRow[pos] = VERY_LARGE_NUMBER;
        if (tmp1 < tmp3) {
            curRow[pos] = tmp1;
            algn_assign_dirMtx(dirMtx, pos, (DELETE));
        }
        else if (tmp3 < tmp1) {
            curRow[pos] = tmp3;
            algn_assign_dirMtx(dirMtx, pos, (ALIGN));
        }
        else {
            curRow[pos] = tmp1;
            algn_assign_dirMtx(dirMtx, pos, (ALIGN | DELETE));
        }
    if (DEBUG_DIR_M) {
        /* Print the alignment matrix */
        if (INSERT & dirMtx[pos])
            printf ("I");
        if (DELETE & dirMtx[pos])
            printf ("D");
        if (ALIGN & dirMtx[pos])
            printf ("A");
        printf ("\t");
    }
    if (DEBUG_COST_M) {
        /* Print the cost matrix */
        printf ("(%d, ,%d)\t", curRow[pos], dncurRow[pos]);
    }
    return;
}

static inline void
algn_fill_last_column_affine (int *curRow,
                              const int *prevRow,
                              int tlc,
                              int tlcprev,
                              int l,
                              DIR_MTX_ARROW_t *dirMtx,
                              int *dncurRow,
                              const int *pdncurRow,
                              int open_gap) {
    int cst, tmp2;
#ifdef DEBUG_ALL_ASSERTIONS
        assert ((curRow + l) < _algn_max_matrix);
        assert ((prevRow + l) < _algn_max_matrix);
        assert ((dirMtx + l) < _algn_max_direction);
        assert ((dncurRow + l) < _algn_max_matrix);
        assert ((pdncurRow + l) < _algn_max_matrix);
#endif
    tmp2 = prevRow[l] + tlc + open_gap;
    { /* Affine gap difficultness */
        if (((0 == tlcprev) && (0 != tlc)) ||
            ((0 != tlcprev) && (0 == tlc)))
            cst = pdncurRow[l] + open_gap + tlc;
        else
            cst = pdncurRow[l] + tlc;
    }
    if (cst < tmp2)
        algn_assign_dirMtx(dirMtx, l, DELETE_V);
    else {
        cst = tmp2;
        algn_assign_dirMtx(dirMtx, l, ALIGN_V);
    }
    dncurRow[l] = cst;
    if (cst < curRow[l]) {
        curRow[l] = cst;
        algn_assign_dirMtx(dirMtx, l, (DELETE));
    }
    else if (cst == curRow[l])
        algn_assign_dirMtx(dirMtx, l, DELETE);
    return;
}

static inline void
algn_fill_full_row_affine (int *curRow,
                           const int *prevRow,
                           const int *gap_row,
                           const int *align_row,
                           DIR_MTX_ARROW_t *dirMtx,
                           int c,
                           int cprev,
                           int tlc,
                           int tlcprev,
                           int l,
                           int *dncurRow,
                           const int *pdncurRow,
                           int *htcurRow,
                           int open_gap) {
    /* first entry is delete */
    htcurRow[0] = VERY_LARGE_NUMBER;
    curRow[0] += c;
    dirMtx[0] = DELETE | DELETE_V;
    dncurRow[0] = c + pdncurRow[0];
    if (DEBUG_COST_M)
        printf ("%d\t", curRow[0]);
    if (DEBUG_DIR_M)
        printf ("D\t");
    algn_fill_row_affine (curRow, prevRow, gap_row, align_row, dirMtx, c, cprev, 1, l - 1,
                       dncurRow, pdncurRow, htcurRow, open_gap);
    algn_fill_last_column_affine (curRow, prevRow, tlc, tlcprev, l - 1, dirMtx, dncurRow, pdncurRow, open_gap);
    return;
}

void
algn_fill_first_row_affine (int *curRow, DIR_MTX_ARROW_t *dirMtx, int len, int const *gap_row,
                            int *dncurRow, int *htcurRow, int open_gap) {
    int i;
    /* We fill the first cell to start with */
    curRow[0] = open_gap;
    dncurRow[0] = htcurRow[0] = VERY_LARGE_NUMBER;
    dirMtx[0] = ALIGN | ALIGN_V | ALIGN_H;
    /* Now the rest of the row */
    if (DEBUG_DIR_M)
        printf ("A\t");
    if (DEBUG_COST_M)
        printf ("%d\t", curRow[0]);
    for (i = 1; i < len; i++) {
        dncurRow[i] = VERY_LARGE_NUMBER;
        curRow[i] = curRow[i - 1] + gap_row[i];
        dirMtx[i] = INSERT | (INSERT_H);
        if (DEBUG_DIR_M)
            printf ("I\t");
        if (DEBUG_COST_M)
            printf ("%d\t", curRow[i]);
    }
    return;
}

void
algn_fill_first_cell_affine (int *curRow,
                             int prevRow,
                             DIR_MTX_ARROW_t *dirMtx,
                             int gap_char,
                             int *dncurRow,
                             int *pdncurRow,
                             int *htcurRow) {
    htcurRow[0] = VERY_LARGE_NUMBER;
    curRow[0] += gap_char;
    *dirMtx = DELETE | DELETE_V;
    dncurRow[0] = gap_char + pdncurRow[0];
    if (DEBUG_DIR_M)
        printf ("D\t");
    if (DEBUG_COST_M)
        printf ("%d\t", *curRow);
    return;
}

/* In the following three functions, we maintain the following invariants in
 * each loop:
 * 1. curRow is a row that has not been filled and is the next to be.
 * 4. dirMtx is the current row of the direction matrix
 * 2. prevRow is the previous row, located right above curRow, which has been filled
 *    already.
 * 3. i is the number of the row of curRow in its containing matrix
 * 5. gap_row is the cost of aligning each base of seq2 with a gap. This is
 *    constant for all the loop.
 * 6. cur_seq1 is the i'th base of seq1
 * 7. const_val is the cost of cur_seq1 aligned with a gap
 * 8. align_row is the vector of costs of aligning seq2 with cur_seq1
 */

int *
algn_fill_extending_right_affine (const seq_p seq1,
                                  int *precalcMtx,
                                  int seq1_len,
                                  int seq2_len,
                                  int *curRow,
                                  int *prevRow,
                                  DIR_MTX_ARROW_t *dirMtx,
                                  const cost_matrices_2d_p c,
                                  int start_row,
                                  int end_row, int len,
                                  int *dncurRow,
                                  int *pdncurRow,
                                  int *htcurRow,
                                  int open_gap) {
    int i;
    int *tmp, *tmp1, cur_seq1, const_val, prev_seq1, prev_const_val;
    const int *gap_row, *align_row;
    /** Invariants block
     *  len is the number of items in the row to be filled
     */
    i = start_row;
    /* This is what we will perform conceptually, I will stop using the
     * cm_get_precal_row function to speed this up a little bit
     * gap_row = cm_get_precal_row (precalcMtx, cm_get_gap (c), seq2_len);
    */
    gap_row = precalcMtx + (c->gap_char * seq2_len);
    while (i < end_row) {
        /** Invariants block */
        assert (i > 0);
        prev_seq1 = seq1->seq_begin[i - 1];
        cur_seq1 = seq1->seq_begin[i];
        const_val = cm_calc_cost (c->cost, cur_seq1, c->gap_char, c->costMtxDimension);
        prev_const_val = cm_calc_cost (c->cost, prev_seq1, c->gap_char, c->costMtxDimension);
        /* This is conceptually what we do in the next line
         * align_row = cm_get_precal_row (precalcMtx, cur_seq1, seq2_len);
         */
        align_row = precalcMtx + (cur_seq1 * seq2_len);
        /* Align! */
        algn_fill_first_cell_affine (curRow, prevRow[0], dirMtx, align_row[0], dncurRow, pdncurRow, htcurRow);
        algn_fill_row_affine (curRow, prevRow, gap_row, align_row, dirMtx, const_val,
                           prev_const_val, 1, len - 2, dncurRow, pdncurRow, htcurRow, open_gap);
        algn_fill_ukk_right_cell_affine (curRow, prevRow, gap_row, align_row, dirMtx,
                                      const_val, prev_const_val, len - 1, dncurRow, htcurRow, open_gap);
        /** Invariants block */
        tmp = curRow;
        tmp1 = dncurRow;
        curRow = prevRow;
        dncurRow = pdncurRow;
        prevRow = tmp;
        pdncurRow = tmp1;
        i++;
        dirMtx += seq2_len;
        len++;
        curRow[0] = prevRow[0];
    }
    return (curRow);
}

int *
algn_fill_extending_left_right_affine (const seq_p seq1,
                                       int *precalcMtx,
                                       int seq1_len,
                                       int seq2_len,
                                       int *curRow,
                                       int *prevRow,
                                       DIR_MTX_ARROW_t *dirMtx,
                                       const cost_matrices_2d_p c,
                                       int start_row,
                                       int end_row,
                                       int start_column,
                                       int len,
                                       int *dncurRow,
                                       int *pdncurRow,
                                       int *htcurRow,
                                       int open_gap) {
    int i;
    int *tmp, *tmp1, cur_seq1, const_val, prev_seq1, prev_const_val;
    const int *gap_row, *align_row;
    /** Invariants block
     * len is the number of cells to fill in the current row minus 1
     * start_column is the first cell to fill in the row */
    i = start_row;
    /* Conceptually,
       gap_row = cm_get_precal_row (precalcMtx, cm_get_gap (c), seq2_len);
    */
    gap_row = precalcMtx + (c->gap_char * seq2_len);
    len--;
    while (i < end_row) {
        /** Invariants block */
        assert (i > 0);
        prev_seq1 = seq1->seq_begin[i - 1];
        cur_seq1 = seq1->seq_begin[i];
        const_val = cm_calc_cost (c->cost, cur_seq1, c->gap_char, c->costMtxDimension);
        prev_const_val = cm_calc_cost (c->cost, prev_seq1, c->gap_char, c->costMtxDimension);
        /* Conceptually,
         * align_row = cm_get_precal_row (precalcMtx, cur_seq1, seq2_len);
         */
        align_row = precalcMtx + (cur_seq1 * seq2_len);
        /* Align! */
        algn_fill_ukk_left_cell_affine (curRow, prevRow, gap_row, align_row, dirMtx, const_val,
                                     prev_const_val, start_column, dncurRow, pdncurRow, htcurRow, open_gap);
        algn_fill_row_affine (curRow, prevRow, gap_row, align_row, dirMtx, const_val,
                           prev_const_val, start_column + 1, start_column + (len - 2),
                           dncurRow, pdncurRow, htcurRow, open_gap);
        algn_fill_ukk_right_cell_affine (curRow, prevRow, gap_row, align_row, dirMtx, const_val,
                                      prev_const_val, start_column + len - 1, dncurRow, htcurRow, open_gap);
        /** Invariants block */
        tmp = curRow;
        tmp1 = dncurRow;
        curRow = prevRow;
        dncurRow = pdncurRow;
        prevRow = tmp;
        pdncurRow = tmp1;
        i++;
        dirMtx += seq2_len;
        start_column++;
    }
    return (curRow);
}

int *
algn_fill_extending_left_affine (const seq_p seq1,
                                 int *precalcMtx,
                                 int seq1_len,
                                 int seq2_len,
                                 int *curRow,
                                 int *prevRow,
                                 DIR_MTX_ARROW_t *dirMtx,
                                 const cost_matrices_2d_p c,
                                 int start_row,
                                 int end_row,
                                 int start_column,
                                 int len,
                                 int *dncurRow,
                                 int *pdncurRow,
                                 int *htcurRow,
                                 int open_gap) {
    int i;
    int *tmp, *tmp1, cur_seq1, const_val, prev_seq1, prev_const_val,
        const_val_tail, prev_const_val_tail;
    const int *gap_row, *align_row;
    /** Invariants block
     *  start_column is the first cell to fill in the row
     *  len is the number of cells to fill in the current row minus 1
     */
    i = start_row;
    /* Conceptually,
     * gap_row = cm_get_precal_row (precalcMtx, cm_get_gap (c), seq2_len);
     */
    gap_row = precalcMtx + (c->gap_char * seq2_len);
    while (i < end_row) {
        /** Invariants block */
        assert (i > 0);
        prev_seq1 = seq1->seq_begin[i - 1];
        cur_seq1 = seq1->seq_begin[i];
        prev_const_val = cm_calc_cost (c->cost, prev_seq1, c->gap_char, c->costMtxDimension);
        const_val = cm_calc_cost (c->cost, cur_seq1, c->gap_char, c->costMtxDimension);
        const_val_tail = (cm_get_tail_cost (c))[cur_seq1];
        prev_const_val_tail = (cm_get_tail_cost (c))[prev_seq1];
        /* Conceptually,
         * align_row = cm_get_precal_row (precalcMtx, cur_seq1, seq2_len);
         */
        align_row = precalcMtx + (cur_seq1 * seq2_len);
        /* Align! */
        algn_fill_ukk_left_cell_affine (curRow, prevRow, gap_row, align_row, dirMtx, const_val,
                                     prev_const_val, start_column, dncurRow, pdncurRow, htcurRow, open_gap);
        algn_fill_row_affine (curRow, prevRow, gap_row, align_row, dirMtx, const_val, prev_const_val,
                           start_column + 1, start_column + len - 1, dncurRow, pdncurRow, htcurRow,
                           open_gap);
        algn_fill_last_column_affine (curRow, prevRow, const_val_tail, prev_const_val_tail,
                                   start_column + len - 1, dirMtx, dncurRow, pdncurRow, open_gap);
        /** Invariants block */
        tmp = curRow;
        tmp1 = dncurRow;
        curRow = prevRow;
        dncurRow = pdncurRow;
        prevRow = tmp;
        pdncurRow = tmp1;
        i++;
        dirMtx += seq2_len;
        start_column++;
        len--;
    }
    if (DEBUG_COST_M) {
        printf ("A_A_A gap cost\n");
        fflush (stdout);
        for (i = 0; i < seq2_len; i++) {
            printf ("%d\t", gap_row[i]);
            fflush (stdout);
        }
        fflush (stdout);
    }

    return (curRow);
}

int *
algn_fill_no_extending_affine (const seq_p seq1, int *precalcMtx, int seq1_len,
                            int seq2_len,  int *curRow, int *prevRow,
                            DIR_MTX_ARROW_t *dirMtx, const cost_matrices_2d_p c,
                            int start_row, int end_row, int *dncurRow, int *pdncurRow, int *htcurRow, int open_gap) {
    int i;
    int *tmp, cur_seq1, const_val, const_val_tail, prev_seq1, prev_const_val, prev_const_val_tail, *tmp1;
    const int *gap_row, *align_row;
    /** Invariants block */
    i = start_row;
    /* Conceptually,
     * gap_row = cm_get_precal_row (precalcMtx, cm_get_gap (c), seq2_len);
     */
    gap_row = precalcMtx + (c->gap_char * seq2_len);
    while (i < end_row) {
        /** Invariants block */
        assert (i > 0);
        prev_seq1 = seq1->seq_begin[i - 1];
        cur_seq1 = seq1->seq_begin[i];
        const_val = cm_calc_cost (c->cost, cur_seq1, c->gap_char, c->costMtxDimension);
        prev_const_val = cm_calc_cost (c->cost, prev_seq1, c->gap_char, c->costMtxDimension);
        const_val_tail = (cm_get_tail_cost (c))[cur_seq1];
        prev_const_val_tail = (cm_get_tail_cost (c))[prev_seq1];
        /* Conceptually,
         * align_row = cm_get_precal_row (precalcMtx, cur_seq1, seq2_len);
         */
        align_row = precalcMtx + (cur_seq1 * seq2_len);
        /* Align! */
        algn_fill_first_cell_affine (curRow, prevRow[0], dirMtx, open_gap, dncurRow, pdncurRow, htcurRow);
        algn_fill_row_affine (curRow, prevRow, gap_row, align_row, dirMtx, const_val,
                           prev_const_val, 1, seq2_len - 1, dncurRow, pdncurRow, htcurRow, open_gap);
        algn_fill_last_column_affine (curRow, prevRow, const_val_tail,
                                   prev_const_val_tail, seq2_len - 1, dirMtx, dncurRow,
                                   pdncurRow, open_gap);
        /** Invariants block */
        tmp = curRow;
        tmp1 = dncurRow;
        curRow = prevRow;
        dncurRow = pdncurRow;
        prevRow = tmp;
        pdncurRow = tmp1;
        i++;
        dirMtx += seq2_len;
    }
    return (curRow);
}

/* SicurRowilar to the previous but when no barriers are set */
static inline int
algn_fill_plane_affine (const seq_p seq1, int *precalcMtx, int seq1_len,
                     int seq2_len, int *curRow, DIR_MTX_ARROW_t *dirMtx, const cost_matrices_2d_p c,
                     int *dncurRow, int *htcurRow, int open_gap) {
    int i;
    const int *align_row;
    int const_val, const_val_tail, prev_const_val, prev_const_val_tail, *newNWMtx, *tmp, *tmp1, *pdncurRow;
    const int *gap_row, *first_gap_row;
    int gapcode;
    /* A precalculated cost of a gap aligned with each base in the array */
    gapcode = cm_get_gap_char_2d (c);
    gap_row = cm_get_precal_row (precalcMtx, gapcode, seq2_len);
    first_gap_row = cm_get_precal_row (precalcMtx, 0, seq2_len);
    newNWMtx = curRow;
    pdncurRow = dncurRow;
    curRow[0] = open_gap;
    dirMtx[0] = ALIGN | ALIGN_H | ALIGN_V;
    htcurRow[0] = VERY_LARGE_NUMBER;
    dncurRow[0] = VERY_LARGE_NUMBER;
    if (DEBUG_COST_M)
        printf ("%d\t", curRow[0]);
    if (DEBUG_DIR_M)
        printf ("A\t");
    /* We fill the first row to start with */
    for (i = 1; i < seq2_len; i++) {
        dncurRow[i] = VERY_LARGE_NUMBER;
        curRow[i] = curRow[i - 1] + first_gap_row[i];
        dirMtx[i] = INSERT | INSERT_H;
        if (DEBUG_COST_M) {
            printf ("%d\t", curRow[i]);
            fflush (stdout);
        }
        if (DEBUG_DIR_M)
            printf ("I\t");
    }
    curRow += seq2_len;
    curRow[0] = newNWMtx[0];
    newNWMtx[0] = 0;
    if (DEBUG_DIR_M || DEBUG_COST_M) {
        printf ("\n");
        fflush (stdout);
    }
    dncurRow += seq2_len;
    /* Now we fill the rest of the matrix */
    for (i = 1, dirMtx += seq2_len; i < seq1_len; i++, dirMtx += seq2_len) {
        prev_const_val_tail = (cm_get_tail_cost (c))[seq_get_element(seq1, i - 1)];
        prev_const_val = cm_calc_cost (c->cost, seq_get_element(seq1, i - 1), c->gap_char, c->costMtxDimension);
        const_val_tail = (cm_get_tail_cost (c))[seq_get_element(seq1, i)];
        const_val = cm_calc_cost (c->cost, seq_get_element(seq1, i), c->gap_char, c->costMtxDimension);
        align_row = cm_get_precal_row (precalcMtx, seq_get_element (seq1, i), seq2_len);
        algn_fill_full_row_affine (curRow, newNWMtx, gap_row, align_row, dirMtx, const_val,
                                prev_const_val, const_val_tail, prev_const_val_tail, seq2_len,
                                dncurRow, pdncurRow, htcurRow, open_gap);
        if (DEBUG_COST_M) {
            printf ("\n");
            fflush (stdout);
        }
        /* We swap curRow and newNWMtx for the next round */
        tmp = curRow;
        tmp1 = dncurRow;
        curRow = newNWMtx;
        dncurRow = pdncurRow;
        newNWMtx = tmp;
        pdncurRow = tmp1;
        curRow[0] = newNWMtx[0];
    }
    return (newNWMtx[seq2_len - 1]);
}

static inline void
algn_choose_affine_other (int *next_row, int *curRow, int **next_dncurRow,
                       int **next_pdncurRow, int *dncurRow, int *pdncurRow) {
    if (next_row == curRow) {
        *next_dncurRow = dncurRow;
        *next_pdncurRow = pdncurRow;
    }
    else {
        *next_dncurRow = pdncurRow;
        *next_pdncurRow = dncurRow;
    }
    return;
}

#define ALIGN_TO_ALIGN      1
#define ALIGN_TO_VERTICAL   2
#define ALIGN_TO_HORIZONTAL 4
#define ALIGN_TO_DIAGONAL   8
#define BEGIN_BLOCK         16
#define END_BLOCK           32
#define BEGIN_VERTICAL      64
#define END_VERTICAL        128
#define BEGIN_HORIZONTAL    256
#define END_HORIZONTAL      512
#define DO_ALIGN            1024
#define DO_VERTICAL         2048
#define DO_HORIZONTAL       4096
#define DO_DIAGONAL         8192
// DO_DIAGONAL MUST BE THE LAST ONE

#define TMPGAP 16
#define NTMPGAP 15

#define LOR_WITH_DIR_MTX_ARROW_t(mask, direction_matrix) direction_matrix |= mask

static inline int
HAS_GAP_EXTENSION (SEQT base, const cost_matrices_2d_p c) {
    return (cm_calc_cost(c->cost, base, c->gap_char, c->costMtxDimension));
}

static inline int
HAS_GAP_OPENING (SEQT prev, SEQT curr, int gap_char, int gap_open) {
    if ((!(gap_char & prev)) && (gap_char & curr)) return 0;
    else return gap_open;
}


static inline void
FILL_EXTEND_HORIZONTAL_NOBT (int sj_horizontal_extension, int sj_gap_extension, int sj_gap_opening, int j,
                             int *extend_horizontal, const cost_matrices_2d_p c,
                             const int *close_block_diagonal) {
    int ext_cost, open_cost;
    ext_cost = extend_horizontal[j - 1] + sj_horizontal_extension;
    open_cost = close_block_diagonal[j - 1] +
                sj_gap_opening + sj_gap_extension;
    if (DEBUG_AFFINE) {
        printf("\nFILL_EXTEND_HORIZONTAL_NOBT\n");
        printf ("Ext cost: %d, Open cost: %d, Gap extension: %d, gap opening: %d, sj_horizontal_extension: %d\n",
                ext_cost, open_cost, sj_gap_extension, sj_gap_opening, sj_horizontal_extension);
    }
    if (ext_cost < open_cost)
        extend_horizontal[j] = ext_cost;
    else
        extend_horizontal[j] = open_cost;
    if (DEBUG_AFFINE)
        printf ("The final cost is %d\n", extend_horizontal[j]);
    return;
}

DIR_MTX_ARROW_t
FILL_EXTEND_HORIZONTAL (int sj_horizontal_extension, int sj_gap_extension, int sj_gap_opening, int j,
                        int *extend_horizontal, const cost_matrices_2d_p c,
                        const int *close_block_diagonal, DIR_MTX_ARROW_t direction_matrix) {
    int ext_cost, open_cost;
    ext_cost = extend_horizontal[j - 1] + sj_horizontal_extension;
    open_cost = close_block_diagonal[j - 1] +
                sj_gap_opening + sj_gap_extension;
    if (DEBUG_AFFINE) {
        printf("\nFILL_EXTEND_HORIZONTAL\n");
        printf ("Ext cost: %d, Open cost: %d, Gap extension: %d, gap opening: %d, sj_horizontal_extension: %d\n",
                ext_cost, open_cost, sj_gap_extension, sj_gap_opening, sj_horizontal_extension);
    }
    if (ext_cost < open_cost) {
        LOR_WITH_DIR_MTX_ARROW_t(BEGIN_HORIZONTAL, direction_matrix);
        extend_horizontal[j] = ext_cost;
    }
    else {
        LOR_WITH_DIR_MTX_ARROW_t(END_HORIZONTAL, direction_matrix);
        extend_horizontal[j] = open_cost;
    }
    if (DEBUG_AFFINE)
        printf ("The final cost is %d\n", extend_horizontal[j]);
    return (direction_matrix);
}

void
FILL_EXTEND_VERTICAL_NOBT (int si_vertical_extension, int si_gap_extension, int si_gap_opening, int j,
                           int *extend_vertical, const int *prev_extend_vertical, const cost_matrices_2d_p c,
                           const int *prev_close_block_diagonal) {
    int ext_cost, open_cost;
    ext_cost = prev_extend_vertical[j] + si_vertical_extension;
    open_cost = prev_close_block_diagonal[j] +
        si_gap_opening + si_gap_extension;
    if (ext_cost < open_cost)
        extend_vertical[j] = ext_cost;
    else
        extend_vertical[j] = open_cost;
    return;
}

DIR_MTX_ARROW_t
FILL_EXTEND_VERTICAL (int si_vertical_extension, int si_gap_extension, int si_gap_opening, int j,
                      int *extend_vertical, const int *prev_extend_vertical, const cost_matrices_2d_p c,
                      const int *prev_close_block_diagonal,
                      DIR_MTX_ARROW_t direction_matrix) {
    int ext_cost, open_cost;
    ext_cost = prev_extend_vertical[j] + si_vertical_extension;
    open_cost = prev_close_block_diagonal[j] +
        si_gap_opening + si_gap_extension;
    if (ext_cost < open_cost) {
        LOR_WITH_DIR_MTX_ARROW_t(BEGIN_VERTICAL, direction_matrix);
        extend_vertical[j] = ext_cost;
    }
    else {
        LOR_WITH_DIR_MTX_ARROW_t(END_VERTICAL, direction_matrix);
        extend_vertical[j] = open_cost;
    }
    return (direction_matrix);
}

void
FILL_EXTEND_BLOCK_DIAGONAL_NOBT (SEQT si_base, SEQT sj_base, SEQT si_prev_base,
                                 SEQT sj_prev_base, int gap_open, int j,
                                 int *extend_block_diagonal, const int *prev_extend_block_diagonal,
                                 const int *prev_close_block_diagonal) {
    int ext_cost, open_cost;
    int diag, open_diag, flag, flag2;
    flag = ((TMPGAP & si_base) && (TMPGAP & sj_base));
    flag2= (!(TMPGAP & si_prev_base) && (!(TMPGAP & sj_base)));
    diag = flag ? 0 : VERY_LARGE_NUMBER;
    open_diag = flag ? (flag2 ? 0 : (2 * gap_open)) : VERY_LARGE_NUMBER;
    ext_cost = prev_extend_block_diagonal[j - 1] + diag;
    open_cost = prev_close_block_diagonal[j - 1] + open_diag;
    if (ext_cost < open_cost)
        extend_block_diagonal[j] = ext_cost;
    else
        extend_block_diagonal[j] = open_cost;
    return;
}

DIR_MTX_ARROW_t
FILL_EXTEND_BLOCK_DIAGONAL (SEQT si_base, SEQT sj_base, SEQT si_prev_base, SEQT sj_prev_base,
                            int gap_open, int j,
                            int *extend_block_diagonal, const int *prev_extend_block_diagonal,
                            const int *prev_close_block_diagonal,
                            DIR_MTX_ARROW_t direction_matrix) {
    int ext_cost, open_cost;
    int diag, open_diag;
    diag = ((TMPGAP & si_base) && (TMPGAP & sj_base)) ? 0 : VERY_LARGE_NUMBER;
    open_diag = (!(TMPGAP & si_prev_base) && (!(TMPGAP & sj_base)) && (TMPGAP & si_base) && (TMPGAP & sj_base))?
        0:(((TMPGAP & si_base) && (TMPGAP & sj_base))?(2 * gap_open) : VERY_LARGE_NUMBER);
    ext_cost = prev_extend_block_diagonal[j - 1] + diag;
    open_cost = prev_close_block_diagonal[j - 1] + diag;
    if (ext_cost < open_cost) {
        LOR_WITH_DIR_MTX_ARROW_t(BEGIN_BLOCK, direction_matrix);
        extend_block_diagonal[j] = ext_cost;
    }
    else {
        LOR_WITH_DIR_MTX_ARROW_t(END_BLOCK, direction_matrix);
        extend_block_diagonal[j] = open_cost;
    }
    return (direction_matrix);
}

void
FILL_CLOSE_BLOCK_DIAGONAL_NOBT(SEQT si_base, SEQT sj_base, SEQT si_no_gap,
                               SEQT sj_no_gap, int si_gap_opening, int sj_gap_opening, int j,
                               const int *c, int *close_block_diagonal,
                               const int *prev_close_block_diagonal, const int *prev_extend_vertical,
                               const int *prev_extend_horizontal, const int *prev_extend_block_diagonal) {
    int diag, extra_gap_opening;
    int algn, from_vertical, from_horizontal, from_diagonal;
    diag = c[sj_no_gap];
    /*
    diag = cm_calc_cost(c->cost, si_no_gap, sj_no_gap, c->costMtxDimension);
    */
    extra_gap_opening = (sj_gap_opening < si_gap_opening)?si_gap_opening:sj_gap_opening;
    if (DEBUG_AFFINE) {
        //printf ("Between %d and %d: Diag : %d, Extra gap opening: %d\n", si_no_gap, sj_no_gap, diag, extra_gap_opening);
        //fflush (stdout);
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
    close_block_diagonal[j] = algn;
    if (close_block_diagonal[j] > from_vertical)
        close_block_diagonal[j] = from_vertical;
    if (close_block_diagonal[j] > from_horizontal)
            close_block_diagonal[j] = from_horizontal;
    if (close_block_diagonal[j] > from_diagonal)
            close_block_diagonal[j] = from_diagonal;
    return;
}

DIR_MTX_ARROW_t
FILL_CLOSE_BLOCK_DIAGONAL(SEQT si_base, SEQT sj_base, SEQT si_no_gap, SEQT sj_no_gap,
                          int si_gap_opening, int sj_gap_opening, int j, const int *c,
                          int *close_block_diagonal,
                          const int *prev_close_block_diagonal, const int *prev_extend_vertical,
                          const int *prev_extend_horizontal, const int *prev_extend_block_diagonal,
        DIR_MTX_ARROW_t direction_matrix) {
    int diag, extra_gap_opening;
    int algn, from_vertical, from_horizontal, from_diagonal;
    DIR_MTX_ARROW_t mask;
    diag = c[sj_no_gap];
    /*
        cm_calc_cost(c->cost, si_no_gap, sj_no_gap, c->costMtxDimension);
        */
    extra_gap_opening =
        (sj_gap_opening < si_gap_opening)?si_gap_opening:sj_gap_opening;
    if (DEBUG_AFFINE) {
        // printf ("Between %d and %d: Diag : %d, Extra gap opening: %d\n", si_no_gap, sj_no_gap, diag, extra_gap_opening);
        // fflush (stdout);
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
        }
        else mask = mask | ALIGN_TO_VERTICAL;
    }
    if (close_block_diagonal[j] >= from_horizontal) {
        if (close_block_diagonal[j] > from_horizontal) {
            close_block_diagonal[j] = from_horizontal;
            mask = ALIGN_TO_HORIZONTAL;
        }
        else mask = mask | ALIGN_TO_HORIZONTAL;
    }
    if (close_block_diagonal[j] >= from_diagonal) {
        if (close_block_diagonal[j] > from_diagonal) {
            close_block_diagonal[j] = from_diagonal;
            mask = ALIGN_TO_DIAGONAL;
        }
        else mask = mask | ALIGN_TO_DIAGONAL;
    }
    LOR_WITH_DIR_MTX_ARROW_t(mask, direction_matrix);
    return (direction_matrix);
}

enum MODE { m_todo, m_vertical, m_horizontal, m_diagonal, m_align } backtrace_mode;


void
algn_backtrace_affine (const seq_p shortSeq,
                       const seq_p longSeq,
                       DIR_MTX_ARROW_t  *direction_matrix,
                       seq_p median,
                       seq_p medianwg,
                       seq_p retShortSeq,
                       seq_p retLongSeq,
                       const cost_matrices_2d_p costMtx) {
#define HAS_FLAG(flag) (*direction_matrix & flag)
    enum MODE mode = m_todo;
    int shortIdx,
        longIdx,
        lenShortSeq,
        len_longerSeq;

    SEQT shortSeqElem,
         longSeqElem,
         prep;

    DIR_MTX_ARROW_t *initial_direction_matrix;

    shortIdx      = seq_get_len(shortSeq) - 1;
    longIdx       = seq_get_len(longSeq) - 1;
    lenShortSeq   = shortIdx;
    len_longerSeq = longIdx;

    assert (lenShortSeq <= len_longerSeq);

    shortSeqElem             = seq_get_element(shortSeq, shortIdx);
    longSeqElem              = seq_get_element(longSeq, longIdx);
    initial_direction_matrix = direction_matrix;
    direction_matrix         = direction_matrix + (((lenShortSeq + 1) * (len_longerSeq + 1)) - 1);
    while ((shortIdx != 0) && (longIdx != 0)) {
        if (DEBUG_AFFINE) {
            printf ("In position %d %d of affine backtrace\n", shortIdx, longIdx);
            fflush (stdout);
        }
        assert (initial_direction_matrix < direction_matrix);
        if (mode == m_todo) {
            if (HAS_FLAG(DO_HORIZONTAL)) {
                mode = m_horizontal;
            } else if (HAS_FLAG(DO_ALIGN)) {
                mode = m_align;
            } else if (HAS_FLAG(DO_VERTICAL)) {
                mode = m_vertical;
            } else {
                assert (HAS_FLAG(DO_DIAGONAL));
                mode = m_diagonal;
            }
        } else if (mode == m_vertical) {
            if (HAS_FLAG(END_VERTICAL)) {
                mode = m_todo;
            }
            if (!(shortSeqElem & TMPGAP)) {
                seq_prepend (median, (shortSeqElem | TMPGAP));
                seq_prepend (medianwg, (shortSeqElem | TMPGAP));
            } else {
                seq_prepend (medianwg, TMPGAP);
            }
            seq_prepend(retShortSeq, shortSeqElem);
            seq_prepend(retLongSeq, TMPGAP);
            shortIdx--;
            direction_matrix -= (len_longerSeq + 1);
            shortSeqElem = seq_get_element(shortSeq, shortIdx);
        } else if (mode == m_horizontal) {
            if (HAS_FLAG(END_HORIZONTAL)) {
                mode = m_todo;
            }
            if (!(longSeqElem & TMPGAP)) {
                seq_prepend (median, (longSeqElem | TMPGAP));
                seq_prepend (medianwg, (longSeqElem | TMPGAP));
            } else {
                seq_prepend (medianwg, TMPGAP);
            }
            seq_prepend (retShortSeq, TMPGAP);
            seq_prepend (retLongSeq, longSeqElem);
            longIdx--;
            direction_matrix -= 1;
            longSeqElem = seq_get_element(longSeq, longIdx);
        } else if (mode == m_diagonal) {
            if (HAS_FLAG(END_BLOCK)) {
                mode = m_todo;
            }
            seq_prepend(retShortSeq, shortSeqElem);
            seq_prepend(retLongSeq, longSeqElem);
            seq_prepend(medianwg, TMPGAP);
            shortIdx--;
            longIdx--;
            direction_matrix -= (len_longerSeq + 2);
            longSeqElem = seq_get_element(longSeq, longIdx);
            shortSeqElem = seq_get_element(shortSeq, shortIdx);
        } else {
            assert (mode == m_align);
            if (HAS_FLAG(ALIGN_TO_HORIZONTAL)) {
                mode = m_horizontal;
            } else if (HAS_FLAG(ALIGN_TO_DIAGONAL)) {
                mode = m_diagonal;
            } else if (HAS_FLAG(ALIGN_TO_VERTICAL)) {
                mode = m_vertical;
            }
            prep = cm_get_median(costMtx, (shortSeqElem & (NTMPGAP)), (longSeqElem & (NTMPGAP)));
            seq_prepend(median, prep);
            seq_prepend(medianwg, prep);
            seq_prepend(retShortSeq, shortSeqElem);
            seq_prepend(retLongSeq, longSeqElem);
            shortIdx--;
            longIdx--;
            direction_matrix -= (len_longerSeq + 2);
            longSeqElem = seq_get_element(longSeq, longIdx);
            shortSeqElem = seq_get_element(shortSeq, shortIdx);
        }
    }
    while (shortIdx != 0) {
        assert (initial_direction_matrix < direction_matrix);
        if (!(shortSeqElem & TMPGAP)) {
            seq_prepend (median, (shortSeqElem | TMPGAP));
            seq_prepend (medianwg, (shortSeqElem | TMPGAP));
        } else {
            seq_prepend (medianwg, TMPGAP);
        }
        seq_prepend(retShortSeq, shortSeqElem);
        seq_prepend(retLongSeq, TMPGAP);
        direction_matrix -= (len_longerSeq + 1);
        shortIdx--;
        shortSeqElem = seq_get_element(shortSeq, shortIdx);
    }
    while (longIdx != 0) {
        assert (initial_direction_matrix < direction_matrix);
        if (!(longSeqElem & TMPGAP)) {
            seq_prepend (median, (longSeqElem | TMPGAP));
            seq_prepend (medianwg, (longSeqElem | TMPGAP));
        } else {
            seq_prepend (medianwg, TMPGAP);
        }
        seq_prepend (retShortSeq, TMPGAP);
        seq_prepend (retLongSeq, longSeqElem);
        longIdx--;
        direction_matrix -= 1;
        longSeqElem = seq_get_element(longSeq, longIdx);
    }
    seq_prepend(retShortSeq, TMPGAP);
    seq_prepend(retLongSeq, TMPGAP);
    seq_prepend(medianwg, TMPGAP);
    if (TMPGAP != seq_get_element(median, 0)) {
        seq_prepend(median, TMPGAP);
    }
#undef HAS_FLAG
    return;

}

void
print_array (char *title, int *arr, int max) {
    int i;
    printf ("%s", title);
    for (i = 0; i <= max; i++) {
        printf ("%d ", arr[i]);
    }
    printf ("\n");
    fflush (stdout);
    return;
}

void
print_dirMtx (char *title, DIR_MTX_ARROW_t *arr, int max) {
    int i;
    printf ("%s", title);
    for (i = 0; i <= max; i++) {
        printf ("%d ", arr[i]);
    }
    printf ("\n");
    fflush (stdout);
    return;
}

// nobt: no backtrace
void
algn_initialize_matrices_affine_nobt (int go,
                                      const seq_p si,
                                      const seq_p sj,
                                      const cost_matrices_2d_p c,
                                      int *close_block_diagonal,
                                      int *extend_block_diagonal,
                                      int *extend_vertical,
                                      int *extend_horizontal,
                                      const int *precalcMtx) {
    int lenShortSeq,
        len_longerSeq,
        i = 1,
        j = 1,
        r;
    int *prev_extend_vertical;
    const int *gap_row;
    SEQT longSeqElem,
         longSeqPrevElem,
         shortSeqElem,
         shortSeqPrevElem;

    lenShortSeq              = seq_get_len(si) - 1;
    len_longerSeq            = seq_get_len(sj) - 1;
    close_block_diagonal[0]  = 0;
    extend_block_diagonal[0] = 0;
    extend_horizontal[0]     = go;
    extend_vertical[0]       = go;
    gap_row                  = cm_get_precal_row(precalcMtx, 0, len_longerSeq);

    if (DEBUG_AFFINE) {
        printf("initialize_matrices_affine_nobt\n");
        printf ("\n\nThe gap opening parameter is %d\n", go);
        printf ("\nPre-initialized values:\n");
        print_array ("EH: ", extend_horizontal,     len_longerSeq);
        print_array ("EV: ", extend_vertical,       len_longerSeq);
        print_array ("EB: ", extend_block_diagonal, len_longerSeq);
        print_array ("CB: ", close_block_diagonal,  len_longerSeq);
    }
    for (; j <= len_longerSeq; j++) {
        longSeqElem              = seq_get_element(sj, j);
        longSeqPrevElem          = seq_get_element(sj, j - 1);
        r                        = extend_horizontal[j - 1] + gap_row[j];
        extend_horizontal[j]     = r;
        close_block_diagonal[j]  = r;
        extend_block_diagonal[j] = VERY_LARGE_NUMBER;
        extend_vertical[j]       = VERY_LARGE_NUMBER;
    }
    if (DEBUG_AFFINE) {
        printf("initialize_matrices_affine_nobt\n");
        printf ("\nInitialized values:\n");
        print_array ("EH: ", extend_horizontal,     len_longerSeq);
        print_array ("EV: ", extend_vertical,       len_longerSeq);
        print_array ("EB: ", extend_block_diagonal, len_longerSeq);
        print_array ("CB: ", close_block_diagonal,  len_longerSeq);
        printf ("Finished initialization\n\n");
    }
    /* for (; i <= lenShortSeq; i++) { */
        prev_extend_vertical     = extend_vertical;
        extend_vertical         += (1 + len_longerSeq);
        close_block_diagonal    += (1 + len_longerSeq);
        extend_block_diagonal   += (1 + len_longerSeq);
        extend_horizontal       += (1 + len_longerSeq);
        shortSeqElem             = seq_get_element(si, i);
        shortSeqPrevElem         = seq_get_element(si, i - 1);
        r                        = prev_extend_vertical[0] + (HAS_GAP_EXTENSION(shortSeqElem, c));
        extend_horizontal[0]     = VERY_LARGE_NUMBER;
        close_block_diagonal[0]  = r;
        extend_block_diagonal[0] = VERY_LARGE_NUMBER;
        extend_vertical[0]       = r;
    /* } */
    return;
}


void
algn_initialize_matrices_affine (int go,
                                 const seq_p shortSeq,
                                 const seq_p longSeq,
                                 const cost_matrices_2d_p costMtx,
                                 int *close_block_diagonal,
                                 int *extend_block_diagonal,
                                 int *extend_vertical,
                                 int *extend_horizontal,
                                 int *final_cost_matrix,
                                 DIR_MTX_ARROW_t  *direction_matrix,
                                 const int *precalcMtx) {
    if (DEBUG_AFFINE) {
        printf("\ninitialize_matrices_affine\n");
        fflush(stdout);
    }
    int lenShortSeq, len_longerSeq, i = 1, j = 1, r;
    int *prev_extend_vertical;
    const int *gap_row;
    SEQT longSeqElem, longSeqPrevElem, shortSeqElem, shortSeqPrevElem;
    lenShortSeq = seq_get_len(shortSeq) - 1; //TODO: is this for deleting opening gap? This is currently unused
    len_longerSeq  = seq_get_len(longSeq)  - 1; //TODO: is this for deleting opening gap?
    final_cost_matrix[0]     = 0;
    close_block_diagonal[0]  = 0;
    extend_block_diagonal[0] = 0;
    extend_horizontal[0]     = go;
    extend_vertical[0]       = go;
    direction_matrix[0]      = 0xFFFF;
    gap_row = cm_get_precal_row(precalcMtx, 0, len_longerSeq);
    if (DEBUG_AFFINE) {
        printf ("\n\nThe gap opening parameter is %d\n", go);
        printf ("\nPre-initialized values:\n");
        print_array ("EH: ", extend_horizontal,     len_longerSeq);
        print_array ("EV: ", extend_vertical,       len_longerSeq);
        print_array ("EB: ", extend_block_diagonal, len_longerSeq);
        print_array ("CB: ", close_block_diagonal,  len_longerSeq);
        print_array ("FC: ", final_cost_matrix,     len_longerSeq);
        print_dirMtx ("DM: ", direction_matrix,     len_longerSeq);
    }
    for (; j <= len_longerSeq; j++) {
        longSeqElem              = seq_get_element(longSeq, j);
        longSeqPrevElem          = seq_get_element(longSeq, j - 1);
        r                        = extend_horizontal[j - 1] + gap_row[j];

        extend_horizontal[j]     = r;
        close_block_diagonal[j]  = r;
        final_cost_matrix[j]     = r;
        extend_block_diagonal[j] = VERY_LARGE_NUMBER;
        extend_vertical[j]       = VERY_LARGE_NUMBER;
        direction_matrix[j]      = DO_HORIZONTAL | END_HORIZONTAL;
    }
    if (DEBUG_AFFINE) {
        printf ("\nInitialized values:\n");
        print_array ("EH: ", extend_horizontal,     len_longerSeq);
        print_array ("EV: ", extend_vertical,       len_longerSeq);
        print_array ("EB: ", extend_block_diagonal, len_longerSeq);
        print_array ("CB: ", close_block_diagonal,  len_longerSeq);
        print_array ("FC: ", final_cost_matrix,     len_longerSeq);
        print_dirMtx ("DM: ", direction_matrix,     len_longerSeq);
        printf ("Finished initializing.\n");
    }
    /* for (; i <= lenShortSeq; i++) { */
        prev_extend_vertical   = extend_vertical;
        extend_vertical       += (1 + len_longerSeq);
        close_block_diagonal  += (1 + len_longerSeq);
        final_cost_matrix     += (1 + len_longerSeq);
        extend_block_diagonal += (1 + len_longerSeq);
        extend_horizontal     += (1 + len_longerSeq);
        direction_matrix      += (1 + len_longerSeq);

        shortSeqElem             = seq_get_element(shortSeq, i);
        shortSeqPrevElem         = seq_get_element(shortSeq, i - 1);
        r                        = prev_extend_vertical[0] + (HAS_GAP_EXTENSION(shortSeqElem, costMtx));

        extend_horizontal[0]     = VERY_LARGE_NUMBER;
        close_block_diagonal[0]  = r;
        final_cost_matrix[0]     = r;
        extend_block_diagonal[0] = VERY_LARGE_NUMBER;
        extend_vertical[0]       = r;
        direction_matrix[0]      = DO_VERTICAL | END_VERTICAL;
    /* } */
    return;
}

DIR_MTX_ARROW_t
ASSIGN_MINIMUM (int *final_cost_matrix,
                int extend_horizontal,
                int extend_vertical,
                int extend_block_diagonal,
                int close_block_diagonal,
                DIR_MTX_ARROW_t direction_matrix) {
    int mask;
    mask                        = DO_HORIZONTAL;
    *final_cost_matrix          = extend_horizontal;
    if (*final_cost_matrix     >= extend_vertical) {
        if (*final_cost_matrix > extend_vertical) {
            *final_cost_matrix = extend_vertical;
            mask               = DO_VERTICAL;
        } else {
            mask = mask | DO_VERTICAL;
        }
    }
    if (*final_cost_matrix >= extend_block_diagonal) {
        if (*final_cost_matrix > extend_block_diagonal) {
            *final_cost_matrix = extend_block_diagonal;
            mask               = DO_DIAGONAL;
        } else {
            mask = mask | DO_DIAGONAL;
        }
    }
    if (*final_cost_matrix >= close_block_diagonal) {
        if (*final_cost_matrix > close_block_diagonal) {
            *final_cost_matrix = close_block_diagonal;
            mask = DO_ALIGN;
        } else {
            mask = mask | DO_ALIGN;
        }
    }
    LOR_WITH_DIR_MTX_ARROW_t(mask, direction_matrix);
    return (direction_matrix);
}

int
algn_fill_plane_2d_affine_nobt (const seq_p shortSeq,
                                const seq_p longSeq,
                                int lenShortSeq,
                                int len_longerSeq,
                                const cost_matrices_2d_p costMtx,
                                int *extend_horizontal,
                                int *extend_vertical,
                                int *close_block_diagonal,
                                int *extend_block_diagonal,
                                const int *precalcMtx,
                                int *gap_open_prec,
                                int *longSeq_horizontal_extension) {
    int start_pos = 1,
        end_pos,
        start_v   = 40,
        i = 1,
        j,
        res;

    int *prev_extend_horizontal,
        *prev_extend_vertical,
        *prev_close_block_diagonal,
        *prev_extend_block_diagonal;

    int *init_extend_horizontal,
        *init_extend_vertical,
        *init_close_block_diagonal,
        *init_extend_block_diagonal;

    const int *shortSeq_no_gap_vector;

    int shortSeq_gap_opening,
        shortSeq_gap_extension,
        longSeq_gap_opening,
        longSeq_gap_extension;

    int gap_char,
        gap_open;

    const int *gap_row;
    int shortSeq_vertical_extension;

    gap_char = costMtx->gap_char;
    gap_open = costMtx->gap_open;
    assert (len_longerSeq >= lenShortSeq);
    init_extend_horizontal     = extend_horizontal;
    init_extend_vertical       = extend_vertical;
    init_extend_block_diagonal = extend_block_diagonal;
    init_close_block_diagonal  = close_block_diagonal;
    gap_row = cm_get_precal_row(precalcMtx, 0, len_longerSeq);
    end_pos = (len_longerSeq - lenShortSeq) + 8;

    if (DEBUG_AFFINE) {
        printf("\n--algn fill plane 3 affine nobt\n");
        printf("Before initializing:\n");
        print_array ("EH: ", extend_horizontal,     len_longerSeq);
        print_array ("EV: ", extend_vertical,       len_longerSeq);
        print_array ("EB: ", extend_block_diagonal, len_longerSeq);
        print_array ("CB: ", close_block_diagonal,  len_longerSeq);
    }

    if (end_pos < 40) end_pos = 40;

    if (end_pos > len_longerSeq) end_pos = len_longerSeq;
    SEQT longSeqElem, longSeqPrevElem, shortSeqElem, shortSeqPrevElem, shortSeq_no_gap, longSeq_no_gap;
    SEQT *seq_begini, *seq_beginj;
    seq_begini = shortSeq->seq_begin;
    seq_beginj = longSeq->seq_begin;
    shortSeqElem = seq_begini[0];

    for (j = 1; j <= len_longerSeq; j++) {
        gap_open_prec[j] = HAS_GAP_OPENING(seq_beginj[j - 1], seq_beginj[j], gap_char, gap_open);
        if ((seq_beginj[j - 1] & gap_char) && (!(seq_beginj[j] & gap_char))) {
            longSeq_horizontal_extension[j] = gap_open_prec[j] + gap_row[j];
        } else {
            longSeq_horizontal_extension[j] = gap_row[j];
        }
    }

    longSeq_horizontal_extension[1] = gap_row[1];
    int r;

    for (;i <= lenShortSeq; i++) {
        prev_extend_horizontal = init_extend_horizontal +
            (((i - 1) % 2) * (len_longerSeq + 1));
        prev_extend_vertical = init_extend_vertical +
            ((len_longerSeq + 1) * ((i - 1) % 2));
        prev_extend_block_diagonal =
            init_extend_block_diagonal + ((len_longerSeq + 1) * ((i - 1) % 2));
        prev_close_block_diagonal = init_close_block_diagonal +
            ((len_longerSeq + 1) * ((i - 1) % 2));
        extend_horizontal     = init_extend_horizontal + ((i % 2) * (len_longerSeq + 1));
        extend_vertical       = init_extend_vertical + ((len_longerSeq + 1) * (i % 2));
        extend_block_diagonal =
            init_extend_block_diagonal + ((len_longerSeq + 1) * (i % 2));
        close_block_diagonal = init_close_block_diagonal + ((len_longerSeq + 1) * (i % 2));

        if (i > start_v) start_pos++;

        extend_horizontal[start_pos - 1] = VERY_LARGE_NUMBER;
        shortSeqPrevElem       = shortSeqElem;
        shortSeqElem           = seq_begini[i];
        shortSeq_gap_extension = HAS_GAP_EXTENSION(shortSeqElem, costMtx);
        shortSeq_gap_opening   = HAS_GAP_OPENING (shortSeqPrevElem, shortSeqElem, gap_char, gap_open);
        shortSeq_no_gap        = (NTMPGAP) & shortSeqElem;

        if ((i > 1) && ((shortSeqPrevElem & gap_char) && (!(shortSeqElem & gap_char)))) {
            shortSeq_vertical_extension = shortSeq_gap_opening + shortSeq_gap_extension;
        } else {
            shortSeq_vertical_extension = shortSeq_gap_extension;
        }

        r = prev_extend_vertical[start_pos - 1] + shortSeq_vertical_extension;
        extend_horizontal    [start_pos - 1] = VERY_LARGE_NUMBER;
        close_block_diagonal [start_pos - 1] = r;
        extend_block_diagonal[start_pos - 1] = VERY_LARGE_NUMBER;
        extend_vertical      [start_pos - 1] = r;
        longSeqElem                          = seq_beginj[start_pos - 1];
        close_block_diagonal [start_pos - 1] = VERY_LARGE_NUMBER;
        shortSeq_no_gap_vector = costMtx->cost + (shortSeq_no_gap << costMtx->costMtxDimension);

        for (j=start_pos; j <= end_pos; j++) {
            longSeqPrevElem       = longSeqElem;
            longSeqElem           = seq_beginj[j];
            longSeq_no_gap        = (NTMPGAP) & longSeqElem;
            longSeq_gap_extension = gap_row[j];
            longSeq_gap_opening   = gap_open_prec[j];
            FILL_EXTEND_HORIZONTAL_NOBT(longSeq_horizontal_extension[j], longSeq_gap_extension,
                                        longSeq_gap_opening, j,
                                        extend_horizontal, costMtx, close_block_diagonal);
            FILL_EXTEND_VERTICAL_NOBT(shortSeq_vertical_extension, shortSeq_gap_extension, shortSeq_gap_opening, j,
                                      extend_vertical, prev_extend_vertical, costMtx,
                                      prev_close_block_diagonal);
            FILL_EXTEND_BLOCK_DIAGONAL_NOBT(shortSeqElem, longSeqElem, shortSeqPrevElem, longSeqPrevElem, gap_open, j, extend_block_diagonal,
                                            prev_extend_block_diagonal,
                                            prev_close_block_diagonal);
            FILL_CLOSE_BLOCK_DIAGONAL_NOBT(shortSeqElem, longSeqElem, shortSeq_no_gap, longSeq_no_gap,
                                           shortSeq_gap_opening, longSeq_gap_opening, j, shortSeq_no_gap_vector, close_block_diagonal,
                                           prev_close_block_diagonal,
                                           prev_extend_vertical, prev_extend_horizontal,
                                           prev_extend_block_diagonal);
        }
        if (end_pos < len_longerSeq) {
            end_pos++;
            extend_vertical[end_pos] = VERY_LARGE_NUMBER;
            close_block_diagonal[end_pos] = VERY_LARGE_NUMBER;
            extend_horizontal[end_pos] = VERY_LARGE_NUMBER;
            extend_block_diagonal[end_pos] = VERY_LARGE_NUMBER;
        }
        if (DEBUG_AFFINE) {
            printf("algn fill plane 3 affine nobt\n");
            printf("After initializing:\n");
            print_array ("EH: ", extend_horizontal,     len_longerSeq);
            print_array ("EV: ", extend_vertical,       len_longerSeq);
            print_array ("EB: ", extend_block_diagonal, len_longerSeq);
            print_array ("CB: ", close_block_diagonal,  len_longerSeq);
        }
    }

    res = extend_horizontal[len_longerSeq];
    if (res > extend_vertical[len_longerSeq]) res = extend_vertical[len_longerSeq];
    if (res > extend_block_diagonal[len_longerSeq]) res = extend_block_diagonal[len_longerSeq];
    if (res > close_block_diagonal[len_longerSeq]) res = close_block_diagonal[len_longerSeq];

    return res;
}


int
algn_fill_plane_2d_affine (const seq_p shortSeq,
                           const seq_p longSeq,
                           int lenShortSeq,
                           int len_longerSeq,
                           int *final_cost_matrix,
                           DIR_MTX_ARROW_t  *direction_matrix,
                           const cost_matrices_2d_p c,
                           int *extend_horizontal,
                           int *extend_vertical,
                           int *close_block_diagonal,
                           int *extend_block_diagonal,
                           const int *precalcMtx,
                           int *gap_open_prec,
                           int *longSeq_horizontal_extension) {
    if (DEBUG_AFFINE) {
        printf("algn_fill_plane_3_affine\n");
        fflush(stdout);
    }
    int start_pos = 1, end_pos, start_v = 40, i = 1, j, res;
    int *prev_extend_horizontal,
        *prev_extend_vertical,
        *prev_close_block_diagonal,
        *prev_extend_block_diagonal;
    int *init_extend_horizontal,
        *init_extend_vertical,
        *init_close_block_diagonal,
        *init_extend_block_diagonal;

    const int *shortSeq_no_gap_vector;

    int shortSeq_gap_opening,
        shortSeq_gap_extension,
        longSeq_gap_opening,
        longSeq_gap_extension;

    int gap_char, gap_open;
    const int *gap_row;
    int shortSeq_vertical_extension;
    DIR_MTX_ARROW_t  tmp_direction_matrix;

    gap_char                   = c->gap_char;
    gap_open                   = c->gap_open;
    assert (len_longerSeq >= lenShortSeq);
    init_extend_horizontal     = extend_horizontal;
    init_extend_vertical       = extend_vertical;
    init_extend_block_diagonal = extend_block_diagonal;
    init_close_block_diagonal  = close_block_diagonal;
    gap_row                    = cm_get_precal_row(precalcMtx, 0, len_longerSeq);
    end_pos                    = (len_longerSeq - lenShortSeq) + 8;
    if (DEBUG_AFFINE) {
        printf("\n--algn fill plane 3 affine\n");
        printf("Before initializing:\n");
        print_array  ("EH: ", extend_horizontal,     len_longerSeq);
        print_array  ("EV: ", extend_vertical,       len_longerSeq);
        print_array  ("EB: ", extend_block_diagonal, len_longerSeq);
        print_array  ("CB: ", close_block_diagonal,  len_longerSeq);
        print_array  ("FC: ", final_cost_matrix,     len_longerSeq);
        print_dirMtx ("DM: ", direction_matrix,      len_longerSeq);
    }
    if (end_pos < 40) {
        end_pos = 40;
    }
    if (end_pos > len_longerSeq) {
        end_pos = len_longerSeq;
    }
    //end_pos = len_longerSeq;
    SEQT longSeqElem,
         longSeqPrevElem,
         shortSeqElem,
         shortSeqPrevElem,
         shortSeq_no_gap,
         longSeq_no_gap;
    SEQT *seq_begini,
         *seq_beginj;

    seq_begini   = shortSeq->seq_begin;
    seq_beginj   = longSeq->seq_begin;
    shortSeqElem = seq_begini[0];

    for (j = 1; j <= len_longerSeq; j++) {
        gap_open_prec[j] = HAS_GAP_OPENING(seq_beginj[j - 1], seq_beginj[j], gap_char, gap_open);
        if ((seq_beginj[j - 1] & gap_char) && (!(seq_beginj[j] & gap_char))) {
            longSeq_horizontal_extension[j] = gap_open_prec[j] + gap_row[j];
        } else {
            longSeq_horizontal_extension[j] = gap_row[j];
        }
    }
    longSeq_horizontal_extension[1] = gap_row[1];
    int r;

    for (;i <= lenShortSeq; i++) {
        prev_extend_horizontal     = init_extend_horizontal +
                                       (((i - 1) % 2) * (len_longerSeq + 1));
        prev_extend_vertical       = init_extend_vertical +
                                       ((len_longerSeq + 1) * ((i - 1) % 2));
        prev_extend_block_diagonal = init_extend_block_diagonal + ((len_longerSeq + 1) * ((i - 1) % 2));
        prev_close_block_diagonal  = init_close_block_diagonal +
                                       ((len_longerSeq + 1) * ((i - 1) % 2));
        extend_horizontal          = init_extend_horizontal + ((i % 2) * (len_longerSeq + 1));
        extend_vertical            = init_extend_vertical + ((len_longerSeq + 1) * (i % 2));
        extend_block_diagonal      = init_extend_block_diagonal + ((len_longerSeq + 1) * (i % 2));
        close_block_diagonal       = init_close_block_diagonal + ((len_longerSeq + 1) * (i % 2));
        direction_matrix           = direction_matrix + (len_longerSeq + 1);

        if (i > start_v) {
            start_pos++;
        }
        direction_matrix[start_pos - 1]  = DO_VERTICAL | END_VERTICAL;
        extend_horizontal[start_pos - 1] = VERY_LARGE_NUMBER;
        shortSeqPrevElem       = shortSeqElem;
        shortSeqElem           = seq_begini[i];
        shortSeq_gap_extension = HAS_GAP_EXTENSION(shortSeqElem, c);
        shortSeq_gap_opening   = HAS_GAP_OPENING (shortSeqPrevElem, shortSeqElem, gap_char, gap_open);
        shortSeq_no_gap        = (NTMPGAP) & shortSeqElem;

        if ((i > 1) && ((shortSeqPrevElem & gap_char) && (!(shortSeqElem & gap_char)))) {
            shortSeq_vertical_extension = shortSeq_gap_opening + shortSeq_gap_extension;
        } else {
            shortSeq_vertical_extension = shortSeq_gap_extension;
        }
        r = prev_extend_vertical[start_pos - 1] + shortSeq_vertical_extension;
        extend_horizontal    [start_pos - 1] = VERY_LARGE_NUMBER;
        close_block_diagonal [start_pos - 1] = r;
        final_cost_matrix    [start_pos - 1] = r;
        extend_block_diagonal[start_pos - 1] = VERY_LARGE_NUMBER;
        extend_vertical      [start_pos - 1] = r;
        direction_matrix     [start_pos - 1] = DO_VERTICAL | END_VERTICAL;
        longSeqElem                          = seq_beginj[start_pos - 1];
        close_block_diagonal [start_pos - 1] = VERY_LARGE_NUMBER;
        shortSeq_no_gap_vector               = c->cost + (shortSeq_no_gap << c->costMtxDimension);

        for (j = start_pos; j <= end_pos; j++) {
            longSeqPrevElem       = longSeqElem;
            longSeqElem           = seq_beginj[j];
            tmp_direction_matrix  = 0;
            longSeq_no_gap        = (NTMPGAP) & longSeqElem;
            longSeq_gap_extension = gap_row[j];
            longSeq_gap_opening   = gap_open_prec[j];
            tmp_direction_matrix  =
                FILL_EXTEND_HORIZONTAL(longSeq_horizontal_extension[j], longSeq_gap_extension,
                                       longSeq_gap_opening, j,
                                       extend_horizontal, c, close_block_diagonal, tmp_direction_matrix);
            tmp_direction_matrix  =
                FILL_EXTEND_VERTICAL(shortSeq_vertical_extension, shortSeq_gap_extension, shortSeq_gap_opening, j,
                                     extend_vertical, prev_extend_vertical, c,
                                     prev_close_block_diagonal, tmp_direction_matrix);
            tmp_direction_matrix  =
                FILL_EXTEND_BLOCK_DIAGONAL(shortSeqElem, longSeqElem, shortSeqPrevElem, longSeqPrevElem, gap_open, j, extend_block_diagonal,
                                           prev_extend_block_diagonal,
                                           prev_close_block_diagonal, tmp_direction_matrix);
            tmp_direction_matrix  =
                FILL_CLOSE_BLOCK_DIAGONAL(shortSeqElem, longSeqElem, shortSeq_no_gap, longSeq_no_gap,
                                          shortSeq_gap_opening, longSeq_gap_opening, j, shortSeq_no_gap_vector, close_block_diagonal,
                                          prev_close_block_diagonal,
                                          prev_extend_vertical, prev_extend_horizontal,
                                          prev_extend_block_diagonal,
                                          tmp_direction_matrix);
            tmp_direction_matrix  =
                ASSIGN_MINIMUM (final_cost_matrix + j, extend_horizontal[j],
                                extend_vertical[j], extend_block_diagonal[j],
                                close_block_diagonal[j], tmp_direction_matrix);
            direction_matrix[j]  = tmp_direction_matrix;
        }
        if (end_pos < len_longerSeq) {
            end_pos++;
            direction_matrix[end_pos]      = DO_HORIZONTAL | END_HORIZONTAL;
            extend_vertical[end_pos]       = VERY_LARGE_NUMBER;
            close_block_diagonal[end_pos]  = VERY_LARGE_NUMBER;
            extend_horizontal[end_pos]     = VERY_LARGE_NUMBER;
            extend_horizontal[end_pos]     = VERY_LARGE_NUMBER;
            extend_block_diagonal[end_pos] = VERY_LARGE_NUMBER;
        }
        if (DEBUG_AFFINE) {
            printf("\n--algn fill plane 3 affine\n");
            printf("Inside loop:\n");
            print_array ("EH: ", extend_horizontal,     len_longerSeq);
            print_array ("EV: ", extend_vertical,       len_longerSeq);
            print_array ("EB: ", extend_block_diagonal, len_longerSeq);
            print_array ("CB: ", close_block_diagonal,  len_longerSeq);
            print_array ("FC: ", final_cost_matrix,     len_longerSeq);
            print_dirMtx ("DM: ", direction_matrix,     len_longerSeq);
        }
    }
    res = final_cost_matrix[len_longerSeq];
    return res;
}



static inline int
algn_fill_plane_2_affine (const seq_p seq1,
                          int *precalcMtx,
                          int seq1_len,
                          int seq2_len,
                          int *curRow,
                          DIR_MTX_ARROW_t *dirMtx,
                          const cost_matrices_2d_p c,
                          int width,
                          int height,
                          int dwidth_height,
                          int *dncurRow,
                          int *htcurRow) {
    int *next_row,
        *next_prevRow,
        *next_dncurRow,
        *next_pdncurRow;
    int *a, *b, *d, *e, open_gap;
    int const *gap_row;
    int start_row, final_row, start_column, length;
    DIR_MTX_ARROW_t *to_go_dirMtx;
    open_gap = cm_get_gap_opening_parameter (c);
    width = width + dwidth_height;
    if (width > seq2_len) width = seq2_len;
    height = height + dwidth_height;
    if (height > seq1_len) height = seq1_len;
    a = curRow;
    b = curRow + seq2_len;
    d = dncurRow;
    e = dncurRow + seq2_len;
    gap_row = cm_get_precal_row (precalcMtx, 0, seq2_len); /* We want the horizontal row */
    /* We have to consider three cases in this new alignment procedure (much
     * cleaner than the previous):
     *
     * Case 1:
     * If seq1 is much longer than seq2, then there is no point on using the
     * barriers, we rather fill the full matrix in one shot */
    if (((float) seq1_len) >= (((float) ((float) 3 / (float) 2) * (float) seq2_len))) {
        return (algn_fill_plane_affine (seq1, precalcMtx, seq1_len, seq2_len, curRow, dirMtx, c, d,
                htcurRow, open_gap));
    }
    /* Case 2:
     * There are no full rows to be filled, therefore we have to break the
     * procedure in three different subsets */
    else if ((2 * height) < seq1_len) {
        algn_fill_first_row_affine (a, dirMtx, width, gap_row, d, htcurRow, open_gap);
        b[0] = a[0];
        a[0] = 0;
        start_row    = 1;
        final_row    = height;
        start_column = 0;
        length       = width + 1;
        to_go_dirMtx = dirMtx + (start_row * seq2_len);
        /* Now we fill that space */
        next_row     = algn_fill_extending_right_affine (seq1, precalcMtx, seq1_len, seq2_len, b, a,
                                                         to_go_dirMtx, c, start_row, final_row,
                                                         length, e, d, htcurRow, open_gap);
        next_prevRow = choose_other (next_row, a, b);

        algn_choose_affine_other (next_row, curRow, &next_dncurRow, &next_pdncurRow, d, e);

        /* Next group */
        start_row    = final_row;
        final_row    = seq1_len - (height - 1);
        start_column = 1;
        length       = width + height;
        to_go_dirMtx = dirMtx + (start_row * seq2_len);
        next_row =
            algn_fill_extending_left_right_affine (seq1, precalcMtx, seq1_len,
                                                   seq2_len, next_row, next_prevRow, to_go_dirMtx, c, start_row,
                                                   final_row, start_column, length, next_dncurRow, next_pdncurRow,
                                                   htcurRow, open_gap);
        next_prevRow = choose_other (next_row, a, b);

        algn_choose_affine_other (next_row, curRow, &next_dncurRow, &next_pdncurRow, d, e);

        /* The final group */
        start_row    = final_row;
        final_row    = seq1_len;
        length       = length - 2;
        start_column = seq2_len - length;
        to_go_dirMtx = dirMtx + (start_row * seq2_len);
        next_row     = algn_fill_extending_left_affine (seq1, precalcMtx, seq1_len, seq2_len,
                                                 next_row, next_prevRow, to_go_dirMtx, c, start_row, final_row,
                                                 start_column, length, next_dncurRow, next_pdncurRow,
                                                 htcurRow, open_gap);
        next_prevRow = choose_other (next_row, a, b);

        algn_choose_affine_other (next_row, curRow, &next_dncurRow, &next_pdncurRow, d, e);
    }
    /* Case 3: (final case)
     * There is a block in the middle of with full rows that have to be filled
     * */
    else {
        /* We will simplify this case even further, if the size of the leftover
         * is too small, don't use the barriers at all, just fill it up all */
        if (8 >= (seq1_len - height)) {
            return (algn_fill_plane_affine (seq1, precalcMtx, seq1_len, seq2_len, curRow, dirMtx, c,
                                         d, htcurRow, open_gap));
        } else {
            algn_fill_first_row_affine (curRow, dirMtx, width, gap_row, dncurRow, htcurRow, open_gap);

            b[0]         = curRow[0];
            curRow[0]    = 0;
            start_row    = 1;
            final_row    = (seq2_len - width) + 1;
            start_column = 0;
            length       = width + 1;
            to_go_dirMtx = dirMtx + (seq2_len * start_row);
            next_row     = algn_fill_extending_right_affine (seq1, precalcMtx, seq1_len, seq2_len,
                                                             b, a, to_go_dirMtx, c, start_row, final_row,
                                                             length, e, d, htcurRow, open_gap);
            next_prevRow = choose_other (next_row, a, b);

            algn_choose_affine_other (next_row, curRow, &next_dncurRow, &next_pdncurRow, d, e);

            start_row    = final_row;
            final_row    = seq1_len - (seq2_len - width) + 1;
            length       = seq2_len;
            to_go_dirMtx = dirMtx + (seq2_len * start_row);
            next_row     = algn_fill_no_extending_affine (seq1, precalcMtx, seq1_len, seq2_len,
                                                      next_row, next_prevRow, to_go_dirMtx, c, start_row,
                                                      final_row, next_dncurRow, next_pdncurRow, htcurRow, open_gap);
            next_prevRow = choose_other (next_row, a, b);

            algn_choose_affine_other (next_row, curRow, &next_dncurRow, &next_pdncurRow, d, e);

            start_row    = final_row;
            final_row    = seq1_len;
            start_column = 1;
            length       = seq2_len - 1;
            to_go_dirMtx = dirMtx + (seq2_len * start_row);
            next_row     =
                algn_fill_extending_left_affine (seq1, precalcMtx, seq1_len, seq2_len,
                                                 next_row, next_prevRow, to_go_dirMtx, c, start_row, final_row,
                                                 start_column, length, next_dncurRow, next_pdncurRow, htcurRow,
                                                 open_gap);
            next_prevRow = choose_other (next_row, a, b);
        }
    }
    return (next_prevRow[seq2_len - 1]);
}
/******************************************************************************/

/** Fill parallel must have been called before */
static inline void
fill_moved (int seq3_len, const int *prev_m, const int *upper_m,
            const int *diag_m, const int *seq1_gap_seq3, const int *gap_seq2_seq3,
            const int *seq1_seq2_seq3, int *curRow, DIR_MTX_ARROW_t *dirMtx) {
    int k, tmp0, tmp1, tmp;
    for (k = 1; k < seq3_len; k++) {
        tmp0 = upper_m[k] + seq1_gap_seq3[k];
        if (tmp0 < curRow[k]) {
            curRow[k] = tmp0;
            dirMtx[k] = A_G_A;
        }

        tmp = prev_m[k] + gap_seq2_seq3[k];

        if (tmp < curRow[k]) {
            curRow[k] = tmp;
            dirMtx[k] = G_A_A;
        }

        tmp1 = diag_m[k] + seq1_seq2_seq3[k];

        if (tmp1 < curRow[k]) {
            curRow[k] = tmp1;
            dirMtx[k] = A_A_A;
        }
    }
}

inline void
fill_parallel (int seq3_len, const int *prev_m, const int *upper_m,
               const int *diag_m, int seq1_gap_gap, int gap_seq2_gap, int seq1_seq2_gap, int *curRow,
               DIR_MTX_ARROW_t *dirMtx) {
    int k, tmp1, tmp;

    for (k = 0; k < seq3_len; k++) {
        curRow[k] = upper_m[k] + seq1_gap_gap;
        dirMtx[k] = A_G_G;
        tmp = prev_m[k] + gap_seq2_gap;

        if (tmp < curRow[k]) {
            curRow[k] = tmp;
            dirMtx[k] = G_A_G;
        }

        tmp1 = diag_m[k] + seq1_seq2_gap;

        if (tmp1 < curRow[k]) {
            curRow[k] = tmp1;
            dirMtx[k] = A_A_G;
        }
    }
}

/**
 *  @param lSeq is a pointer to the sequence lSeq (vertical; columns).
 *  @param mSeq is a pointer to the middle-lengthed sequence (depth; pages).
 *  @param precalcMtx is a pointer to a precalculated three dimensional matrix that holds
 *     the alignment values for all the combinations of elements (i.e. alphabet
 *     plus ambiguities) with the sequence sSeq (see cm_precalc_4algn_3d for more
 *     information).
 *  @param lSeqLen, @param mSeqLen and @param sSeqLen are the lengths of the three sequences
 *     to be aligned
 *  @param curCostColPtr is a pointer to the first element of the 3dMtx that will
 *     hold the cost information for the 3-way N-W algorithm
 *  @param curDirColPtr does the same job, holding the direction information for the backtrace.
 *  @param uk is the value of the Ukkonen barriers (not used in this version of the program).

 *  Now to nomenclature and 3dMtx set up:
 *
 *  Consider a 2-dimentional matrix. Now extend the matrix forward into the third dimension.
 *  Under this framework, a row in a 3d matrix is the plane extending from each row in the
 *  2d matrix. Likewise for columns: they are the planes extending forward from each column
 *  in the original 2d matrix. The last dimension, the planes aligned forward parallel to the
 *  original matrix, are pages.

 *  The 3dMtxs (actually, not 3dMtxs, but whatever) that hold the costs and directions are set up
 *  so that the elements of the longest sequence are the heads of the rows,
 *  the elements of the shortest sequence are the heads of the columns,
 *  and the elements of the middle-length sequence are the heads of the pages.
 *  The cell at (0,0,0) is in the left-upper-rear of the 3dMtx, so the indices grow from left-to-right,
 *  bottom-to-top, back-to-front.

 *  To be ridiculously clear:
 *  To move within a row, or move between pages, increment by one;
 *  to move within a column, or move between rows, increment by medium sequence length;
 *  to move within a page, or move between columns, increment by mSeqLen * sSeqLen.

 *  Moving orthogonally toward any plane inserts a gap in the associated sequence, so moving
 *  from right to left inserts a gap in the middle sequence and the long sequence.

 *  Possible combinations are:
 *  lSeq, gap,  gap  -> move to a new row               (moving towards both mSeq and sSeq)
 *  gap,  mSeq, gap  -> move to a new      column
 *  lSeq, mSeq, gap  -> move to a new row, column
 *  gap,  mSeq, sSeq -> move to a new      column, page
 *  lSeq, gap,  sSeq -> move to a new row,         page
 *  lSeq, mSeq, sSeq -> move to a new row, column, page
 *  gap,  gap,  sSeq -> move to a new              page (the last one to be done, not parallelizable)

 *  Including the precalcMtx allows us to get the value of the alignments of the current elements
 *  quickly using just a lookup. For this reason we don't need to include sSeq as an input. Instead,
 *  we can simply iterate down the precalculated matrix.

 *  Function iterates from back to front, so page by page.
 *
 */
int
algn_fill_3dMtx (const seq_p lSeq, const seq_p mSeq, const int *precalcMtx,
                int lSeqLen, int mSeqLen, int sSeqLen, int *costMtxPtr, DIR_MTX_ARROW_t  *dirMtxPtr,
                int uk, int gap_char, int alphSize) {
    if (DEBUG_CALL_ORDER) {
        printf("  --algn_fill_3dMtx\n");
    }
    SEQT *lSeqPtr, *mSeqPtr;

    /* Each of the following arrays hold some precalculated value for the
     * sequence sSeq which is not passed as argument.
     */
    const int *gap_mSeq_sSeq;     /* Align a gap and the current base of mSeq with sSeq */
    const int *lSeq_gap_sSeq;     /* Align the current base of lSeq and a gap with sSeq */
    const int *lSeq_mSeq_sSeq;    /* Align the current bases of lSeq and mSeq with sSeq */
    const int *gap_gap_sSeq;      /* Align two gaps with sSeq */

    /* Each of the following arrays hold the arrays of the three dimensional
     * matrix that are located around the row that is filled on each iteration.
     * These rows are already filled in some previous iteration and used in
     * the dynamic programming matrix.
     */
    int *curCostColPtr;
    DIR_MTX_ARROW_t *curDirColPtr;     // points to the head of the current column
    int *upper_m;                       /* The row above the current row; the same row in the plane above the current plane */
    int *prevCostColPtr;                /* The column to the left the current column in the same plane */
    int *diag_m;                        /* The upper_m relative to prevCostColPtr */
    int *tmp_curCostColPtr;             /* A temporary pointer to the column that is being filled currently */
    DIR_MTX_ARROW_t *tmp_curDirColPtr; /* Same as previous for curDirColPtr */
    int i, j, k, tmp;

    curCostColPtr     = costMtxPtr;
    curDirColPtr      = dirMtxPtr;
    lSeqPtr           = seq_get_seq_begin (lSeq);
    mSeqPtr           = seq_get_seq_begin (mSeq);
    tmp_curDirColPtr  = curDirColPtr;
    tmp_curCostColPtr = curCostColPtr;
    upper_m           = curCostColPtr + sSeqLen;
    diag_m            = curCostColPtr;
    if (DEBUG_MAT) {
        printf ("Three dimensional sequence alignment matrix.\n");
    }


    /****************************** Fill the first plane only at the beginning, this is special ******************************/

    *curCostColPtr = 0;              /* Fill the first cell, of course to 0 */
    *curDirColPtr  = A_A_A;

    /* Fill first row based on precalculated row.
     * The first row consists of aligning sSeq with gaps, we have that
     * precalculated, so all we really need is to add up that vector.
     */
    gap_gap_sSeq = cm_get_row_precalc_3d (precalcMtx, sSeqLen, alphSize, gap_char, gap_char);
    for (i = 1; i <= sSeqLen; i++) {
        curCostColPtr[i] = curCostColPtr[i - 1] + gap_gap_sSeq[i];
        curDirColPtr[i]  = G_G_A;
    }

    prevCostColPtr = curCostColPtr;
    curCostColPtr += sSeqLen;
    curDirColPtr  += sSeqLen;

    /* Finish filling the first plane.
     * In this plane filling all we really need to deal with is aligning mSeq
     * and sSeq, as the first plane holds the inital gap of lSeq. */
    for (i = 1; i < mSeqLen; i++,
                             prevCostColPtr += sSeqLen,
                             curCostColPtr  += sSeqLen,
                             curDirColPtr   += sSeqLen) {

        lSeq_gap_sSeq = cm_get_row_precalc_3d (precalcMtx, sSeqLen, alphSize, gap_char, lSeqPtr[i]);
        gap_mSeq_sSeq = cm_get_row_precalc_3d (precalcMtx, sSeqLen, alphSize, mSeqPtr[i], gap_char);

        /* Fill the first cell with the cost of extending the gap from the
         * previous column */
        curCostColPtr[0] = prevCostColPtr[0] + lSeq_gap_sSeq[0];
        curDirColPtr[0]  = G_A_G;
        /* Everyone else requires the three comparisons we used when filling
         * rows as usual in a two dimensional alignment. Note that this code
         * is almost the same as algn_fill_row, so if something is modified
         * there, there will be need of modifying it in the same way here. */
        /* TODO: Add the ukkonen barriers */
        for (j = 1; j < sSeqLen; j++) {
            curCostColPtr[j] = prevCostColPtr[j] + lSeq_gap_sSeq[0];
            curDirColPtr[j] = G_A_G;
            tmp = prevCostColPtr[j - 1] + gap_mSeq_sSeq[j];
            if (tmp < curCostColPtr[j]) {
                curCostColPtr[j] = tmp;
                curDirColPtr[j] = G_A_A;
            }
            tmp = curCostColPtr[j - 1] + gap_gap_sSeq[j];
            if (tmp < curCostColPtr[j]) {
                curCostColPtr[j] = tmp;
                curDirColPtr[j] = G_G_A;
            }
        }
    }
    if (DEBUG_COST_M) {  /* Printing the cost matrix */
        int *tmp_curCostColPtr_debug;
        tmp_curCostColPtr_debug = tmp_curCostColPtr;
        printf ("\n");
        for (i = 0; i < mSeqLen; i++) {
            for (int l = mSeqLen - i; l > 1; l--) {
                printf("  ");
            }
            for (j = 0; j < sSeqLen; j++)
                printf ("%-6d", tmp_curCostColPtr_debug[j]);
            tmp_curCostColPtr_debug += mSeqLen;
            printf ("\n");
        }
        printf ("\n");
    }
    /****************************** Finished the first plane filling ******************************/



    /****************************** Fill plane by plane ******************************/
    curCostColPtr  = tmp_curCostColPtr + (sSeqLen * mSeqLen);
    curDirColPtr  = tmp_curDirColPtr + (sSeqLen * mSeqLen);
    diag_m  = tmp_curCostColPtr;
    upper_m = tmp_curCostColPtr + sSeqLen;
    prevCostColPtr  = curCostColPtr - sSeqLen;

    for (i = 1; i < lSeqLen; i++) { /* For each plane */
        int lSeq_curElmt; /* The element in lSeq represented by this plane */
        lSeq_curElmt = lSeqPtr[i];
        lSeq_gap_sSeq = cm_get_row_precalc_3d (precalcMtx, sSeqLen, alphSize, lSeq_curElmt, gap_char);
        /* Filling the first row of the current plane. */

        /* This requires only three operations, equivalent to the
         * 2-dimensional alignment (the three for loops) */
        curCostColPtr[0] = diag_m[0] + lSeq_gap_sSeq[0]; /* diag is upper in this step */
        curDirColPtr[0] = A_G_G;
        if (DEBUG_COST_M) {
            printf ("%-6d", curCostColPtr[0]);
        }
        for (j = 1, k = 0; j < sSeqLen; j++, k++) {
            curCostColPtr[j] = diag_m[j] + lSeq_gap_sSeq[0];
            curDirColPtr[j] = A_G_G;
            tmp = diag_m[k] + lSeq_gap_sSeq[j];
            if (tmp < curCostColPtr[j]) {
                curCostColPtr[j] = tmp;
                curDirColPtr[j] = A_G_A;
            }
            tmp = gap_gap_sSeq[j] + curCostColPtr[k];
            if (tmp < curCostColPtr[j]) {
                curCostColPtr[j] = tmp;
                curDirColPtr[j] = G_G_A;
            }
            if (DEBUG_COST_M) {
                printf ("%-6d", curCostColPtr[j]);
            }
        }
        if (DEBUG_COST_M) {
            printf ("\n");
        }
        /* Now we should move to the next row to continue filling the matrix */
        curDirColPtr += sSeqLen;
        curCostColPtr += sSeqLen;

        /* Now, go on with the rest of the rows. On each row, curCostColPtr is the row
         * being constructed, prevCostColPtr is the previous row in the same horizontal
         * plane, upper_m is the previous row in the vertical plane and diag_m
         * is the row in the previous planes (vertical and horizontal).
         */
        for (j = 1;
             j < mSeqLen;
             j++,
                diag_m += sSeqLen,
                upper_m += sSeqLen,
                prevCostColPtr += sSeqLen,
                curCostColPtr += sSeqLen,
                curDirColPtr += sSeqLen) {
            /* We first set the vectors that are needed */
            int mSeq_it;
            mSeq_it = mSeqPtr[j];
            gap_mSeq_sSeq  = cm_get_row_precalc_3d (precalcMtx, sSeqLen, alphSize, gap_char, mSeq_it);
            lSeq_mSeq_sSeq = cm_get_row_precalc_3d (precalcMtx, sSeqLen, alphSize, lSeq_curElmt, mSeq_it);
            fill_parallel (sSeqLen, prevCostColPtr, upper_m, diag_m, lSeq_gap_sSeq[0], gap_mSeq_sSeq[0],
                    lSeq_mSeq_sSeq[0], curCostColPtr, curDirColPtr);
            fill_moved (sSeqLen, prevCostColPtr - 1, upper_m - 1, diag_m - 1, lSeq_gap_sSeq,
                        gap_mSeq_sSeq, lSeq_mSeq_sSeq, curCostColPtr, curDirColPtr);
            /* In the final step we run over the array filling the self check.
             * */
            if (DEBUG_COST_M) {
                printf ("%-6d", curCostColPtr[0]);
            }
            for (k = 1; k < sSeqLen; k++) {
                tmp = curCostColPtr[k - 1] + gap_gap_sSeq[k];
                if (tmp < curCostColPtr[k]) {
                    curCostColPtr[k] = tmp;
                    curDirColPtr[k] = G_G_A;
                }
                if (DEBUG_COST_M) {
                    printf ("%-6d", curCostColPtr[k]);
                }
            }
            if (DEBUG_COST_M) {
                printf ("\n");
            }
        }
        if (DEBUG_COST_M) {
            printf ("\n");
        }
    }
    return (curCostColPtr[-1]); /** We return the last item in the previous row */
}

/* Same as the previous function but with Ukkonen barriers turned on. The
 * additional parameters are:
 * @param w is the maximum width to be filled.
 * @param d is the maximum depth to be filled
 * @param h is the maximum height to be filled.
 * TODO: Finish this implementation, I will stop now to test the sequence
 * analysis procedures in the rest of POY.
 * */
inline int
algn_fill_3dMtx_ukk (const seq_p seq1, const seq_p seq2, const int *precalcMtx,
                    int seq1_len, int seq2_len, int seq3_len, int *curRow, DIR_MTX_ARROW_t *dirMtx, int uk,
                    int gap_char, int alphSize, int w, int d, int h) {
    SEQT *seq1Ptr, *seq2Ptr;
    /* Each of the following arrays hold some precalculated value for the
     * sequence seq3 which is not passed as argument. */
    const int *gap_seq2_seq3;     /** Align a gap and the current base of seq2 with seq3 */
    const int *seq1_gap_seq3;     /** Align the current base of seq1 and a gap with seq3 */
    const int *seq1_seq2_seq3;    /** Align the current bases of seq1 and seq2 with seq3 */
    const int *gap_gap_seq3;      /** Align two gaps with seq3 */
    /* Each of the following arrays hold the arrays of the three dimensional
     * matrix that are located around the row that is filled on each iteration.
     * These rows are already filled in some previous iteration and used in
     * the dynamic progracurRowing matrix. */
    int *upper_m;       /** The row in the upper plane of the 3dMtx */
    int *prev_m;        /** The row behind the current row in the same plane */
    int *diag_m;        /** The upper_m relative to prev_m */
    int *tmp_curRow;        /** A temporary pointer to the row that is being filled
                        currently */
    DIR_MTX_ARROW_t *tmp_dirMtx;       /** Same as previous for dirMtx */
    int i, j, k, tmp;

    seq1Ptr = seq_get_seq_begin (seq1);
    seq2Ptr = seq_get_seq_begin (seq2);
    tmp_dirMtx = dirMtx;
    tmp_curRow = curRow;
    upper_m = curRow + seq3_len;
    diag_m = curRow;
    if (DEBUG_MAT)
        printf ("Three dimensional sequence alignment matrix.\n");
    /* Fill the first plane only at the beginning, this is special */
    {
        curRow[0] = 0;              /* Fill the first cell, of course to 0 */
        dirMtx[0] = A_A_A;

        /* Fill first row based on precalculated row.
         * The first row consists of aligning seq3 with gaps, we have that
         * precalculated, so all we really need is to add up that vector.*/
        gap_gap_seq3 = cm_get_row_precalc_3d (precalcMtx, seq3_len, alphSize, gap_char, gap_char);
        for (i = 1; i < seq3_len; i++) {
            curRow[i] = curRow[i - 1] + gap_gap_seq3[i];
            dirMtx[i] = G_G_A;
        }

        prev_m = curRow;
        curRow += seq3_len;
        dirMtx += seq3_len;

        /* Finish filling the first plane.
         * In this plane filling, all we really need to deal with is aligning seq2
         * and seq3, as the first plane holds the inital gap of seq1. */
        for (i = 1; i < seq2_len; i++, prev_m += seq3_len, curRow += seq3_len, dirMtx += seq3_len) {
            gap_seq2_seq3 = cm_get_row_precalc_3d (precalcMtx, seq3_len, alphSize, gap_char, seq2Ptr[i]);

            /** Fill the first cell with the cost of extending the gap from the
             * previous row */
            curRow[0] = prev_m[0] + gap_seq2_seq3[0];
            dirMtx[0] = G_A_G;
            /* Everyone else requires the three comparisons we used when filling
             * rows as usual in a two dimensional alignment. Note that this code
             * is almost the same as algn_fill_row, so if something is modified
             * there, there will be need of modifying it in the same way here. */
            /* TODO: Add the ukkonen barriers */
            for (j = 1; j < seq3_len; j++) {
                curRow[j] = prev_m[j] + gap_seq2_seq3[0];
                dirMtx[j] = G_A_G;
                tmp = prev_m[j - 1] + gap_seq2_seq3[j];
                if (tmp < curRow[j]) {
                    curRow[j] = tmp;
                    dirMtx[j] = G_A_A;
                }
                tmp = curRow[j - 1] + gap_gap_seq3[j];
                if (tmp < curRow[j]) {
                    curRow[j] = tmp;
                    dirMtx[j] = G_G_A;
                }
            }
        }
        if (DEBUG_COST_M) {  /* Printing the cost matrix */
            int *tmp_curRow_debug;
            tmp_curRow_debug = tmp_curRow;
            for (i = 0; i < seq2_len; i++) {
                for (int l = seq2_len; l > 1; l++) {
                    printf("  ");
                }
                for (j = 0; j < seq3_len; j++) {
                    printf ("%d\t", tmp_curRow_debug[j]);
                }
                tmp_curRow_debug += seq2_len;
                printf ("\n");
            }
            printf ("\n");
        }
    } /* Finished the first plane filling */

    /* Fill plane by plane */
    curRow = tmp_curRow + (seq3_len * seq2_len);
    dirMtx = tmp_dirMtx + (seq3_len * seq2_len);
    diag_m = tmp_curRow;
    upper_m = tmp_curRow + seq3_len;
    prev_m = curRow - seq3_len;
    for (i = 1; i < seq1_len; i++) { /* For each plane */
        int seq1_curElmt; /* The element in seq1 represented by this plane */
        seq1_curElmt = seq1Ptr[i];
        seq1_gap_seq3 = cm_get_row_precalc_3d (precalcMtx, seq3_len, alphSize, seq1_curElmt, gap_char);
        /* Filling the first row of the current plane. */
        {
            /* This requires only three operations, equivalent to the
             2-dimensional alignment (the three for loops) */
            curRow[0] = diag_m[0] + seq1_gap_seq3[0]; /* diag is upper in this step */
            dirMtx[0] = A_G_G;
            if (DEBUG_DIR_M)
                printf ("%d\t", curRow[0]);
            for (j = 1, k = 0; j < seq3_len; j++, k++) {
                curRow[j] = diag_m[j] + seq1_gap_seq3[0];
                dirMtx[j] = A_G_G;
                tmp = diag_m[k] + seq1_gap_seq3[j];
                if (tmp < curRow[j]) {
                    curRow[j] = tmp;
                    dirMtx[j] = A_G_A;
                }
                tmp = gap_gap_seq3[j] + curRow[k];
                if (tmp < curRow[j]) {
                    curRow[j] = tmp;
                    dirMtx[j] = G_G_A;
                }
                if (DEBUG_DIR_M)
                    printf ("%d\t", curRow[j]);
            }
            if (DEBUG_DIR_M)
                printf ("\n");
            /* Now we should move to the next row to continue filling the matrix
             * */
            dirMtx += seq3_len;
            curRow += seq3_len;
        }
        /* Now, go on with the rest of the rows.  On each row, curRow is the row
         * being constructed, prev_m is the previous row in the same horizontal
         * plane, upper_m is the previous row in the vertical plane and diag_m
         * is the row in the previous planes (vertical and horizontal).
         */
        for (j = 1; j < seq2_len; j++, diag_m += seq3_len,
                upper_m += seq3_len, prev_m += seq3_len, curRow += seq3_len,
                dirMtx += seq3_len) {
            /* We first set the vectors that are needed */
            int seq2_it;
            seq2_it = seq2Ptr[j];
            gap_seq2_seq3 = cm_get_row_precalc_3d (precalcMtx, seq3_len, alphSize, gap_char, seq2_it);
            seq1_seq2_seq3 = cm_get_row_precalc_3d (precalcMtx, seq3_len, alphSize, seq1_curElmt, seq2_it);
            fill_parallel (seq3_len, prev_m, upper_m, diag_m, seq1_gap_seq3[0], gap_seq2_seq3[0],
                           seq1_seq2_seq3[0], curRow, dirMtx);
            fill_moved (seq3_len, prev_m - 1, upper_m - 1, diag_m - 1, seq1_gap_seq3,
                        gap_seq2_seq3, seq1_seq2_seq3, curRow, dirMtx);
            /* In the final step we run over the array filling the self check.
             * */
            if (DEBUG_DIR_M)
                printf ("%d\t", curRow[0]);
            for (k = 1; k < seq3_len; k++) {
                tmp = curRow[k - 1] + gap_gap_seq3[k];
                if (tmp < curRow[k]) {
                    curRow[k] = tmp;
                    dirMtx[k] = G_G_A;
                }
                if (DEBUG_DIR_M)
                    printf ("%d\t", curRow[k]);
            }
            if (DEBUG_DIR_M)
                printf ("\n");
        }
        if (DEBUG_DIR_M)
            printf ("\n");
    }
    return (curRow[-1]); /** We return the last item in the previous row */
}

/** [algn_nw_limit_2d performs a pairwise
 *  alignment of the subsequences of seq1 and seq2 starting in position st_seq1
 *  and st_seq2, respectively, with length len_seq1 and len_seq2 using the
 *  transformation cost matrix cstMtx and the alignment matrices nw_mtxs.
 */
/** TODO: st_seq1 and st_seq2 are unused??? Also, limit defined here seems to be defined in sequence.ml but
 *        never used. Its only call traces back to algn_CAML_limit_2, which, in turn, seems never to be called?
 */
static inline int
algn_nw_limit_2d (const seq_p shorterSeq, const seq_p longerSeq, const cost_matrices_2d_p costMtx,
                  nw_matrices_p nw_mtxs, int deltawh, int len_shorterSeq, int len_longerSeq) {
    printf("algn_nw_limit_2d %d\n", deltawh);
    fflush(stdout);
    const SEQT *slongerSeq, *sshorterSeq;
    int *curRow, *precalcMtx;
    DIR_MTX_ARROW_t  *dirMtx;
    slongerSeq     = seq_get_seq_begin (longerSeq);
    sshorterSeq    = seq_get_seq_begin (shorterSeq);
    curRow         = mat_get_2d_nwMtx  (nw_mtxs);
    dirMtx         = mat_get_2d_direct (nw_mtxs);

    int  *cost;          // The transformation cost matrix.
    SEQT *median;        /** The matrix of possible medians between elements in the
                          *  alphabet. The best possible medians according to the cost
                          *  matrix.
                          */
    int *worst;          /* Missing in 3d */
    int *prepend_cost;   /* Missing in 3d */
    int *tail_cost;      /* Missing in 3d */


    printf("before pre-calc \n");
    fflush(stdout);
    precalcMtx = mat_get_2d_prec (nw_mtxs);
    printf("after  pre-calc \n");
    fflush(stdout);

    printf("before pre-calc alignment\n");
    fflush(stdout);
    cm_precalc_4algn (costMtx, nw_mtxs, shorterSeq); // returns precalculated cost matrix (in matrices) computed using sequence from shorterSeq.
                                               // shorterSeq bases will be column heads of that matrix
    printf("after  pre-calc alignment\n");
    fflush(stdout);

    if (cm_get_affine_flag (costMtx)) {
        return algn_fill_plane_2_affine ( longerSeq, precalcMtx
                                        , len_longerSeq
                                        , len_shorterSeq
                                        , curRow
                                        , dirMtx
                                        , costMtx
                                        , 50
                                        , (len_longerSeq - len_shorterSeq) + 50
                                        , deltawh
                                        , curRow + (2 * len_shorterSeq)
                                        , curRow + (4 * len_shorterSeq)
                                        );
                // TODO: why all the 50s here and below? It looks like it's a default
                // starting value for the matrix width/height
    } else {
        return algn_fill_plane_2 ( longerSeq
                                 , precalcMtx
                                 , len_longerSeq
                                 , len_shorterSeq
                                 , curRow
                                 , dirMtx
                                 , costMtx
                                 , 50
                                 , (len_longerSeq - len_shorterSeq) + 50
                                 , deltawh
                                 );
    }
}

/** TODO: can probably eliminate either this or algn_nw_limit_2d, since
 *  both seem to be doing the same thing
 */
/** second sequence must be longer!!! */
int
algn_nw_2d ( const seq_p shorterSeq
           , const seq_p longerSeq
           , const cost_matrices_2d_p costMtx
           , nw_matrices_p nw_mtxs
           , int deltawh)
{
    // deltawh is the size of the direction matrix, and was determined by the following algorithm:
    // let dif = longerSeqlen - shorterSeqlen
    // let lower_limit = longerSeqlen * .1
    // if deltawh has no value
    //    then if dif < lower_limit
    //            then (lower_limit / 2)
    //            else 2
    //    else if dif < lower_limit
    //            then lower_limit
    //            else v

    // m and costMtx come from OCAML, so much be allocated in Haskell. Must
    // Determine correct size.

    // at this point, gap is already set at beginning of seq
    // bases are set as bit streams, with gap as most-significant bit.
    if(DEBUG_NW) {
        printf("---algn_nw_2d\n");
        printf("first sequence\n");
        seq_print(longerSeq);
        seq_print(shorterSeq);
        printf("second sequence\n");
        print_matrices(nw_mtxs, costMtx->costMtxDimension);
    }


    int len_longerSeq,
        shorterSeq_len;
    len_longerSeq  = seq_get_len (longerSeq);
    shorterSeq_len = seq_get_len (shorterSeq);
    assert (len_longerSeq >= shorterSeq_len);
    return algn_nw_limit_2d (shorterSeq, longerSeq, costMtx, nw_mtxs, deltawh, shorterSeq_len, len_longerSeq);
}

// Unused
// TODO: clean up arguments and then costMtxVals variable name
int
algn_nw_3d ( const seq_p seq1
           , const seq_p seq2
           , const seq_p seq3
           , const cost_matrices_3d_p costMtx
           , nw_matrices_p nwMtxs
           , int deltawh)
{
    //const SEQT *sseq1, *sseq2, *sseq3;
    int *costMtxVals,
        *precalcMtx,
         seq1_len,
         seq2_len,
         seq3_len,
         gap_char,
         res;
    DIR_MTX_ARROW_t *dirMtx;
   /*
    sseq1 = seq_get_seq_begin (seq1);
    sseq2 = seq_get_seq_begin (seq2);
    sseq3 = seq_get_seq_begin (seq3);
    */
    mat_setup_size ( nwMtxs
                   , seq_get_len (seq2)
                   , seq_get_len (seq3)
                   , seq_get_len (seq1)
                   , costMtx->costMtxDimension);

    costMtxVals = mat_get_3d_matrix (nwMtxs);
    dirMtx      = mat_get_3d_direct (nwMtxs);
    precalcMtx  = mat_get_3d_prec (nwMtxs);
    seq1_len    = seq_get_len (seq1);
    seq2_len    = seq_get_len (seq2);
    seq3_len    = seq_get_len (seq3);
    gap_char    = cm_get_gap_char_3d (costMtx);

    cm_precalc_4algn_3d (costMtx, precalcMtx, seq3);
    /* TODO Check how is this ukkonen barrier affecting this fill 3dMtx, the deltawh
     * was called uk */
    res = algn_fill_3dMtx ( seq1
                          , seq2
                          , precalcMtx
                          , seq1_len
                          , seq2_len
                          , seq3_len
                          , costMtxVals
                          , dirMtx
                          , deltawh
                          , gap_char
                          , costMtx->alphSize);
    return res;
}

int
algn_calculate_from_2_aligned (seq_p seq1, seq_p seq2, cost_matrices_2d_p c, int *matrix) {
    int i,
        res = 0,
        gap_opening,
        gap_row = 0;
    SEQT gap_char, seq1b, seq2b;
    gap_char = cm_get_gap_char_2d (c);
    /* We initialize i to the proper location */
    seq1b = seq_get_element (seq1, 0);
    seq2b = seq_get_element (seq2, 0);
    if ((c->combinations && (gap_char & seq1b) && (gap_char & seq2b)) ||
            (!c->combinations && (gap_char == seq1b) && (gap_char == seq2b)))
        i = 1;
    else i = 0;
    gap_opening = cm_get_gap_opening_parameter (c);
    assert ((seq_get_len (seq1)) == (seq_get_len (seq2)));
    for (; i < seq_get_len (seq1); i++) {
        seq1b = seq_get_element (seq1, i);
        seq2b = seq_get_element (seq2, i);
        if (0 == gap_row) { /* We have no gaps */
            if ((c->combinations && (seq1b & gap_char) && !(seq2b & gap_char)) ||
                        ((!c->combinations) && (seq1b == gap_char)))
            {
                res += gap_opening;
                gap_row = 1;
            } else if ((c->combinations && (seq2b & gap_char) && !(seq1b & gap_char)) ||
                        ((!c->combinations) && (seq2b == gap_char))) {
                res += gap_opening;
                gap_row = 2;
            }
        }
        else if (1 == gap_row) { /* We are in seq1's block of gaps */
            if ((c->combinations && !(seq1b & gap_char)) ||
                        ((!c->combinations) && (seq1b != gap_char))) {
                if ((c->combinations && (seq2b & gap_char) && !(seq1b & gap_char)) ||
                        ((!c->combinations) && (seq2b == gap_char))) {
                    res += gap_opening;
                    gap_row = 2;
                }
                else gap_row = 0;
            }
        }
        else { /* We are in seq2's block of gaps */
            assert (2 == gap_row);
            if ((c->combinations && !(seq2b & gap_char)) ||
                        ((!c->combinations) && (seq2b != gap_char))) {
                if ((c->combinations && (seq1b & gap_char)) ||
                        ((!c->combinations) && (seq1b == gap_char))) {
                    res += gap_opening;
                    gap_row = 1;
                }
                else gap_row = 0;
            }
        }
        res += (cm_calc_cost (matrix, seq_get_element (seq1, i), seq_get_element (seq2, i), c->costMtxDimension));
    }
    return (res);
}

int
algn_worst_2 (seq_p seq1, seq_p seq2, cost_matrices_2d_p c) {
    return (algn_calculate_from_2_aligned (seq1, seq2, c, c->worst));
}

int
algn_verify_2 (seq_p seq1, seq_p seq2, cost_matrices_2d_p c) {
    return (algn_calculate_from_2_aligned (seq1, seq2, c, c->cost));
}

void
algn_print_bcktrck_2d (const seq_p seq1, const seq_p seq2,
              const nw_matrices_p m) {
    int i, j;
    DIR_MTX_ARROW_t *d;
    d = mat_get_2d_direct (m);
    printf ("\n");
    for (i = 0; i < seq_get_len (seq1); i++) {
        for (j = 0; j < seq_get_len (seq2); j++) {
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


void
algn_print_dynmtrx_2d (const seq_p seq1, const seq_p seq2, nw_matrices_p matrices) {

  int i, j;

  const int seqLen1 = seq_get_len (seq1);
  const int seqLen2 = seq_get_len (seq2);

  const seq_p longerSeq  = seqLen1 > seqLen2 ? seq1 : seq2;
  const seq_p lesserSeq  = seqLen1 > seqLen2 ? seq2 : seq1;

  const int longerSeqLen = seqLen1 > seqLen2 ? seqLen1 : seqLen2;
  const int lesserSeqLen = seqLen1 > seqLen2 ? seqLen2 : seqLen1;

  const int n       = longerSeqLen + 1;
  const int m       = lesserSeqLen + 1;

  int *nw_costMtx;

  nw_costMtx = mat_get_2d_nwMtx (matrices);

  DIR_MTX_ARROW_t *nw_dirMtx  = mat_get_2d_direct (matrices);

  printf ("Sequence 1 length: %d\n", seqLen1);
  printf ("Sequence 2 length: %d\n", seqLen2);
  printf ("Length    Product: %d\n", seqLen1 * seqLen2);
  printf ("Length +1 Product: %d\n", n * m);
  printf ("Allocated space  : %zu\n\n", matrices->cap_nw);

  printf("Cost matrix:\n");

  // print column heads

  printf("  x |       * ");
  for (i = 1; i < longerSeqLen; i++) {
    printf("%7d ", longerSeq->seq_begin[i]);
  }
  printf("\n");
  printf(" ---+-");

  for (i = 0; i < longerSeqLen; i++) {
    printf("--------");
  }
  printf("\n");

  for (i = 0; i < lesserSeqLen; i++) {
    if (i == 0) printf ("  * | ");
    else        printf (" %2d | ", lesserSeq->seq_begin[i]);

    // We do this because only the last two "rows" are ever used and constantly
    // overwritten as the algorithm moves from the first "row" to the last row.
    // This means that only the last two "rows" are preserved in memory.
    // We place this loop here to pad out the unkown values.
    //
    // Also, the algorithm works on the transpose of the standard needleman-wunsh
    // matrix, so we actually access the "rows" as columns in the traditional
    // needleman-wunsh matrix with the longer sequence on the top and the shorter
    // on the left.
    //
    // The result of these design choices is that we can only see the last two
    // columns in the "propper" cost matrix. Hurray! /s
    for (j = 0; j < longerSeqLen - 2; j++) {
      // if (j == 0 && i == 0) {
      //     printf("%7d ", 0);
      // } else {
      printf ("      ? ");
      // }
    }
    for (j = 0; j < 2; j++) {
      // if (j == 0 && i == 0) {
      //     printf("%7d ", 0);
      // } else {
      printf ("%7d ", nw_costMtx[lesserSeqLen * j + i]);
      // }
    }
    printf ("\n");
  }

  // Print direction matrix
  setlocale(LC_CTYPE, "en_US.UTF-8");

  wchar_t *name;
  printf("\n\nDirection matrix:\n");

  // print column heads
  printf("  x |       * ");
  for (i = 1; i < longerSeqLen; i++) {
    printf("%7d ", longerSeq->seq_begin[i]);
  }
  printf("\n");
  printf(" ---+-");

  for (i = 1; i < longerSeqLen + 1; i++) {
    printf("--------");
  }
  printf("\n");

  for (i = 0; i < lesserSeqLen; i++) {
    if (i == 0) printf ("  * | ");
    else        printf (" %2d | ", lesserSeq->seq_begin[i]);

    for (j = 0; j < longerSeqLen; j++) {
      DIR_MTX_ARROW_t dirToken = nw_dirMtx[lesserSeqLen * j + i];

      printf("    "); // leading pad
      printf("%s", dirToken & DELETE ? "<"  : " " );
      printf("%s", dirToken & ALIGN  ? "\\" : " " );
      printf("%s", dirToken & INSERT ? "^"  : " " );
      printf(" ");
      /*
      printf("    "); // leading pad
      printf("%d", dirToken`);
      wprintf(L"%s", dirToken & DELETE ? (wchar_t *) "\u2191" : (wchar_t *) " ");
      wprintf(L"%s", dirToken & ALIGN  ? (wchar_t *) "\u2196" : (wchar_t *) " ");
      wprintf(L"%s", dirToken & INSERT ? (wchar_t *) "\u2190" : (wchar_t *) " ");
      printf(" ");
      */
    }
    printf ("\n");
  }

  return;
}


void
algn_string_of_2d_direction (DIR_MTX_ARROW_t v) {
    if (v & ALIGN   ) printf ("A");
    if (v & DELETE  ) printf ("D");
    if (v & INSERT  ) printf ("I");
    if (v & ALIGN_V ) printf ("VA");
    if (v & DELETE_V) printf ("VD");
    if (v & ALIGN_H ) printf ("HA");
    if (v & INSERT_H) printf ("HI");
    return;
}

#define my_prepend(a, b) assert (a->cap > a->len); \
                        a->seq_begin = (a->seq_begin) - 1; \
                        a->len = 1 + a->len; \
                        *(a->seq_begin) = b

#define my_get(a, b) (a->seq_begin)[b]


void
algn_backtrace_2d ( const seq_p shorterSeq, const seq_p longerSeq,
                    seq_p ret_longerSeq, seq_p ret_shorterSeq,
                    const nw_matrices_p m, const cost_matrices_2d_p c,
                    int st_longerSeq, int st_shorterSeq,
                    int swapped
                   ) {
    int l, idx_longerSeq, idx_shorterSeq;
    DIR_MTX_ARROW_t  *beg, *end;
    int new_item_for_ret_longerSeq  = 0;
    int new_item_for_ret_shorterSeq = 0;
    int shifter                     = 0;
    idx_longerSeq                   = seq_get_len (longerSeq);
    idx_shorterSeq                  = seq_get_len (shorterSeq);
    l                               = idx_longerSeq * idx_shorterSeq;
    beg                             = mat_get_2d_direct (m) + st_shorterSeq; // offset by limit into matrix
    // TODO: figure out wtf this means:
    /* Stitching goes to hell now
    end = beg + (idx_shorterSeq * idx_longerSeq) + idx_shorterSeq - 1;
    */
    end = beg + (idx_longerSeq * idx_shorterSeq) - 1; // TODO: These were just initialized. Why change them already?
    l   = idx_shorterSeq;

    if (DEBUG_ALGN) {
        printf("\nst_longerSeq:   %d\n", st_longerSeq);
        printf("st_shorterSeq:  %d\n", st_shorterSeq);
        printf("idx_longerSeq:  %d\n", idx_longerSeq);
        printf("idx_shorterSeq: %d\n", idx_shorterSeq);

        if (DEBUG_DIR_M) {
            printf("\n");
            DIR_MTX_ARROW_t *beg_debug;
            int i, j;
            beg_debug = beg;

            printf ("Printing a two dimensional direction matrix.\n");
            for (i = 0; i < idx_longerSeq; i++, beg_debug += idx_shorterSeq) {
                for (j  = 0; j < idx_shorterSeq; j++) {
                    algn_string_of_2d_direction (beg_debug[j]);
                    fprintf (stdout, "\t");
                    fflush (stdout);
                    end = beg_debug + j;
                }
                fprintf (stdout, "\n");
                fflush (stdout);
            }
            fprintf (stdout, "\n");
            fflush (stdout);
        }
    }

    end = beg + (idx_shorterSeq * (idx_longerSeq - 1)) + idx_shorterSeq - 1;

    idx_longerSeq  += st_longerSeq;
    idx_shorterSeq += st_shorterSeq;
    // The following pair of while loops are the same lines of code, each
    // has swapped INSERT and DELETE procedures, so that depending on the ordering
    // of the two sequences (swap) either INSERTING or DELETING will be preferred.
    // During the downpass and all the optimization procedures, keeping the same
    // ordering for the medians output is important to keep consistency in the
    // diagnosis at every step. In other words, if a join is performed starting
    // in any position of the tree, it is necessary to make sure that the very
    // same median would be produced if the calculation started in any of its
    // children.
    // Note that these two lines could be defined as macros, but we (Andres?) have
    // decided not to do so to keep it readable. Besides, once correct, there is
    // nothing (or very few things) to do here.
    if (!(cm_get_affine_flag (c))) { // not affine
        if (swapped) {               // swapped
            while (end >= beg) {
                if (*end & ALIGN) {
                    idx_longerSeq--;
                    new_item_for_ret_longerSeq = my_get(longerSeq, idx_longerSeq);
                    my_prepend(ret_longerSeq, new_item_for_ret_longerSeq);
                    idx_shorterSeq--;
                    new_item_for_ret_shorterSeq = my_get(shorterSeq, idx_shorterSeq);
                    my_prepend(ret_shorterSeq, new_item_for_ret_shorterSeq);
                    end -= l + 1;
                    if (DEBUG_ALGN) {
                        printf("Align:\n");
                        printf("  idx_longerSeq:    %d, idx_shorterSeq:    %d\n", idx_longerSeq, idx_shorterSeq);
                        printf("  new item a: %d, new item b: %d\n", *ret_longerSeq->seq_begin, *ret_shorterSeq->seq_begin);
                    }
                }
                else if (*end & INSERT) {
                    new_item_for_ret_longerSeq = cm_get_gap_char_2d (c);
                    my_prepend(ret_longerSeq, new_item_for_ret_longerSeq);
                    idx_shorterSeq--;
                    new_item_for_ret_shorterSeq = my_get(shorterSeq, idx_shorterSeq);
                    my_prepend(ret_shorterSeq, new_item_for_ret_shorterSeq);
                    end -= 1;
                    if (DEBUG_ALGN) {
                        printf("Insert:\n");
                        printf("  idx_longerSeq:    %d, idx_shorterSeq:    %d\n", idx_longerSeq, idx_shorterSeq);
                        printf("  new item a: %d, new item b: %d\n", new_item_for_ret_longerSeq, new_item_for_ret_shorterSeq);
                    }
                }
                else if ((*end & DELETE)) {
                    idx_longerSeq--;
                    new_item_for_ret_longerSeq = my_get (longerSeq, idx_longerSeq);
                    my_prepend(ret_longerSeq, new_item_for_ret_longerSeq);
                    new_item_for_ret_shorterSeq = cm_get_gap_char_2d (c);
                    my_prepend(ret_shorterSeq, new_item_for_ret_shorterSeq);
                    end -= l;
                    if (DEBUG_ALGN) {
                        printf("Delete:\n");
                        printf("  idx_longerSeq:    %d, idx_shorterSeq:    %d\n", idx_longerSeq, idx_shorterSeq);
                        printf("  new item a: %d, new item b: %d\n", new_item_for_ret_longerSeq, new_item_for_ret_shorterSeq);
                    }
                }
                else { // something terrible has happened!!!!
                    printf("Error. Alignment cost matrix:\n");
                    algn_print_dynmtrx_2d (longerSeq, shorterSeq, m);
                    printf("*beg: %d\n", *beg);
                    printf("*end: %d\n", *end);
                    printf("beg: 0x%d\n", (unsigned int) beg);
                    printf("end: 0x%d\n", (unsigned int) end);
                    int limit = end - beg;
                    for (int i = 0; i <= limit; i++)
                    {
                         printf("%d, ", (beg[i]));
                    }
                    printf("\n");
                    assert (*end & (ALIGN | INSERT | DELETE));
                }
            }
        } else { // not affine, not swapped
            while (end >= beg) {
                if (*end & ALIGN) {
                    idx_longerSeq--;
                    new_item_for_ret_longerSeq = my_get(longerSeq, idx_longerSeq);
                    my_prepend(ret_longerSeq, new_item_for_ret_longerSeq);
                    idx_shorterSeq--;
                    new_item_for_ret_shorterSeq = my_get(shorterSeq, idx_shorterSeq);
                    my_prepend(ret_shorterSeq, new_item_for_ret_shorterSeq);
                    end -= l + 1;
                }
                else if (*end & DELETE) {
                    idx_longerSeq--;
                    new_item_for_ret_longerSeq = my_get(longerSeq, idx_longerSeq);
                    my_prepend(ret_longerSeq, new_item_for_ret_longerSeq);
                    new_item_for_ret_shorterSeq = cm_get_gap_char_2d (c);
                    my_prepend(ret_shorterSeq, new_item_for_ret_shorterSeq);
                    end -= l;
                }
                else {
                    assert (*end & INSERT);
                    new_item_for_ret_longerSeq = cm_get_gap_char_2d (c);
                    my_prepend(ret_longerSeq, new_item_for_ret_longerSeq);
                    idx_shorterSeq--;
                    new_item_for_ret_shorterSeq = my_get(shorterSeq, idx_shorterSeq);
                    my_prepend(ret_shorterSeq, new_item_for_ret_shorterSeq);
                    end -= 1;
                }
            }
        }
    } else {             // affine
        if (swapped) {   // swapped
            while (end >= beg) {
                if (*end & (ALIGN << shifter)) {
                    if (0 == shifter) {
                        if (DEBUG_BT) printf ("1\t");
                        idx_longerSeq--;
                        new_item_for_ret_longerSeq = my_get(longerSeq, idx_longerSeq);
                        my_prepend(ret_longerSeq, new_item_for_ret_longerSeq);
                        idx_shorterSeq--;
                        new_item_for_ret_shorterSeq = my_get(shorterSeq, idx_shorterSeq);
                        my_prepend(ret_shorterSeq, new_item_for_ret_shorterSeq);
                        end -= l + 1;
                    } else if (SHIFT_V == shifter) {
                        if (DEBUG_BT) printf ("2\t");
                        idx_longerSeq--;
                        new_item_for_ret_longerSeq = my_get (longerSeq, idx_longerSeq);
                        my_prepend(ret_longerSeq, new_item_for_ret_longerSeq);
                        new_item_for_ret_shorterSeq = cm_get_gap_char_2d (c);
                        my_prepend(ret_shorterSeq, new_item_for_ret_shorterSeq);
                        end -= l;
                        shifter = 0;
                    } else {
                        if (DEBUG_BT) printf ("3\t");
                        assert (SHIFT_H == shifter);
                        new_item_for_ret_longerSeq = cm_get_gap_char_2d (c);
                        my_prepend(ret_longerSeq, new_item_for_ret_longerSeq);
                        idx_shorterSeq--;
                        new_item_for_ret_shorterSeq = my_get(shorterSeq, idx_shorterSeq);
                        my_prepend(ret_shorterSeq, new_item_for_ret_shorterSeq);
                        end -= 1;
                        shifter = 0;
                    }
                }
                else if (*end & (INSERT << shifter)) {
                    if (0 == shifter) {
                        if (DEBUG_BT) printf ("4\t");
                        shifter = SHIFT_H;
                    } else if (SHIFT_H == shifter) {
                        if (DEBUG_BT) printf ("5\t");
                        new_item_for_ret_longerSeq = cm_get_gap_char_2d (c);
                        my_prepend(ret_longerSeq, new_item_for_ret_longerSeq);
                        idx_shorterSeq--;
                        new_item_for_ret_shorterSeq = my_get(shorterSeq, idx_shorterSeq);
                        my_prepend(ret_shorterSeq, new_item_for_ret_shorterSeq);
                        end -= 1;
                    } else {
                        if (DEBUG_BT) printf ("6\t");
                        assert (0);
                    }
                } else {
                    assert (*end & (DELETE << shifter));
                    if (0 == shifter) {
                        if (DEBUG_BT) printf ("7\t");
                        shifter = SHIFT_V;
                    } else if (SHIFT_V == shifter) {
                        if (DEBUG_BT) printf ("8\t");
                        idx_longerSeq--;
                        new_item_for_ret_longerSeq = my_get (longerSeq, idx_longerSeq);
                        my_prepend(ret_longerSeq, new_item_for_ret_longerSeq);
                        new_item_for_ret_shorterSeq = cm_get_gap_char_2d (c);
                        my_prepend(ret_shorterSeq, new_item_for_ret_shorterSeq);
                        end -= l;
                    } else {
                        if (DEBUG_BT) printf ("9\t");
                        assert (0);
                    }
                }
            } // end while
        } else { // affine, not swapped
            while (end >= beg) {
                if (*end & (ALIGN << shifter)) {
                    if (0 == shifter) {
                        idx_longerSeq--;
                        new_item_for_ret_longerSeq = my_get(longerSeq, idx_longerSeq);
                        my_prepend(ret_longerSeq, new_item_for_ret_longerSeq);
                        idx_shorterSeq--;
                        new_item_for_ret_shorterSeq = my_get(shorterSeq, idx_shorterSeq);
                        my_prepend(ret_shorterSeq, new_item_for_ret_shorterSeq);
                        end -= l + 1;
                    } else if (SHIFT_V == shifter) {
                        idx_longerSeq--;
                        new_item_for_ret_longerSeq = my_get (longerSeq, idx_longerSeq);
                        my_prepend(ret_longerSeq, new_item_for_ret_longerSeq);
                        new_item_for_ret_shorterSeq = cm_get_gap_char_2d (c);
                        my_prepend(ret_shorterSeq, new_item_for_ret_shorterSeq);
                        end -= l;
                        shifter = 0;
                    } else {
                        assert (SHIFT_H == shifter);
                        new_item_for_ret_longerSeq = cm_get_gap_char_2d (c);
                        my_prepend(ret_longerSeq, new_item_for_ret_longerSeq);
                        idx_shorterSeq--;
                        new_item_for_ret_shorterSeq = my_get(shorterSeq, idx_shorterSeq);
                        my_prepend(ret_shorterSeq, new_item_for_ret_shorterSeq);
                        end -= 1;
                        shifter = 0;
                    }
                } else if (*end & (DELETE << shifter)) {
                    if (0 == shifter) {
                        shifter = SHIFT_V;
                    } else if (SHIFT_V == shifter) {
                        idx_longerSeq--;
                        new_item_for_ret_longerSeq = my_get (longerSeq, idx_longerSeq);
                        my_prepend(ret_longerSeq, new_item_for_ret_longerSeq);
                        new_item_for_ret_shorterSeq = cm_get_gap_char_2d (c);
                        my_prepend(ret_shorterSeq, new_item_for_ret_shorterSeq);
                        end -= l;
                    } else {
                        assert (0);
                    }
                } else {
                    assert (*end & (INSERT << shifter));
                    if (0 == shifter)
                        shifter = SHIFT_H;
                    else if (SHIFT_H == shifter) {
                        new_item_for_ret_longerSeq = cm_get_gap_char_2d (c);
                        my_prepend(ret_longerSeq, new_item_for_ret_longerSeq);
                        idx_shorterSeq--;
                        new_item_for_ret_shorterSeq = my_get(shorterSeq, idx_shorterSeq);
                        my_prepend(ret_shorterSeq, new_item_for_ret_shorterSeq);
                        end -= 1;
                    }
                    else {
                        assert (0);
                    }
                }
            } // end while
        }
    }
    return;
}

char *
algn_string_of_3d_direction (char v) {
    printf("%c\n", v);
    if      (v & A_A_A) return "ALGN-ALL";
    else if (v & A_G_A)  return "ALGN--13";
    else if (v & G_A_A)  return "ALGN--23";
    else if (v & A_G_G)    return "GAP---23";
    else if (v & G_G_A)    return "GAP---12";
    else if (v & G_A_G)    return "GAP---13";
    else if (v & A_A_G)  return "ALGN--12";
    else {
        assert (0);
    }
    return "Empty";
}

void
algn_backtrace_3d ( const seq_p seq1, const seq_p seq2, const seq_p seq3,
               seq_p ret_seq1, seq_p ret_seq2, seq_p ret_seq3,
               const cost_matrices_3d_p c, nw_matrices_p m
             ) {
    int len_dirMtx, idx_seq1, idx_seq2, idx_seq3;
    int a_plane, a_line, a_cell;
    DIR_MTX_ARROW_t *beg, *end;
    idx_seq1   = seq_get_len (seq1);
    idx_seq2   = seq_get_len (seq2);
    idx_seq3   = seq_get_len (seq3);
    len_dirMtx = idx_seq1 * idx_seq2 * idx_seq3;
    a_plane    = idx_seq2 * idx_seq3; // move one plane up (so move one down seq1) TODO: check these three assertions
    a_line     = idx_seq3;            // move one line over (so move one down seq3)
    a_cell     = 1;                   // move over 1 (so move one down seq2)
    beg        = mat_get_3d_direct (m);
    if (DEBUG_DIR_M) {
        char *toprint;
        DIR_MTX_ARROW_t *beg_debug;
        int i, j, k;
        beg_debug = beg;
        printf ("\n\n*** Printing a three dimensional direction matrix.\n");
        printf("*** Width is shortest sequence.\n");
        printf("*** Depth is middle sequence;\n");
        printf("*** Height (# of blocks) longest sequence.\n\n");
        for (i = 0; i < idx_seq1; i++) {
            for (j  = 0; j < idx_seq2; j++) {
                for (int l = idx_seq2 - j; l > 1; l--) {
                    printf("  ");
                }
                for (k = 0 ; k < idx_seq3; k++, beg_debug++) {
                    toprint = algn_string_of_3d_direction (*beg_debug);
                    printf ("%-9s  ", toprint);
                }
                printf ("\n");
            }
            printf ("\n");
        }
    }
    end = beg + len_dirMtx - 1;
    idx_seq1--;
    idx_seq2--;
    idx_seq3--;
    while (end >= beg) {
        if (*end & A_A_A) {        /* A plane, line, and cell */
            seq_prepend (ret_seq1, seq_get_element (seq1, idx_seq1--));
            seq_prepend (ret_seq2, seq_get_element (seq2, idx_seq2--));
            seq_prepend (ret_seq3, seq_get_element (seq3, idx_seq3--));
            end -= a_plane + a_line + a_cell;
        } else if (*end & A_G_A) { /* A plane and cell */
            seq_prepend (ret_seq1, seq_get_element (seq1, idx_seq1--));
            seq_prepend (ret_seq2, cm_get_gap_char_3d (c));
            seq_prepend (ret_seq3, seq_get_element (seq3, idx_seq3--));
            end -= a_plane + a_cell;
        } else if (*end & G_A_A) { /* A line and cell */
            seq_prepend (ret_seq1, cm_get_gap_char_3d (c));
            seq_prepend (ret_seq2, seq_get_element (seq2, idx_seq2--));
            seq_prepend (ret_seq3, seq_get_element (seq3, idx_seq3--));
            end -= a_line + a_cell;
        } else if (*end & A_G_G) { /* A plane */
            seq_prepend (ret_seq1, seq_get_element (seq1, idx_seq1--));
            seq_prepend (ret_seq2, cm_get_gap_char_3d (c));
            seq_prepend (ret_seq3, cm_get_gap_char_3d (c));
            end -= a_plane;
        } else if (*end & G_G_A) { /* A cell */
            seq_prepend (ret_seq1, cm_get_gap_char_3d (c));
            seq_prepend (ret_seq2, cm_get_gap_char_3d (c));
            seq_prepend (ret_seq3, seq_get_element (seq3, idx_seq3--));
            end -= a_cell;
        } else if (*end & G_A_G) { /* A line */
            seq_prepend (ret_seq1, cm_get_gap_char_3d (c));
            seq_prepend (ret_seq2, seq_get_element (seq2, idx_seq2--));
            seq_prepend (ret_seq3, cm_get_gap_char_3d (c));
            end -= a_line;
        } else if (*end & A_A_G) { /* A plane and line */
            seq_prepend (ret_seq1, seq_get_element (seq1, idx_seq1--));
            seq_prepend (ret_seq2, seq_get_element (seq2, idx_seq2--));
            seq_prepend (ret_seq3, cm_get_gap_char_3d (c));
            end -= a_plane + a_line;
        } else {
            assert (0);
        }
    }
    return;
}


void
algn_get_median_2d_with_gaps (seq_p shorterSeq, seq_p longerSeq, cost_matrices_2d_p m, seq_p sm) {
    SEQT *seq_begin_longerSeq, *seq_begin_shorterSeq;
    int interm;
    int i;
    seq_begin_longerSeq  = seq_get_seq_begin (longerSeq);
    seq_begin_shorterSeq = seq_get_seq_begin (shorterSeq);
    printf("seqLen-1 of longer: %d\n", seq_get_len (longerSeq) - 1);
    for (i = seq_get_len (longerSeq) - 1; i >= 0; i--) {
        interm = cm_get_median (m, seq_begin_longerSeq[i], seq_begin_shorterSeq[i]);
        seq_prepend (sm, interm);
    }
    return;
}

void
algn_get_median_2d_no_gaps (seq_p shorterSeq, seq_p longerSeq, cost_matrices_2d_p m, seq_p sm) {
    SEQT *seq_begin_longerSeq, *seq_begin_shorterSeq;
    int interm;
    int i;
    seq_begin_longerSeq = seq_get_seq_begin (longerSeq);
    seq_begin_shorterSeq = seq_get_seq_begin (shorterSeq);
    for (i = seq_get_len (longerSeq) - 1; i >= 0; i--) {
        interm = cm_get_median (m, seq_begin_longerSeq[i], seq_begin_shorterSeq[i]);
        if (interm != cm_get_gap_char_2d (m)) {
            seq_prepend (sm, interm);
        }
    }
    //seq_prepend (sm, cm_get_gap_char_2d (m)); // TODO: make sure this shouldn't be here
    return;
}

void
algn_remove_gaps (int gap_char, seq_p s) {
    int i, len;
    len = seq_get_len (s);
    SEQT *source, *destination;
    int newlen = 0;
    source = destination = s->end;
    for (i = len - 1; i >= 0; i--) {
        if (gap_char != *source) {
            *destination = *source;
            destination--;
            newlen++;
        }
        source--;
    }
    s->len = newlen;
    s->seq_begin = destination + 1;
    /* We restore the leading gap */
    seq_prepend (s, gap_char);
    return;
}

void
algn_correct_blocks_affine (int gap_char, seq_p s, seq_p a, seq_p b) {
    int i, len, ab, bb, sb, extending_gap,
        inside_block = 0, prev_block = 0;
    len = seq_get_len (s);
    extending_gap = 0;
    inside_block = 0;
    for (i = 0; i < len; i++) {
        ab = seq_get_element (a, i);
        bb = seq_get_element (b, i);
        sb = seq_get_element (s, i);
        if (i > 2) {

        }
        if (!inside_block && (!(ab & gap_char) || !(bb & gap_char)))
            inside_block = 0;
        else if (inside_block && ((!(ab & gap_char)) || (!(bb & gap_char))))
            inside_block = 0;
        else if (((ab & gap_char) || (bb & gap_char)) && ((ab != gap_char) || (bb != gap_char)))
            inside_block = 1;
        else
            inside_block = 0;
        if (((gap_char & ab) || (gap_char & bb)) && (!(sb & gap_char)) && !extending_gap) {
            prev_block = inside_block;
            extending_gap = 1;
        }
        else if ((gap_char & ab) && (gap_char & bb) && (sb & gap_char) && (sb != gap_char) &&
                extending_gap && inside_block && !prev_block) {
            sb = (~gap_char) & sb;
            prev_block = 0;
        }
        else if ((gap_char & ab) && (gap_char & bb) && (1 == extending_gap)) {
            prev_block = inside_block;
            extending_gap = 0;
        }

        seq_set (s, i, sb);
    }
    algn_remove_gaps (gap_char, s);
    return;
}

inline void
algn_ancestor_2 (seq_p seq1, seq_p seq2, cost_matrices_2d_p m, seq_p sm ) {
    SEQT *seq_begin1, *seq_begin2;
    int interm;
    int i, gap_char, is_combinations, cost_model;
    seq_begin1 = seq_get_seq_begin (seq1);
    seq_begin2 = seq_get_seq_begin (seq2);
    gap_char = cm_get_gap_char_2d (m);
    is_combinations = m->combinations;
    cost_model = m->cost_model_type;
    for (i = seq_get_len (seq1) - 1; i >= 0; i--) {
        interm = cm_get_median (m, seq_begin1[i], seq_begin2[i]);
        if ((!is_combinations) || (1 != cost_model)) {
            if (interm != gap_char) seq_prepend (sm, interm);
        }
        else seq_prepend (sm, interm);
    }
    if ((!is_combinations) || ((1 != cost_model) && (gap_char != seq_get_element (sm, 0))))
        seq_prepend (sm, gap_char);
    else if (is_combinations)
        algn_correct_blocks_affine (gap_char, sm, seq1, seq2);
    return;
}



/*
 * Given three aligned sequences seq1, seq2, and seq3, the median between them is
 * returned in the sequence sm, using the cost matrix stored in m.
 */
void
algn_get_median_3d (seq_p seq1, seq_p seq2, seq_p seq3,
                    cost_matrices_3d_p m, seq_p sm) {
    SEQT *end1, *end2, *end3;
    int interm;
    int i;
    end1 = seq_get_end (seq1);
    end2 = seq_get_end (seq2);
    end3 = seq_get_end (seq3);
    for (i = seq_get_len (seq1) - 1; i >= 0; i--) {
        interm = cm_get_median_3d (m, *end1, *end2, *end3);
        seq_prepend (sm, interm);
    }
    return;
}

void
algn_union (seq_p shorterSeq, seq_p longerSeq, seq_p su) {
    assert (seq_get_len (longerSeq) == seq_get_len (shorterSeq));
    assert (seq_get_cap (longerSeq) >= seq_get_len (shorterSeq));
    int len, i;
    len = seq_get_len (longerSeq);
    for (i = len - 1; i >= 0; i--)
        seq_prepend (su, (seq_get_element (longerSeq, i) | seq_get_element (shorterSeq, i)));
    return;
}
