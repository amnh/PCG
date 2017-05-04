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

#include <stdio.h>
#include <stdlib.h>

#include "debug_constants.h"
#include "nwMatrices.h"
// #include "cm.h"


/*
 * For memory management efficiency, I will keep all the matrices in one big
 * chunk of memory, that I can reallocate as a whole, and reduce fragmentation
 * a lot if possible, all the alignment calculations and all the matrices that
 * are precomputed to speedup the alignments are held here.
 */
inline size_t
mat_size_of_3d_matrix (size_t w, size_t d, size_t h) { // originally had a fourth parameter, k for ukkunonen
    /* Not sure what this was for, as it was commented out, but kept for posterity's sake
       int basic_cube;
       int prism_1, prism_2, pyramid;
       basic_cube = k * k * k;
       prism_1 = (d - k) * (d - k) * k;
       prism_2 = (w - k) * (w - k) * k;
       pyramid = (w - k) * (w - k + 1) * (2 * (w - k) + 1) / 6;
       if (h > 2 * k) basic_cube += (h - (2 * k)) * w * d;
       return (basic_cube + prism_2 + prism_1 + pyramid);
    */
    return (w * d * h);
}

void print_matrices(nw_matrices_p nwMatrix, size_t alphSize) {
    printf("\nMatrices:\n");
    printf("    NW Matrix cap:         %zu\n", nwMatrix->cap_nw);
    printf("    Efficiency mtx cap:    %d\n",  nwMatrix->cap_eff);
    printf("    precalcMtx mtx cap:    %zu\n", nwMatrix->cap_pre);

    printf("\n    precalcMtxulated nw matrix:\n");
    for( size_t i = 0; i < nwMatrix->cap_pre; i += alphSize) {
        printf("    ");
        for( size_t j = 0; j < alphSize; j++) {
            printf("%4d", nwMatrix->precalcMtx[i + j]);
        }
        printf("\n");
    }

}

// The 12 is because we only use 2 rows of the matrix at a time on the alignment matrix,
// and we have four alignment matrices plus two shorter ones for the gap costs.
inline size_t
mat_size_of_2d_matrix (size_t w, size_t h) {
    if (w > h) return (w * 12); //TODO: do I need a cast here?
    else       return (h * 12);
}

void
mat_clean_direction_matrix (nw_matrices_p nwMatrix) {
    size_t cap = nwMatrix->cap_nw;
    size_t i;
    for (i = 0; i < cap; i++) {
        nwMatrix->nw_dirMtx[i] = (DIR_MTX_ARROW_t) 0;
    }
}

/** Allocate or reallocate space for the six matrices, for both 2d and 3d alignments.
 *  @param slphabetSize is length of original alphabet including gap.
 *  Checks current allocation size and increases size if necessary.
 */
inline void
mat_setup_size (nw_matrices_p nwMatrix, size_t len_char1, size_t len_char2, size_t len_char3, size_t alphabetSize) {
    if(DEBUG_MAT) {
        printf("\n---mat_setup_size\n");
        printf("capacity: %zu\nefficiency: %d\nprecalc: %zu\n", nwMatrix->cap_nw, nwMatrix->cap_eff, nwMatrix->cap_pre);
    }
    int cap;
    size_t cap_2d = 0,
           cap_precalcMtx,
           cap_dir;
    //cap_dir     = (len_char1 + 1) * (len_char2 + 1);

    if (len_char3 == 0) {           /* If the size setup is only for 2d */
        cap            = (int) mat_size_of_2d_matrix (len_char1, len_char2);
        cap_precalcMtx = (1 << alphabetSize) * len_char1;
        cap_dir        = (len_char1 + 1) * (len_char2 + 1);
    } else {                       /* If the size setup is for 3d */
        cap            = (int) mat_size_of_3d_matrix (len_char1, len_char2, len_char3);
        cap_precalcMtx = (1 << alphabetSize) * (1 << alphabetSize) * len_char2;  // TODO: why character 2?
        cap_2d         = len_char1 * len_char2;
        cap_dir        = cap_2d * len_char3;
    }
    if (DEBUG_MAT) {
        printf("cap_eff: %d, \ncap_nw: %d\n", nwMatrix->cap_eff, cap);
    }
    if (nwMatrix->cap_eff < cap) {         /* If the current 2d or 3d matrix is not large enough */
        if (DEBUG_MAT) {
            printf("cap_eff too small. New allocation: %d\n", cap);
        }
        nwMatrix->nw_costMtx3d_d =
        nwMatrix->nw_costMtx =
            realloc (nwMatrix->nw_costMtx, (cap * sizeof(int)));

        nwMatrix->cap_eff = cap;
    }
    if (nwMatrix->cap_nw < cap_dir) {         /* If the other matrices are not large enough */
        if (DEBUG_MAT) {
            printf("cap nw cost mtx too small. New allocation: %zu\n", cap_dir);
        }
        nwMatrix->nw_dirMtx3d_d =
        nwMatrix->nw_dirMtx =
                realloc (nwMatrix->nw_dirMtx, cap_dir * sizeof(DIR_MTX_ARROW_t) );

        if (cap_2d) {
            if (DEBUG_MAT) {
                printf("\n3d alignment. cap_2d: %zu\n", cap_2d);
            }
        }
        nwMatrix->cap_nw = cap_dir;
    }
    if (nwMatrix->cap_pre < cap_precalcMtx) {
        if (DEBUG_MAT) {
            printf("precalc matrix too small. New allocation: %zu\n", cap_precalcMtx);
        }
        nwMatrix->precalcMtx = realloc (nwMatrix->precalcMtx, cap_precalcMtx * sizeof(int));
        nwMatrix->cap_pre    = cap_precalcMtx;
    }
    /* Check if there is an allocation error then abort program */
    if ((cap > 0) && (nwMatrix->nw_costMtx3d_d == NULL)) {
        printf("capacity: %d:\n", cap);
        printf("Memory allocation problem in cost matrix.\n");
        exit(1);
        // failwith ("Memory allocation problem in nw_costMtx3d_d.");
    }
    if ((cap_dir > 0) && (nwMatrix->nw_dirMtx == NULL)) {
        printf("Memory allocation problem in direction matrix\n");
        exit(1);
        // failwith ("Memory allocation problem in nw_dirMtx");
    }
    if ((cap_precalcMtx > 0) && (nwMatrix->precalcMtx == NULL)) {
        printf("Memory allocation problem in precalc matrix\n");
        exit(1);
        // failwith ("Memory allocation problem in precalcMtx");
    }
    if (DEBUG_MAT) {
        printf("\nFinal allocated size of matrices:\n" );
        printf("    efficiency: %d\n",  nwMatrix->cap_eff);
        printf("    nw matrix:  %zu\n", nwMatrix->cap_nw);
        printf("    precalcMtx: %zu\n", nwMatrix->cap_pre);
    }
}


void
mat_print_algn_2d (nw_matrices_p nwMatrix, size_t w, size_t h) {
    int *nwCostMatrix = nwMatrix->nw_costMtx;

    size_t i, j;

    for (i = 0; i < h; i++) {
        for (j = 0; j < w; j++)
            fprintf (stdout, "%d\t", *(nwCostMatrix + (w * i) + j));
        fprintf (stdout, "\n");
    }
    fprintf (stdout, "\n");
}


void
mat_print_algn_3d (nw_matrices_p alignmentMatrices, size_t w, size_t h, size_t d) {
    int *costs;

    size_t i, j, k, pos;

    costs = alignmentMatrices->nw_costMtx3d_d;
    for (i = 0; i < h; i++) {
        for (j = 0; j < d; j++) {
            for (k = 0; k < w; k++) {
                pos = (i * d * w) + (d * j) + k;
                fprintf (stdout, "%d\t", *(costs + pos));
            }
            fprintf (stdout, "\n");
        }
        fprintf (stdout, "\n");
    }
    fprintf (stdout, "\n");
}
