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

#ifndef MATRICES_H

#define MATRICES_H 1
#define DIAGONAL (1 << 0)
#define BEHIND (1 << 1)
#define UPPER (1 << 2)
#define ALIGN DIAGONAL 
#define INSERT BEHIND
#define DELETE UPPER
#define SHIFT_V 3
#define SHIFT_H 6
#define ALIGN_V (ALIGN << SHIFT_V)
#define DELETE_V (DELETE << SHIFT_V) 
#define ALIGN_H (ALIGN << SHIFT_H)
#define INSERT_H (INSERT << SHIFT_H)
#define P1 (1 << 0)
#define P2 (1 << 1)
#define P3 (1 << 2)
#define S1 (1 << 3)     /** Align the sequence from s2 and s3 */
#define S2 (1 << 4)     /** Align the three bases */
#define S3 (1 << 5)     /** Align the sequence from s1 and s3 */
#define SS (1 << 6)

#define DIRECTION_MATRIX unsigned short

#define Matrices_struct(a) ((struct matrices *) Data_custom_val(a))

struct matrices {
                                  /****** In each of the following calculations, seq length includes opening gap *******/
    int len;                      /* Total length of available memory allocated to matrix or cube == for 2D: 12 * max(len_s1, len_s2)
                                   *                                                                 for 3D: len_s1 * len_s2 * len_s3 
                                   */
    int len_eff;                  // Length of the efficiency matrix; at least as large as len
    int len_pre;                  // Length of the precalculated matrix == max(len_s1, len_s2) * (alphSize + 1) ---extra 1 is for gap 
    int *matrix;                  // Matrix for regular alignment 
    DIRECTION_MATRIX *dir_mtx_2d; // Matrix for directions in a 2d alignment 
    int **pointers_3d;            // Matrix of pointers to each row in a 3d align 
    int *cube;                    // Matrix for 3d alignment 
    DIRECTION_MATRIX *cube_d;     // Matrix for directions in a 3d alignment 
    int *precalc;                 // Matrix of precalculated arrays 
};

typedef struct matrices * nw_matrices_p;

void print_matrices(nw_matrices_p m, int alphSize);

/* 
 * Calculates the amount of memory required to perform a three dimensional
 * alignment between sequences of length w, d, h with ukkonen barriers set to k
 */
int
mat_size_of_3d_matrix (int w, int d, int h, int k);

/*
 * Calculates the amount of memory required to perform a two dimensional
 * alignment between sequences of length w and d. This is a small amount of
 * memory, so no ukkonen barriers for this. 
 */
int
mat_size_of_2d_matrix (int w, int h);

/*
 * Rearrange or reallocate memory if necessary to perform an alignment between
 * sequences of length w, d and h. Note that for 2d alignments is necessary to
 * set h=0, and uk=0. 
 */
int
mat_setup_size (nw_matrices_p m, int len_seq1, int len_seq2, int len_seq3, int is_ukk, int lcm);

/* 
 * Gets the pointer to the first memory position of the 2d alignment matrix. 
 */
int *
mat_get_2d_matrix (nw_matrices_p m);

int *
mat_get_2d_prec (const nw_matrices_p m);

int *
mat_get_3d_prec (const nw_matrices_p m);

DIRECTION_MATRIX *
mat_get_2d_direct (const nw_matrices_p m);

/* 
 * Gets a pointer to the first memory positon of the matrix of row pointers for
 * 3d aligments.
 */
int **
mat_get_3d_pointers (nw_matrices_p m);

/*
 * Gets a pointer to the first memory position of the memory batch (this is not
 * a matrix!) for the 3d alignments. 
 */
int *
mat_get_3d_matrix (nw_matrices_p m);

DIRECTION_MATRIX *
mat_get_3d_direct (nw_matrices_p m);

/* Printout the contents of the matrix */
void
mat_print_algn_2d (nw_matrices_p m, int w, int h);

void
mat_print_algn_3d (nw_matrices_p m, int w, int h, int d);

#endif /* MATRICES_H */