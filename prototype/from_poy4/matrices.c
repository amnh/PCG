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

#include "matrices.h"

#define DEBUG 1


/* 
 * For memory management efficiency, I will keep all the matrices in one big
 * chunck of memory, that I can reallocate as a whole, and reduce fragmentation
 * a lot if possible, all the alignment calculations and all the matrices that
 * are precomputed to speedup the alignments are held here.
 */
inline int
mat_size_of_3d_matrix (int w, int d, int h, int k) {
    /*
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

void print_matrices(matricest m) {
    printf("\nMatrices:\n");
    printf("  Allocated len:         %d\n", m->len);
    printf("  3D efficiency mtx len: %d\n", m->len_eff);
    printf("  Precalc mtx len:       %d\n", m->len_pre);

    printf("\n  Precalculated nw matrix:\n");
    for( size_t i = 0; i < m->len_pre; i++) {
        printf("    %d\t", m->precalc[i]);
    }
    printf("\n");
}

inline int
mat_size_of_2d_matrix (int w, int h) {
    if (w > h) return (w * 12);
    else return (h * 12);
}

void
mat_clean_direction_matrix (matricest m) {
    int len = m->len;
    int i;
    for (i = 0; i < len; i++) 
        m->dir_mtx_2d[i] = (DIRECTION_MATRIX) 0;
    return;
}

inline int
mat_setup_size (matricest m, int w, int d, int h, int is_ukk, int lcm) {
    if(DEBUG) {
        printf("\n---mat_setup_size\n");
    }
    int len, len_2d, len_precalc, len_dir;
    if (h == 0) {           /* If the size setup is only for 2d */
        len = mat_size_of_2d_matrix (w, d);
        len_precalc = (1 << lcm) * w;
        len_dir = (w + 1) * (d + 1);
        len_2d = 0;
    } else {                /* If the size setup is for 3d */
        len = mat_size_of_3d_matrix (w, d, h, is_ukk);
        len_precalc = (1 << lcm) * (1 << lcm) * d;
        len_2d = w * d;
        len_dir = len_2d * h;
    }
    if (m->len_eff < len) { /* If the 3d or 2d matrix is not enough */
        m->cube = m->matrix = 
            (int *) realloc (m->matrix, (len * sizeof(int)));
        m->len_eff = len;
    }
    if (m->len < len_dir) { /* If the other matrices are not enough */
        m->cube_d = m->dir_mtx_2d = 
            (DIRECTION_MATRIX *) 
            realloc (m->dir_mtx_2d, (len_dir * sizeof(DIRECTION_MATRIX)));
        if (0 != len_2d) {
            m->pointers_3d = 
                (int **) realloc (m->pointers_3d, len_2d * sizeof(int));
            if (m->pointers_3d == NULL) {
                printf("Memory allocation problem in pointers 3d.\n");
                exit(1);
                // failwith ("Memory allocation problem in pointers 3d.");
            }
        }
        m->len = len_dir;
    }
    if (m->len_pre < len_precalc) {
        m->precalc = (int *) realloc (m->precalc, len_precalc * sizeof(int));
        m->len_pre = len_precalc;
    }
    /* Check if there is an allocation error then abort program */
    if ((len > 0) && (m->cube == NULL)) {
        printf("Memory allocation problem in cube.\n");
        exit(1);
        // failwith ("Memory allocation problem in cube.");
    }
    if ((len_dir > 0) && (m->dir_mtx_2d == NULL)) {
        printf("Memory allocation problem in dir_mtx_2d\n");
        exit(1);
        // failwith ("Memory allocation problem in dir_mtx_2d");
    }
    if ((len_precalc > 0) && (m->precalc == NULL)) {
        printf("Memory allocation problem in precalc\n");
        exit(1);
        // failwith ("Memory allocation problem in precalc");
    }
    return 0;
}

int *
mat_get_2d_prec (const matricest m) {
    return (m->precalc);
}

int *
mat_get_3d_prec (const matricest m) {
    return (m->precalc);
}

int *
mat_get_2d_matrix (matricest m) {
    return (m->matrix);
}

DIRECTION_MATRIX *
mat_get_2d_direct (const matricest m) {
    return (m->dir_mtx_2d);
}

int **
mat_get_3d_pointers (matricest m) {
    return (m->pointers_3d);
}

int *
mat_get_3d_matrix (matricest m) {
    return (m->cube);
}

DIRECTION_MATRIX *
mat_get_3d_direct (matricest m) {
    return (m->cube_d);
}

/*
void
mat_CAML_free (value m) {
    matricest tmp;
    tmp = Matrices_struct(m);
    free (tmp->matrix);
    free (tmp->cube);
    free (tmp->pointers_3d);
    free (tmp->precalc);
    return;
}

void
mat_CAML_serialize (value c, unsigned long *wsize_32, \
        unsigned long *wsize_64) {
    CAMLparam1(c);
    *wsize_64 = *wsize_32 = sizeof (struct matrices);
    CAMLreturn0;
}

unsigned long
mat_CAML_deserialize (void *v) {
    matricest m;
    m = (matricest) v;
    m->len_pre = m->len_eff = m->len = 0;
    m->matrix = m->cube = m->precalc = NULL;
    m->dir_mtx_2d = m->cube_d = NULL;
    m->pointers_3d = NULL;
    return (sizeof (struct matrices));
}


static struct custom_operations alignment_matrix = {
    "http://www.amnh.org/poy/alignment_matrix/alignment_matrix0.1",
    &mat_CAML_free,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};
*/

/*
value 
mat_CAML_create_general (value a) {
    CAMLparam1(a);
    CAMLlocal1(res);
    matricest m;
    res = 
        alloc_custom (&alignment_matrix, sizeof(struct matrices), 1, 1000);
    m = Matrices_struct(res);
    m->len_pre = m->len_eff = m->len = 0;
    m->matrix = m->cube = m->precalc = NULL;
    m->dir_mtx_2d = m->cube_d = NULL;
    m->pointers_3d = NULL;
    CAMLreturn(res);
}
*/

void
mat_print_algn_2d (matricest m, int w, int h) {
    int *mm;
    int i, j;
    mm = mat_get_2d_matrix (m);
    for (i = 0; i < h; i++) {
        for (j = 0; j < w; j++)
            fprintf (stdout, "%d\t", *(mm + (w * i) + j));
        fprintf (stdout, "\n");
    }
    fprintf (stdout, "\n");
    return;
}

/*
value
mat_CAML_print_algn_2d (value res, value mw, value mh) {
    CAMLparam3(res, mw, mh);
    matricest m;
    int w, h;
    m = Matrices_struct(res);
    w = Int_val(mw);
    h = Int_val(mh);
    mat_print_algn_2d (m, w, h);
    CAMLreturn (Val_unit);
}

value
mat_CAML_get_value (value res, value mw, value mh, value vrow, value vcolumn) {
    CAMLparam5(res, mw, mh, vrow, vcolumn);
    matricest m;
    int w, h, row, column; 
    DIRECTION_MATRIX *mm;
    m = Matrices_struct(res);
    w = Int_val(mw);
    h = Int_val(mh);
    row = Int_val(vrow);
    column = Int_val(vcolumn);
    mm = mat_get_2d_direct(m);
    CAMLreturn (Val_int(*(mm + (row * w) + column)));
}
*/

void
mat_print_algn_3d (matricest m, int w, int h, int d) {
    int *mm;
    int i, j, k, pos;
    mm = mat_get_3d_matrix (m);
    for (i = 0; i < h; i++) {
        for (j = 0; j < d; j++) {
            for (k = 0; k < w; k++) {
                pos = (i * d * w) + (d * j) + k;
                fprintf (stdout, "%d\t", *(mm +pos));
            }
            fprintf (stdout, "\n");
        }
        fprintf (stdout, "\n");
    }
    fprintf (stdout, "\n");
    return;
}

/*
value
mat_CAML_print_algn_3d (value res, value mw, value mh, value md) {
    CAMLparam4(res, mw, mh, md);
    matricest m;
    int w, h, d;
    m = Matrices_struct(res);
    w = Int_val(mw);
    h = Int_val(mh);
    d = Int_val(md);
    mat_print_algn_3d (m, w, h, d);
    CAMLreturn (Val_unit);
}

value 
mat_CAML_initialize (value unit) {
    CAMLparam1(unit);
    caml_register_custom_operations (&alignment_matrix);
    CAMLreturn(Val_unit);
}

value 
mat_CAML_flush_memory (value vm) {
    CAMLparam1(vm);
    matricest m;
    m = Matrices_struct(vm);
    free (m->matrix);
    free (m->dir_mtx_2d);
    free (m->precalc);
    m->len_pre = m->len_eff = m->len = 0;
    m->matrix = m->cube = m->precalc = NULL;
    m->dir_mtx_2d = m->cube_d = NULL;
    m->pointers_3d = NULL;
    CAMLreturn(Val_unit);
}

value
mat_CAML_clear_direction (value vm) {
    CAMLparam1(vm);
    matricest m;
    m = Matrices_struct(vm);
    mat_clean_direction_matrix (m);
    CAMLreturn(Val_unit);
}
*/
