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

/* 
 * Implementation of an array with negative indices.
 * This implementation creates an array with index positions 
 * -length ... 0 ... length.
 */

#include <stdlib.h>
#include "zarr.h"

#define ARRCHECK 1

int 
zarr_test_pos (const zarrt arr, int pos) {
#ifdef ARRCHECK
    if ((pos >= 0) && (pos <= arr->length)) return 1;
    else if ((pos < 0) && (pos >= arr->length)) return 1;
    else return 0;
#else
    return 1;
#endif
}

zarrt
zarr_alloc (int length) {
    zarrt tmp;
    tmp = malloc (sizeof (struct zarr));
    if (tmp == NULL) return NULL;
    tmp->length = length;
    /* The length is made even to speedup zarr_clear */
    tmp->arr = malloc (((2 * length) + 2) * sizeof (int)); 
    if (tmp->arr == NULL) {
        free (tmp);
        return NULL;
    } 
    else return (tmp);
}

int
zarr_get (zarrt arr, int pos, int *val) {
    if (zarr_test_pos (arr, pos)) {
        *val = arr->arr[pos + arr->length];
        return 0;
    }
    else return 1;
}

int
zarr_set (zarrt arr, int pos, int val) {
    if (zarr_test_pos (arr, pos)) {
        arr->arr[pos + arr->length] = val;
        return 0;
    } 
    else return 1;
}

int
zarr_realloc (zarrt arr, int length) {
    int *tmp;
    int len;
    len = 1 + (length * 2);
    tmp = realloc (arr->arr, len * sizeof (int));
    if (tmp != NULL) {
        arr->arr = tmp;
        return 0;
    }
    else return 1;
}

int
zarr_length (zarrt arr) {
    if (arr != NULL) return (arr->length);
    else return 0;
}

int
zarr_clear (zarrt arr, int len) {
    int i;
    if (zarr_test_pos (arr, len)) {
        for (i = 0; i <= (2 * len) + 1; i++) arr->arr[i] = 0;
        return 1;
    }
    else return 0;
}
