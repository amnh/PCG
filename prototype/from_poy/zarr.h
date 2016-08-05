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

#ifndef ZARR_H 
#define ZARR_H 1

struct zarr {
    int length;
    int check;
    int expansion;
    int *arr;
};

typedef struct zarr * zarrt;

int 
zarr_test_pos (const zarrt arr, int pos);

zarrt
zarr_alloc (int length); 

int
zarr_get (zarrt arr, int pos, int *val);

int
zarr_set (zarrt arr, int pos, int val);

int
zarr_realloc (zarrt arr, int length);

int
zarr_length (zarrt arr);

int
zarr_clear (zarrt arr, int len);

#endif /* ZARR_H */
