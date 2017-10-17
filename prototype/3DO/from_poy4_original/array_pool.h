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

/* A pool of available arrays for usage in many places */
#include <stdlib.h>
#ifndef ARRAY_POOL_H
#define ARRAY_POOL_H 1

#define POOL_SUCCESS 0
#define POOL_FAILED 1
#define Pool_pointer(v) ((struct pool **) Data_custom_val(v))
#define Pool_custom_val(v) (*((struct pool **) Data_custom_val(v)))

struct llist {
    struct llist *next;
    void *item;
    void *update_item;
};

struct pool {
    struct avl_table *in_use;
    struct llist *available;
    size_t size;
    int grow_rate;
};

void
pool_free (struct pool *p);

void
pool_available (struct pool *p, void *item);

inline void *
pool_alloc (struct pool *p, void *update_item);

struct pool *
pool_create (size_t s, int gr);

#endif /* ARRAY_POOL_H */
