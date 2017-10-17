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
#include <assert.h>
#include "memstack.h"
#define NDEBUG 0

memstack_t
mem_create (int len, size_t arr) {
    memstack_t res;
    res = (memstack_t) malloc (sizeof (struct memstack));
    if (NULL != res) {
        res->location = (void **) malloc (len * sizeof (void *));
        if (NULL != res->location) {
            res->cur_item = (len - 1);
            res->items = len;
            res->len = arr;
        }
        else {
            free (res);
            res = NULL;
        }
    }
    return res;
}

void *
mem_malloc (memstack_t m, size_t l) {
    void *res;
    if ((NULL != m) && (m->cur_item < (m->items - 1))) {
        assert (l == m->len);
        m->cur_item = m->cur_item + 1;
        return ((void *) m->location[m->cur_item]);
    }
    else {
        res = malloc (l);
        return (res);
    }
}

void
mem_free (memstack_t m, void *it) {
    if ((NULL != m) && (m->cur_item > (-1))) { /* There is space in the memstack */
            m->location[m->cur_item] = it;
            m->cur_item = m->cur_item - 1;
    } 
    else free (it);
}

void
mem_memstack_free (memstack_t m) {
    int i;
    for (i = m->cur_item + 1; i < m->items; i++) 
        free (m->location[i]);
    free (m);
    return;
}
