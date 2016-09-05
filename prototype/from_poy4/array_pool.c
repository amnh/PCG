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

#include <stdlib.h>
#include <stdio.h>
#include "seq.h"
#include "avl.h"
#include "array_pool.h"

#define GET_AVAILABLE(p) (p->sts->right)
#define GET_EMPTY(p) (p->sts->left)
#define GET_MINE(p) (p->mine)

void
_llist_check_consistency (struct llist *next) {
    void *x;
    while (NULL != next) {
        x = next->item;
        next = next->next;
    }
    return;
}

struct llist *
llist_create (struct llist *next, void *item, void *update_item) {
    struct llist *res;
    res = malloc (sizeof (struct llist));
    if (NULL != res) {
        res->next = next;
        res->item = item;
        res->update_item = update_item;
    }
    return res;
}

struct llist *
llist_prepend_new (struct llist *next, void *item, void *update_item) {
    return (llist_create (next, item, update_item));
}

void
llist_prepend (struct llist *new_head, struct llist *tail) {
    new_head->next = tail;
    return;
}

struct llist *
llist_find (void *searchfor, struct llist *in) {
    while ((NULL != in) && (in->item != searchfor))
        in = in->next;
    return in;
}

int
llist_remove (void *searchfor, struct llist *in, struct llist **res, \
        struct llist **rem) {
    struct llist *tmp_res;
    int final;
    if (NULL == in) {
        *res = NULL; 
        *rem = NULL;
        return (POOL_FAILED);
    }
    else if (in->item == searchfor) {
        *res = in->next;
        *rem = in;
        return (POOL_SUCCESS);
    }
    else {
        final = llist_remove (searchfor, in->next, &tmp_res, rem);
        in->next = tmp_res;
        *res = in;
        return (final);
    }
}

int
pool_pointer_compare (const void *a, const void *b, void *c) {
    unsigned long x, y;
    x = (unsigned long) ((struct llist *) a)->item;
    y = (unsigned long) ((struct llist *) b)->item;
    if (x > y) return 1;
    else if (x < y) return (-1);
    else return 0;
}

struct pool *
pool_create (size_t s, int gs) {
    struct pool *res;
    res = malloc (sizeof (struct pool));
    if (NULL != res) {
        res->available = NULL;
        res->in_use = avl_create (&pool_pointer_compare, NULL, NULL);
        res->size = s;
        res->grow_rate = gs;
    }
    return res;
}

int
_pool_add_item (struct pool *p) {
    void *new_item;
    struct llist *new_available;
    new_item = malloc (p->size);
/*    printf ("Allocated %u\n", new_item); fflush (stdout);*/
    if (NULL == new_item) return POOL_FAILED;
    else {
        new_available = llist_prepend_new (p->available, new_item, NULL);
        if (NULL == new_available) {
            free (new_item);
            return POOL_FAILED;
        }
        else {
            p->available = new_available;
            return POOL_SUCCESS;
        }
    }
}

int
pool_add_item (struct pool *p) {
    int i;
    for (i = 0; i < p->grow_rate; i++) {
        if (POOL_FAILED == _pool_add_item (p)) return POOL_FAILED;
    }
    return POOL_SUCCESS;
}

inline void *
pool_alloc (struct pool *p, void *update_item) {
    struct llist *new_available, *old_available;
    if (NULL == p->available) {
        if (POOL_SUCCESS != pool_add_item (p))
            return NULL;
    }
    new_available = p->available->next;
    p->available->update_item = update_item;
    old_available = p->available;
    avl_insert (p->in_use, old_available);
    p->available = new_available;
    return (old_available->item);
}

void
pool_available (struct pool *p, void *item) {
    struct llist *new_available;
    struct llist wrapper;
    wrapper.item = item;
/*    printf ("Deallocated %u\n", item); fflush (stdout);*/
    new_available = (struct llist *) avl_delete (p->in_use, &wrapper);
    if (NULL == new_available) {
        printf("Huh?\n");
        exit(1);
        // failwith ("Huh?");
    }
    llist_prepend (new_available, p->available);
    ((struct seq *) (new_available->update_item))->my_pool = NULL;
    new_available->update_item = NULL;
    p->available = new_available;
    return;
}

void
pool_free_in_use (void *next, void *param) {
    struct llist *l = NULL;
    l = (struct llist *) next;
    ((struct seq *) (l->update_item))->my_pool = NULL;
    free (l);
    return;
}

void
pool_free_available (struct pool *p) {
    struct llist *m, *tmpm;
    m = p->available;
    while (NULL != m) {
        tmpm = m->next;
        assert (NULL == m->update_item);
        free (m->item);
        free (m);
        m = tmpm;
    }
    p->available = NULL;
    return;
}

void
pool_free (struct pool *p) {
    pool_free_available (p);
    avl_destroy (p->in_use, pool_free_in_use);
    return;
}

/*
void
pool_CAML_serialize (value c, unsigned long *wsize_32, \
        unsigned long *wsize_64) {
    struct pool *pv;
    pv = Pool_custom_val(c);
    caml_serialize_int_4 (pv->size);
    caml_serialize_int_4 (pv->grow_rate);
    *wsize_64 = *wsize_32 = sizeof (struct pool *);
    return;
}

unsigned long
pool_CAML_deserialize (void *v) {
    struct pool *p, **tmp;
    tmp = (struct pool **) v;
    p = malloc (sizeof (struct pool));
    *tmp = p;
    if (NULL == p) {
        printf("Memory error\n");
        exit(1);
        // failwith ("Memory error");
    }
    p->size = (size_t) caml_deserialize_uint_4();
    p->grow_rate = (size_t) caml_deserialize_uint_4();
    p->available = NULL;
    p->in_use = avl_create (&pool_pointer_compare, NULL, NULL);
    pool_add_item (p);
    return (sizeof (struct pool *));
}

void
pool_CAML_free (value v) {
    struct pool *p;
    p = Pool_custom_val(v);
    pool_free (p);
    free (p);
    return;
}

static struct custom_operations pool_custom_ops = {
    "http://www.amnh.org/poy/alignment_matrix/array_pool0.1",
    &pool_CAML_free,
    custom_compare_default,
    custom_hash_default,
    pool_CAML_serialize,
    pool_CAML_deserialize
};

value
pool_CAML_create (value cap, value prealloc) {
    CAMLparam2(cap, prealloc);
    CAMLlocal1(res);
    int c_prealloc;
    size_t s;
    struct pool *r, **tmp;
    c_prealloc = Int_val(prealloc);
    s = (size_t) (sizeof(SEQT) * (Int_val(cap)));
    res = alloc_custom (&pool_custom_ops, (sizeof (struct pool*)), 1, 2);
    tmp = Pool_pointer(res);
    r = *tmp = malloc (sizeof (struct pool));
    r->size = s;
    r->in_use = avl_create (&pool_pointer_compare, NULL, NULL);
    r->available = NULL;
    r->grow_rate = c_prealloc;
    pool_add_item (r);
    fflush (stdout);
    CAMLreturn (res);
}

value
pool_CAML_free_available (value p) {
    CAMLparam1(p);
    struct pool **tmp;
    tmp = Pool_pointer(p);
    pool_free_available (*tmp);
    CAMLreturn(Val_unit);
}

value
pool_CAML_register (value u) {
    CAMLparam1(u);
    register_custom_operations (&pool_custom_ops);
    CAMLreturn (Val_unit);
}
*/
