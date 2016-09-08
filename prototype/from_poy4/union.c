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

/* A library to handle the union offsets and their associated information */

#include <stdio.h>
#include <assert.h>
#include "union.h"

void
union_prepend_pair (unionofft a, unionofft b, unionofft c, SEQT or){
    seq_prepend (c->s, \
            or | seq_get (a->s, a->position) \
            | seq_get (b->s, b->position));
    a->position--;
    b->position--;
    a->end--;
    b->end--;
    return;
}

void
union_prepend_item (unionofft source, unionofft target, SEQT or) {
    seq_prepend (target->s, or | seq_get (source->s, source->position));
    source->position--;
    source->end--;
    return;
}

int 
union_copy_single (unionofft a, unionofft c, UNION_OFFT it, SEQT or) {
    UNION_OFFT i;
    i = *(a->end);
    it += i;
    while (i > 0) {
        union_prepend_item (a, c, or);
        i--;
    }
    return (it);
}

int
union_copy_non_homologous (unionofft au, unionofft bu, unionofft c, \
        UNION_OFFT items, SEQT gap) {
    if (au->position != -1) 
        items = union_copy_single (au, c, items, gap);
    if (bu->position != -1)
        items = union_copy_single (bu, c, items, gap);
    return (items);
}

void
union_prepend_and_fill_items (SEQT interm, unionofft c, UNION_OFFT prep, unionofft a, \
        unionofft b, UNION_OFFT apos, UNION_OFFT bpos) {
    UNION_OFFT i;
    for (i = prep; i > 0; i--) {
        *(c->end) = i;
        c->end--;
        *(c->ca_offsets) = 0;
        c->ca_offsets--;
        *(c->cb_offsets) = 0;
        c->cb_offsets--;
    }
    *(c->ca_offsets) = apos;
    *(c->cb_offsets) = bpos;
    c->ca_offsets--;
    c->cb_offsets--;
    seq_prepend (c->s, interm);
    *(c->end) = 0;
    c->end--;
    return;
}

SEQT
union_move_left (unionofft a) {
    SEQT res;
    res = seq_get (a->s, a->position);
    a->position--;
    a->end--;
    return (res);
}

void
union_merge (seq_p a, seq_p b, seq_p median, unionofft au, \
        unionofft bu, unionofft c, cost_matrices_p m) {
    UNION_OFFT items_prepended = 0, i, gap, lena, interm, apos, bpos;
    SEQT *begina, *beginb, *beginm;
    lena = seq_get_len (a);
    gap = cm_get_gap (m);
    begina = seq_get_begin (a);
    beginb = seq_get_begin (b);
    beginm = seq_get_begin (median);
    assert (lena == seq_get_len (b));
    i = lena - 1;
    while ((au->begin <= au->end) || (bu->begin <= bu->end)) {
        items_prepended = union_copy_non_homologous (au, bu, c, \
                items_prepended, gap);
        if (i >= 0) {
            interm = beginm[i];
            if (gap == interm) {
                if (i != 0) {
                    if (gap == begina[i]) union_prepend_item (bu, c, gap);
                    else if (gap == beginb[i]) union_prepend_item (au, c, gap);
                    else union_prepend_pair (au, bu, c, gap);
                }
                else {
                    union_prepend_and_fill_items (gap, c, items_prepended, au, bu, \
                            0, 0);
                    au->end--;
                    bu->end--;
                    return;
                }
                items_prepended += 1;
            }
            else {
                if (gap == begina[i]) {
                    apos = 0;
                    bpos = bu->position;
                    interm = interm | union_move_left (bu);
                }
                else if (gap == beginb[i]) {
                    bpos = 0;
                    apos = au->position;
                    interm = interm | union_move_left (au);
                } 
                else {
                    bpos = bu->position;
                    apos = au->position;
                    interm = interm | union_move_left (au);
                    interm = interm | union_move_left (bu);
                }
                union_prepend_and_fill_items (interm, c, items_prepended, au, bu, \
                        apos, bpos);
                items_prepended = 0;
            }
            i--;
        }
    }
    if (gap != seq_get (c->s, 0)) {
        seq_prepend (c->s, gap);
        *(c->ca_offsets) = 0;
        *(c->cb_offsets) = 0;
        *(c->end) = 0;
    }
    return;
}

/*
void
union_CAML_produce (unionofft u, seq_p s, UNION_OFFT *off, UNION_OFFT *begin, \
        UNION_OFFT *ca_offsets, UNION_OFFT *cb_offsets, UNION_OFFT length) {
    u->s = s;
    u->offsets = off;
    u->begin = begin;
    if (length == 0) {
        u->end = u->begin;
        u->ca_offsets = ca_offsets;
        u->cb_offsets = cb_offsets;
    }
    else {
        u->end = begin + length - 1;
        u->ca_offsets = ca_offsets + length - 1;
        u->cb_offsets = cb_offsets + length - 1;
    }
    u->counter = 0;
    u->length = length;
    u->position = length - 1;
}

void
union_CAML_unwrap (value a, unionofft u) {
    UNION_OFFT *offset, length, capacity, *ca_offsets, *cb_offsets;
    seq_p s;
    Seq_custom_val(s, (Field (a, 0))); // The sequence 
    offset = (UNION_OFFT *) Data_bigarray_val(Field(a, 1));
    length = seq_get_len (s);
    capacity = seq_get_cap (s);
    ca_offsets = (UNION_OFFT *) Data_bigarray_val(Field(a, 2));
    cb_offsets = (UNION_OFFT *) Data_bigarray_val(Field(a, 3));
    if (length == 0) 
        union_CAML_produce (u, s, offset, (offset + capacity - 1), \
                (ca_offsets + capacity - 1), (cb_offsets + capacity - 1),
                length);
    else
        union_CAML_produce (u, s, offset, (offset + capacity - length), \
                (ca_offsets + capacity - length), (cb_offsets + capacity - length),
                length);
    return;
}

value
union_CAML_make (value s1, value s2, value smedian, value a, value b, \
        value c, value cm) {
    CAMLparam5 (s1, s2, a, b, c);
    CAMLxparam2 (smedian, cm);
    struct unionoff ua, ub, uc;
    seq_p ss1, ss2, ssmedian;
    cost_matrices_p cmc;
    cmc = Cost_matrix_struct(cm);
    union_CAML_unwrap (a, &ua);
    union_CAML_unwrap (b, &ub);
    union_CAML_unwrap (c, &uc);
    Seq_custom_val(ss1,s1);
    Seq_custom_val(ss2,s2);
    Seq_custom_val(ssmedian,smedian);
    union_merge (ss1, ss2, ssmedian, &ua, &ub, &uc, cmc);
    CAMLreturn(Val_unit);
}

value
union_CAML_make_b (value *argv, int argn) {
    return (union_CAML_make (argv[0], argv[1], argv[2], argv[3], argv[4], \
                argv[5], argv[6]));
}
*/
