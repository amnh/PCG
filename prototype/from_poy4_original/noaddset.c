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

#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/bigarray.h"
#include "caml/fail.h"
#include "caml/alloc.h"
#include <assert.h>
#include "noaddset.h"
#include <stdio.h>

#ifdef _WIN32
__inline int
#else
inline int
#endif
intersection (int a, int b) {
    return (a & b);
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
unio (int a, int b) {
    return (a | b);
}

/* Using algorithm defined in Systematic Zoology Vol 20 NO 4 Dec 1971 p406-416
 * Toward Defining the course of evolution ... by Walter Finch */
#ifdef _WIN32
__inline int
#else
inline int
#endif
noaddset_downpass (struct storage ch1, struct storage ch2, struct storage p) {
    int i;
    int tmp;
    int total_cost = 0;
    assert (ch1.num_chrs == ch2.num_chrs && ch2.num_chrs == p.num_chrs);

    for (i = 1; i < ch1.num_chrs; i++) {
        tmp = intersection(*(ch1.preliminary + i), *(ch2.preliminary + i));
        if (tmp) {
            *(p.preliminary + i) = tmp;
            *(p.total_cost + i) = *(ch1.total_cost + i) + *(ch2.total_cost + i);
            *(p.my_cost + i) = 0;
        }
        else {
            *(p.preliminary + i) =
                unio (*(ch1.preliminary + i), *(ch2.preliminary + i));
            total_cost += *(p.my_cost + i) = *(p.my_weight + i);
            *(p.total_cost + i) = *(p.my_weight + i) + *(ch1.total_cost + i) +
                *(ch2.total_cost + i);
        }
    }
    return (total_cost);
}

/* Using algorithm defined in Systematic Zoology Vol 20 NO 4 Dec 1971 p406-416
 * Toward Defining the course of evolution ... by Walter Finch
 * - Note - Important - For the root: uppass should not be called but
 *   preliminary_is_final should be called to set its final row = to its prel */
#ifdef _WIN32
__inline void
#else
inline void
#endif
noaddset_uppass (struct storage ch1, struct storage ch2, struct storage gp, \
        struct storage p){
    int i, tmp, tmp2, tmp3;
    assert (   ch1.num_chrs == ch2.num_chrs && ch2.num_chrs == p.num_chrs
            && p.num_chrs == gp.num_chrs);
    for (i = 1; i < ch1.num_chrs; i++) {
        tmp = intersection(*(gp.final + i), *(p.preliminary + i));
        if (tmp == *(gp.final + i) )
            *(p.final + i) = tmp;
        else {
            if ( *(p.preliminary +i) ==
                    unio( *(ch1.preliminary + i), *(ch2.preliminary + i) ) )
                *(p.final + i) = unio( *(p.preliminary + i), *(gp.final + i) );
            else {
                tmp  = intersection( *(gp.final + i), *(ch1.preliminary + i) );
                tmp2 = intersection( *(gp.final + i), *(ch2.preliminary + i) );
                tmp3 = unio( tmp, tmp2 );
                *(p.final + i) = unio(tmp3,*(p.preliminary + i) );
            }
        }
    }
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
noaddset_set_preliminary_as_final (struct storage it) {
    int i;
    for (i = 1; i < it.num_chrs; i++)
        *(it.final + i) = *(it.preliminary + i);
    return;
}

#ifdef _WIN32
__inline struct storage
#else
inline struct storage
#endif
noaddset_create_structure (value mtx, value number) {
    struct storage tmp;
    int num;
    int *a, *b, *c, *d, *e, *f, *g;
    num = Int_val(number);
    a = Data_bigarray_val(mtx);
    b = a + num;
    c = b + num;
    d = c + num;
    e = d + num;
    f = e + num;
    g = f + num;
    tmp.num_chrs = Int_val(number);
    tmp.my_cost = a;
    tmp.my_weight = b;
    tmp.total_cost = c;
    tmp.codes = d;
    tmp.preliminary= f;
    tmp.final= g;
    return tmp;
}

/* Below are functions to handle transistion from CAML to C */
/************************************************************/

value
noaddset_CAML_downpass (value ch1, value ch2, value p, value n) {
    CAMLparam4(ch1, ch2, p, n);
    struct storage cch1, cch2, cp;
    int res;
    cch1 = noaddset_create_structure (ch1, n);
    cch2 = noaddset_create_structure (ch2, n);
    cp = noaddset_create_structure (p, n);
    res = noaddset_downpass (cch1, cch2, cp);
    CAMLreturn(Val_int(res));
}

value
noaddset_CAML_uppass (value ch1, value ch2, value p, value gp, value n) {
    CAMLparam5(ch1, ch2, p, gp, n);
    struct storage cch1, cch2, cp, cgp;
    cch1 = noaddset_create_structure (ch1, n);
    cch2 = noaddset_create_structure (ch2, n);
    cgp = noaddset_create_structure (gp, n);
    cp = noaddset_create_structure (p, n);
    noaddset_uppass (cch1, cch2, cgp, cp);
    CAMLreturn(Val_unit);
}

value
noaddset_CAML_set_pre_to_fin (value ch, value n) {
    CAMLparam2(ch, n);
    struct storage tmp;
    tmp = noaddset_create_structure (ch, n);
    noaddset_set_preliminary_as_final (tmp);
    CAMLreturn(Val_unit);
}
/*
   printf("%d %d %d\n",*(ch1.my_cost),*(ch1.my_cost + 1),*(ch1.my_cost+2));
   printf("%d %d %d\n",*(ch1.my_weight),*(ch1.my_weight + 1),*(ch1.my_weight+2));
   printf("%d %d %d\n",*(ch1.total_cost),*(ch1.total_cost + 1),*(ch1.total_cost+2));
   printf("%d %d %d\n",*(ch1.codes),*(ch1.codes + 1),*(ch1.codes+2));
   printf("%d %d %d\n",*(ch1.preliminary),*(ch1.preliminary + 1),*(ch1.preliminary+2));
   printf("%d %d %d\n",*(ch1.final),*(ch1.final + 1),*(ch1.final+2));
            */
