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

/*
 * $Id: nonaddCSc.h 1644 2007-02-14 19:05:47Z andres $
 */

#ifndef         CHAR_NONADD_CCODE_H_
# define        CHAR_NONADD_CCODE_H_

/* This file is described in char_nonadd_ccode.tex */

#ifndef _WIN32
#include <stdint.h>
#endif

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <caml/alloc.h>
#include <caml/intext.h>
#include <assert.h>
#include <stdio.h>

value char_nonadd_CAML_make_new (value len, value code);
value char_nonadd_CAML_make_new_unsafe (value len, value code);
void  char_nonadd_CAML_set_elt_bit (value v, value loc, value val);
value char_nonadd_CAML_basic_median (value a, value b);
value char_nonadd_CAML_distance (value a, value b);
value char_nonadd_CAML_distance_list (value a, value b);
value char_nonadd_CAML_equal (value a, value b);
value char_nonadd_CAML_cardinal (value a);
value char_nonadd_CAML_elt_to_list (value va, value vindex);
value char_nonadd_CAML_to_int (value va, value vindex);
value char_nonadd_CAML_to_list (value va);
value char_nonadd_CAML_of_list_helper (value list, value vlen);
value char_nonadd_CAML_get_heu (value a);
value char_nonadd_CAML_set_heu (value h, value a);




#endif      /* !CHAR_NONADD_CCODE_H_ */
