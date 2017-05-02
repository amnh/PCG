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

#ifndef DYN_CHAR_H
#define DYN_CHAR_H

// TODO: Here's another wtf:
#define POY_SEQ_MAGIC_NUMBER 9873123

/** Macro to retrieve and cast a pointer to a seq structure from the Ocaml custom type. */
#define Seq_pointer(a) ((dyn_character_t *) Data_custom_val(a))
#define Seq_custom_val(to_asgn, a)  to_asgn              = Seq_pointer(a); \
                                    to_asgn->array_head  = (elem_t *) ((dyn_char_p) (to_asgn + 1)); \
                                    to_asgn->end         = to_asgn->array_head + to_asgn->cap - 1; \
                                    to_asgn->seq_begin   = to_asgn->end - to_asgn->len + 1; \
//    assert (to_asgn->magic_number == POY_SEQ_MAGIC_NUMBER)  // TODO: figure out wtf this is.
#define USE_LARGE_ALPHABETS

// #ifdef USE_LARGE_ALPHABETS
#define elem_t unsigned int

// #define DESERIALIZE_elem_t(a,b) caml_deserialize_block_4((a),(b))
// #define SERIALIZE_elem_t(a,b) caml_serialize_block_4((a),(b))
// #else
// #define elem_t unsigned char
// // #define DESERIALIZE_elem_t(a,b) caml_deserialize_block_1((a),(b))
// // #define SERIALIZE_elem_t(a,b) caml_serialize_block_1((a),(b))
// #endif

/* Dynamic character structure to be used inside ocaml custom types. */
/********************* CHARACTER AS IT COMES IN MUST BE IN LAST X SPACES IN ARRAY! *********************/
typedef struct dyn_character_t {
//    int magic_number;
    size_t cap;         /* Capacity of the character memory structure. */
    size_t len;         /* Total length of the character stored. */
    elem_t *array_head; /* beginning of the allocated array */
    elem_t *seq_begin;  /* Position where the first element of the character is actually stored. */
    elem_t *end;
    //struct pool *my_pool; ARRAY_POOL_DELETE
} dyn_character_t;

typedef struct dyn_character_t *dyn_char_p;

void dyn_char_print(dyn_char_p inSeq);

void dyn_char_prepend (dyn_char_p a, elem_t v);

/** Does allocation for a character struct. Also sets seq pointers within array to correct positions.
 *
 *  resChar must be alloced before this call.
 */
// dyn_char_p initializeChar(dyn_char_p retChar, size_t allocSize) {
//     retChar->cap        = allocSize;                              // capacity
//     retChar->array_head = calloc(allocSize, sizeof(elem_t));        // beginning of array that holds dynamic character

//     retChar->end        = retChar->array_head + allocSize;        // end of array
//     retChar->seq_begin  = retChar->end;                           // position of first element in dynamic character
//     retChar->len        = 0;                                      // number of elements in character
// }


/* Stores the value v in the position p of character a. */
void seq_set (dyn_char_p character, size_t position, elem_t value);

#endif /* DYN_CHAR_H */
