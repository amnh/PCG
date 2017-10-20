/* POY 4.0 Beta. A phylogenetic analysis program using Dynamic Homologies.    */
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
#include <assert.h>

// #include "array_pool.h" ARRAY_POOL_DELETE
#include "dyn_character.h"

void dyn_char_print( const dyn_character_t *inChar )
{
    elem_t *start = inChar->char_begin;
    elem_t *end   = inChar->end;
    printf("Char length   %3zu\n", inChar->len);
    printf("Char capacity %3zu\n", inChar->cap);
    for( ; start <= end; start++) {
        printf("%2d, ", *start);
    }
    printf("\n");
}

size_t
char_begin (size_t cap, size_t len) {
    return (cap - len);
}


void
dyn_char_set ( dyn_character_t *inChar
             , size_t           position
             , elem_t           value
             )
{
    if (inChar->len == 0) {
        assert (position == 0);
        inChar->len++;
    } else {
        assert (position < inChar->len);
        assert (position >= 0);
    }
    *(inChar->char_begin + position) = value;
}

inline void
dyn_char_reverse_ip (dyn_character_t *inChar) {
    elem_t *beginning, *end, tmp;

    beginning = inChar->char_begin;
    end       = inChar->end;

    while (end > beginning) {
        tmp        = *end;
        *end       = *beginning;
        *beginning = tmp;
        end--;
        beginning++;
    }
}

void
dyn_char_prepend (dyn_character_t *a, elem_t v) {
    if ( a->cap <= a->len ) {
        printf("Failing values: capacity: %zu, length: %zu\n", a->cap, a->len);
        assert(a->cap > a->len);
    }
    a->char_begin--;
    *(a->char_begin) = v;
    a->len++;
}


inline void
dyn_char_reverse (dyn_character_t *target, dyn_character_t *source) {
    size_t i;
    target->len = source->len;
    target->char_begin = target->array_head + (target->cap - target->len);

    for (i = 0; i < source->len; i++) {
        *(target->char_begin + i) = *(source->end - i);
    }
}


void
dyn_char_clear (dyn_character_t *s) {
    s->len = 0;
    s->char_begin = s->end + 1;
}


inline int
dyn_char_compare (dyn_character_t *char1, dyn_character_t *char2) {
    size_t i;
    size_t len_char1, len_char2;
    elem_t curElem1, curElem2;
    len_char1 = char1->len;
    len_char2 = char2->len;
    if (len_char2 != len_char1) {
        if (len_char1 > len_char2) return 1;
        else return -1;
    }
    for (i = 0; i < len_char1; i++) {
        curElem1 = char1->char_begin[i];
        curElem2 = char2->char_begin[i];
        if (curElem1 != curElem2) {
            if (curElem1 > curElem2) return 1;
            else return -1;
        }
    }
    return 0;
}
