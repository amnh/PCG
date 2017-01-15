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
#include "seq.h"

void seq_print(seq_p inSeq) {
    SEQT * start = seq_get_seq_begin(inSeq);
    SEQT * end   = seq_get_end(inSeq);
    printf("Seq length %3zu\n",   inSeq->len);
    printf("Seq capacity %3zu\n", inSeq->cap);
    for( ; start < end; start++) {
        printf("%2d, ", *start);
    }
    printf("\n");
}

int
seq_get_cap (const seq_p a) {
    return a->cap;
}

 int
seq_get_len (const seq_p a) {
    return a->len;
}

 SEQT *
seq_get_seq_begin (const seq_p a) {
    return (a->seq_begin);
}

 SEQT *
seq_get_array_head (const seq_p seq) {
    return (seq->array_head);
}

 SEQT *
seq_get_end (const seq_p seq) {
    return (seq->end);
}

inline int
seq_begin (int cap, int len) {
    return (cap - len);
}

static inline SEQT *
seq_get_ptr (const seq_p seq, int pos) {
    // assert (p < a->len);  // removed these because it's always checked in
    // assert (p >= 0);      // calling fns.
    return (seq->seq_begin + pos);
}

SEQT
seq_get_element (const seq_p seq, int pos) {
    assert (pos < seq->len);
    assert (pos >= 0);
    return (*(seq_get_ptr (seq, pos)));
}

void
seq_set (seq_p seq, int pos, SEQT v) {
    SEQT *tmp;
    if (seq->len == 0) {
        assert (pos == 0);
        seq->len++;
    }
    else {
        assert (pos < seq->len);
        assert (pos >= 0);
    }
    tmp = seq_get_ptr (seq, pos);
    *tmp = v;
    return;
}

inline void
seq_reverse_ip (seq_p cs) {
    SEQT *a, *b, tmp;
    a = seq_get_seq_begin (cs);
    b = seq_get_end (cs);
    while (b > a) {
        tmp = *b;
        *b = *a;
        *a = tmp;
        b--;
        a++;
    }
    return;
}

void
seq_prepend (seq_p a, SEQT v) {
    if ( a->cap <= a->len ) {
      printf("Failing values: capacity: %zu, length: %zu\n", a->cap, a->len);
      assert(a->cap > a->len);
    }
    a->seq_begin    -= 1;
    *(a->seq_begin)  = v;
    a->len      += 1;
    return;
}

inline void
seq_reverse (seq_p src, seq_p tgt) {
    SEQT *a, *b, *c;
    int i;
    tgt->len = src->len;
    tgt->seq_begin = tgt->array_head + (tgt->cap - tgt->len);
    a = seq_get_seq_begin (src);
    b = seq_get_seq_begin (tgt);
    c = seq_get_end (src);
    for (i = 0; i < src->len; i++)
        *(tgt->seq_begin + i) = *(src->end - i);
    return;
}

void
seq_clear (seq_p s) {
    s->len = 0;
    s->seq_begin = s->end + 1;
    return;
}



inline int
seq_compare (seq_p a, seq_p b) {
    int i;
    int la, lb;
    SEQT ca, cb;
    la = seq_get_len (a);
    lb = seq_get_len (b);
    if (lb != la) {
        if (la > lb) return 1;
        else return -1;
    }
    for (i = 0; i < la; i++) {
        ca = seq_get_element (a, i);
        cb = seq_get_element (b, i);
        if (ca != cb) {
            if (ca > cb) return 1;
            else return -1;
        }
    }
    return 0;
}