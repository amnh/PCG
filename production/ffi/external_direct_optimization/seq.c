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
    SEQT * start = inSeq->seq_begin;
    SEQT * end   = inSeq->end;
    printf("Seq length %3zu\n",   inSeq->len);
    printf("Seq capacity %3zu\n", inSeq->cap);
    for( ; start < end; start++) {
        printf("%2d, ", *start);
    }
    printf("\n");
}

inline int
seq_begin (int cap, int len) {
    return (cap - len);
}


void
seq_set (seq_p seq, int pos, SEQT val)
{
    if (seq->len == 0) {
        assert (pos == 0);
        seq->len++;
    } else {
        assert (pos < seq->len);
        assert (pos >= 0);
    }
    *(seq->seq_begin + pos) = val;
}

inline void
seq_reverse_ip (seq_p seq) {
    SEQT *beginning, *end, tmp;

    beginning = seq->seq_begin;
    end       = seq->end;

    while (end > beginning) {
        tmp        = *end;
        *end       = *beginning;
        *beginning = tmp;
        end--;
        beginning++;
    }
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
}


inline void
seq_reverse (seq_p target, seq_p source) {
    size_t i;
    target->len = source->len;
    target->seq_begin = target->array_head + (target->cap - target->len);

    for (i = 0; i < source->len; i++) {
        *(target->seq_begin + i) = *(source->end - i);
    }
}


void
seq_clear (seq_p s) {
    s->len = 0;
    s->seq_begin = s->end + 1;
}


inline int
seq_compare (seq_p seq1, seq_p seq2) {
    size_t i;
    size_t len_seq1, len_seq2;
    SEQT cseq1, cseq2;
    len_seq1 = seq1->len;
    len_seq2 = seq2->len;
    if (len_seq2 != len_seq1) {
        if (len_seq1 > len_seq2) return 1;
        else return -1;
    }
    for (i = 0; i < len_seq1; i++) {
        cseq1 = seq1->seq_begin[i];
        cseq2 = seq2->seq_begin[i];
        if (cseq1 != cseq2) {
            if (cseq1 > cseq2) return 1;
            else return -1;
        }
    }
    return 0;
}