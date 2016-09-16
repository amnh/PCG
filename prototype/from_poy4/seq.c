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

void seq_print(seq_p inSeq, int num) {
    SEQT * start = seq_get_begin(inSeq);
    SEQT * end   = seq_get_end(inSeq);
    printf("Seq num: %d\n", num);
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
seq_get_begin (const seq_p a) {
    return (a->begin);
}

 SEQT *
seq_get_head (const seq_p a) {
    return (a->head);
}

 SEQT *
seq_get_end (const seq_p a) {
    return (a->end);
}

inline int
seq_begin (int cap, int len) {
    return (cap - len);
}

static inline SEQT *
seq_get_ptr (const seq_p a, int p) {
    // assert (p < a->len);  // removed these because it's always checked in
    // assert (p >= 0);      // calling fns.
    return (a->begin + p);
}
    
SEQT 
seq_get_element (const seq_p a, int p) {
    assert (p < a->len);
    assert (p >= 0);
    return (*(seq_get_ptr (a, p)));
}

void
seq_set (seq_p a, int p, SEQT v) {
    SEQT *tmp;
    if (a->len == 0) {
        assert (p == 0);
        a->len++;
    } 
    else {
        assert (p < a->len);
        assert (p >= 0);
    }
    tmp = seq_get_ptr (a, p);
    *tmp = v;
    return;
}

inline void
seq_reverse_ip (seq_p cs) { 
    SEQT *a, *b, tmp;
    a = seq_get_begin (cs);
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
    assert(a->cap > a->len);
    a->begin = a->begin - 1;
    *(a->begin) = v;
    a->len = a->len + 1;
    return;
}

inline void
seq_reverse (seq_p src, seq_p tgt) {
    SEQT *a, *b, *c;
    int i;
    tgt->len = src->len;
    tgt->begin = tgt->head + (tgt->cap - tgt->len);
    a = seq_get_begin (src);
    b = seq_get_begin (tgt);
    c = seq_get_end (src);
    for (i = 0; i < src->len; i++) 
        *(tgt->begin + i) = *(src->end - i);
    return;
}

void
seq_clear (seq_p s) {
    s->len = 0;
    s->begin = s->end + 1;
    return;
}

/*
value
seq_CAML_clear (value s) {
    CAMLparam1(s);
    seq_p cs;
    Seq_custom_val(cs,s);
    seq_clear (cs);
    CAMLreturn(Val_unit);
}
    
void
seq_CAML_free_seq (value v) {
    seq_p s;
    Seq_custom_val(s,v);
    if (NULL == s->my_pool) 
        free (s->head);
    else 
        pool_available (s->my_pool, s->head);
    free (s);
    return;
}
*/

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

/*
#define A 1
#define C (A << 1)
#define G (C << 1)
#define T (G << 1)
#define GAP (T << 1)

int
seq_choose_base_randomly (int base) {
    long rnd;
    int counter = 0;
    int res;
    rnd = random();
    if (base & A) counter++;
    if (base & C) counter++;
    if (base & G) counter++;
    if (base & T) counter++;
    if (base & GAP) counter++;
    rnd = rnd % counter;
    res = A;
    if ((counter > 0) && (base & C)) counter--, res = C;
    if ((counter > 0) && (base & G)) counter--, res = G;
    if ((counter > 0) && (base & T)) counter--, res = T;
    if ((counter > 0) && (base & GAP)) counter--, res = GAP;
    return res;
}

void
seq_fix_randomly (struct seq *t) {
    int i;
    int len;
    int *tmp, base;
    len = seq_get_len (t);
    tmp = seq_get_head (t);
    for (i = 0; i < len; i++) {
        base = tmp[i];
        if ((base == A) || (base == C) || (base == G) || (base == T) ||
                (base == GAP)) continue;
        else tmp[i] = seq_choose_base_randomly (base);
    }
    return;
}

#undef A 
#undef C
#undef G
#undef T
#undef GAP 

*/
/*
value
seq_CAML_get_cap (value s) {
    CAMLparam1(s);
    seq_p tmp;
    Seq_custom_val(tmp,s);
    CAMLreturn(Val_int(tmp->cap));
}

value 
seq_CAML_length (value v) {
    CAMLparam1(v);
    seq_p tmp;
    Seq_custom_val(tmp,v);
    CAMLreturn(Val_int(seq_get_len(tmp)));
}

int
seq_CAML_compare (value a, value b) {
    int cmp;
    seq_p ap, bp;
    Seq_custom_val(ap,a);
    Seq_custom_val(bp,b);
    cmp = seq_compare (ap, bp);
    return (cmp);
}

unsigned long
seq_CAML_deserialize (void *v) {
    seq_p n;
    SEQT *head;
    n = (seq_p) v;
    head = (SEQT *) ((seq_p) n + 1);
    n->cap = deserialize_sint_4();
    n->len = deserialize_sint_4();
    n->magic_number = deserialize_sint_4();
    assert (n->magic_number == POY_SEQ_MAGIC_NUMBER);
    n->head = head;
    n->begin = head;
    n->end = n->head + n->cap - 1;
    assert (n->len > 0);
    DESERIALIZE_SEQT(n->begin,n->len);
    n->my_pool = NULL;
    return ((n->len * sizeof(SEQT)) + sizeof(struct seq));
}

void
seq_CAML_serialize (value vo, unsigned long *wsize_32, unsigned long *wsize_64) 
{
    seq_p v;
    SEQT *tmp;
    Seq_custom_val(v,vo);
    serialize_int_4(v->len);
    serialize_int_4(v->len);
    serialize_int_4(v->magic_number);
    tmp = v->begin;
    SERIALIZE_SEQT(tmp,v->len);
    *wsize_64 = *wsize_32 = sizeof(struct seq) + ((sizeof(SEQT) * (v -> len)));
    return;
}

long
seq_CAML_hash (value v) {
    long x = 0;
    seq_p s;
    int i;
    Seq_custom_val(s,v);
    for (i = (seq_get_len(s)) - 1; i >= 0; i--) {
        x ^= (((x << 5) + (x >> 2)) ^ seq_get(s,i));
    }
    return x;
}
static struct custom_operations sequence_custom_operations  = {
    "http://www.amnh.org/poy/seq/seq.0.2",
    custom_finalize_default,
    (&seq_CAML_compare), 
    (&seq_CAML_hash), 
    (&seq_CAML_serialize),
    (&seq_CAML_deserialize)
};
*/

#define SEQ_UNUSED_MEMORY 1000000
#define SHORT_SEQUENCES 16384

/*
value 
seq_CAML_create (value cap) {
    CAMLparam1(cap);
    CAMLlocal1(res);
    seq_p tmp2;
    int len;
    size_t s;
    len = Int_val(cap);
#ifndef USE_LONG_SEQUENCES
    if (len > SHORT_SEQUENCES) 
        failwith ("You are analyzing long sequences. This version \
                of POY was compiled without the --enable-long-sequences option, \
                setting a hard-coded limit of SHORT_SEQUENCES in their length. \
                To run this analysis you need to enable that option at compile time. \
                Either compile yourself the program, or request a version suited \
                for your needs in the POY mailing list (poy4@googlegroups.com).");
#endif
    s = sizeof (SEQT) * len;
    res = caml_alloc_custom 
        (&sequence_custom_operations, (sizeof(struct seq) + s), len, SEQ_UNUSED_MEMORY);
    tmp2 = Seq_pointer(res);
    if (NULL == tmp2) failwith ("Memory error");
    tmp2->magic_number = POY_SEQ_MAGIC_NUMBER;
    tmp2->cap = len;
    tmp2->len = 0;
    tmp2->head = (SEQT *) ((seq_p) (tmp2 + 1));
    tmp2->my_pool = NULL;
    if (tmp2->head == NULL) failwith ("Memory error.");
    tmp2->end = tmp2->begin = tmp2->head + len - 1;
    tmp2->begin++;
    assert (tmp2 == Seq_pointer(res));
    CAMLreturn(res);
}
*/
// value 
// seq_CAML_create_same (value other_seq, value cap) {
//     CAMLparam2(other_seq, cap);
    /*
    CAMLlocal1(res);
    seq_p tmp2;
    seq_p *tmp3, tmp_p;
    struct pool *p;
    int len;
    size_t s;
    len = Int_val(cap);
    Seq_custom_val(tmp_p,other_seq);
    if (NULL != tmp_p->my_pool)
        res = caml_alloc_custom 
            (&sequence_custom_operations, (sizeof(struct seq *)), (tmp_p->my_pool->size) / (sizeof (SEQT)), SEQ_UNUSED_MEMORY);
    else
        res = caml_alloc_custom 
            (&sequence_custom_operations, (sizeof(struct seq *)), len, SEQ_UNUSED_MEMORY);
    tmp3 = Seq_pointer(res);
    tmp2 = *tmp3 = (struct seq *) malloc (sizeof (struct seq));
    p = tmp_p->my_pool;
    tmp2->cap = len;
    tmp2->len = 0;
    s = sizeof (SEQT) * len;
    if ((NULL != p) && (s <= p->size)) {
        tmp2->head = (SEQT *) pool_alloc (p, tmp2);
        tmp2->my_pool = p;
    }
    else {
        tmp2->head = (SEQT *) malloc (s);
        tmp2->my_pool = NULL;
    }
    if (tmp2->head == NULL) failwith ("Memory error.");
    tmp2->end = tmp2->begin = tmp2->head + len - 1;
    tmp2->begin++;
    CAMLreturn(res);
    */
//     CAMLreturn(seq_CAML_create (cap));
// }

// value 
// seq_CAML_create_pool (value pl, value cap) {
//     CAMLparam2(pl, cap);
    /*
    CAMLlocal1(res);
    seq_p tmp2;
    seq_p *tmp3;
    struct pool *p;
    int len;
    size_t s;
    len = Int_val(cap);
    p = Pool_custom_val(pl);
    res = caml_alloc_custom 
        (&sequence_custom_operations, (sizeof(struct seq *)), p->size / (sizeof(SEQT)), SEQ_UNUSED_MEMORY);
    tmp3 = Seq_pointer(res);
    tmp2 = *tmp3 = (struct seq *) malloc (sizeof (struct seq));
    tmp2->cap = len;
    tmp2->len = 0;
    s = sizeof (SEQT) * len;
    if (s <= p->size) {
        tmp2->head = (SEQT *) pool_alloc (p, tmp2);
        tmp2->my_pool = p;
    }
    else {
        tmp2->head = (SEQT *) malloc (s);
        tmp2->my_pool = NULL;
    }
    if (tmp2->head == NULL) failwith ("Memory error.");
    tmp2->end = tmp2->begin = tmp2->head + len - 1;
    tmp2->begin++;
    CAMLreturn(res);
    */
//     CAMLreturn(seq_CAML_create(cap));
// }

/*
value
seq_CAML_get (value s, value p) {
    CAMLparam2(s, p);
    seq_p cs;
    int cp;
    Seq_custom_val(cs,s);
    cp = Int_val(p);
    CAMLreturn (Val_int((int) (seq_get (cs, cp))));
}

value
seq_CAML_set (value s, value p, value v) {
    CAMLparam3(s, p, v);
    seq_p cs;
    int cp, cv;
    Seq_custom_val(cs,s);
    cp = Int_val(p);
    cv = Int_val(v);
    seq_set (cs, cp, cv); 
    CAMLreturn (Val_unit);
}

value 
seq_CAML_copy (value from, value to) {
    CAMLparam2(from, to);
    seq_p cto, cfrom;
    int i;
    Seq_custom_val(cto,to);
    Seq_custom_val(cfrom,from);
    assert (cto->cap >= cfrom->len);
    cto->len = 0;
    cto->begin = cto->end + 1;
    for (i = cfrom->len - 1; i > -1; i--)
        seq_prepend (cto, seq_get (cfrom, i));
    CAMLreturn(Val_unit);
}

value
seq_CAML_reverse_ip (value s) {
    CAMLparam1(s);
    seq_p cs;
    Seq_custom_val(cs,s);
    seq_reverse_ip (cs);
    CAMLreturn(Val_unit);
}

value 
seq_CAML_reverse (value src, value tgt) {
    CAMLparam2(src, tgt);
    seq_p csrc, ctgt;
    Seq_custom_val(csrc,src);
    Seq_custom_val(ctgt,tgt);
    seq_reverse (csrc, ctgt);
    CAMLreturn(Val_unit);
}

value
seq_CAML_prepend (value s, value v) {
    CAMLparam2(s, v);
    seq_p tmp;
    Seq_custom_val(tmp,s);
    seq_prepend (tmp, Int_val(v));
    CAMLreturn(Val_unit);
}

value 
seq_CAML_register (value u) {
    CAMLparam1(u);
    register_custom_operations (&sequence_custom_operations);
    CAMLreturn (Val_unit);
}

value
seq_CAML_fix_randomly (value tgt) { 
    CAMLparam1(tgt);
    seq_fix_randomly((struct seq *) Seq_custom_val(tgt));
    CAMLreturn (Val_unit);
}
*/

// void    ARRAY_POOL_DELETE
// seq_pool_free (void *item) {
//     seq_p s;
//     s = (struct seq *) item;
//     s->my_pool = NULL;
// }

/*
value 
seq_CAML_assign_pool (value s, value p) {
    CAMLparam2(s, p);
    seq_p sc;
    Seq_custom_val(sc,s);
    //sc->my_pool = Pool_custom_val(p);
    sc->my_pool = NULL;
    CAMLreturn(Val_unit);
}


value
seq_CAML_count (value gap, value seq) {
    CAMLparam2(gap, seq);
    seq_p sc;
    int i, cnt = 0;
    SEQT cgap;
    Seq_custom_val (sc,seq);
    cgap = Int_val(gap);
    for (i = 0; i < sc->len; i++) 
        if (0 != (cgap & (seq_get (sc, i)))) cnt++;
    CAMLreturn(Val_int(cnt));
}

value 
seq_CAML_encoding (value enc, value seq) {
    CAMLparam2(enc, seq);
    CAMLlocal1(resc);
    seq_p sc;
    double *ec, res = 0.0, cost;
    int i;
    SEQT base;
    Seq_custom_val(sc,seq);
    ec = Data_bigarray_val(enc);
    // TODO: This only works for DNA sequences right now 
   for (i = 1; i < sc->len; i++) {
       base = seq_get (sc, i);
       if (!(base & 16)) {
           // Get the least expensive of all the bits 
           cost = 10000000;
           if ((base & 1) && (ec[1] < cost))
               cost = ec[1];
           if ((base & 2) && (ec[2] < cost))
               cost = ec[2];
           if ((base & 4) && (ec[4] < cost))
               cost = ec[4];
           if ((base & 8) && (ec[8] < cost))
               cost = ec[8];
           res += cost;
       }
   }
   resc = caml_copy_double (res);
   CAMLreturn(resc);
}
*/
