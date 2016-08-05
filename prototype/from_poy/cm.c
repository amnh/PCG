/* POY 5.1.1. A phylogenetic analysis program using Dynamic Homologies.       */
/* Copyright (C) 2014 Andrés Varón, Lin Hong, Nicholas Lucaroni, Ward Wheeler,*/
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
#include <string.h>
#include <caml/bigarray.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
#include <caml/intext.h>
#include <caml/fail.h>
#include "cm.h"

#ifdef _WIN32
__inline int
#else
inline int
#endif
ceil_log_2 (int v) {
    int i = 0;
    while (v != 0) {
        i++;
        v = v >> 1;
    }
    return (i + 1);
}

int cm_check_level (cmt c)
{
    int level = c->level;
    int ori_sz = c->ori_a_sz;
    return ((level >=1)&&(level<=ori_sz));
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_check_level_3d (cm_3dt c)
{
    int level = c->level;
    int ori_sz = c->ori_a_sz;
    if ((level>=1)&&(level<=ori_sz)) 
        return 1;
    else 
        return 0;
}


#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_combinations_of_alphabet (const int a_sz) {
    assert (a_sz >= 0);
    return ((1 << a_sz) - 1);
}

void
cm_free (cmt c) {
    free (c->combmap);
    free (c->comb2list);
    free (c->cost);
    free (c->median);
    free (c->worst);
    free (c->prepend_cost);
    free (c->tail_cost);
    free (c);
    return;
}

void
cm_3d_free (cm_3dt c) {
    free (c->cost);
    free (c->median);
    free (c->combcode_2_comblist);
    free (c->comblist_2_combcode);
    free (c);
    return;
}

#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_total_size (cmt c, long int v) {
    assert(c != NULL);
    c->total_size = v;
    //printf("set total size = %li\n",c->total_size);
    return;
}

#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_total_size_3d (cm_3dt c, long int v) {
    assert(c != NULL);
    c->total_size = v;
    //printf("set total size 3d = %li\n",c->total_size);
    return;
}


#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_a_sz (cmt c, int v) {
    assert(c != NULL);
    c->a_sz = v;
    return;
}

#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_a_sz_3d (cm_3dt c, int v) {
    assert(c != NULL);
    c->a_sz = v;
    return;
}

#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_gap (cmt c, int v) {
    assert(c != NULL);
    c->gap = v;
    return;
}



#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_ori_a_sz (cmt c, int v) {
    assert(c != NULL);
    c->ori_a_sz = v;
    return;
}


#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_tie_breaker (cmt c, int v) {
    assert(c != NULL);
    c->tie_breaker = v;
    return;
}


#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_map_sz (cmt c, int v) {
    assert(c != NULL);
    c->map_sz = v;
    return;
}


#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_level (cmt c, int v) {
    assert(c != NULL);
    c->level = v;
    return;
}

// gap_start is the first code in alphabet has gap.
#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_gap_startNO (cmt c, int v) {
    assert(c != NULL);
    c->gap_startNO = v;
    return;
}


#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_gap_startNO_3d (cm_3dt c, int v) {
    assert(c != NULL);
    c->gap_startNO = v;
    return;
}


#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_ori_a_sz_3d (cm_3dt c, int v) {
    assert(c != NULL);
    c->ori_a_sz = v;
    return;
}

#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_tie_breaker_3d (cm_3dt c, int v) {
    assert(c != NULL);
    c->tie_breaker = v;
    return;
}


#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_map_sz_3d (cm_3dt c, int v) {
    assert(c != NULL);
    c->map_sz = v;
    return;
}


#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_level_3d (cm_3dt c, int v) {
    assert(c != NULL);
    c->level = v;
    return;
}


#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_gap_3d (cm_3dt c, int v) {
    assert(c != NULL);
    c->gap = v;
    return;
}

void cm_set_affine (cmt c, int do_aff, int go) {
    assert(c != NULL);
    c->cost_model_type = do_aff;
    c->gap_open = go;
    return;
}

#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_affine_3d (cm_3dt c, int do_aff, int go) {
    assert(c != NULL);
    c->cost_model_type = do_aff;
    c->gap_open = go;
    return;
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_lcm (cmt c) {
    assert(c != NULL);
    return (c->lcm);
}

#ifdef _WIN32
__inline int 
#else
inline int 
#endif
cm_get_lcm_3d (cm_3dt c) {
    assert(c != NULL);
    return (c->lcm);
}

#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_lcm (cmt c, int v) {
    assert(c != NULL);
    c->lcm = v;
    return;
}

#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_lcm_3d (cm_3dt c, int v) {
    assert(c != NULL);
    c->lcm = v;
    return;
}

#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_combinations (cmt c) {
    assert(c != NULL);
    c->combinations = 1;
    return;
}

#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_set_combinations_3d (cm_3dt c) {
    assert(c != NULL);
    c->combinations = 1;
    return;
}

#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_unset_combinations (cmt c) {
    assert(c != NULL);
    c->combinations = 0;
    return;
}

#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_unset_combinations_3d (cm_3dt c) {
    assert(c != NULL);
    c->combinations = 0;
    return;
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_combinations (cmt c) {
    assert(c != NULL);
    return (c->combinations);
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_combinations_3d (cm_3dt c) {
    assert(c != NULL);
    return (c->combinations);
}

void
cm_set_all_elements (cmt c, int v) {
    assert (c != NULL);
    c->all_elements = v;
    return;
}

void
cm_set_all_elements_3d (cm_3dt c, int v) {
    assert (c != NULL);
    c->all_elements = v;
    return;
}

int
cm_get_all_elements (cmt c) {
    return (c->all_elements);
}

int
cm_get_all_elements_3d (cm_3dt c) {
    return (c->all_elements);
}

value
cm_CAML_set_all_elements (value cm, value v) {
    CAMLparam2(cm, v);
    cmt c;
    int i;
    c =Cost_matrix_struct(cm);
    i = Int_val(v);
    cm_set_all_elements (c, i);
    CAMLreturn(Val_unit);
}

value
cm_CAML_set_all_elements_3d (value cm, value v) {
    CAMLparam2(cm, v);
    cm_3dt c;
    int i;
    c = Cost_matrix_struct_3d(cm);
    i = Int_val(v);
    cm_set_all_elements_3d (c, i);
    CAMLreturn(Val_unit);
}

value
cm_CAML_get_all_elements_3d (value cm) {
    CAMLparam1(cm);
    cm_3dt c;
    c = Cost_matrix_struct_3d(cm);
    CAMLreturn(Val_int(cm_get_all_elements_3d(c)));
}

value
cm_CAML_get_all_elements (value cm) {
    CAMLparam1(cm);
    cmt c;
    c = Cost_matrix_struct(cm);
    CAMLreturn(Val_int(cm_get_all_elements(c)));
}


value
cm_CAML_comb2list_to_bigarr (value cm){
    CAMLparam1(cm);
    CAMLlocal1(res);
    int * stuff;
    //double * newstuff;
    int w; int h;
    long dims[2];
    cmt c;
    c = Cost_matrix_struct(cm);
    h = (c->map_sz+1);
    w = (2+1);
    dims[0] = h;
    dims[1] = w;
    stuff = (int*) calloc (h*w, sizeof(int));
    //newstuff = (double*) malloc ( dims[0]*dims[1]*sizeof(double));
    if( stuff == NULL ) {
     free(stuff);
      failwith ("cannot alloc memory in cm.c, cm_CAML_comb2list_to_bigarr ");
    }
    memcpy( stuff, (int*)(c->comb2list), h * w * sizeof(int) );
    /*
    int count = 0 ; double * tmp1 = newstuff; int * tmp2 = stuff;
    while(count< (h*w))
    {
       *tmp1 = (double) (* tmp2 );
        tmp1 = tmp1 + 1; tmp2 = tmp2 + 1;
        count = count + 1;
    };
    */
     res = alloc_bigarray( BIGARRAY_INT32 | BIGARRAY_C_LAYOUT,2,stuff,dims);
    //res = alloc_bigarray(BIGARRAY_FLOAT64 | BIGARRAY_C_LAYOUT,2,newstuff,dims);
    free(stuff); 
    CAMLreturn(res);
}


/*
value
cm_CAML_bigarr_to_comb2list (value bigarr, value cm) {
    CAMLparam2(cm, bigarr);
    cmt c;
    int w;
    int h;
    int * stuff, * old_stuff; 
    c = Cost_matrix_struct(cm);
    h = Bigarray_val(bigarr)->dim[0];
    w = Bigarray_val(bigarr)->dim[1];
    assert( h == (c->map_sz + 1) );
    assert( w == (2+1) );
    stuff = (int*) malloc ( h * w * sizeof(int));
    if( stuff == NULL) 
        failwith ("memory allco failed in cm.c: cm_CAML_bigarr_to_comb2list");
    old_stuff = (int*) Data_bigarray_val(bigarr);
    memcpy(stuff, old_stuff, h*w*sizeof(int) );
    c -> comb2list = stuff;
    CAMLreturn(c);
}
*/
/* 
 * Creates a cost matrix with memory allocated for an alphabet of size a_sz
 * (not including the gap representation which is internally chosen), and whose
 * size must consider all possible combinations of characters in the alphabeet
 * iff combinations != 0. Set the affine gap model paramters to the values
 * stored in do_aff, gap_open, in the cost matrix res. 
 * In case of error the function fails with the message "Memory error.".
 */
cmt 
cm_set_val (int a_sz, int combinations, int do_aff, int gap_open, \
        int is_metric, int is_identity, int all_elements, cmt res, int level, int comb_num, int gap_startNO,\
        int tie_breaker) {
    int debug = 0;
    size_t size; 
    size_t combmatrix_size; 
    size_t comb2list_size;
#ifndef USE_LARGE_ALPHABETS
    if (comb_num > 255) 
        failwith ("Apparently you are analyzing large alphabets. This version of POY was configured without the --enable-large-alphabets option. To run this analysis you need to enable that option at compile time. Either reconfigured and compile yourself the program,   or request a version suited for your needs in the POY mailing list (poy4@googlegroups.com).");
#endif
    if (debug) { printf ("cm_set_val,a_sz=%d,combination=%d,all_elements=%d,level=%d,comb_num=%d,gap_startNO=%d,tie_breaker=%d\n",
            a_sz,combinations,all_elements,level,comb_num,gap_startNO,tie_breaker); fflush(stdout); }
    cm_set_ori_a_sz(res, a_sz);
    cm_set_all_elements (res, all_elements);
    cm_set_affine (res, do_aff, gap_open);
    res->is_metric = is_metric;
    res->is_identity = is_identity;
    cm_set_tie_breaker(res,tie_breaker);
    //safe guard
    if ((level<=1)&&(combinations!=0)) level=0;
    if (combinations != 0) {
        cm_set_level(res,level);
        cm_set_map_sz(res, comb_num);
        cm_set_lcm (res, a_sz);
        if( (level >1)&&(level<=a_sz) )
        {
            if (debug) { 
                printf ("use combinations,use level(1<%d<=a_sz=%d),gap <- a_sz, a_sz <- comb_num(%d) \n",level,a_sz,comb_num); 
                fflush(stdout);
            }
            cm_set_gap(res, a_sz);
            cm_set_gap_startNO(res,gap_startNO);
            cm_set_a_sz (res, comb_num);
        }
        else 
        { 
            int combsz = cm_combinations_of_alphabet (a_sz) ;
           if (debug) { 
               printf ("use combinations, not using level (%d<= 1 or > a_sz=%d), set a_sz <- combsz(%d),gap <- 1<<(a_sz-1) = %d\n",
                       level,a_sz, combsz, 1 << (a_sz-1)); 
           fflush(stdout);}
            cm_set_gap (res, 1 << (a_sz - 1));
            cm_set_gap_startNO(res,0);
            cm_set_a_sz (res, combsz);
        }
        cm_set_combinations (res);
    } 
    else {
        int lcmvalue = ceil_log_2 (a_sz + 1);
        if (debug) {
            printf ("no combinations,lcm <- %d, level <- 1, gap <- a_sz(%d)\n",lcmvalue,a_sz); 
            fflush(stdout);}
        cm_set_map_sz(res, a_sz);
        cm_set_level(res,1);
        cm_set_gap (res, a_sz);
        cm_set_a_sz (res, a_sz);
        cm_set_lcm (res, lcmvalue);
        cm_unset_combinations (res);
    }
    combmatrix_size = sizeof(int) * (comb_num+1) * (comb_num+1) ;
    comb2list_size = sizeof(int) * (comb_num+1) * (2+1);
    res->combmap = (int *) calloc(combmatrix_size,1);
    res->comb2list = (int *) calloc( comb2list_size,1);
    if( (level > 1)&&(level<=a_sz) )  {    
       size = combmatrix_size;
    } else {
        size = 2 * (1 << (res->lcm)) * (1 << (res->lcm)) * sizeof(int);
    }
    //set total size 
    cm_set_total_size (res,size/sizeof(int));
    if (0==size)
    {   printf ("alphabet size=%d,combinations=%d,comb_num=%d,lcm=%d\n", a_sz, combinations,comb_num,res->lcm);
        printf ("if combination is true(which is 1 above),level is 0 or bigger than alphabet size, full combination will be used, we will try to create a cost matrix with size 2*lcm*lcm, it might be too big for your system. in that case, try to reduce the size of cost matrix by transform(level:x), x~(0,alphabet size)");
        fflush(stdout);
        failwith ("Your cost matrix is too large to fit in your memory.\
                I can't continue with your data loading.");
    }
   res->cost = (int *) calloc (size, 1);
    if(res->cost == NULL)
        failwith("ERROR: cannot alloc res->cost");
    res->worst = (int *) calloc (size, 1);
    if(res->worst == NULL)
        failwith("ERROR: cannot alloc res->worst");
    res->prepend_cost = (int *) calloc (size, 1);
    if(res->prepend_cost == NULL)
        failwith("ERROR: cannot alloc res->prepend_cost");
    res->tail_cost = (int *) calloc (size, 1);
    if(res->tail_cost==NULL)
        failwith("ERROR: cannot alloc res->tail_cost");
    if( (level >1)&&(level<=a_sz) ){
        size =  sizeof(SEQT) * (comb_num+1) * (comb_num+1) ;
    } else {
        size = 2 * (1 << (res->lcm)) * (1 << (res->lcm)) * sizeof(SEQT);
    }
    if (0==size)
        printf ("alphabet size: %d,%d \n", a_sz, combinations);
    if (0 == size)
        failwith ("2Your cost matrix is too large to fit in your memory. I can't continue with your data loading.");
    res->median = (SEQT *) calloc (size, 1);
    if ((res->combmap == NULL)||(res->comb2list == NULL)||(res->cost == NULL) || (res->median == NULL)) {
        free (res->combmap);
        free (res->comb2list);
        free (res->cost);
        free (res->median);
        failwith ("Memory error during cost matrix allocation.");
    }
    return res;
}

#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_ini_combmap (cmt res, int map_sz)
{
    int size; 
    size =  sizeof(int) * (map_sz+1)  * (map_sz+1); // why the other matrix add 2* ?
    res->combmap = (int*) calloc (size,0);
    if( res->combmap == NULL) 
    {
        free(res->combmap); 
        failwith ("Memonry error during combination map matrix allocation");
    }
    return ;
}

#ifdef _WIN32
static __inline void
#else
static inline void
#endif
cm_ini_comb2list (cmt res, int map_sz)
{
    int size; 
    size = sizeof(int) * (map_sz+1) * (2+1);
    res->comb2list = (int*)calloc(size,0);
    if(res->comb2list == NULL)
    {
        free(res->comb2list);
        failwith ("Memonry error during comb2list matrix allocation");
    }
}


/* 
 * Creates a cost matrix with memory allocated for an alphabet of size a_sz
 * (not including the gap representation which is internally chosen), and whose
 * size must consider all possible combinations of characters in the alphabeet
 * iff combinations != 0. Set the affine gap model paramters to the values
 * stored in do_aff, gap_open, in the cost matrix res. 
 * In case of error the function fails with the message "Memory error.".
 */
cm_3dt 
cm_set_val_3d (int a_sz, int combinations, int do_aff, int gap_open, \
        int all_elements, int level, int comb_num, int gap_startNO, cm_3dt res) {
    int size;
    int combmatrix_size; 
    int comb2list_size;
    if (!NDEBUG) {
        printf ("Allocating a three dimensional matrix:\n");
        printf ("alphabet size: %d \n", a_sz);
        printf ("combinations: %d \n", combinations);
        printf ("cost model: %d \n", do_aff);
        printf ("gap open cost: %d \n", gap_open);
        printf ("level: %d \n",level);
    }
    if (combinations != 0) {
        cm_set_level_3d (res, level);
        cm_set_ori_a_sz_3d (res, a_sz);
        cm_set_map_sz_3d (res, comb_num);
        if( (level>1)&&(level<=a_sz)) 
        {
            cm_set_gap_3d(res, a_sz);
            cm_set_a_sz_3d (res, comb_num);
            cm_set_gap_startNO_3d(res, gap_startNO);
        }
        else
        {
            cm_set_gap_3d (res, 1 << (a_sz - 1));
            cm_set_a_sz_3d (res, cm_combinations_of_alphabet (a_sz));
             cm_set_gap_startNO_3d(res,0);
        }
        cm_set_lcm_3d (res, a_sz);
        cm_set_combinations_3d (res);
    } else {
        cm_set_ori_a_sz_3d(res, a_sz);
        cm_set_map_sz_3d (res, a_sz);
        cm_set_level_3d (res, 1);
        cm_set_gap_3d (res, a_sz);
        cm_set_a_sz_3d (res, a_sz);
        cm_set_lcm_3d (res, ceil_log_2 (a_sz + 1));
        cm_unset_combinations_3d (res);
    }
    cm_set_all_elements_3d (res, all_elements);
    combmatrix_size = sizeof(int) * (comb_num+1) * (comb_num+1);
    comb2list_size = sizeof(int) * (comb_num+1) * (2+1);
    cm_set_affine_3d (res, do_aff, gap_open);
    if( (level >1)&&(level<=a_sz) )
        size =  (comb_num+1) * (comb_num+1) * (comb_num+1);
    else
        size = (1 << (res->lcm + 1)) * (1 << (res->lcm + 1)) * (1 << (res->lcm + 1));
    cm_set_total_size_3d(res,size);
    res->comblist_2_combcode = (int *) calloc(combmatrix_size,1);
    if(res->comblist_2_combcode == NULL)
        fprintf(stderr,"Cannot allocate the map of combination codelist to combination code, with size=%d\n",combmatrix_size);
    res->combcode_2_comblist =  (int *) calloc( comb2list_size,1);
    if(res->combcode_2_comblist==NULL) 
         fprintf(stderr,"Cannot allocate the map of combination code to combination codelist, with size=%d\n",comb2list_size);
    res->median = (SEQT *) calloc (size * sizeof(SEQT), 1);
    if (res->median == NULL)
        fprintf(stderr,"Cannot allocate cost matrix with size=%ld\n",(size*sizeof(SEQT)));
    res->cost = (int *) calloc (size * sizeof(int), 1);
    if (res->cost == NULL)
        fprintf(stderr,"Cannot allocate median matrix with size=%ld\n",(size*sizeof(int)));
    if ((res->cost == NULL) || (res->median == NULL) || (res->comblist_2_combcode==NULL) || (res->comblist_2_combcode == NULL)) {
        free (res->comblist_2_combcode);
        free (res->combcode_2_comblist);
        free (res->cost);
        free (res->median);
        failwith ("Memory error during cost 3D matrix allocation. If the total size of memory in need is bigger than 4G, you might need to compile POY with 64bit ocaml compiler\n");
    }
    return res;
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_alphabet_size (const cmt c) {
    assert(c != NULL);
    return c->a_sz;
}


#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_tie_breaker (const cmt c) {
    assert(c != NULL);
    return c->tie_breaker;
}


#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_tie_breaker_3d (const cm_3dt c) {
    assert(c != NULL);
    return c->tie_breaker;
}


#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_alphabet_size_3d (const cm_3dt c) {
    assert(c != NULL);
    return c->a_sz;
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_ori_a_sz (const cmt c) {
    assert(c != NULL);
    return c->ori_a_sz;
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_ori_a_sz_3d (const cm_3dt c) {
    assert(c != NULL);
    return c->ori_a_sz;
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_map_sz (const cmt c) {
    assert(c != NULL);
    return c->map_sz;
}

int cm_get_map_sz_3d (const cm_3dt c) {
    assert(c != NULL);
    return c->map_sz;
}


int cm_get_level (const cmt c) {
    assert(c != NULL);
    return c->level;
}

int cm_get_level_3d (const cm_3dt c) {
    assert(c != NULL);
    return c->level;
}

int cm_get_gap_startNO (const cmt c) {
    assert(c != NULL);
    return c->gap_startNO;
}

SEQT cm_get_gap (const cmt c) {
    assert(c != NULL);
    return c->gap;
}

int cm_get_gap_3d (const cm_3dt c) {
    assert(c != NULL);
    return c->gap;
}

int cm_get_affine_flag (cmt c) {
    assert(c != NULL);
    return c->cost_model_type;
}

int cm_get_affine_flag_3d (cm_3dt c) {
    assert(c != NULL);
    return c->cost_model_type;
}

int cm_get_gap_opening_parameter (cmt c) {
    assert(c != NULL);
    return c->gap_open;
}

int cm_get_gap_opening_parameter_3d (const cm_3dt c) {
    assert(c != NULL);
    return c->gap_open;
}

int cm_calc_cost_position (int a, int b, int a_sz) {
    assert(a_sz >= 0);
    return ((a << a_sz) + b);
}

int cm_calc_cost_position_seqt (SEQT a, SEQT b, int a_sz) {
    assert(a_sz >= 0);
   return ((((int) a) << a_sz) + ((int) b));
}

#ifdef _WIN32
__inline INDEXSIZE
#else
inline INDEXSIZE
#endif
cm_calc_cost_position_seqt_nonbit (SEQT a, SEQT b, int a_sz) {
    assert(a_sz >= 0);
    return ((INDEXSIZE)(((INDEXSIZE) a) * (INDEXSIZE)a_sz) + ((INDEXSIZE) b));
}

int cm_calc_cost_position_3d_seqt (SEQT a, SEQT b, SEQT c, int a_sz) {
    assert(a_sz >= 0);
    return ((((((int) a) << a_sz) + ((int) b)) << a_sz) + ((int) c));
}

#ifdef _WIN32
__inline INDEXSIZE
#else
inline INDEXSIZE
#endif
cm_calc_cost_position_3d_seqt_nonbit (SEQT a, SEQT b, SEQT c, int a_sz) {
    assert(a_sz >= 0);
    return ((((((INDEXSIZE) a) * (INDEXSIZE)a_sz) + ((INDEXSIZE) b)) * (INDEXSIZE)a_sz) + ((INDEXSIZE) c));
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_calc_cost_position_3d (int a, int b, int c, int a_sz) {
    assert(a_sz >= 0);
    return ((((a << a_sz) + b) << a_sz) + c);
}

#ifdef _WIN32
__inline INDEXSIZE
#else
inline INDEXSIZE
#endif
cm_calc_cost_position_3d_nonbit (int a, int b, int c, int a_sz) {
    assert(a_sz >= 0);
    return (((((INDEXSIZE)a * (INDEXSIZE)a_sz) + (INDEXSIZE)b) * (INDEXSIZE)a_sz) + (INDEXSIZE)c);
}

#ifdef _WIN32
__inline SEQT
#else
inline SEQT
#endif
cm_calc_median (SEQT *tcm, SEQT a, SEQT b, int a_sz) {
    SEQT *res;
    assert (a_sz >= 0);
    assert ((1 << a_sz) > a);
    assert ((1 << a_sz) > b);
    res = tcm + cm_calc_cost_position_seqt (a, b, a_sz);
    return (*res);
}

#ifdef _WIN32
__inline SEQT
#else
inline SEQT
#endif
cm_calc_median_nonbit (SEQT *tcm, SEQT a, SEQT b, int a_sz) {
    SEQT *res;
    assert (a_sz >= 0);
    assert (a_sz >= a);
    assert (a_sz >= b);
    res = tcm + cm_calc_cost_position_seqt_nonbit (a, b, a_sz);
    return (*res);
}


#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_median_level (SEQT *tcm, SEQT a, SEQT b, int mapsize) {
    assert(mapsize>0);
    assert(a<=mapsize);
    assert(b<=mapsize);
    SEQT *res;
    res = tcm +  ( ( ((int)a) * mapsize ) + ( (int)b ) ) ;
    if (res == 0 ) 
        failwith ("failed in cm_get_median_level in cm.c, median value cannot be 0\n");
    return (*res);
}

int cm_get_cost (int *tcm, int a, int b, int mapsize) {
    assert(mapsize>0);
    assert(a<=mapsize);
    assert(b<=mapsize);
    assert(a>=0);
    assert(b>=0);
    return tcm[ (a*mapsize)+b ];
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_combmap (int *tcm, int a, int b, int mapsize) {
    assert(mapsize>0);
    assert(a<=mapsize);
    assert(b<=mapsize);
    if (a==b) return a;
    else
    {
        int *res;
        res = tcm + ((a)*mapsize+(b));
        return (*res);
    }
}


#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_get_comblist ( int * tcm, int combcode, int pos, int mapwide )
{
    int * res;
    res = tcm + (combcode*mapwide+pos);
    return (*res);
}

void
cm_print_matrix (int* m, int w, int h) {
    int i, j;
    fprintf (stdout, "begin matrix: \n");
    for (i = 0; i < h; i++) {
        fprintf(stdout,"%d: ", i);
        for (j = 0; j < w; j++)
            fprintf (stdout, "%d\t", *(m + (w * i) + j));
        fprintf (stdout, "\n");
    }
    fprintf (stdout, "\n end of matrix \n");
    return;
}

int
cm_get_min_non0_cost (cmt c) {
    int * thisc = c->cost;
    int i;
    int min_non0_cost = INT_MAX/2;
    int size;
    if ( ( c->level > 1) && (c->level <= c->a_sz) )
    {
        int comb_num = c->map_sz;
        size = (comb_num+1) * (comb_num+1);
    }
    else 
    {
        size = 2 * (1 << (c->lcm)) * (1 << (c->lcm)) ;
    }
    //fprintf(stdout, "size=%d,",size); fflush(stdout);
    for (i = 0; i < size; i++) {
        if (*thisc<0) fprintf (stderr,"Warning, reach non cost matrix area!");
        if ( *thisc>0 && *thisc<min_non0_cost )
        {
            min_non0_cost = *thisc;
        }
        thisc ++;
    }
    //fprintf (stdout, "min_non0_cost=%d\n",min_non0_cost); fflush(stdout);
    return min_non0_cost;
}

int
cm_calc_cost (int *tcm, SEQT a, SEQT b, int a_sz) {
    int *res;
    assert (a_sz >= 0);
    assert ((1 << a_sz) > a);
    assert ((1 << a_sz) > b);
    res = tcm + cm_calc_cost_position_seqt (a, b, a_sz);
    return (*res);
}

#ifdef _WIN32
__inline SEQT
#else
inline SEQT
#endif
cm_calc_median_3d (SEQT *tcm, SEQT a, SEQT b, SEQT c, int a_sz) {
    if (a_sz <= 0) failwith ("Alphabet size = 2");
    if ((1 << a_sz) <= a) failwith ("2a is bigger than alphabet size");
    if ((1 << a_sz) <= b) failwith ("b is bigger than alphabet size");
    return (*(tcm + cm_calc_cost_position_3d (a, b, c, a_sz)));
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_calc_cost_3d (int *tcm, SEQT a, SEQT b, SEQT c, int a_sz) {
    if (a_sz <= 0) failwith ("Alphabet size = 2");
    if ((1 << a_sz) <= a) failwith ("2a is bigger than alphabet size");
    if ((1 << a_sz) <= b) failwith ("b is bigger than alphabet size");
    return (*(tcm + cm_calc_cost_position_3d (a, b, c, a_sz)));
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_calc_cost_3d_level (int *tcm, SEQT a, SEQT b, SEQT c, int a_sz) {
    if (a_sz <= 0) failwith ("Alphabet size cannot be 0");
    if (a_sz < a) failwith ("index a is bigger than alphabet size");
    if (a_sz < b) failwith ("index b is bigger than alphabet size");
    return (*(tcm + cm_calc_cost_position_3d_nonbit (a, b, c, a_sz)));
}

#ifdef _WIN32
__inline SEQT
#else
inline SEQT
#endif
cm_calc_cost_3d_seqt (SEQT *tcm, SEQT a, SEQT b, SEQT c, int a_sz) {
    if (a_sz <= 0) failwith ("Alphabet size = 2");
    if ((1 << a_sz) <= a) failwith ("2a is bigger than alphabet size");
    if ((1 << a_sz) <= b) failwith ("b is bigger than alphabet size");
    return (*(tcm + cm_calc_cost_position_3d (a, b, c, a_sz)));
}

#ifdef _WIN32
__inline SEQT
#else
inline SEQT
#endif
cm_calc_cost_3d_seqt_level (SEQT *tcm, SEQT a, SEQT b, SEQT c, int a_sz) {
    if (a_sz <= 0) failwith ("Alphabet size cannot be 0");
    if (a_sz < a) failwith ("index a is bigger than alphabet size");
    if (a_sz < b) failwith ("index b is bigger than alphabet size");
    return (*(tcm + cm_calc_cost_position_3d_nonbit (a, b, c, a_sz)));
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_calc_tmm (int *tmm, int a, int b, int a_sz) {
    return (cm_calc_cost (tmm, a, b, a_sz));
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
cm_calc_median_position (SEQT a, SEQT b, int a_sz) {
    return (cm_calc_cost_position (a, b, a_sz));
}

/* 
 * Position of the first memory location of the transformation cost matrix given
 * a bigarray from ocaml.
 */
#ifdef _WIN32
__inline int *
#else
inline int *
#endif
cm_get_transformation_cost_matrix (const cmt a) {
    return (a->cost);
}

#ifdef _WIN32
__inline int *
#else
inline int *
#endif
cm_get_tail_cost (const cmt a) {
    return (a->tail_cost);
}

#ifdef _WIN32
__inline int *
#else
inline int *
#endif
cm_get_prepend_cost (const cmt a) {
    return (a->prepend_cost);
}

#ifdef _WIN32
__inline int *
#else
inline int *
#endif
cm_get_transformation_cost_matrix_3d (const cm_3dt a) {
    return (a->cost);
}

#ifdef _WIN32
__inline int *
#else
inline int *
#endif
cm_get_row (int *tcm, SEQT a, int a_sz) {
    if (a_sz <= 0) failwith ("Alphabet size = 3");
    if ((1 << a_sz) <= a) failwith ("3a is bigger than alphabet size");
    return (tcm + (a << a_sz));
}

#ifdef _WIN32
__inline int *
#else
inline int *
#endif
cm_get_row_level (int *tcm, SEQT a, int a_sz)
{
    if (a_sz <= 0) failwith ("cm_get_row_level:Alphabet size <= 0");
    if (a_sz <= a) 
    { 
        failwith ("cm_get_row_level: index a is bigger than alphabet size");
    }
    return (tcm + a * a_sz);
}

#ifdef _WIN32
__inline int *
#else
inline int *
#endif
cm_get_row_3d (int *tcm, SEQT a, SEQT b, int a_sz) {
    if (a_sz <= 0) failwith ("Alphabet size must bigger than 0");
    if ((1 << a_sz) <= a) failwith ("4a is bigger than alphabet size");
    if ((1 << a_sz) <= b) failwith ("b is bigger than alphabet size");
    return (tcm + (((a << a_sz) + b) << a_sz));
}

#ifdef _WIN32
__inline int *
#else
inline int *
#endif
cm_get_row_3d_level (int *tcm, SEQT a, SEQT b, int a_sz) {
    if (a_sz <= 0) failwith ("Alphabet size must bigger than 0");
    if (a_sz <= a) failwith ("index a is bigger than alphabet size");
    if (a_sz <= b) failwith ("index b is bigger than alphabet size");
    return (tcm + (((a * a_sz) + b) * a_sz));
}


#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_value_seqt (SEQT a, SEQT b, SEQT v, SEQT *p, int a_sz) {
    *(p + (cm_calc_cost_position_seqt (a, b, a_sz))) = v;
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_value_seqt_nonbit (SEQT a, SEQT b, SEQT v, SEQT *p, int a_sz) {
    *(p + ( ( ((int)a) * a_sz ) + ( (int)b ) ) ) = v;
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_value (int a, int b, int v, int *p, int a_sz) {
    *(p + (cm_calc_cost_position (a, b, a_sz))) = v;
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_value_nonbit (int a, int b, int v, int *p, int sz) {
    *(p + (a*sz+b)) = v;
    return;
}


void
cm_precalc_4algn (const cmt c, matricest matrix, const seqt s) {
    int i, j, l, m, *tmp_cost, *tcm, *tmp_to, *prepend, *tail, *to;
    int alphabet_size, uselevel;
    SEQT *begin;
    l = seq_get_len (s);
    to = mat_get_2d_prec (matrix);
    tcm = cm_get_transformation_cost_matrix (c);
    prepend = cm_get_prepend_cost (c);
    tail = cm_get_tail_cost (c);
    tmp_to = to + l;
    begin = seq_get_begin (s);         /* Inlined seq_get for speed purposes */
    /* We will use the 0'th row to store the cost of the prepend */
    for (m = 0; m < l; m++) {
        to[m] = prepend[begin[m]];
    }
    uselevel =  cm_check_level(c);
    alphabet_size = (uselevel==1)? c->map_sz : c->a_sz;
    if( uselevel == 1 ){
        for (j = 1; j <= alphabet_size; j++, tmp_to += l) {
            tmp_to[0] = tail[j];
            for (i = 1; i < l; i++) {
                tmp_to[i] = cm_get_cost(tcm,j,begin[i],c->map_sz+1);
            }
        }
    } else {
        for (j = 1; j <= alphabet_size; j++, tmp_to += l) {
            tmp_to[0] = tail[j];
            tmp_cost = cm_get_row (tcm, j, c->lcm);
            for (i = 1; i < l; i++) {
                 tmp_to[i] = tmp_cost[begin[i]];
            }
        }
    }
    return;
}

const int * cm_get_precal_row (const int *p, SEQT item, int len) {
    return (p + (len * item));
}


int *
cm_get_pos_in_precalc (const int *to, int s3l, int a_sz, int s1c, int s2c, int s3p)
{
    int *res;
    a_sz++;
    res = (int *) to + ((s1c * (a_sz * s3l)) + (s3l * s2c) + s3p);
    return (res);
}

int *
cm_get_row_precalc_3d (const int *to, int s3l, int a_sz, int s1c, int s2c) {
    return (cm_get_pos_in_precalc (to, s3l, a_sz, s1c, s2c, 0));
}

void
cm_precalc_4algn_3d (const cm_3dt c, int *to, const seqt s) {
    int i, j, k, l, *tmp_cost, *tcm; int alphabet_size, uselevel;
    int sequen, *precalc_pos;
    l = seq_get_len (s);
    tcm = cm_get_transformation_cost_matrix_3d (c);
    uselevel =  cm_check_level_3d(c);
    if (uselevel==1) alphabet_size = c->map_sz;
    else alphabet_size = c->a_sz;
    for (j = 1; j <=alphabet_size; j++) 
        for (k = 1; k <=alphabet_size; k++) {
            if (uselevel==1){
                tmp_cost = cm_get_row_3d_level (tcm, j, k, c->map_sz+1);
            } else {
                tmp_cost = cm_get_row_3d (tcm, j, k, c->lcm);
            }
            for (i = 0; i < l; i++) {
                sequen = seq_get (s, i);
                precalc_pos = (int *) cm_get_pos_in_precalc (to, l, c->a_sz, j, k, i);
                *precalc_pos = *(tmp_cost + sequen); 
            }
        }
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_value_3d_seqt (SEQT a, SEQT b, SEQT c, SEQT v, SEQT *p, int a_sz) {
    *(p + (cm_calc_cost_position_3d_seqt (a, b, c, a_sz))) = v;
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_value_3d_seqt_nonbit (SEQT a, SEQT b, SEQT c, SEQT v, SEQT *p, int a_sz) {
    *(p + (cm_calc_cost_position_3d_seqt_nonbit (a, b, c, a_sz))) = v;
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_value_3d (int a, int b, int c, int v, int *p, int a_sz) {
    *(p + (cm_calc_cost_position_3d (a, b, c, a_sz))) = v;
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_value_3d_nonbit (int a, int b, int c, int v, int *p, int a_sz) {
    *(p + (cm_calc_cost_position_3d_nonbit (a, b, c, a_sz))) = v;
    return;
}


#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_cost_level (int a, int b, int v, cmt c) {
    cm_set_value_nonbit (a, b, v, c->cost, c->map_sz+1);
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_cost (int a, int b, int v, cmt c) {
    cm_set_value (a, b, v, c->cost, c->lcm);
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_combmap (int a, int b, int v, cmt c) {
     cm_set_value_nonbit ((a), (b), v, c->combmap, (c->map_sz+1));
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_comb2list (int a, int b, int v, cmt c) {
    cm_set_value_nonbit ((v), 1, a, c->comb2list, 3);
    cm_set_value_nonbit ((v), 2, b, c->comb2list, 3);
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_worst (int a, int b, int v, cmt c) {
    cm_set_value (a, b, v, c->worst, c->lcm);
    return;
}


#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_worst_level (int a, int b, int v, cmt c) {
    cm_set_value_nonbit (a, b, v, c->worst, c->map_sz+1);
    return;
}


#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_cost_3d (int a, int b, int cp, int v, cm_3dt c) {
    cm_set_value_3d (a, b, cp, v, c->cost, c->lcm);
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_cost_3d_level (int a, int b, int cp, int v, cm_3dt c) {
    cm_set_value_3d_nonbit (a, b, cp, v, c->cost, c->map_sz+1);
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_prepend (int a, int b, cmt c) {
    c->prepend_cost[a] = b;
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_tail (int a, int b, cmt c) {
    c->tail_cost[a] = b;
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_median (SEQT a, SEQT b, SEQT v, cmt c) {
    cm_set_value_seqt (a, b, v, c->median, c->lcm);
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_median_level (SEQT a, SEQT b, SEQT v, cmt c) {
    cm_set_value_seqt_nonbit (a, b, v, c->median, (c->map_sz+1));
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_median_3d (SEQT a, SEQT b, SEQT cp, SEQT v, cm_3dt c) {
    cm_set_value_3d_seqt (a, b, cp, v, c->median, c->lcm);
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
cm_set_median_3d_level (SEQT a, SEQT b, SEQT cp, SEQT v, cm_3dt c) {
    cm_set_value_3d_seqt_nonbit (a, b, cp, v, c->median, c->map_sz+1);
    return;
}


unsigned long cm_CAML_deserialize (void *v) {
    cmt n;
    int len; 
    n = (cmt) v;
    // add following for level
    int mapsize; 
    int comb_map_sz; 
    int comb2list_sz;
    int level;
    int a_sz;
    level = n -> level;
    a_sz = n -> ori_a_sz; 
    mapsize = n->map_sz;
    n->ori_a_sz = deserialize_sint_4();
    n->level = deserialize_sint_4();
    n->map_sz = deserialize_sint_4();
    n->gap_startNO = deserialize_sint_4();
    // add above for level
    n->total_size = deserialize_sint_8();
    n->a_sz = deserialize_sint_4();
    n->lcm = deserialize_sint_4();
    n->gap = deserialize_sint_4();
    n->cost_model_type = deserialize_sint_4();
    n->combinations = deserialize_sint_4();
    n->gap_open = deserialize_sint_4();
    n->is_metric = deserialize_sint_4();
    n->all_elements = deserialize_sint_4();
    // add following for level
    comb_map_sz = (mapsize+1) * (mapsize+1);
    comb2list_sz = (mapsize+1) * (2+1);
    n->combmap = (int *) calloc (comb_map_sz*sizeof(int),1);
    n->comb2list = (int *) calloc (comb2list_sz *sizeof(int),1);
    if ((n->combmap == NULL) || (n->comb2list == NULL)) failwith ("Memory error.");
    deserialize_block_4(n->combmap, comb_map_sz);
    deserialize_block_4(n->comb2list,comb2list_sz);
    // add above for level
    if ((level>1)&&(level<=a_sz)) 
        len = comb_map_sz;
    else  len = 2 * (1 << (n->lcm)) * (1 << n->lcm) ;
    n->cost = (int *) calloc (len * sizeof(int), 1);
    n->median = (SEQT *) calloc (len * sizeof(SEQT), 1);
    n->worst = (int *) calloc (len * sizeof(int), 1);
    n->prepend_cost = (int *) calloc (len * sizeof(int), 1);
    n->tail_cost = (int *) calloc (len * sizeof(int), 1);
    if ((n->cost == NULL) || (n->median == NULL)) failwith ("Memory error.");
    deserialize_block_4(n->cost, len);
    DESERIALIZE_SEQT(n->median, len);
    deserialize_block_4(n->worst, len);
    deserialize_block_4(n->prepend_cost, len);
    deserialize_block_4(n->tail_cost, len);
    return (sizeof(struct cm));
}

unsigned long cm_CAML_deserialize_3d (void *v) {
    cm_3dt n;
    int len;
    n = (cm_3dt) v;
    //add following for level
    int mapsize; 
    int comb_map_sz; 
    int comb2list_sz;
    int level;
    int a_sz;
    level = n -> level;
    a_sz = n -> ori_a_sz;
    mapsize = n->map_sz;
    n->ori_a_sz = deserialize_sint_4();
    n->level = deserialize_sint_4();
    n->map_sz = deserialize_sint_4();
    n->gap_startNO = deserialize_sint_4();
    //add above for level
    n->total_size = deserialize_sint_8();
    n->a_sz = deserialize_sint_4();
    n->lcm = deserialize_sint_4();
    n->gap = deserialize_sint_4();
    n->cost_model_type = deserialize_sint_4();
    n->combinations = deserialize_sint_4();
    n->gap_open = deserialize_sint_4();
    n->all_elements = deserialize_sint_4();
    // add following for level
    comb_map_sz = (mapsize+1) * (mapsize+1) ;
    comb2list_sz = (mapsize+1) * (2+1);
    n->comblist_2_combcode = (int *) calloc (comb_map_sz*sizeof(int),1);
    n->combcode_2_comblist = (int *) calloc (comb2list_sz *sizeof(int),1);
    if ((n->comblist_2_combcode == NULL) || (n->combcode_2_comblist == NULL)) failwith ("Memory error.");
    deserialize_block_4(n->comblist_2_combcode, comb_map_sz);
    deserialize_block_4(n->combcode_2_comblist,comb2list_sz);
    // add above for level
    if ((level>1)&&(level<=a_sz))  len = (mapsize+1)* (mapsize+1)*(mapsize+1) ;
    else 
    len = (1 << (n->lcm + 1)) * (1 << (n->lcm + 1)) * (1 << (n->lcm + 1));
    n->cost = (int *) calloc (len * sizeof(int), 1);
    n->median = (SEQT *) calloc (len * sizeof(SEQT), 1);
    if ((n->cost == NULL) || (n->median == NULL)) failwith ("Memory error.");
    deserialize_block_4(n->cost, len);
    DESERIALIZE_SEQT(n->median, len);
    return (sizeof(struct cm_3d));
}

void
cm_CAML_serialize (value vcm, unsigned long *wsize_32, unsigned long *wsize_64)
{
    cmt c;
    int len; 
    if (!NDEBUG) {
        printf ("I will serialize cm!\n");
        fflush (stdout);
    }
    c = Cost_matrix_struct(vcm);
    //add following for level
    int mapsize; 
    int comb_map_sz; int comb2list_sz;
    int level; int a_sz;
    level = c -> level;
    a_sz = c -> ori_a_sz;
    mapsize = c->map_sz; 
    serialize_int_8(c->total_size);
    serialize_int_4(c->ori_a_sz);
    serialize_int_4(c->level);
    serialize_int_4(c->map_sz);
    serialize_int_4(c->gap_startNO);
    // add above for level
    serialize_int_4(c->a_sz);
    serialize_int_4(c->lcm);
    serialize_int_4(c->gap);
    serialize_int_4(c->cost_model_type);
    serialize_int_4(c->combinations);
    serialize_int_4(c->gap_open);
    serialize_int_4(c->is_metric);
    serialize_int_4(c->all_elements);
    *wsize_64 = *wsize_32 = sizeof(struct cm);
    // add following for level
    comb_map_sz = (mapsize+1) * (mapsize+1) ;
    comb2list_sz = (mapsize+1) * (2+1);
    serialize_block_4(c->combmap,comb_map_sz);
    serialize_block_4(c->comb2list,comb2list_sz);
    // add above for level
    if ((level>1)&&(level<=a_sz))
        len = comb_map_sz;
    else  len = 2 * (1 << (c->lcm)) * (1 << (c->lcm));
    serialize_block_4(c->cost, len);
    SERIALIZE_SEQT(c->median, len);
    serialize_block_4(c->worst, len);
    serialize_block_4(c->prepend_cost, len);
    serialize_block_4(c->tail_cost, len);
    return;
}

void
cm_CAML_serialize_3d (value vcm, unsigned long *wsize_32, unsigned long *wsize_64)
{
    cm_3dt c;
    int len;
    c = Cost_matrix_struct_3d(vcm);
    // add following for level
    int mapsize; 
    int comb_map_sz; int comb2list_sz;
    int level; int a_sz;
    level = c -> level;
    a_sz = c -> ori_a_sz;
    mapsize = c->map_sz; 
    serialize_int_4(c->ori_a_sz);
    serialize_int_4(c->level);
    serialize_int_4(c->map_sz);
    serialize_int_4(c->gap_startNO);
    // add above for level
    serialize_int_8(c->total_size);
    serialize_int_4(c->a_sz);
    serialize_int_4(c->lcm);
    serialize_int_4(c->gap);
    serialize_int_4(c->cost_model_type);
    serialize_int_4(c->combinations);
    serialize_int_4(c->gap_open);
    serialize_int_4(c->all_elements);
    // add following for level
    comb_map_sz = (mapsize+1) * (mapsize+1);
    comb2list_sz = (mapsize+1) * (2+1);
    serialize_block_4(c->comblist_2_combcode,comb_map_sz);
    serialize_block_4(c->combcode_2_comblist,comb2list_sz);
    // add above for level
    *wsize_64 = *wsize_32 = sizeof(struct cm_3d);
    if ((level>1)&&(level<=a_sz))
        len = (mapsize+1) * (mapsize+1) * (mapsize+1) ;
    else
        len = (1 << (c->lcm + 1)) * (1 << (c->lcm + 1)) * (1 << (c->lcm + 1));
    serialize_block_4(c->cost, len);
    SERIALIZE_SEQT(c->median, len);
    return;
}

void
cm_CAML_free (value v) {
    cmt c;
    c = Cost_matrix_struct(v);
    free (c->combmap);
    free (c->comb2list);
    free (c->cost);
    free (c->median);
    free (c->worst);
    free (c->prepend_cost);
    free (c->tail_cost);
    return;
}

void
cm_CAML_free_3d (value v) {
    cm_3dt c;
    c = Cost_matrix_struct_3d(v);
    free (c->cost);
    free (c->median);
    return;
}

int
cm_compare (cmt a, cmt b) {
    int cmp, len_g;
    size_t len, len1;
    int level, a_sz;
    level = a -> level;
    a_sz = a -> ori_a_sz;
    if (a->total_size != b->total_size) {
        return (a->total_size - b->total_size);
    }
    else if (a->a_sz != b->a_sz) {
        return (a->a_sz - b->a_sz);
    }
    else if (a->combinations != b->combinations) {
        return (a->combinations - b->combinations);
    }
    else if (a->cost_model_type != b->cost_model_type) {
        return (a->cost_model_type - b->cost_model_type);
    }
    else if (a->gap_open != b->gap_open) {
        return (a->gap_open - b->gap_open);
    }
    else if (a->is_metric != b->is_metric) {
        return (a->is_metric - b->is_metric);
    }
    else {
        if ((level>1)&&(level<=a_sz)) 
            len_g = (a->map_sz +1) * (a->map_sz+1);
        else 
            len_g = 2 * (1 << (a->lcm)) * (1 << (a->lcm));
        len = len_g * sizeof(int);
        len1 = len_g * sizeof(SEQT);
        cmp = memcmp (a->cost, b->cost, len);
        if (cmp != 0) return (cmp);
        cmp = memcmp (a->median, b->median, len1);
        if (cmp != 0) return (cmp);
        cmp = memcmp (a->worst, b->worst, len);
        return (cmp);
    }
}

int
cm_CAML_compare (value a, value b) {
    int res;
    res = cm_compare (Cost_matrix_struct(a), Cost_matrix_struct(b));
    return (res);
}

int
cm_compare_3d (value a, value b) {
    return(0);
}

static struct custom_operations cost_matrix = {
    "http://www.amnh.org/poy/cost_matrix/two_dimensional.0.1",
    &cm_CAML_free,
    &cm_CAML_compare,
    custom_hash_default,
    cm_CAML_serialize, 
    cm_CAML_deserialize
};

static struct custom_operations cost_matrix_3d = {
    "http://www.amnh.org/poy/cost_matrix/three_dimensional.0.1",
    &cm_CAML_free_3d,
    &cm_compare_3d,
    custom_hash_default,
    cm_CAML_serialize_3d, 
    cm_CAML_deserialize_3d
};

void
cm_copy_contents (int *src, int *tgt, int len) {
    int i;
    for (i = 0; i < len; i++)
        *(tgt + i) = *(src + i);
    return;
}


void
cm_copy_contents_seqt (SEQT *src, SEQT *tgt, int len) {
    int i;
    for (i = 0; i < len; i++)
        *(tgt + i) = *(src + i);
    return;
}

value
cm_CAML_clone (value v) {
    CAMLparam1(v);
    value clone;
    cmt clone2;
    cmt c;
    int len;
    int combmatrix_size;
    int comb2list_size;
    int level, a_sz;
    clone = alloc_custom (&cost_matrix, sizeof(struct cm), 1, 1000000);
    clone2 = Cost_matrix_struct(clone);
    c = Cost_matrix_struct(v);
    //fprintf(stdout,"clone, c->map_sz=%d,c->a_sz=%d,level=%d,", c->map_sz,c->a_sz,c->level); fflush(stdout);
    if (c->combinations)
        cm_set_val (c->lcm, c->combinations, c->cost_model_type, \
                c->gap_open, c->is_metric, c->is_identity, c->all_elements, clone2, c->level, c->map_sz, c->gap_startNO, c->tie_breaker);
    else
        cm_set_val (c->a_sz, c->combinations, c->cost_model_type, \
                c->gap_open, c->is_metric, c->is_identity, c->all_elements, clone2, 1, c->a_sz, 0, c->tie_breaker);
    level = c->level;
    a_sz = c->map_sz;
    combmatrix_size = (c->map_sz+1)  * (c->map_sz+1);
    comb2list_size = (c->map_sz+1) * (2+1);
    if ((level>1)&&(level<=a_sz)) 
        len = combmatrix_size;
    else
        len = 2 *(1 << (c->lcm)) * (1 << (c->lcm));
    //fprintf(stdout,"copy int/seqt of SIZE =%d\n",len); fflush(stdout);
    cm_copy_contents (c->combmap, clone2->combmap, combmatrix_size);
    cm_copy_contents (c->comb2list, clone2->comb2list, comb2list_size);
    cm_copy_contents (c->cost, clone2->cost, len);
    cm_copy_contents_seqt (c->median, clone2->median, len);
    cm_copy_contents (c->worst, clone2->worst, len);
    cm_copy_contents (c->prepend_cost, clone2->prepend_cost, len);
    cm_copy_contents (c->tail_cost, clone2->tail_cost, len);
    CAMLreturn(clone);
}

value
cm_CAML_clone_3d (value v) {
    CAMLparam1(v);
    CAMLlocal1(clone);
    cm_3dt clone2;
    cm_3dt c;
    int len;
    clone = alloc_custom (&cost_matrix_3d, sizeof(struct cm), 1, 1000000);
    clone2 = Cost_matrix_struct_3d(clone);
    c = Cost_matrix_struct_3d(v);
    cm_set_val_3d (c->lcm, c->combinations, c->cost_model_type, \
            c->gap_open, c->all_elements, c->level, c->map_sz, c->gap_startNO, clone2);
    len = (c->a_sz + 1) * (c->a_sz + 1) * (c->a_sz + 1);
    cm_copy_contents (c->cost, clone2->cost, len);
    cm_copy_contents_seqt (c->median, clone2->median, len);
    CAMLreturn(clone);
}


value 
cm_CAML_set_tie_breaker_3d (value c, value v) {
    CAMLparam2 (c, v);
    cm_set_tie_breaker_3d (Cost_matrix_struct_3d(c), Int_val(v));
    CAMLreturn (Val_unit);
}

value 
cm_CAML_set_tie_breaker (value c, value v) {
    CAMLparam2 (c, v);
    cm_set_tie_breaker (Cost_matrix_struct(c), Int_val(v));
    CAMLreturn (Val_unit);
}

value 
cm_CAML_set_gap_3d (value c, value v) {
    CAMLparam2 (c, v);
    cm_set_gap_3d (Cost_matrix_struct_3d(c), Int_val(v));
    CAMLreturn (Val_unit);
}

value 
cm_CAML_set_gap (value c, value v) {
    CAMLparam2 (c, v);
    cm_set_gap (Cost_matrix_struct(c), Int_val(v));
    CAMLreturn (Val_unit);
}

value 
cm_CAML_set_ori_a_sz (value c, value v) {
    CAMLparam2 (c, v);
    cm_set_ori_a_sz (Cost_matrix_struct(c), Int_val(v));
    CAMLreturn (Val_unit);
}

value 
cm_CAML_set_map_sz (value c, value v) {
    CAMLparam2 (c, v);
    cm_set_map_sz (Cost_matrix_struct(c), Int_val(v));
    CAMLreturn (Val_unit);
}

value 
cm_CAML_set_level (value c, value v) {
    CAMLparam2 (c, v);
    cm_set_level (Cost_matrix_struct(c), Int_val(v));
    CAMLreturn (Val_unit);
}

value
cm_CAML_ini_combmap (value c, value v) {
    CAMLparam2(c, v);
    cm_ini_combmap ( Cost_matrix_struct(c), Int_val(v));
    CAMLreturn (Val_unit);
}

value
cm_CAML_ini_comb2list (value c, value v) {
    CAMLparam2(c, v);
    cm_ini_comb2list ( Cost_matrix_struct(c), Int_val(v));
    CAMLreturn (Val_unit);
}

value
cm_CAML_set_combmap (value va, value vb, value vc, value c) {
    CAMLparam4(va,vb,vc,c);
    cm_set_combmap ( Int_val(va),Int_val(vb), Int_val(vc),Cost_matrix_struct(c));
    CAMLreturn (Val_unit);
}

value
cm_CAML_set_comb2list (value va, value vb, value vc, value c) {
    CAMLparam4(va,vb,vc,c);
    cm_set_comb2list ( Int_val(va),Int_val(vb), Int_val(vc),Cost_matrix_struct(c));
    CAMLreturn (Val_unit);
}

value 
cm_CAML_set_affine_3d (value c, value do_aff, value go) {
    CAMLparam3(c, do_aff, go);
    cm_set_affine_3d (Cost_matrix_struct_3d(c), Int_val(do_aff), Int_val(go));
    CAMLreturn(Val_unit);
}

value 
cm_CAML_set_affine (value c, value do_aff, value go) {
    CAMLparam3(c, do_aff, go);
    cm_set_affine (Cost_matrix_struct(c), Int_val(do_aff), Int_val(go));
    CAMLreturn(Val_unit);
}

value 
cm_CAML_get_a_sz_3d (value cm) {
    CAMLparam1 (cm);
    CAMLreturn (Val_int((Cost_matrix_struct_3d(cm))->a_sz));
}

value 
cm_CAML_get_a_sz (value cm) {
    CAMLparam1 (cm);
    CAMLreturn (Val_int((Cost_matrix_struct(cm))->a_sz));
}

value
cm_CAML_set_a_sz_3d (value v, value cm) {
    CAMLparam2 (cm, v);
    cm_set_a_sz_3d (Cost_matrix_struct_3d(cm), Int_val(v));
    CAMLreturn(Val_unit);
}


value
cm_CAML_set_a_sz (value cm, value v) {
    CAMLparam2 (cm, v);
    cm_set_a_sz (Cost_matrix_struct(cm), Int_val(v));
    CAMLreturn(Val_unit);
}


value 
cm_CAML_get_tie_breaker_3d (value c) {
    CAMLparam1 (c);
    CAMLreturn (Val_int((cm_get_tie_breaker_3d (Cost_matrix_struct_3d(c)))));
}

value 
cm_CAML_get_tie_breaker (value c) {
    CAMLparam1 (c);
    CAMLreturn (Val_int((cm_get_tie_breaker (Cost_matrix_struct(c)))));
}

value 
cm_CAML_get_gap_3d (value c) {
    CAMLparam1 (c);
    CAMLreturn (Val_int((cm_get_gap_3d (Cost_matrix_struct_3d(c)))));
}

value 
cm_CAML_get_gap (value c) {
    CAMLparam1 (c);
    CAMLreturn (Val_int((cm_get_gap (Cost_matrix_struct(c)))));
}

value 
cm_CAML_get_ori_a_sz (value c) {
    CAMLparam1 (c);
    CAMLreturn (Val_int((cm_get_ori_a_sz (Cost_matrix_struct(c)))));
}

value 
cm_CAML_get_ori_a_sz_3d (value c) {
    CAMLparam1 (c);
    CAMLreturn (Val_int((cm_get_ori_a_sz_3d (Cost_matrix_struct_3d(c)))));
}

value 
cm_CAML_get_map_sz (value c) {
    CAMLparam1 (c);
    CAMLreturn (Val_int((cm_get_map_sz (Cost_matrix_struct(c)))));
}

value 
cm_CAML_get_map_sz_3d (value c) {
    CAMLparam1 (c);
    CAMLreturn (Val_int((cm_get_map_sz_3d (Cost_matrix_struct_3d(c)))));
}


value 
cm_CAML_get_gap_startNO (value c) {
    CAMLparam1 (c);
    CAMLreturn (Val_int((cm_get_gap_startNO (Cost_matrix_struct(c)))));
}

value 
cm_CAML_get_level (value c) {
    CAMLparam1 (c);
    CAMLreturn (Val_int((cm_get_level (Cost_matrix_struct(c)))));
}

value 
cm_CAML_get_level_3d (value c) {
    CAMLparam1 (c);
    CAMLreturn (Val_int((cm_get_level_3d (Cost_matrix_struct_3d(c)))));
}

value 
cm_CAML_get_affine_3d (value c) {
    CAMLparam1(c);
    CAMLreturn(Val_int(cm_get_affine_flag_3d (Cost_matrix_struct_3d(c))));
}

value 
cm_CAML_get_affine (value c) {
    CAMLparam1(c);
    CAMLreturn(Val_int(cm_get_affine_flag (Cost_matrix_struct(c))));
}

value 
cm_CAML_get_gap_opening_3d (value c) {
    CAMLparam1(c);
    int i;
    i = cm_get_gap_opening_parameter_3d(Cost_matrix_struct_3d(c));
    CAMLreturn(Val_int(i));
}

value 
cm_CAML_get_gap_opening (value c) {
    CAMLparam1(c);
    int i;
    i = cm_get_gap_opening_parameter(Cost_matrix_struct(c));
    CAMLreturn(Val_int(i));
}

value 
cm_CAML_get_combinations (value c) {
    CAMLparam1(c);
    int i;
    i = cm_get_combinations (Cost_matrix_struct(c));
    CAMLreturn(Val_int(i));
}

value
cm_CAML_get_combinations_3d (value c) {
    CAMLparam1(c);
    int i;
    i = cm_get_combinations_3d (Cost_matrix_struct_3d(c));
    CAMLreturn(Val_int(i));
}

value
cm_CAML_get_cost_3d (value a, value b, value c, value cm) {
    CAMLparam3(a, b, c);
    int *tcm;
    cm_3dt tmp;
    tmp = Cost_matrix_struct_3d(cm);
    tcm = tmp->cost;
    if (cm_check_level_3d(tmp)==1) 
         CAMLreturn(Val_int(cm_calc_cost_3d_level(tcm, Int_val(a), Int_val(b), \
                    Int_val(c), tmp->map_sz+1)));
    else
        CAMLreturn(Val_int(cm_calc_cost_3d(tcm, Int_val(a), Int_val(b), \
                    Int_val(c), tmp->lcm)));
}

value
cm_CAML_get_cost (value a, value b, value c) {
    CAMLparam3(a, b, c);
    CAMLlocal1( d );
    int *tcm, res;
    cmt tmp;
    tmp = Cost_matrix_struct(c);
    tcm = tmp->cost;
    if (cm_check_level(tmp) == 1 )
        res = cm_get_cost(tcm, Int_val(a), Int_val(b), (tmp->map_sz+1));
    else
        res = cm_calc_cost(tcm, Int_val(a), Int_val(b), tmp->lcm);
    d = Val_int( res );
    CAMLreturn( d );
}

value
cm_CAML_get_combmap (value a, value b, value c) {
     CAMLparam3(a, b, c);
    int *tcm;
    cmt tmp;
    tmp = Cost_matrix_struct(c);
    tcm = tmp->combmap;
    CAMLreturn(Val_int(cm_get_combmap(tcm, Int_val(a), Int_val(b), (tmp->map_sz+1))));
}

value
cm_CAML_get_comblist (value combcode, value position, value c)
{
    CAMLparam2(combcode,c);
    int * tcm;
    cmt tmp;
    tmp = Cost_matrix_struct(c);
    tcm = tmp->comb2list;
    CAMLreturn(Val_int(cm_get_comblist(tcm, Int_val(combcode), Int_val(position), 3)));
    
   // CAMLreturn ( alloc_bigarray (BIGARRAY_INT32 | BIGARRAY_C_LAYOUT, 1, 
    //    cm_get_comblist(tcm, Int_val(combcode), 2), dims) ) ;
    
}


value
cm_CAML_print_matrix (value mw, value mh, value c) {
    CAMLparam3(mw, mh, c);
    int w = Int_val(mw);
    int h = Int_val(mh);
    int *tcm;
    cmt tmp;
    tmp = Cost_matrix_struct(c);
    tcm = tmp->cost;
    cm_print_matrix (tcm, w, h);
    CAMLreturn (Val_unit);
}


value
cm_CAML_get_worst (value a, value b, value c) {
    CAMLparam3(a, b, c);
    int *tcm;
    cmt tmp;
    tmp = Cost_matrix_struct(c);
    tcm = tmp->worst;
    if(cm_check_level(tmp) == 1 ) 
         CAMLreturn(Val_int(cm_get_cost(tcm, Int_val(a), Int_val(b), (tmp->map_sz+1))));
    else
        CAMLreturn(Val_int(cm_calc_cost(tcm, Int_val(a), Int_val(b), tmp->lcm)));
}


value
cm_CAML_get_median_3d (value a, value b, value c, value cm) {
    CAMLparam3(a, b, c);
    SEQT *tcm;
    cm_3dt tmp;
    tmp = Cost_matrix_struct_3d(cm);
    tcm = tmp->median;
    if(cm_check_level_3d(tmp) == 1)
        CAMLreturn(Val_int(cm_calc_cost_3d_seqt_level(tcm, Int_val(a), Int_val(b), \
                    Int_val(c), tmp->map_sz+1)));
    else
        CAMLreturn(Val_int(cm_calc_cost_3d_seqt(tcm, Int_val(a), Int_val(b), \
                    Int_val(c), tmp->lcm)));
}

#ifdef _WIN32
__inline SEQT
#else
inline SEQT
#endif
cm_get_median (const cmt tmp, SEQT a, SEQT b) {
    if ( cm_check_level(tmp) == 1 )
        return (cm_calc_median_nonbit((tmp->median), a, b, tmp->map_sz+1));
    else
        return (cm_calc_median((tmp->median), a, b, tmp->lcm));
}

#ifdef _WIN32
__inline SEQT
#else
inline SEQT
#endif
cm_get_median_3d (const cm_3dt t, SEQT a, SEQT b, SEQT c) {
    return (cm_calc_median_3d((t->median), a, b, c, t->lcm));
}

value
cm_CAML_get_median (value a, value b, value c) {
    CAMLparam3(a, b, c);
    SEQT *tcm;
    cmt tmp;
    tmp = Cost_matrix_struct(c);
    tcm = tmp->median;
    if (cm_check_level(tmp) == 1 )
         CAMLreturn(Val_int(cm_get_median_level(tcm, Int_val(a), Int_val(b), tmp->map_sz+1)));
    else
        CAMLreturn(Val_int(cm_calc_median(tcm, Int_val(a), Int_val(b), tmp->lcm)));
}

value
cm_CAML_set_cost_3d (value a, value b, value c, value cc, value v) {
    CAMLparam5(a, b, c, cc, v);
    cm_3dt tmp;
    tmp = Cost_matrix_struct_3d(cc);
    if (cm_check_level_3d(tmp)==1)
        cm_set_cost_3d_level (Int_val(a), Int_val(b), Int_val(c), Int_val(v), tmp);
    else
        cm_set_cost_3d (Int_val(a), Int_val(b), Int_val(c), Int_val(v), tmp);
    CAMLreturn(Val_unit);
}

value
cm_CAML_set_cost (value a, value b, value c, value v) {
    CAMLparam4(a, b, c, v);
    cmt tmp;
    tmp = Cost_matrix_struct(c);
    if ( cm_check_level(tmp) == 1 )
        cm_set_cost_level (Int_val(a), Int_val(b), Int_val(v), tmp);
    else
        cm_set_cost (Int_val(a), Int_val(b), Int_val(v), tmp);
    CAMLreturn(Val_unit);
}

value 
cm_CAML_set_worst (value a, value b, value c, value v) {
    CAMLparam4(a, b, c, v);
    cmt tmp;
    tmp = Cost_matrix_struct(c);
    if ( cm_check_level(tmp) == 1 )
        cm_set_worst_level (Int_val(a), Int_val(b), Int_val(v), tmp);
    else
        cm_set_worst (Int_val(a), Int_val(b), Int_val(v), tmp);
    CAMLreturn(Val_unit);
}

value
cm_CAML_set_median_3d (value a, value b, value c, value cp, value v) {
    CAMLparam4(a, b, c, v);
    cm_3dt tmp;
    tmp = Cost_matrix_struct_3d(cp);
    if(cm_check_level_3d(tmp)==1)
        cm_set_median_3d_level (Int_val(a), Int_val(b), Int_val(c), Int_val(v), tmp);
    else
        cm_set_median_3d (Int_val(a), Int_val(b), Int_val(c), Int_val(v), tmp);
    CAMLreturn(Val_unit);
}

value
cm_CAML_set_median (value a, value b, value c, value v) {
    CAMLparam4(a, b, c, v);
    cmt tmp;
    tmp = Cost_matrix_struct(c);
    if (cm_check_level(tmp)==1)
        cm_set_median_level (Int_val(a), Int_val(b), Int_val(v), tmp);
    else
         cm_set_median (Int_val(a), Int_val(b), Int_val(v), tmp);
    CAMLreturn(Val_unit);
}

value
cm_CAML_set_prepend (value a, value b, value v) {
    CAMLparam3(a, b, v);
    cmt tmp;
    tmp = Cost_matrix_struct(v);
    cm_set_prepend (Int_val(a), Int_val(b), tmp);
    CAMLreturn(Val_unit);
}

value
cm_CAML_set_tail (value a, value b, value v) {
    CAMLparam3(a, b, v);
    cmt tmp;
    tmp = Cost_matrix_struct(v);
    cm_set_tail (Int_val(a), Int_val(b), tmp);
    CAMLreturn(Val_unit);
}

value 
cm_CAML_get_prepend (value a, value v) {
    CAMLparam2(a, v);
    int r;
    cmt tmp;
    tmp = Cost_matrix_struct(v);
    assert ((1 + tmp->a_sz) > Int_val(a));
    r = tmp->prepend_cost[Int_val(a)];
    CAMLreturn(Val_int(r));
}

value 
cm_CAML_get_tail (value a, value v) {
    CAMLparam2(a, v);
    int r;
    cmt tmp;
    tmp = Cost_matrix_struct(v);
    assert ((1 + tmp->a_sz) > Int_val(a));
    r = tmp->tail_cost[Int_val(a)];
    CAMLreturn(Val_int(r));
}

value
cm_CAML_create_3d (value a_sz, value combine, value aff, value go, value d, value all, value level, value map_sz, value gap_startNO) {
    CAMLparam5(a_sz, combine, aff, go, d);
    CAMLxparam1(all);
    value tmp;
    cm_3dt tmp2;
    tmp = alloc_custom (&cost_matrix_3d, sizeof(struct cm_3d), 1, 1000000);
    tmp2 = Cost_matrix_struct_3d(tmp);
    cm_set_val_3d (Int_val(a_sz), Bool_val(combine), Int_val(aff), \
            Int_val(go), Int_val(all), Int_val(level), Int_val(map_sz), Int_val(gap_startNO), tmp2);
    CAMLreturn(tmp);
}

value 
cm_CAML_create_3d_bc (value *argv, int argn) {
    return (cm_CAML_create_3d (argv[0], argv[1], argv[2], argv[3], argv[4], argv[5],argv[6], argv[7],argv[8]));
}


value
cm_CAML_create (value a_sz, value combine, value aff, value go, value all, value level, value combine_number, value gap_start, value tie_breaker) {
    CAMLparam5(a_sz, combine, aff, go, all);
    CAMLxparam4(level, combine_number, gap_start, tie_breaker);
    value tmp;
    cmt m;
    tmp = alloc_custom (&cost_matrix, sizeof(struct cm), 1, 1000000);
    m = Cost_matrix_struct(tmp);
    //init matrix with -1/NULL
    m->total_size = m->a_sz = m->lcm = m->gap = m->cost_model_type = m->gap_open = 
        m->all_elements = m->ori_a_sz = m->map_sz = m->level = m->gap_startNO = m->tie_breaker= -1;
    m->combmap = m->comb2list = m->cost = m->worst = m->prepend_cost = m->tail_cost = NULL;
    m->median = NULL;
    //allocate memory
    cm_set_val (Int_val(a_sz), Bool_val(combine), Int_val(aff), Int_val(go), \
            0, 0, Int_val(all), m, Int_val(level), Int_val(combine_number), Int_val(gap_start), Int_val(tie_breaker));
    CAMLreturn(tmp);
}

value 
cm_CAML_create_bytecode (value * argv, int argn){
    return (cm_CAML_create 
        (argv[0],argv[1], argv[2], argv[3], argv[4], argv[5],argv[6],argv[7],argv[8]));
}

value 
cm_CAML_set_lcm (value c, value v) {
    CAMLparam2(c, v);
    cmt tmp;
    tmp = Cost_matrix_struct(c);
    cm_set_lcm (tmp, Int_val(v));
    CAMLreturn(Val_unit);
}

value 
cm_CAML_set_lcm_3d (value c, value v) {
    CAMLparam2(c, v);
    cm_3dt tmp;
    tmp = Cost_matrix_struct_3d(c);
    cm_set_lcm_3d (tmp, Int_val(v));
    CAMLreturn(Val_unit);
}

value 
cm_CAML_get_lcm (value c) {
    CAMLparam1(c);
    cmt tmp;
    tmp = Cost_matrix_struct(c);
    CAMLreturn(Val_int(cm_get_lcm(tmp)));
}

value 
cm_CAML_get_lcm_3d (value c) {
    CAMLparam1(c);
    cm_3dt tmp;
    tmp = Cost_matrix_struct_3d(c);
    CAMLreturn(Val_int(cm_get_lcm_3d(tmp)));
}

value 
cm_CAML_clone_to_3d (value c) {
    CAMLparam1(c);
    CAMLlocal1(res);
    cmt init;
    cm_3dt final;
    int len;
    init = Cost_matrix_struct(c);
    res = alloc_custom (&cost_matrix_3d, sizeof(struct cm_3d), 1, 1000000);
    final = Cost_matrix_struct_3d(res);
    cm_set_val_3d (init->lcm, init->combinations, init->cost_model_type, \
            init->gap_open, init->all_elements, init->level, init->map_sz, init->gap_startNO, final);
    len = init->map_sz;
    cm_copy_contents (init->combmap, final->comblist_2_combcode,(len+1)*(len+1));
    cm_copy_contents (init->comb2list, final->combcode_2_comblist, (len+1)*(2+1));
    CAMLreturn(res);
}

value 
cm_CAML_initialize (value unit) {
    CAMLparam1(unit);
    caml_register_custom_operations (&cost_matrix);
    caml_register_custom_operations (&cost_matrix_3d);
    CAMLreturn(Val_unit);
}

value
cm_CAML_set_is_metric (value c) {
    CAMLparam1(c);
    cmt init;
    init = Cost_matrix_struct(c);
    init->is_metric = 1;
    CAMLreturn(Val_unit);
}

value
cm_CAML_get_is_metric (value c) {
    CAMLparam1(c);
    cmt init;
    init = Cost_matrix_struct(c);
    CAMLreturn(Val_int(init->is_metric));
}


value
cm_CAML_set_is_identity (value c) {
    CAMLparam1(c);
    cmt init;
    init = Cost_matrix_struct(c);
    init->is_identity = 1;
    CAMLreturn(Val_unit);
}

value
cm_CAML_get_is_identity (value c) {
    CAMLparam1(c);
    cmt init;
    init = Cost_matrix_struct(c);
    CAMLreturn(Val_int(init->is_metric));
}
