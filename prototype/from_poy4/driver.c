#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "algn.h"
// #include "array_pool.h"
// #include "cm.h"
#include "matrices.h"

int main() {
    cost_matrices_p costMtx;

    nw_matrices_p algn_mtxs = malloc( sizeof(struct matrices) );

    algn_mtxs->len_eff     = 0;
    algn_mtxs->matrix      = malloc ( sizeof( int ) );
    algn_mtxs->dir_mtx_2d  = malloc ( sizeof( DIRECTION_MATRIX ) );
    algn_mtxs->pointers_3d = malloc ( sizeof( int* ) );
    algn_mtxs->cube        = malloc ( sizeof( int ) );
    algn_mtxs->cube_d      = malloc ( sizeof( DIRECTION_MATRIX ) );
    algn_mtxs->precalc     = malloc ( sizeof( int ) );  

    seq_p seq1 = malloc( sizeof(struct seq) );
    seq_p seq2 = malloc( sizeof(struct seq) );

    seq_p retSeq1 = malloc( sizeof(struct seq) );
    seq_p retSeq2 = malloc( sizeof(struct seq) );

/* This seems to be necessary only for OCaml interaction
    poolt pool1, pool2;

    pool1 = pool_create( 20, 2 );
    // pool_alloc( pool1, 3 );
    // pool_alloc( pool1, 2 );
    // pool_alloc( pool1, 3 );
    // pool_alloc( pool1, 3 );
    // pool_alloc( pool1, 2 );

    pool2 = pool_create( 20, 2 );
    // pool_alloc( pool2, 5 );
    // pool_alloc( pool2, 2 );
    // pool_alloc( pool2, 3 );
    // pool_alloc( pool2, 1 );
    // pool_alloc( pool2, 4 );
*/
    SEQT s1[20] = {16,3,8,5,6,8};
    SEQT s2[20] = {16,8,8,5,7};
    SEQT *ret_s1 = calloc(20, sizeof(SEQT));
    SEQT *ret_s2 = calloc(20, sizeof(SEQT));


    seq1->magic_number = 0; // This was only used in OCaml code
    seq1->cap          = 20; // capacity
    seq1->len          = 6;
    seq1->head         = s1;
    seq1->begin        = seq1->head; // because the beginning is at the head at first
    seq1->end          = seq1->begin + seq1->len;
 
    seq2->magic_number = 1; // came from OCaml
    seq2->cap          = 20;
    seq2->len          = 5;
    seq2->head         = s2;
    seq2->begin        = seq2->head; // because the beginning is at the head at first
    seq2->end          = seq2->begin + seq2->len;

    retSeq1->magic_number = 0; // This was only used in OCaml code
    retSeq1->cap          = 20; // capacity
    retSeq1->len          = 0;
    retSeq1->head         = ret_s1;
    retSeq1->begin        = retSeq1->head; // because the beginning is at the head at first
    retSeq1->end          = retSeq1->begin + retSeq1->len;
    
    retSeq2->magic_number = 1; // came from OCaml
    retSeq2->cap          = 20;
    retSeq2->len          = 0;
    retSeq2->head         = ret_s2;
    retSeq2->begin        = retSeq2->head; // because the beginning is at the head at first
    retSeq2->end          = retSeq2->begin + retSeq2->len;


    int alphSize     = 4; // not including gap or ambiguities
    int combinations = 1; // false if matrix is sparse. In this case, it's DNA, so not sparse.
    int do_aff       = 0;
    int gap_open     = 0;
    int is_metric    = 0;
    int all_elements = 31; // How is this used?
    costMtx          = malloc( sizeof(struct cost_matrices) );

    costMtx = cm_set_val ( alphSize, 
                           combinations, 
                           do_aff, 
                           gap_open, 
                           is_metric, 
                           all_elements, 
                           costMtx
                         );

    

    // Now initialize prepend_cost, median, cost, worst, tail_cost
    // code taken from cm.c:cm_CAML_deserialize
    size_t len = 2 * (1 << (costMtx->lcm)) * (1 << costMtx->lcm);
    costMtx->cost         = (int *)  calloc (len, sizeof(int));
    costMtx->median       = (SEQT *) calloc (len, sizeof(SEQT));
    costMtx->worst        = (int *)  calloc (len, sizeof(int));
    costMtx->prepend_cost = (int *)  calloc (len, sizeof(int));
    costMtx->tail_cost    = (int *)  calloc (len, sizeof(int));

    //cm_print(costMtx);

    
    size_t total = costMtx->lcm * costMtx->lcm; // the size of the input tcm
    printf("%zu\n", total);
    int* tcm = calloc(total, sizeof(int)); // this is the input tcm, not the generated one
    for (size_t i = 0; i < total; i += costMtx->lcm) {
        //printf("i: %zu\n", i);
        for (size_t j = 0; j < costMtx->lcm; j++) {
            //printf("i: %zu, j: %zu, cost: %lu\n", i, j, 2 * i + 2 * j);
            //tcm[i + j] = 2 * i + 2 * j;
            if ( i == j ) {
                tcm[i + j] = 0;
            } else {
                tcm[i + j] = 1;
            }
            //printf("cost: %d\n", tcm[i+j]);

            //cm_set_cost (i, j, or, costMtx);
            // printf("should be: %zu, is: %d\n", or, cm_get_cost (i,j,costMtx));
            // or += 2;
        }
    }

    // set up cost, median and worst matrices.
    int max, cost, min;
    SEQT median;
    size_t i, j;
    // for each element, ambiguous or not, cycle through all other elements
    for (SEQT base1 = 1; base1 <= 31; base1++) {
        for (SEQT base2 = 1; base2 <= 31; base2++) {
            median = 0;
            max = 0;
            min = INT_MAX; // this should be largest integer value.

            // now cycle through single-bit values, 1, 2, 4, etc.
            // if that bit is set in base1, look at all bits in base2
            // accumulate lowest cost and highest cost of all existing combinations
            // into cost and worst matrices.
            for (i = 1; i <= costMtx->alphSize; i++) {
                if ( 1 << (i - 1) & base1) {
                    //printf("base1: %hhu, i: %zu\n", base1, i);
                    for (j = 1; j < costMtx->alphSize; j++) {
                        //printf("%d\n", costMtx->alphSize);
                        if (1 << (j - 1) & base2) {
                            cost = tcm[(i - 1) * costMtx->lcm + j - 1];
                            if( cost < min ) {
                                min = cost;
                                median = (SEQT) (1 << (i - 1)) | (1 << (j - 1));
                                printf("i: %zu, j: %zu, same %hhu\n", i, j, median);
                                //cm_set_median(base1, base2, median, costMtx);
                            } else if (cost == min) {
                                median |= (SEQT) (1 << (i - 1)) | (1 << (j - 1));
                                printf("low  %hhu\n", median);
                                //cm_set_median(base1, base2, median, costMtx);
                            }
                            if( cost >= max ) {
                                max = cost;
                            }
                            
                        }
                    }
                }
                
            }
            cm_set_cost  (base1, base2, min, costMtx);
            cm_set_worst (base1, base2, max, costMtx);
            cm_set_median(base1, base2, median, costMtx);
            //printf("cost: %d, min: %d, max: %d, base1: %hhu, base2: %hhu, median: %d\n", cost, min, max, base1, base2, median );
        }
    }

    // now that cost matrix is set up, use it to calculate prepend and tail matrices
    for (size_t i = 0; i < costMtx->alphSize; i++) {

        cm_set_prepend(i, cm_get_cost(cm_get_gap(costMtx), i, costMtx), costMtx);
        cm_set_tail(i, cm_get_cost(i, cm_get_gap(costMtx), costMtx), costMtx);
    }
    
    cm_print (costMtx);


    mat_setup_size (algn_mtxs, seq1->len, seq2->len, 0, 0, costMtx->lcm);

    printf("Original alignment matrix before algn_nw: \n");
    print_dynmtrx( seq1, seq2, algn_mtxs );

    int deltawh = 2; // Increase in height or width of 

    algn_nw( seq1, seq2, costMtx, algn_mtxs, deltawh );
    printf("Final alignment matrix: \n");
    print_dynmtrx( seq1, seq2, algn_mtxs );

    seq_print(seq1, 1);
    seq_print(seq2, 2);

    backtrack_2d (seq1, seq2, retSeq1, retSeq2, algn_mtxs, costMtx, 0, 0, seq1->len, seq2->len, 1);

    seq_print(retSeq1, 3);
    seq_print(retSeq2, 4);

    for (SEQT *base = retSeq1->begin; base != retSeq1->end; base++) {
        printf("a: %c\n", *base);
    }
    for (SEQT *base = retSeq2->begin; base != retSeq2->end; base++) {
        printf("b: %s\n", base);
    }


    return 1;
}