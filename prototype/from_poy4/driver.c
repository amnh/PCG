#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "algn.h"
// #include "array_pool.h"
// #include "cm.h"
#include "matrices.h"

int main() {
    cmt costMtx;

    matricest algn_mtxs = malloc( sizeof(struct matrices) );

    algn_mtxs->matrix      = malloc ( sizeof( int ) );
    algn_mtxs->dir_mtx_2d  = malloc ( sizeof( DIRECTION_MATRIX ) );
    algn_mtxs->pointers_3d = malloc ( sizeof( int* ) );
    algn_mtxs->cube        = malloc ( sizeof( int ) );
    algn_mtxs->cube_d      = malloc ( sizeof( DIRECTION_MATRIX ) );
    algn_mtxs->precalc     = malloc ( sizeof( int ) );  

    seqt seq1 = malloc( sizeof(struct seq) );
    seqt seq2 = malloc( sizeof(struct seq) );

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
    SEQT s1[20] = {16,1,2,4,1,1};
    SEQT s2[20] = {16,1,8,2,4,8};


    seq1->magic_number = 0; // This was only used in OCaml code
    seq1->cap          = 20; // capacity
    seq1->len          = 6;
    seq1->head         = s1;
    seq1->begin        = seq1->head; // because the beginning is at the head at first
    seq1->end          = seq1->begin + seq1->len;
    // seq1->my_pool      = pool1;

    seq2->magic_number = 1; // came from OCaml
    seq2->cap          = 20;
    seq2->len          = 6;
    seq2->head         = s2;
    seq2->begin        = seq2->head; // because the beginning is at the head at first
    seq2->end          = seq2->begin + seq2->len;
    // seq2->my_pool      = pool2;


    int alphSize     = 5; // not including gap or ambiguities
    int combinations = 1; // false if matrix is sparse. In this case, it's DNA, so not sparse.
    int do_aff       = 0;
    int gap_open     = 0;
    int is_metric    = 0;
    int all_elements = 31; // How is this used?
    costMtx          = malloc( sizeof(struct cm) );

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
            printf("i: %zu, j: %zu, cost: %lu\n", i, j, 2 * i + 2 * j);
            tcm[i + j] = 2 * i + 2 * j;
            //printf("cost: %d\n", tcm[i+j]);

            //cm_set_cost (i, j, or, costMtx);
            // printf("should be: %zu, is: %d\n", or, cm_get_cost (i,j,costMtx));
            // or += 2;
        }
    }

    int max, cost, min, noMin;
    size_t i, j;
    for (size_t base1 = 1; base1 <= 31; base1++) {
        for (size_t base2 = 1; base2 <= 31; base2++) {
            //overlap = base1 | base2;
            max = 0;
            min = INT_MAX; // this should be largest integer value.
            for (i = 1; i < costMtx->alphSize; i++) {
                if ( 1 << (i - 1) & base1) {
                    //printf("base1: %zu, i: %zu\n", base1, i);
                    for (j = 1; j < costMtx->alphSize; j++) {
                        //printf("%d\n", costMtx->alphSize);
                        if (1 << (j - 1) & base2) {
                            cost = tcm[(i - 1) * costMtx->lcm + j - 1];
                            if( cost <= min ) {
                                min = cost;
                                cm_set_cost (base1, base2, min, costMtx);
                            }
                            if( cost >= max ) {
                                max = cost;
                                cm_set_worst (base1, base2, max, costMtx);
                            }
                            
                        }
                    }
                }
                
            }
            printf("cost: %d, min: %d, max: %d, base1: %zu, base2: %zu\n", cost, min, max, base1, base2 );
        }
    }

    cm_print (costMtx);

/*
    // gap
    costMtx->cost[0] = 2;
    costMtx->cost[1] = 2;
    costMtx->cost[2] = 2;
    costMtx->cost[3] = 2;
    costMtx->cost[4] = 2;
    // A
    costMtx->cost[5] = 2;
    costMtx->cost[6] = 0;
    costMtx->cost[7] = 1;
    costMtx->cost[8] = 1;
    costMtx->cost[9] = 2;
    // C
    costMtx->cost[10] = 2;
    costMtx->cost[11] = 1;
    costMtx->cost[12] = 0;
    costMtx->cost[13] = 1;
    costMtx->cost[14] = 1;
    // G
    costMtx->cost[15] = 2;
    costMtx->cost[16] = 1;
    costMtx->cost[17] = 1;
    costMtx->cost[18] = 0;
    costMtx->cost[19] = 1;
    // T
    costMtx->cost[20] = 2;
    costMtx->cost[21] = 1;
    costMtx->cost[22] = 1;
    costMtx->cost[23] = 1;
    costMtx->cost[24] = 0;
*/
    //cm_print_matrix(costMtx->cost, alphSize + 1, alphSize + 1);

    // costMtx->gap = 4;
    // printf("%d\n", costMtx->gap);


    mat_setup_size (algn_mtxs, seq1->len, seq2->len, 0, 0, costMtx->lcm);

    // printf("Alignment matrix before algn_nw: \n");
    // print_dynmtrx( seq1, seq2, algn_mtxs );

    int deltawh = 2; // Increase in height or width of 

    algn_nw( seq1, seq2, costMtx, algn_mtxs, deltawh );
    printf("Final alignment matrix: \n");
    print_dynmtrx( seq1, seq2, algn_mtxs );

    print_seq(seq1, 1);
    print_seq(seq2, 2);

    /*if (  ) {
        printf("seq1 head: %s\nseq2 head: %s\n", seq_get_head(seq1), seq_get_head(seq2) );
        
        
    } 
    */
    //cm_free(costMtx);

    return 1;
}