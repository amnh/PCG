#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "alignCharacters.h"
#include "c_code_alloc_setup.h"
#include "debug_constants.h"
#include "costMatrix.h"
#include "alignmentMatrices.h"


int distance( unsigned int const *tcm
            , size_t              alphSize
            , elem_t              nucleotide
            , elem_t              ambElem
            )
{
    int min     = INT_MAX;
    // int max     = 0;
    int curCost = 0;
    for (size_t pos = 0; pos < alphSize; pos++) {
        if (1 << pos & ambElem) { // if pos is set in ambElem, meaning pos is possible value of ambElem
            curCost = tcm[pos * alphSize + nucleotide - 1];
            if (curCost < min) {
                min = curCost;
            }
        }
    }
    return min;
}


void freeChar(dyn_character_t *toFree) {
    free(toFree->array_head);
    free(toFree);
}


void freeCostMtx(void * input, int is_2d) {

    if (is_2d) {
        free( ( (cost_matrices_2d_p *) input )->cost);
        free( ( (cost_matrices_2d_p *) input )->median);
        free( ( (cost_matrices_2d_p *) input )->worst);
        free( ( (cost_matrices_2d_p *) input )->prepend_cost);
        free( ( (cost_matrices_2d_p *) input )->tail_cost);
    } else {
        free( ( (cost_matrices_3d_t *) input )->cost);
        free( ( (cost_matrices_3d_t *) input )->median);
    }

    free (input);
}


/**
 *
 * TODO: make sure I'm actually deallocing right here.
 */
void freeNWMtx(alignment_matrices_t *input) {
    free (input->algn_costMtx);
    free (input->algn_dirMtx);
    // free (input->cube);    // don't have to deallocate these two,
    // free (input->cube_d);  // because they're just pointing to algn_costMtx and algn_dirMtx
    free (input->algn_precalcMtx);

    free(input);
}


void initializeAlignmentMtx( alignment_matrices_t *retMtx
                           , size_t                len_char1
                           , size_t                len_char2
                           , size_t                len_char3
                           , size_t                alphSize
                           )
{
    // printf("initializeAlignmentMtx\n");
    // in six following allocations all matrices are set to their shortest length because they get realloced in algnMat_setup_size
    retMtx->cap_nw          =  0; // a suitably small number to trigger realloc, but be larger than len_eff
    retMtx->cap_eff         =  0; // cap_eff was -1 so that cap_eff < cap, triggering the realloc
    retMtx->cap_pre         =  0; // again, trigger realloc

    retMtx->algn_costMtx    = malloc ( sizeof( unsigned int    ) );
    retMtx->algn_dirMtx     = malloc ( sizeof( DIR_MTX_ARROW_t ) );
    retMtx->algn_precalcMtx = malloc ( sizeof( unsigned int     ) );
    // retMtx->cube          = malloc ( sizeof( int* ) );  // don't have to allocate these two,
    // retMtx->cube_d        = malloc ( sizeof( int* ) );  // because they're just pointing to algn_costMtx and algn_dirMtx

    algnMat_setup_size (retMtx, len_char1, len_char2, len_char3, alphSize);
}


void initializeChar(dyn_character_t *retChar, size_t allocSize) {
    retChar->cap        = allocSize;                              // capacity
    retChar->array_head = calloc(allocSize, sizeof(elem_t));      // beginning of array that holds dynamic character

    retChar->end        = retChar->array_head + allocSize - 1;    // end of array
    retChar->char_begin = retChar->end;                           // position of first element in dynamic character, starts at end because
                                                                  // we'll be prepending
    retChar->len        = 0;                                      // number of elements in character
}


void resetCharValues(dyn_character_t *retChar) {
    //retChar->end   = retChar->begin + retChar->len;
    memset(retChar->array_head, 0, retChar->cap * sizeof(elem_t));
    retChar->char_begin = retChar->end;
    retChar->len       = 0;
}


void setUp2dCostMtx( cost_matrices_2d_p *retCostMtx
                   , unsigned int       *tcm
                   , size_t              alphSize
                   , unsigned int        gap_open
                   )
{

    // first allocate retMatrix
    int combinations  = 1;                     // false if matrix is sparse. In this case, it's DNA, so not sparse.
    int do_aff        = gap_open == 0 ? 0 : 3; // The 3 is because affine's cost_model_type is 3, according to my reading of ML code.
                                               // (Actually, I changed that; it used to be 2, now it's 3.)
                                               // This value set in cm_set_affine().
    int is_metric     = 1;
    elem_t all_elements = (1 << alphSize) - 1;   // Given data is DNA (plus gap), there are 2^5 - 1 possible character states

    int minCost       = INT_MAX;
    elem_t median       = 0;                     // cumulative median for 2d; combo of median1, etc., below
    int curCost;

    // int median1, median2;                      // median of a given nucleotide and current ambElem, for each ambElem

    //    int tcm2[25] = {0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0};

    //    tcm = tcm2;

    cm_alloc_2d( retCostMtx
               , alphSize
               , combinations
               , do_aff
               , gap_open
               , is_metric
               , all_elements
               );
    // Print TCM in pretty format
    if(DEBUG_MAT) {
        printf("setUp2dCostMtx\n");
        const size_t n = retCostMtx->costMatrixDimension;
        for (size_t i = 0; i < n; ++i) {
            for (size_t j = 0; j < n; ++j) {
                printf("%2d ", tcm[ n * i + j ]);
            }
            printf("\n");
        }
    }

    for (elem_t ambElem1 = 1; ambElem1 <= all_elements; ambElem1++) { // for every possible value of ambElem1, ambElem2
        for (elem_t ambElem2 = 1; ambElem2 <= all_elements; ambElem2++) {
            //curCost = 0;                // don't actually need to do this
            minCost = INT_MAX;
            median  = 0;
            // median1 = median2 = 0;

            elem_t nucleotide;
            for ( nucleotide = 1; nucleotide <= alphSize; nucleotide++) {
                curCost = distance (tcm, alphSize, nucleotide, ambElem1)
                        + distance (tcm, alphSize, nucleotide, ambElem2);
                // now seemingly recreating logic in distance(), but that was to get the cost for each
                // ambElem; now we're combining those costs get overall cost and median
                if (curCost < minCost) {
                    minCost = curCost;
                    median  = 1 << (nucleotide - 1); // median = this nucleotide, because it has the lowest cost thus far
                } else if (curCost == minCost) {
                    median |= 1 << (nucleotide - 1); // median = this nucleotide | old median
                }
            } // nucleotide
            cm_set_cost_2d   (retCostMtx, ambElem1, ambElem2, minCost);
            cm_set_median_2d (retCostMtx, ambElem1, ambElem2, median);
        } // ambElem2
    } // ambElem1
    // Gap number is alphSize - 1, which makes bit representation
    // i << (alphSize - 1), because first char value is i << 0.

    /* TODO: which of following two loops is correct? */

    elem_t gap = 1 << (alphSize - 1);
    retCostMtx->gap_char = gap;
    for ( size_t i = 1; i <= all_elements; i++) {
        cm_set_prepend_2d (retCostMtx, i, cm_get_cost_2d(retCostMtx, gap,   i));
        cm_set_tail_2d    (retCostMtx, i, cm_get_cost_2d(retCostMtx,   i, gap));
    }

    /*
    elem_t* charStart = longChar->char_begin;
    int gap        = 1 << (alphSize - 1);
    int charElem;
    for ( size_t i = 0; i < longChar->len; i++) {
        charElem = (int) *(charStart + i);
        cm_set_prepend_2d (i, cm_get_cost(gap, charElem, retCostMtx), retCostMtx);
        cm_set_tail_2d    (cm_get_cost(charElem, gap, retCostMtx), i, retCostMtx);
    } */
//    return retCostMtx;
    if(DEBUG_COST_M) {
        printf("2d:\n");
        cm_print_2d (retCostMtx);
    }
}


void setUp3dCostMtx( cost_matrices_3d_t *retMtx
                   , unsigned int       *tcm
                   , size_t              alphSize
                   , unsigned int        gap_open
                   )
{
    // first allocate retMatrix
    int combinations = 1;                     // false if matrix is sparse. In this case, it's DNA, so not sparse.
    int do_aff       = gap_open == 0 ? 0 : 3; // The 3 is because affine's cost_model_type is 3, according to my reading of ML code.
                                              // (Actually, I changed that; it used to be 2, now it's 3.)
                                              // This value set in cm_set_affine().
    // int is_metric    = 1;
    elem_t all_elements = (1 << alphSize) - 1;   // Given data is DNA (plus gap), for instance, there are 2^5 - 1 possible character states

    int minCost    = INT_MAX;
    elem_t median    = 0;        // and 3d; combos of median1, etc., below
    int curCost;

    cm_alloc_3d( retMtx
               , alphSize
               , combinations
               , do_aff
               , gap_open
               , all_elements
               );
    retMtx->gap_char = 1 << (alphSize - 1);

    for (elem_t ambElem1 = 1; ambElem1 <= all_elements; ambElem1++) { // for every possible value of ambElem1, ambElem2, ambElem3
        for (elem_t ambElem2 = 1; ambElem2 <= all_elements; ambElem2++) {
            for (elem_t ambElem3 = 1; ambElem3 <= all_elements; ambElem3++) {
                curCost = 0;                // don't actually need to do this
                minCost = INT_MAX;
                median  = 0;
                for (elem_t nucleotide = 1; nucleotide <= alphSize; nucleotide++) {
                    curCost = distance (tcm, alphSize, nucleotide, ambElem1)
                            + distance (tcm, alphSize, nucleotide, ambElem2)
                            + distance (tcm, alphSize, nucleotide, ambElem3);
                    if (curCost < minCost) {
                        minCost = curCost;
                        median  = ((elem_t) 1) << (nucleotide - 1); // median1 | median2 | median3;
                    }
                    else if (curCost == minCost) {
                        median |= ((elem_t) 1) << (nucleotide - 1); // median1 | median2 | median3;
                    }
                } // nucleotide
                // printf("%2u %2u %2u %2d %2u\n", ambElem1, ambElem2, ambElem3, minCost, median);
                cm_set_cost_3d   (retMtx, ambElem1, ambElem2, ambElem3, minCost);
                cm_set_median_3d (retMtx, ambElem1, ambElem2, ambElem3, median);
                // cm_set_worst     (ambElem1, ambElem2, max_2d,    (cost_matrices_2d_p *) retMtx);    // no worst in 3d
            } // ambElem3
        } // ambElem2
    } // ambElem1
   // return retMtx;
    if(DEBUG_COST_M) {
        printf("3d:\n");
        cm_print_3d (retMtx);
    }

}
