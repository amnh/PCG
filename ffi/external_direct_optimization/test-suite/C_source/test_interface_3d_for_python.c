/** Tests external DO code under various conditions:
    1. with varying alphabet lengths
    2. with random-length characters
    3. with varying character patterns
 */

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "../../alignCharacters.h"
#include "../../alignmentMatrices.h"
#include "../../c_alignment_interface.h"
#include "../../c_code_alloc_setup.h"
#include "../../costMatrix.h"
#include "../../debug_constants.h"


int power_2 (int input)
{
    return (__builtin_popcount(input) == 1);
}


void set_vals( elem_t *vals, size_t vals_length, size_t max_val)
{
    elem_t curVal;
    for (size_t k = 0; k < vals_length; k++ ) {
        curVal = rand() % (max_val);

        while (curVal == 0) curVal = rand() % (max_val);

        vals[k] = curVal;
    }
}


int wrapperFunction( elem_t *input_vals1
                   , size_t  lenChar1
                   , elem_t *input_vals2
                   , size_t  lenChar2
                   , elem_t *input_vals3
                   , size_t  lenChar3
                   )
{

/******************************** set up and allocate all variables and structs ************************************/

    size_t i, j;   // various loop indices

    const size_t MAXLENGTH = lenChar1 + lenChar2 + lenChar3; // Max length of an aligned character
                                                             // given three chars to align.

    // inputs
    alignIO_t *powellInput1 = malloc( sizeof(struct alignIO_t) );    // inputs to align2d fn.
    alignIO_t *powellInput2 = malloc( sizeof(struct alignIO_t) );    // inputs to align2d fn.
    alignIO_t *powellInput3 = malloc( sizeof(struct alignIO_t) );    // additional input to align3d fn.
    assert(   powellInput1 != NULL
           && powellInput2 != NULL
           && powellInput3 != NULL
           && "Couldn't allocate powell inputs in 3D interface test." );

   // set to 1 so I can realloc later
    allocAlignIO( powellInput1, 1 );
    allocAlignIO( powellInput3, 1 );
    allocAlignIO( powellInput2, 1 );

    // aligned outputs
    alignIO_t *powellOutput1 = malloc( sizeof(struct alignIO_t) );    // Outputs from 3d alignment
    alignIO_t *powellOutput2 = malloc( sizeof(struct alignIO_t) );    //
    alignIO_t *powellOutput3 = malloc( sizeof(struct alignIO_t) );    //
    assert(   powellOutput1 != NULL
           && powellOutput2 != NULL
           && powellOutput3 != NULL
           && "Couldn't allocate powell outputs in 3D interface test." );

    allocAlignIO( powellOutput1, 1 );
    allocAlignIO( powellOutput2, 1 );
    allocAlignIO( powellOutput3, 1 );

    // medians
    alignIO_t *ungappedMedianChar = malloc( sizeof(struct alignIO_t) );
    alignIO_t *gappedMedianChar   = malloc( sizeof(struct alignIO_t) );
    assert(   ungappedMedianChar != NULL
           && gappedMedianChar   != NULL
           && "Couldn't allocate median characters in 3D interface test." );
    // alignIO_t *unionMedianChar    = malloc( sizeof(struct alignIO_t) );

    allocAlignIO( ungappedMedianChar, 1 );
    allocAlignIO( gappedMedianChar,   1 );

    /************  Allocate cost matrices  **************/

    size_t alphSize = 5;   // includes gap

    size_t tcm_total_len = alphSize * alphSize;
    // const size_t CHAR_LENGTH = 100;

    int algnCost;

    /** TCM is only for non-ambiguous nucleotides, and it used to generate
     *  the entire cost matrix, which includes ambiguous elements.
     *  TCM is row-major, with each row being the left character element.
     *  It is therefore indexed not by powers of two, but by cardinal integer.
     *  This particular example is both metric and symmetric. All TCMs must be
     *  symmetric. Metricity is decided by PCG application.
     */
    unsigned int *tcm = calloc(tcm_total_len, sizeof(int)); // this is the input tcm, not the generated one
    assert( tcm != NULL && "Couldn't allocate simple tcm." );
    for (i = 0; i < tcm_total_len; i += alphSize) {
        //printf("i: %zu\n", i);
        for (j = 0; j < alphSize; j++) {
            //tcm[i + j] = 2 * i + 2 * j;
            if ( i == j * alphSize ) {
                tcm[i + j] = IDENTITY_COST;    // identity
                // printf("i: %zu, j: %zu, cost: %d\n", i, j, IDENTITY_COST);
            } else if (i == (tcm_total_len - alphSize) || j == (alphSize - 1)) {
                tcm[i + j] = INDEL_COST;       // indel cost
                // printf("i: %zu, j: %zu, cost: %d\n", i, j, INDEL_COST);
            } else {
                tcm[i + j] = SUB_COST;         // sub cost
                // printf("i: %zu, j: %zu, cost: %d\n", i, j, SUB_COST);
            }
         }
    }

    elem_t one      = 1;
    elem_t gap_char = one << (alphSize - 1);

    cost_matrices_3d_t *costMtx3d = malloc(sizeof(struct cost_matrices_3d_t));
    assert( costMtx3d != NULL && "Couldn't allocate simple costMtx3d." );
    setUp3dCostMtx (costMtx3d, tcm, alphSize, 0);


/************************************************ Do 3d alignment *************************************************/

    reallocAlignIO( powellInput1, MAXLENGTH );
    reallocAlignIO( powellInput2, MAXLENGTH );
    reallocAlignIO( powellInput3, MAXLENGTH );

    reallocAlignIO( ungappedMedianChar, MAXLENGTH );
    reallocAlignIO( gappedMedianChar,   MAXLENGTH );

    copyValsToAIO( powellInput1, input_vals1, lenChar1, MAXLENGTH );
    copyValsToAIO( powellInput2, input_vals2, lenChar2, MAXLENGTH );
    copyValsToAIO( powellInput3, input_vals3, lenChar3, MAXLENGTH );

    printf("\n\n\n******************** Align 3 characters **********************\n\n");
    printf(      "*****************  Original 3d characters:  ******************\n");
    alignIO_print( powellInput1 );
    alignIO_print( powellInput2 );
    alignIO_print( powellInput3 );

    algnCost = align3d( powellInput1
                      , powellInput2
                      , powellInput3
                      , powellOutput1
                      , powellOutput2
                      , powellOutput3
                      , ungappedMedianChar
                      , gappedMedianChar
                      , costMtx3d
                      , 1        // substitution cost
                      , 2        // gap open cost > 1 == affine
                      , 1        // gap extension cost
                      );

    printf( "alphSize: %zu\n", alphSize );
    printf( "gap char: %u\n",  gap_char );

    printf( "\nAligned 3d characters\n" );
    alignIO_print( powellOutput1 );
    alignIO_print( powellOutput2 );
    alignIO_print( powellOutput3 );

    printf( "Alignment cost: %d\n", algnCost );

    printf( "\nGapped median\n  " );
    alignIO_print( gappedMedianChar );

    printf( "\nUngapped median\n  " );
    alignIO_print( ungappedMedianChar );

    printf("\n\n\n");

    freeCostMtx( costMtx3d, 0 );  // 0 is !2d

    return 0;
}
