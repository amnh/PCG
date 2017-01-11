#ifndef C_ALIGNMENT_INTERFACE_H
#define C_ALIGNMENT_INTERFACE_H

#include "seqAlign.h"
#include "c_code_alloc_setup.h"
#include "debug_constants.h"
#include "costMatrix.h"
#include "nwMatrices.h"
#include "seqAlign.h"

struct alignIO {
    int *character;
    size_t length;
    size_t capacity;
};

typedef struct alignIO * alignIO_p;

/** Given an allocated seq struct, retChar, assign vals into correct positions
 *  in internal array (i.e. at end of array).
 */
void setChar(alignIO_p input, seq_p retChar);

/** Aligns two characters using non-affine algorithm.
 *  Takes in two arrays of integer values, as well as two previously allocated
 *  characters.
 */
int align2d(alignIO_p char1,
            alignIO_p char2,
            alignIO_p gappedOutputSeq,
            alignIO_p ungappedOutputSeq,
            alignIO_p unionOutputSeq,
            cost_matrices_2d_p costMtx2d,
            int doUnion,
            int doMedians);

/** Aligns two characters using affine algorithm.
 *  Takes in two arrays of integer values, as well as two previously allocated
 *  characters.
 */
int align2dAffine(alignIO_p char1,
                  alignIO_p char2,
                  alignIO_p gappedOutputSeq,
                  alignIO_p ungappedOutputSeq,
                  alignIO_p unionOutputSeq,
                  cost_matrices_2d_p costMtx2d,
                  int doUnion,
                  int doMedians);

/** Aligns two characters using non-affine algorithm.
 *  Takes in thee arrays of integer values, as well as three previously allocated
 *  characters.
 */
int align3d(alignIO_p character1,
            alignIO_p character2,
            alignIO_p character3,
            alignIO_p medianSeq,
            cost_matrices_3d_p costMtx3d);

#endif // C_ALIGNMENT_INTERFACE_H