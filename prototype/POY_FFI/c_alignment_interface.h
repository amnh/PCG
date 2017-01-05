#include "seqAlign.h"
#include "c_code_alloc_setup.h"
#include "debug_constants.h"
#include "costMatrix.h"
#include "nwMatrices.h"
#include "seqAlign.h"

struct alignIO {
    int *sequence;
    size_t length;
    size_t capacity;
};

typedef struct alignIO * alignIO_p;

/** Given an allocated sequence struct, retSeq, assign vals into correct positions
 *  in internal array (i.e. at end of array).
 */
void setSeq(alignIO_p input, seq_p retSeq);

/** Aligns two sequences using non-affine algorithm.
 *  Takes in two arrays of integer values, as well as two previously allocated
 *  sequences.
 */
int align2d(alignIO_p seq1,
            alignIO_p seq2,
            alignIO_p medianSeq,
            cost_matrices_2d_p costMtx2d);

/** Aligns two sequences using affine algorithm.
 *  Takes in two arrays of integer values, as well as two previously allocated
 *  sequences.
 */
int align2dAffine(alignIO_p seq1,
                  alignIO_p seq2,
                  alignIO_p medianSeq,
                  cost_matrices_2d_p costMtx2d);

/** Aligns two sequences using non-affine algorithm.
 *  Takes in thee arrays of integer values, as well as three previously allocated
 *  sequences.
 */
int align3d(alignIO_p seq1,
            alignIO_p seq2,
            alignIO_p seq3,
            alignIO_p medianSeq,
            cost_matrices_3d_p costMtx3d);
