#ifndef C_ALIGNMENT_INTERFACE_H
#define C_ALIGNMENT_INTERFACE_H

#include "seqAlign.h"
#include "c_code_alloc_setup.h"
#include "debug_constants.h"
#include "costMatrix.h"
#include "nwMatrices.h"
#include "seqAlign.h"

/** Input/output structure for Haskell FFI */
struct alignIO {
    SEQT *character;
    size_t length;
    size_t capacity;
};

typedef struct alignIO * alignIO_p;

/**  prints an alignIO struct */
void alignIO_print(alignIO_p character);

/** Takes in an array of values. Copies those values into an already alloced alignIO struct. Then sets length to input length and
 *  capacity to input capacity. Does not allocate.
 */
void copyValsToAIO(alignIO_p outChar, SEQT *vals, size_t length, size_t capacity) ;

/** resets an alignIO struct. Note: does not realloc or change capacity, so can only be reused if not changing allocation size. */
void resetAlignIO(alignIO_p inChar);

void freeAlignIO(alignIO_p toFree);

void allocAlignIO(alignIO_p toAlloc, size_t capacity);

/** Given an allocated seq struct, retChar, assign vals into correct positions
 *  in internal array (i.e. at end of array).
 */
void alignIOtoChar(alignIO_p input, seq_p retChar, size_t alphabetSize);

/** Do a 2d alignment. Depending on the values of last two inputs,
 *  | (0,0) = return only a cost
 *  | (0,1) = calculate gapped and ungapped characters
 *  | (1,0) = calculate union
 *  | (1,1) = calculate both union and ungapped characters.
 *
 *  In the last two cases the union will replace the gapped character placeholder.
 */
int align2d(const alignIO_p char1,
            const alignIO_p char2,
            const alignIO_p gappedOutputSeq,
            const alignIO_p ungappedOutputSeq,
            // alignIO_p unionOutputSeq,
            const cost_matrices_2d_p costMtx2d,
            int getUngapped,
            int getGapped,
            int getUnion);

/** As align2d, but affine */
int align2dAffine(alignIO_p char1,
                  alignIO_p char2,
                  alignIO_p gappedOutputSeq,
                  alignIO_p ungappedOutputSeq,
                  // alignIO_p unionOutputSeq,
                  cost_matrices_2d_p costMtx2d,
                  int doMedians);

/** Aligns three characters using non-affine algorithm.
 *  Takes in thee arrays of integer values,.
 */
int align3d(alignIO_p character1,
            alignIO_p character2,
            alignIO_p character3,
            alignIO_p medianSeq,
            cost_matrices_3d_p costMtx3d);

#endif // C_ALIGNMENT_INTERFACE_H