#ifndef SEQUENTIAL_ALIGN_INTERFACE
#define SEQUENTIAL_ALIGN_INTERFACE

#include "../memoized_tcm/dynamicCharacterOperations.h"
#include "sequentialAlignOutputTypes.h"

/** Input dynamic characters are charA and charB.
 *  The order of base lookups may be important. If so, elements from charA are on the left.
 */
int performSequentialAlignment(dynChar_t* charA, dynChar_t* charB, costMatrix_p costMatrix, alignResult_t* result);

/** Looks up the median of the two packed characters in the provided cost matrix.
 *  Recall that a packedChar is an array of uint64_t: a packed series of character elements.
 *  Allocates, so returnd packedChar must be tracked.
 */
packedChar *getMedian( const packedChar *const lhs
                     , const packedChar *const rhs
                     , const size_t length
                     , const size_t alphSize
                     , costMatrix_p costMatrix
                     );

#endif // SEQUENTIAL_ALIGN_INTERFACE
