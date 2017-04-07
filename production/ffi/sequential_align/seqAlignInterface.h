#ifndef SEQ_ALIGN_INTERFACE
#define SEQ_ALIGN_INTERFACE

#include "../memoized_tcm/dynamicCharacterOperations.h"

/** TODO: put some documentation!! */
int performSequentialAlignment(dynChar_t* seqA, dynChar_t* seqB, costMatrix_p costMatrix, alignResult_t* result);

packedChar *getMedian(const packedChar * const lhs, const packedChar * const rhs, const size_t length, const size_t alphSize, costMatrix_p costMatrix);

#endif // SEQ_ALIGN_INTERFACE
