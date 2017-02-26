#ifndef SEQ_ALIGN_INTERFACE
#define SEQ_ALIGN_INTERFACE

#include "dynamicCharacterOperations.h"

/** TODO: put some documentation!! */
int performSequentialAlignment(dynChar_t* seqA, dynChar_t* seqB, costMatrix_p costMatrix, alignResult_t* result);

void getMedian(alignResult_t *input, costMatrix_p *costMatrix, size_t alphSize);

#endif // SEQ_ALIGN_INTERFACE