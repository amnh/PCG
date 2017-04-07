#ifndef SEQ_ALIGN_OUTPUT_TYPES
#define SEQ_ALIGN_OUTPUT_TYPES

#include "../memoized_tcm/dynamicCharacterOperations.h"

/** alignResult_t is where results get put for return to Haskell. For further notes see retType_t */
typedef struct alignResult_t {
    size_t      finalCost;
    size_t      finalLength;
    packedChar *finalChar1;
    packedChar *finalChar2;
    packedChar *medianChar;
} alignResult_t;

/** retType_t differs from alignResult_t in that it's the return type from the C sequence alignment code,
 *  which doesn't return packedChars, but ints. Also, the output from the sequence alignment gets post-processed
 *  to create a median, which is placed in medianChar in alignResult_t.
 */
typedef struct retType_t {
    int       cost;
    uint64_t *seq1;
    size_t    seq1Len;
    uint64_t *seq2;
    size_t    seq2Len;
    size_t    alignmentLength;
} retType_t;

#endif // SEQ_ALIGN_OUTPUT_TYPES