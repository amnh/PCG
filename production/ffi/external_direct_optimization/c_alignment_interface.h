#ifndef C_ALIGNMENT_INTERFACE_H
#define C_ALIGNMENT_INTERFACE_H

#include "alignCharacters.h"
#include "c_code_alloc_setup.h"
#include "debug_constants.h"
#include "costMatrix.h"
#include "alignmentMatrices.h"
#include "alignCharacters.h"

/** Input/output structure for Haskell FFI.
 *  Note that the character will be in the last `length` elements of the array.
 */
typedef struct alignIO_t {
    elem_t *character;
    size_t length;
    size_t capacity;
} alignIO_t;


/**  prints an alignIO struct */
void alignIO_print( const alignIO_t *character );

/** Takes in an array of values. Copies those values into an already alloced alignIO struct. Then sets length to input length and
 *  capacity to input capacity. Does not allocate.
 */
void copyValsToAIO(alignIO_t *outChar, elem_t *vals, size_t length, size_t capacity);

/** resets an alignIO struct. Note: does not realloc or change capacity, so can only be reused if not changing allocation size. */
void resetAlignIO(alignIO_t *inChar);

void freeAlignIO(alignIO_t *toFree);

/** Allocates alignIO, setting length to 0 and capacity to capacity */
void allocAlignIO(alignIO_t *toAlloc, size_t capacity);

/** As allocAlignIO, but reallocs character. */
void reallocAlignIO(alignIO_t *toAlloc, size_t capacity);

/** Takes in an alignIO struct and a dyn_character struct. Copies values of alignIO to dyn_character.
 *  Points dyn_character->char_begin, dyn_character->end to respective points in alignIO->character.
 *  Adds a gap character at the front of the array, to deal with old OCaml-forced interface.
 *
 *  Nota bene: assumes that retChar->character has already been allocated correctly.
 */
void alignIOtoDynChar(       dyn_character_t *retChar
                     , const alignIO_t       *input
                     ,       size_t           alphabetSize
                     );

/** Takes in an alignIO and a dyn_character. *Copies* values of character from _end_ of dyn_character to _beginning_ of alignIO->character.
 *  Also eliminates extra gap needed by legacy code.
 */
void dynCharToAlignIO(alignIO_t *output, dyn_character_t *input);

/** Do a 2d alignment. Depending on the values of last two inputs,
 *  | (0,0) = return only a cost
 *  | (0,1) = calculate gapped and ungapped characters
 *  | (1,0) = calculate union
 *  | (1,1) = calculate both union and ungapped characters.
 *
 *  In the last two cases the union will replace the gapped character placeholder.
 */
int align2d( alignIO_t          *inputChar1_aio
           , alignIO_t          *inputChar2_aio
           , alignIO_t          *gappedOutput_aio
           , alignIO_t          *ungappedOutput_aio
           // , alignIO_t          *unionOutput_aio
           , cost_matrices_2d_t *costMtx2d
           , int                 getUngapped
           , int                 getGapped
           , int                 getUnion
           );

/** As align2d, but affine */
int align2dAffine( alignIO_t          *inputChar1_aio
                 , alignIO_t          *inputChar2_aio
                 , alignIO_t          *gappedOutput_aio
                 , alignIO_t          *ungappedOutput_aio
                 // , alignIO_t          *unionOutput_aio
                 , cost_matrices_2d_t *costMtx2d_affine
                 , int                 getMedians
                 );

/** Aligns three characters using affine algorithm.
 *  Set `gap_open_cost` to 0 for non-affine.
 */
int align3d( alignIO_t          *inputChar1_aio
           , alignIO_t          *inputChar2_aio
           , alignIO_t          *inputChar3_aio
           , alignIO_t          *ungappedOutput_aio
           , alignIO_t          *gappedOutput_aio
           // , alignment_matrices_t *algn_mtxs3d
           , cost_matrices_3d_t *costMtx3d
           , unsigned int        gap_open_cost
           // , unsigned int        gap_extension_cost
           );

#endif // C_ALIGNMENT_INTERFACE_H
