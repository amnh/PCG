#ifndef C_ALIGNMENT_INTERFACE_H
#define C_ALIGNMENT_INTERFACE_H

#include "alignCharacters.h"
#include "c_code_alloc_setup.h"
#include "debug_constants.h"
#include "costMatrix.h"
#include "nwMatrices.h"
#include "alignCharacters.h"

/** Input/output structure for Haskell FFI.
 *  Note that the character will be in the last `length` elements of the array.
 */
struct alignIO {
    elem_t *character;
    size_t length;
    size_t capacity;
};

typedef struct alignIO * alignIO_p;

/**  prints an alignIO struct */
void alignIO_print(alignIO_p character);

/** Takes in an array of values. Copies those values into an already alloced alignIO struct. Then sets length to input length and
 *  capacity to input capacity. Does not allocate.
 */
void copyValsToAIO(alignIO_p outChar, elem_t *vals, size_t length, size_t capacity);

/** resets an alignIO struct. Note: does not realloc or change capacity, so can only be reused if not changing allocation size. */
void resetAlignIO(alignIO_p inChar);

void freeAlignIO(alignIO_p toFree);

/** Allocates alignIO, setting length to 0 and capacity to capacity */
void allocAlignIO(alignIO_p toAlloc, size_t capacity);

/** As allocAlignIO, but reallocs character. */
void reallocAlignIO(alignIO_p toAlloc, size_t capacity);

/** Takes in an alignIO struct and a dyn_character struct. Copies values of alignIO to dyn_character.
 *  Points dyn_character->char_begin, dyn_character->end to respective points in alignIO->character.
 *  Adds a gap character at the front of the array, to deal with old OCaml-forced interface.
 *
 *  Nota bene: assumes that retChar->character has already been allocated correctly.
 */
void alignIOtoDynChar(dyn_char_p retChar, alignIO_p input, size_t alphabetSize);

/** Takes in an alignIO and a dyn_character. *Copies* values of character from _end_ of dyn_character to _beginning_ of alignIO->character.
 *  Also eliminates extra gap needed by legacy code.
 */
void dynCharToAlignIO(alignIO_p output, dyn_char_p input);

/** Do a 2d alignment. Depending on the values of last two inputs,
 *  | (0,0) = return only a cost
 *  | (0,1) = calculate gapped and ungapped characters
 *  | (1,0) = calculate union
 *  | (1,1) = calculate both union and ungapped characters.
 *
 *  In the last two cases the union will replace the gapped character placeholder.
 */
int align2d( const alignIO_p char1
           , const alignIO_p char2
           , const alignIO_p gappedOutputChar
           , const alignIO_p ungappedOutputChar
           // , alignIO_p unionOutputChar
           , const cost_matrices_2d_p costMtx2d
           , int getUngapped
           , int getGapped
           , int getUnion
           );

/** As align2d, but affine */
int align2dAffine( const alignIO_p char1
                 , const alignIO_p char2
                 , const alignIO_p gappedOutputChar
                 , const alignIO_p ungappedOutputChar
                 // , alignIO_p unionOutputChar
                 , const cost_matrices_2d_p costMtx2d
                 , int doMedians
                 );

/** Aligns three characters using non-affine algorithm.
 *  Takes in thee arrays of integer values,.
 */
int align3d(alignIO_p character1
           , alignIO_p character2
           , alignIO_p character3
           , alignIO_p medianChar
           , cost_matrices_3d_p costMtx3d
           );

#endif // C_ALIGNMENT_INTERFACE_H
